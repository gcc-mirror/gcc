/* Pass manager for Fortran front end.
   Copyright (C) 2010 Free Software Foundation, Inc.
   Contributed by Thomas König.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "gfortran.h"
#include "arith.h"
#include "flags.h"
#include "dependency.h"
#include "constructor.h"
#include "opts.h"

/* Forward declarations.  */

static void strip_function_call (gfc_expr *);
static void optimize_namespace (gfc_namespace *);
static void optimize_assignment (gfc_code *);
static bool optimize_op (gfc_expr *);
static bool optimize_comparison (gfc_expr *, gfc_intrinsic_op);
static bool optimize_trim (gfc_expr *);

/* How deep we are inside an argument list.  */

static int count_arglist;

/* Entry point - run all passes for a namespace.  So far, only an
   optimization pass is run.  */

void
gfc_run_passes (gfc_namespace *ns)
{
  if (optimize)
    {
      optimize_namespace (ns);
      if (gfc_option.dump_fortran_optimized)
	gfc_dump_parse_tree (ns, stdout);
    }
}

/* Callback for each gfc_code node invoked through gfc_code_walker
   from optimize_namespace.  */

static int
optimize_code (gfc_code **c, int *walk_subtrees ATTRIBUTE_UNUSED,
	       void *data ATTRIBUTE_UNUSED)
{

  gfc_exec_op op;

  op = (*c)->op;

  if (op == EXEC_CALL || op == EXEC_COMPCALL || op == EXEC_ASSIGN_CALL
      || op == EXEC_CALL_PPC)
    count_arglist = 1;
  else
    count_arglist = 0;

  if (op == EXEC_ASSIGN)
    optimize_assignment (*c);
  return 0;
}

/* Callback for each gfc_expr node invoked through gfc_code_walker
   from optimize_namespace.  */

static int
optimize_expr (gfc_expr **e, int *walk_subtrees ATTRIBUTE_UNUSED,
	       void *data ATTRIBUTE_UNUSED)
{
  bool function_expr;

  if ((*e)->expr_type == EXPR_FUNCTION)
    {
      count_arglist ++;
      function_expr = true;
    }
  else
    function_expr = false;

  if (optimize_trim (*e))
    gfc_simplify_expr (*e, 0);

  if ((*e)->expr_type == EXPR_OP && optimize_op (*e))
    gfc_simplify_expr (*e, 0);

  if (function_expr)
    count_arglist --;

  return 0;
}

/* Optimize a namespace, including all contained namespaces.  */

static void
optimize_namespace (gfc_namespace *ns)
{
  gfc_code_walker (&ns->code, optimize_code, optimize_expr, NULL);

  for (ns = ns->contained; ns; ns = ns->sibling)
    optimize_namespace (ns);
}

/* Replace code like
   a = matmul(b,c) + d
   with
   a = matmul(b,c) ;   a = a + d
   where the array function is not elemental and not allocatable
   and does not depend on the left-hand side.
*/

static bool
optimize_binop_array_assignment (gfc_code *c, gfc_expr **rhs, bool seen_op)
{
  gfc_expr *e;

  e = *rhs;
  if (e->expr_type == EXPR_OP)
    {
      switch (e->value.op.op)
	{
	  /* Unary operators and exponentiation: Only look at a single
	     operand.  */
	case INTRINSIC_NOT:
	case INTRINSIC_UPLUS:
	case INTRINSIC_UMINUS:
	case INTRINSIC_PARENTHESES:
	case INTRINSIC_POWER:
	  if (optimize_binop_array_assignment (c, &e->value.op.op1, seen_op))
	    return true;
	  break;

	default:
	  /* Binary operators.  */
	  if (optimize_binop_array_assignment (c, &e->value.op.op1, true))
	    return true;

	  if (optimize_binop_array_assignment (c, &e->value.op.op2, true))
	    return true;

	  break;
	}
    }
  else if (seen_op && e->expr_type == EXPR_FUNCTION && e->rank > 0
	   && ! (e->value.function.esym 
		 && (e->value.function.esym->attr.elemental 
		     || e->value.function.esym->attr.allocatable
		     || e->value.function.esym->ts.type != c->expr1->ts.type
		     || e->value.function.esym->ts.kind != c->expr1->ts.kind))
	   && ! (e->value.function.isym
		 && (e->value.function.isym->elemental
		     || e->ts.type != c->expr1->ts.type
		     || e->ts.kind != c->expr1->ts.kind)))
    {

      gfc_code *n;
      gfc_expr *new_expr;

      /* Insert a new assignment statement after the current one.  */
      n = XCNEW (gfc_code);
      n->op = EXEC_ASSIGN;
      n->loc = c->loc;
      n->next = c->next;
      c->next = n;

      n->expr1 = gfc_copy_expr (c->expr1);
      n->expr2 = c->expr2;
      new_expr = gfc_copy_expr (c->expr1);
      c->expr2 = e;
      *rhs = new_expr;
      
      return true;

    }

  /* Nothing to optimize.  */
  return false;
}

/* Optimizations for an assignment.  */

static void
optimize_assignment (gfc_code * c)
{
  gfc_expr *lhs, *rhs;

  lhs = c->expr1;
  rhs = c->expr2;

  /* Optimize away a = trim(b), where a is a character variable.  */

  if (lhs->ts.type == BT_CHARACTER)
    {
      if (rhs->expr_type == EXPR_FUNCTION &&
	  rhs->value.function.isym &&
	  rhs->value.function.isym->id == GFC_ISYM_TRIM)
	{
	  strip_function_call (rhs);
	  optimize_assignment (c);
	  return;
	}
    }

  if (lhs->rank > 0 && gfc_check_dependency (lhs, rhs, true) == 0)
    optimize_binop_array_assignment (c, &rhs, false);
}


/* Remove an unneeded function call, modifying the expression.
   This replaces the function call with the value of its
   first argument.  The rest of the argument list is freed.  */

static void
strip_function_call (gfc_expr *e)
{
  gfc_expr *e1;
  gfc_actual_arglist *a;

  a = e->value.function.actual;

  /* We should have at least one argument.  */
  gcc_assert (a->expr != NULL);

  e1 = a->expr;

  /* Free the remaining arglist, if any.  */
  if (a->next)
    gfc_free_actual_arglist (a->next);

  /* Graft the argument expression onto the original function.  */
  *e = *e1;
  gfc_free (e1);

}

/* Recursive optimization of operators.  */

static bool
optimize_op (gfc_expr *e)
{
  gfc_intrinsic_op op = e->value.op.op;

  switch (op)
    {
    case INTRINSIC_EQ:
    case INTRINSIC_EQ_OS:
    case INTRINSIC_GE:
    case INTRINSIC_GE_OS:
    case INTRINSIC_LE:
    case INTRINSIC_LE_OS:
    case INTRINSIC_NE:
    case INTRINSIC_NE_OS:
    case INTRINSIC_GT:
    case INTRINSIC_GT_OS:
    case INTRINSIC_LT:
    case INTRINSIC_LT_OS:
      return optimize_comparison (e, op);

    default:
      break;
    }

  return false;
}

/* Optimize expressions for equality.  */

static bool
optimize_comparison (gfc_expr *e, gfc_intrinsic_op op)
{
  gfc_expr *op1, *op2;
  bool change;
  int eq;
  bool result;

  op1 = e->value.op.op1;
  op2 = e->value.op.op2;

  /* Strip off unneeded TRIM calls from string comparisons.  */

  change = false;

  if (op1->expr_type == EXPR_FUNCTION 
      && op1->value.function.isym
      && op1->value.function.isym->id == GFC_ISYM_TRIM)
    {
      strip_function_call (op1);
      change = true;
    }

  if (op2->expr_type == EXPR_FUNCTION 
      && op2->value.function.isym
      && op2->value.function.isym->id == GFC_ISYM_TRIM)
    {
      strip_function_call (op2);
      change = true;
    }

  if (change)
    {
      optimize_comparison (e, op);
      return true;
    }

  /* An expression of type EXPR_CONSTANT is only valid for scalars.  */
  /* TODO: A scalar constant may be acceptable in some cases (the scalarizer
     handles them well). However, there are also cases that need a non-scalar
     argument. For example the any intrinsic. See PR 45380.  */
  if (e->rank > 0)
    return false;

  /* Don't compare REAL or COMPLEX expressions when honoring NaNs.  */

  if (flag_finite_math_only
      || (op1->ts.type != BT_REAL && op2->ts.type != BT_REAL
	  && op1->ts.type != BT_COMPLEX && op2->ts.type != BT_COMPLEX))
    {
      eq = gfc_dep_compare_expr (op1, op2);
      if (eq == -2)
	{
	  /* Replace A // B < A // C with B < C, and A // B < C // B
	     with A < C.  */
	  if (op1->ts.type == BT_CHARACTER && op2->ts.type == BT_CHARACTER
	      && op1->value.op.op == INTRINSIC_CONCAT
	      && op2->value.op.op == INTRINSIC_CONCAT)
	    {
	      gfc_expr *op1_left = op1->value.op.op1;
	      gfc_expr *op2_left = op2->value.op.op1;
	      gfc_expr *op1_right = op1->value.op.op2;
	      gfc_expr *op2_right = op2->value.op.op2;

	      if (gfc_dep_compare_expr (op1_left, op2_left) == 0)
		{
		  /* Watch out for 'A ' // x vs. 'A' // x.  */

		  if (op1_left->expr_type == EXPR_CONSTANT
			&& op2_left->expr_type == EXPR_CONSTANT
			&& op1_left->value.character.length
			   != op2_left->value.character.length)
		    return -2;
		  else
		    {
		      gfc_free (op1_left);
		      gfc_free (op2_left);
		      e->value.op.op1 = op1_right;
		      e->value.op.op2 = op2_right;
		      optimize_comparison (e, op);
		      return true;
		    }
		}
	      if (gfc_dep_compare_expr (op1_right, op2_right) == 0)
		{
		  gfc_free (op1_right);
		  gfc_free (op2_right);
		  e->value.op.op1 = op1_left;
		  e->value.op.op2 = op2_left;
		  optimize_comparison (e, op);
		  return true;
		}
	    }
	}
      else
	{
	  /* eq can only be -1, 0 or 1 at this point.  */
	  switch (op)
	    {
	    case INTRINSIC_EQ:
	    case INTRINSIC_EQ_OS:
	      result = eq == 0;
	      break;
	      
	    case INTRINSIC_GE:
	    case INTRINSIC_GE_OS:
	      result = eq >= 0;
	      break;

	    case INTRINSIC_LE:
	    case INTRINSIC_LE_OS:
	      result = eq <= 0;
	      break;

	    case INTRINSIC_NE:
	    case INTRINSIC_NE_OS:
	      result = eq != 0;
	      break;

	    case INTRINSIC_GT:
	    case INTRINSIC_GT_OS:
	      result = eq > 0;
	      break;

	    case INTRINSIC_LT:
	    case INTRINSIC_LT_OS:
	      result = eq < 0;
	      break;
	      
	    default:
	      gfc_internal_error ("illegal OP in optimize_comparison");
	      break;
	    }

	  /* Replace the expression by a constant expression.  The typespec
	     and where remains the way it is.  */
	  gfc_free (op1);
	  gfc_free (op2);
	  e->expr_type = EXPR_CONSTANT;
	  e->value.logical = result;
	  return true;
	}
    }

  return false;
}

/* Optimize a trim function by replacing it with an equivalent substring
   involving a call to len_trim.  This only works for expressions where
   variables are trimmed.  Return true if anything was modified.  */

static bool
optimize_trim (gfc_expr *e)
{
  gfc_expr *a;
  gfc_ref *ref;
  gfc_expr *fcn;
  gfc_actual_arglist *actual_arglist, *next;

  /* Don't do this optimization within an argument list, because
     otherwise aliasing issues may occur.  */

  if (count_arglist != 1)
    return false;

  if (e->ts.type != BT_CHARACTER || e->expr_type != EXPR_FUNCTION
      || e->value.function.isym == NULL
      || e->value.function.isym->id != GFC_ISYM_TRIM)
    return false;

  a = e->value.function.actual->expr;

  if (a->expr_type != EXPR_VARIABLE)
    return false;

  if (a->ref)
    {
      /* FIXME - also handle substring references, by modifying the
	 reference itself.  Make sure not to evaluate functions in
	 the references twice.  */
      return false;
    }
  else
    {
      strip_function_call (e);

      /* Create the reference.  */

      ref = gfc_get_ref ();
      ref->type = REF_SUBSTRING;

      /* Set the start of the reference.  */

      ref->u.ss.start = gfc_get_int_expr (gfc_default_integer_kind, NULL, 1);

      /* Build the function call to len_trim(x, gfc_defaul_integer_kind).  */

      fcn = gfc_get_expr ();
      fcn->expr_type = EXPR_FUNCTION;
      fcn->value.function.isym =
	gfc_intrinsic_function_by_id (GFC_ISYM_LEN_TRIM);
      actual_arglist = gfc_get_actual_arglist ();
      actual_arglist->expr = gfc_copy_expr (e);
      next = gfc_get_actual_arglist ();
      next->expr = gfc_get_int_expr (gfc_default_integer_kind, NULL,
				     gfc_default_integer_kind);
      actual_arglist->next = next;
      fcn->value.function.actual = actual_arglist;

      /* Set the end of the reference to the call to len_trim.  */

      ref->u.ss.end = fcn;
      e->ref = ref;
      return true;
    }
}

#define WALK_SUBEXPR(NODE) \
  do							\
    {							\
      result = gfc_expr_walker (&(NODE), exprfn, data);	\
      if (result)					\
	return result;					\
    }							\
  while (0)
#define WALK_SUBEXPR_TAIL(NODE) e = &(NODE); continue

/* Walk expression *E, calling EXPRFN on each expression in it.  */

int
gfc_expr_walker (gfc_expr **e, walk_expr_fn_t exprfn, void *data)
{
  while (*e)
    {
      int walk_subtrees = 1;
      gfc_actual_arglist *a;
      gfc_ref *r;
      gfc_constructor *c;

      int result = exprfn (e, &walk_subtrees, data);
      if (result)
	return result;
      if (walk_subtrees)
	switch ((*e)->expr_type)
	  {
	  case EXPR_OP:
	    WALK_SUBEXPR ((*e)->value.op.op1);
	    WALK_SUBEXPR_TAIL ((*e)->value.op.op2);
	    break;
	  case EXPR_FUNCTION:
	    for (a = (*e)->value.function.actual; a; a = a->next)
	      WALK_SUBEXPR (a->expr);
	    break;
	  case EXPR_COMPCALL:
	  case EXPR_PPC:
	    WALK_SUBEXPR ((*e)->value.compcall.base_object);
	    for (a = (*e)->value.compcall.actual; a; a = a->next)
	      WALK_SUBEXPR (a->expr);
	    break;

	  case EXPR_STRUCTURE:
	  case EXPR_ARRAY:
	    for (c = gfc_constructor_first ((*e)->value.constructor); c;
		 c = gfc_constructor_next (c))
	      {
		WALK_SUBEXPR (c->expr);
		if (c->iterator != NULL)
		  {
		    WALK_SUBEXPR (c->iterator->var);
		    WALK_SUBEXPR (c->iterator->start);
		    WALK_SUBEXPR (c->iterator->end);
		    WALK_SUBEXPR (c->iterator->step);
		  }
	      }

	    if ((*e)->expr_type != EXPR_ARRAY)
	      break;

	    /* Fall through to the variable case in order to walk the
	       the reference.  */

	  case EXPR_SUBSTRING:
	  case EXPR_VARIABLE:
	    for (r = (*e)->ref; r; r = r->next)
	      {
		gfc_array_ref *ar;
		int i;

		switch (r->type)
		  {
		  case REF_ARRAY:
		    ar = &r->u.ar;
		    if (ar->type == AR_SECTION || ar->type == AR_ELEMENT)
		      {
			for (i=0; i< ar->dimen; i++)
			  {
			    WALK_SUBEXPR (ar->start[i]);
			    WALK_SUBEXPR (ar->end[i]);
			    WALK_SUBEXPR (ar->stride[i]);
			  }
		      }

		    break;

		  case REF_SUBSTRING:
		    WALK_SUBEXPR (r->u.ss.start);
		    WALK_SUBEXPR (r->u.ss.end);
		    break;

		  case REF_COMPONENT:
		    break;
		  }
	      }

	  default:
	    break;
	  }
      return 0;
    }
  return 0;
}

#define WALK_SUBCODE(NODE) \
  do								\
    {								\
      result = gfc_code_walker (&(NODE), codefn, exprfn, data);	\
      if (result)						\
	return result;						\
    }								\
  while (0)

/* Walk code *C, calling CODEFN on each gfc_code node in it and calling EXPRFN
   on each expression in it.  If any of the hooks returns non-zero, that
   value is immediately returned.  If the hook sets *WALK_SUBTREES to 0,
   no subcodes or subexpressions are traversed.  */

int
gfc_code_walker (gfc_code **c, walk_code_fn_t codefn, walk_expr_fn_t exprfn,
		 void *data)
{
  for (; *c; c = &(*c)->next)
    {
      int walk_subtrees = 1;
      int result = codefn (c, &walk_subtrees, data);
      if (result)
	return result;

      if (walk_subtrees)
	{
	  gfc_code *b;
	  gfc_actual_arglist *a;

	  switch ((*c)->op)
	    {
	    case EXEC_DO:
	      WALK_SUBEXPR ((*c)->ext.iterator->var);
	      WALK_SUBEXPR ((*c)->ext.iterator->start);
	      WALK_SUBEXPR ((*c)->ext.iterator->end);
	      WALK_SUBEXPR ((*c)->ext.iterator->step);
	      break;

	    case EXEC_CALL:
	    case EXEC_ASSIGN_CALL:
	      for (a = (*c)->ext.actual; a; a = a->next)
		WALK_SUBEXPR (a->expr);
	      break;

	    case EXEC_CALL_PPC:
	      WALK_SUBEXPR ((*c)->expr1);
	      for (a = (*c)->ext.actual; a; a = a->next)
		WALK_SUBEXPR (a->expr);
	      break;

	    case EXEC_SELECT:
	      WALK_SUBEXPR ((*c)->expr1);
	      for (b = (*c)->block; b; b = b->block)
		{
		  gfc_case *cp;
		  for (cp = b->ext.block.case_list; cp; cp = cp->next)
		    {
		      WALK_SUBEXPR (cp->low);
		      WALK_SUBEXPR (cp->high);
		    }
		  WALK_SUBCODE (b->next);
		}
	      continue;

	    case EXEC_ALLOCATE:
	    case EXEC_DEALLOCATE:
	      {
		gfc_alloc *a;
		for (a = (*c)->ext.alloc.list; a; a = a->next)
		  WALK_SUBEXPR (a->expr);
		break;
	      }

	    case EXEC_FORALL:
	      {
		gfc_forall_iterator *fa;
		for (fa = (*c)->ext.forall_iterator; fa; fa = fa->next)
		  {
		    WALK_SUBEXPR (fa->var);
		    WALK_SUBEXPR (fa->start);
		    WALK_SUBEXPR (fa->end);
		    WALK_SUBEXPR (fa->stride);
		  }
		break;
	      }

	    case EXEC_OPEN:
	      WALK_SUBEXPR ((*c)->ext.open->unit);
	      WALK_SUBEXPR ((*c)->ext.open->file);
	      WALK_SUBEXPR ((*c)->ext.open->status);
	      WALK_SUBEXPR ((*c)->ext.open->access);
	      WALK_SUBEXPR ((*c)->ext.open->form);
	      WALK_SUBEXPR ((*c)->ext.open->recl);
	      WALK_SUBEXPR ((*c)->ext.open->blank);
	      WALK_SUBEXPR ((*c)->ext.open->position);
	      WALK_SUBEXPR ((*c)->ext.open->action);
	      WALK_SUBEXPR ((*c)->ext.open->delim);
	      WALK_SUBEXPR ((*c)->ext.open->pad);
	      WALK_SUBEXPR ((*c)->ext.open->iostat);
	      WALK_SUBEXPR ((*c)->ext.open->iomsg);
	      WALK_SUBEXPR ((*c)->ext.open->convert);
	      WALK_SUBEXPR ((*c)->ext.open->decimal);
	      WALK_SUBEXPR ((*c)->ext.open->encoding);
	      WALK_SUBEXPR ((*c)->ext.open->round);
	      WALK_SUBEXPR ((*c)->ext.open->sign);
	      WALK_SUBEXPR ((*c)->ext.open->asynchronous);
	      WALK_SUBEXPR ((*c)->ext.open->id);
	      WALK_SUBEXPR ((*c)->ext.open->newunit);
	      break;

	    case EXEC_CLOSE:
	      WALK_SUBEXPR ((*c)->ext.close->unit);
	      WALK_SUBEXPR ((*c)->ext.close->status);
	      WALK_SUBEXPR ((*c)->ext.close->iostat);
	      WALK_SUBEXPR ((*c)->ext.close->iomsg);
	      break;

	    case EXEC_BACKSPACE:
	    case EXEC_ENDFILE:
	    case EXEC_REWIND:
	    case EXEC_FLUSH:
	      WALK_SUBEXPR ((*c)->ext.filepos->unit);
	      WALK_SUBEXPR ((*c)->ext.filepos->iostat);
	      WALK_SUBEXPR ((*c)->ext.filepos->iomsg);
	      break;

	    case EXEC_INQUIRE:
	      WALK_SUBEXPR ((*c)->ext.inquire->unit);
	      WALK_SUBEXPR ((*c)->ext.inquire->file);
	      WALK_SUBEXPR ((*c)->ext.inquire->iomsg);
	      WALK_SUBEXPR ((*c)->ext.inquire->iostat);
	      WALK_SUBEXPR ((*c)->ext.inquire->exist);
	      WALK_SUBEXPR ((*c)->ext.inquire->opened);
	      WALK_SUBEXPR ((*c)->ext.inquire->number);
	      WALK_SUBEXPR ((*c)->ext.inquire->named);
	      WALK_SUBEXPR ((*c)->ext.inquire->name);
	      WALK_SUBEXPR ((*c)->ext.inquire->access);
	      WALK_SUBEXPR ((*c)->ext.inquire->sequential);
	      WALK_SUBEXPR ((*c)->ext.inquire->direct);
	      WALK_SUBEXPR ((*c)->ext.inquire->form);
	      WALK_SUBEXPR ((*c)->ext.inquire->formatted);
	      WALK_SUBEXPR ((*c)->ext.inquire->unformatted);
	      WALK_SUBEXPR ((*c)->ext.inquire->recl);
	      WALK_SUBEXPR ((*c)->ext.inquire->nextrec);
	      WALK_SUBEXPR ((*c)->ext.inquire->blank);
	      WALK_SUBEXPR ((*c)->ext.inquire->position);
	      WALK_SUBEXPR ((*c)->ext.inquire->action);
	      WALK_SUBEXPR ((*c)->ext.inquire->read);
	      WALK_SUBEXPR ((*c)->ext.inquire->write);
	      WALK_SUBEXPR ((*c)->ext.inquire->readwrite);
	      WALK_SUBEXPR ((*c)->ext.inquire->delim);
	      WALK_SUBEXPR ((*c)->ext.inquire->encoding);
	      WALK_SUBEXPR ((*c)->ext.inquire->pad);
	      WALK_SUBEXPR ((*c)->ext.inquire->iolength);
	      WALK_SUBEXPR ((*c)->ext.inquire->convert);
	      WALK_SUBEXPR ((*c)->ext.inquire->strm_pos);
	      WALK_SUBEXPR ((*c)->ext.inquire->asynchronous);
	      WALK_SUBEXPR ((*c)->ext.inquire->decimal);
	      WALK_SUBEXPR ((*c)->ext.inquire->pending);
	      WALK_SUBEXPR ((*c)->ext.inquire->id);
	      WALK_SUBEXPR ((*c)->ext.inquire->sign);
	      WALK_SUBEXPR ((*c)->ext.inquire->size);
	      WALK_SUBEXPR ((*c)->ext.inquire->round);
	      break;

	    case EXEC_WAIT:
	      WALK_SUBEXPR ((*c)->ext.wait->unit);
	      WALK_SUBEXPR ((*c)->ext.wait->iostat);
	      WALK_SUBEXPR ((*c)->ext.wait->iomsg);
	      WALK_SUBEXPR ((*c)->ext.wait->id);
	      break;

	    case EXEC_READ:
	    case EXEC_WRITE:
	      WALK_SUBEXPR ((*c)->ext.dt->io_unit);
	      WALK_SUBEXPR ((*c)->ext.dt->format_expr);
	      WALK_SUBEXPR ((*c)->ext.dt->rec);
	      WALK_SUBEXPR ((*c)->ext.dt->advance);
	      WALK_SUBEXPR ((*c)->ext.dt->iostat);
	      WALK_SUBEXPR ((*c)->ext.dt->size);
	      WALK_SUBEXPR ((*c)->ext.dt->iomsg);
	      WALK_SUBEXPR ((*c)->ext.dt->id);
	      WALK_SUBEXPR ((*c)->ext.dt->pos);
	      WALK_SUBEXPR ((*c)->ext.dt->asynchronous);
	      WALK_SUBEXPR ((*c)->ext.dt->blank);
	      WALK_SUBEXPR ((*c)->ext.dt->decimal);
	      WALK_SUBEXPR ((*c)->ext.dt->delim);
	      WALK_SUBEXPR ((*c)->ext.dt->pad);
	      WALK_SUBEXPR ((*c)->ext.dt->round);
	      WALK_SUBEXPR ((*c)->ext.dt->sign);
	      WALK_SUBEXPR ((*c)->ext.dt->extra_comma);
	      break;

	    case EXEC_OMP_DO:
	    case EXEC_OMP_PARALLEL:
	    case EXEC_OMP_PARALLEL_DO:
	    case EXEC_OMP_PARALLEL_SECTIONS:
	    case EXEC_OMP_PARALLEL_WORKSHARE:
	    case EXEC_OMP_SECTIONS:
	    case EXEC_OMP_SINGLE:
	    case EXEC_OMP_WORKSHARE:
	    case EXEC_OMP_END_SINGLE:
	    case EXEC_OMP_TASK:
	      if ((*c)->ext.omp_clauses)
		{
		  WALK_SUBEXPR ((*c)->ext.omp_clauses->if_expr);
		  WALK_SUBEXPR ((*c)->ext.omp_clauses->num_threads);
		  WALK_SUBEXPR ((*c)->ext.omp_clauses->chunk_size);
		}
	      break;
	    default:
	      break;
	    }

	  WALK_SUBEXPR ((*c)->expr1);
	  WALK_SUBEXPR ((*c)->expr2);
	  WALK_SUBEXPR ((*c)->expr3);
	  for (b = (*c)->block; b; b = b->block)
	    {
	      WALK_SUBEXPR (b->expr1);
	      WALK_SUBEXPR (b->expr2);
	      WALK_SUBCODE (b->next);
	    }
	}
    }
  return 0;
}
