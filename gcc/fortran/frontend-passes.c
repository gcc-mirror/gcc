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

/* Forward declarations.  */

static void strip_function_call (gfc_expr *);
static void optimize_assignment (gfc_code *);
static void optimize_expr_0 (gfc_expr *);
static bool optimize_expr (gfc_expr *);
static bool optimize_op (gfc_expr *);
static bool optimize_equality (gfc_expr *, bool);
static void optimize_code (gfc_code *);
static void optimize_code_node (gfc_code *);
static void optimize_actual_arglist (gfc_actual_arglist *);

/* Entry point - run all passes for a namespace.  So far, only an
   optimization pass is run.  */

void
gfc_run_passes (gfc_namespace * ns)
{
  if (optimize)
    optimize_code (ns->code);
}

static void
optimize_code (gfc_code *c)
{
  for (; c; c = c->next)
    optimize_code_node (c);
}


/* Do the optimizations for a code node.  */

static void
optimize_code_node (gfc_code *c)
{

  gfc_forall_iterator *fa;
  gfc_code *d;
  gfc_alloc *a;

  switch (c->op)
    {
    case EXEC_ASSIGN:
      optimize_assignment (c);
      break;

    case EXEC_CALL:
    case EXEC_ASSIGN_CALL:
    case EXEC_CALL_PPC:
      optimize_actual_arglist (c->ext.actual);
      break;

    case EXEC_ARITHMETIC_IF:
      optimize_expr_0 (c->expr1);
      break;

    case EXEC_PAUSE:
    case EXEC_RETURN:
    case EXEC_ERROR_STOP:
    case EXEC_STOP:
    case EXEC_COMPCALL:
      optimize_expr_0 (c->expr1);
      break;

    case EXEC_SYNC_ALL:
    case EXEC_SYNC_MEMORY:
    case EXEC_SYNC_IMAGES:
      optimize_expr_0 (c->expr2);
      break;

    case EXEC_IF:
      d = c->block;
      optimize_expr_0 (d->expr1);
      optimize_code (d->next);

      for (d = d->block; d; d = d->block)
	{
	  optimize_expr_0 (d->expr1);

	  optimize_code (d->next);
	}


      break;

    case EXEC_SELECT:
    case EXEC_SELECT_TYPE:
      d = c->block;

      optimize_expr_0 (c->expr1);

      for (; d; d = d->block)
	optimize_code (d->next);

      break;

    case EXEC_WHERE:
      d = c->block;
      optimize_expr_0 (d->expr1);
      optimize_code (d->next);

      for (d = d->block; d; d = d->block)
	{
	  optimize_expr_0 (d->expr1);
	  optimize_code (d->next);
	}
      break;

    case EXEC_FORALL:

      for (fa = c->ext.forall_iterator; fa; fa = fa->next)
	{
	  optimize_expr_0 (fa->start);
	  optimize_expr_0 (fa->end);
	  optimize_expr_0 (fa->stride);
	}

      if (c->expr1 != NULL)
	  optimize_expr_0 (c->expr1);

      optimize_code (c->block->next);

      break;

    case EXEC_CRITICAL:
      optimize_code (c->block->next);
      break;

    case EXEC_DO:
      optimize_expr_0 (c->ext.iterator->start);
      optimize_expr_0 (c->ext.iterator->end);
      optimize_expr_0 (c->ext.iterator->step);
      optimize_code (c->block->next);

      break;

    case EXEC_DO_WHILE:
      optimize_expr_0 (c->expr1);
      optimize_code (c->block->next);
      break;


    case EXEC_ALLOCATE:
      for (a = c->ext.alloc.list; a; a = a->next)
	  optimize_expr_0 (a->expr);
      break;

      /* Todo:  Some of these may need to be optimized, as well.  */
    case EXEC_WRITE:
    case EXEC_READ:
    case EXEC_OPEN:
    case EXEC_INQUIRE:
    case EXEC_REWIND:
    case EXEC_ENDFILE:
    case EXEC_BACKSPACE:
    case EXEC_CLOSE:
    case EXEC_WAIT:
    case EXEC_TRANSFER:
    case EXEC_FLUSH:
    case EXEC_IOLENGTH:
    case EXEC_END_PROCEDURE:
    case EXEC_NOP:
    case EXEC_CONTINUE:
    case EXEC_ENTRY:
    case EXEC_INIT_ASSIGN:
    case EXEC_LABEL_ASSIGN:
    case EXEC_POINTER_ASSIGN:
    case EXEC_GOTO:
    case EXEC_CYCLE:
    case EXEC_EXIT:
    case EXEC_BLOCK:
    case EXEC_END_BLOCK:
    case EXEC_OMP_ATOMIC:
    case EXEC_OMP_BARRIER:
    case EXEC_OMP_CRITICAL:
    case EXEC_OMP_FLUSH:
    case EXEC_OMP_DO:
    case EXEC_OMP_MASTER:
    case EXEC_OMP_ORDERED:
    case EXEC_OMP_PARALLEL:
    case EXEC_OMP_PARALLEL_DO:
    case EXEC_OMP_PARALLEL_SECTIONS:
    case EXEC_OMP_PARALLEL_WORKSHARE:
    case EXEC_OMP_SECTIONS:
    case EXEC_OMP_SINGLE:
    case EXEC_OMP_TASK:
    case EXEC_OMP_TASKWAIT:
    case EXEC_OMP_WORKSHARE:
    case EXEC_DEALLOCATE:
      
      break;

    default:
      gcc_unreachable ();

    }
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

  /* All direct optimizations have been done.  Now it's time
     to optimize the rhs.  */

  optimize_expr_0 (rhs);
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

/* Top-level optimization of expressions.  Calls gfc_simplify_expr if
   optimize_expr succeeds in doing something.
   TODO: Optimization of multiple function occurrence to come here.  */

static void
optimize_expr_0 (gfc_expr * e)
{
  if (optimize_expr (e))
    gfc_simplify_expr (e, 0);

  return;
}

/* Recursive optimization of expressions.
 TODO:  Make this handle many more things.  */

static bool
optimize_expr (gfc_expr *e)
{
  bool ret;

  if (e == NULL)
    return false;

  ret = false;

  switch (e->expr_type)
    {
    case EXPR_OP:
      return optimize_op (e);
      break;

    case EXPR_FUNCTION:
      optimize_actual_arglist (e->value.function.actual);
      break;

    default:
      break;
    }

  return ret;
}

/* Recursive optimization of operators.  */

static bool
optimize_op (gfc_expr *e)
{

  gfc_intrinsic_op op;

  op = e->value.op.op;

  switch (op)
    {
    case INTRINSIC_EQ:
    case INTRINSIC_EQ_OS:
    case INTRINSIC_GE:
    case INTRINSIC_GE_OS:
    case INTRINSIC_LE:
    case INTRINSIC_LE_OS:
      return optimize_equality (e, true);
      break;

    case INTRINSIC_NE:
    case INTRINSIC_NE_OS:
    case INTRINSIC_GT:
    case INTRINSIC_GT_OS:
    case INTRINSIC_LT:
    case INTRINSIC_LT_OS:
      return optimize_equality (e, false);
      break;

    default:
      break;
    }

  return false;
}

/* Optimize expressions for equality.  */

static bool
optimize_equality (gfc_expr *e, bool equal)
{

  gfc_expr *op1, *op2;
  bool change;

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
      optimize_equality (e, equal);
      return true;
    }

  /* An expression of type EXPR_CONSTANT is only valid for scalars.  */
  /* TODO: A scalar constant may be acceptable in some cases (the scalarizer
     handles them well). However, there are also cases that need a non-scalar
     argument. For example the any intrinsic. See PR 45380.  */
  if (e->rank > 0)
    return false;

  /* Check for direct comparison between identical variables.  Don't compare
     REAL or COMPLEX because of NaN checks.  */
  if (op1->expr_type == EXPR_VARIABLE
      && op2->expr_type == EXPR_VARIABLE
      && op1->ts.type != BT_REAL && op2->ts.type != BT_REAL
      && op1->ts.type != BT_COMPLEX && op2->ts.type !=BT_COMPLEX
      && gfc_are_identical_variables (op1, op2))
    {
      /* Replace the expression by a constant expression.  The typespec
	 and where remains the way it is.  */
      gfc_free (op1);
      gfc_free (op2);
      e->expr_type = EXPR_CONSTANT;
      e->value.logical = equal;
      return true;
    }
  return false;
}

/* Optimize a call list.  Right now, this just goes through the actual
   arg list and optimizes each expression in turn.  */

static void
optimize_actual_arglist (gfc_actual_arglist *a)
{

  for (; a; a = a->next)
    {
      if (a->expr != NULL)
	optimize_expr_0 (a->expr);
    }
  
  return;
}
