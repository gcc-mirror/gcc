/* Routines for manipulation of expression nodes.
   Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005 Free Software Foundation,
   Inc.
   Contributed by Andy Vaught

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
Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.  */

#include "config.h"
#include "system.h"
#include "gfortran.h"
#include "arith.h"
#include "match.h"

/* Get a new expr node.  */

gfc_expr *
gfc_get_expr (void)
{
  gfc_expr *e;

  e = gfc_getmem (sizeof (gfc_expr));

  gfc_clear_ts (&e->ts);
  e->shape = NULL;
  e->ref = NULL;
  e->symtree = NULL;

  return e;
}


/* Free an argument list and everything below it.  */

void
gfc_free_actual_arglist (gfc_actual_arglist * a1)
{
  gfc_actual_arglist *a2;

  while (a1)
    {
      a2 = a1->next;
      gfc_free_expr (a1->expr);
      gfc_free (a1);
      a1 = a2;
    }
}


/* Copy an arglist structure and all of the arguments.  */

gfc_actual_arglist *
gfc_copy_actual_arglist (gfc_actual_arglist * p)
{
  gfc_actual_arglist *head, *tail, *new;

  head = tail = NULL;

  for (; p; p = p->next)
    {
      new = gfc_get_actual_arglist ();
      *new = *p;

      new->expr = gfc_copy_expr (p->expr);
      new->next = NULL;

      if (head == NULL)
	head = new;
      else
	tail->next = new;

      tail = new;
    }

  return head;
}


/* Free a list of reference structures.  */

void
gfc_free_ref_list (gfc_ref * p)
{
  gfc_ref *q;
  int i;

  for (; p; p = q)
    {
      q = p->next;

      switch (p->type)
	{
	case REF_ARRAY:
	  for (i = 0; i < GFC_MAX_DIMENSIONS; i++)
	    {
	      gfc_free_expr (p->u.ar.start[i]);
	      gfc_free_expr (p->u.ar.end[i]);
	      gfc_free_expr (p->u.ar.stride[i]);
	    }

	  break;

	case REF_SUBSTRING:
	  gfc_free_expr (p->u.ss.start);
	  gfc_free_expr (p->u.ss.end);
	  break;

	case REF_COMPONENT:
	  break;
	}

      gfc_free (p);
    }
}


/* Workhorse function for gfc_free_expr() that frees everything
   beneath an expression node, but not the node itself.  This is
   useful when we want to simplify a node and replace it with
   something else or the expression node belongs to another structure.  */

static void
free_expr0 (gfc_expr * e)
{
  int n;

  switch (e->expr_type)
    {
    case EXPR_CONSTANT:
      if (e->from_H)
	{
	  gfc_free (e->value.character.string);
	  break;
	}

      switch (e->ts.type)
	{
	case BT_INTEGER:
	  mpz_clear (e->value.integer);
	  break;

	case BT_REAL:
	  mpfr_clear (e->value.real);
	  break;

	case BT_CHARACTER:
	case BT_HOLLERITH:
	  gfc_free (e->value.character.string);
	  break;

	case BT_COMPLEX:
	  mpfr_clear (e->value.complex.r);
	  mpfr_clear (e->value.complex.i);
	  break;

	default:
	  break;
	}

      break;

    case EXPR_OP:
      if (e->value.op.op1 != NULL)
	gfc_free_expr (e->value.op.op1);
      if (e->value.op.op2 != NULL)
	gfc_free_expr (e->value.op.op2);
      break;

    case EXPR_FUNCTION:
      gfc_free_actual_arglist (e->value.function.actual);
      break;

    case EXPR_VARIABLE:
      break;

    case EXPR_ARRAY:
    case EXPR_STRUCTURE:
      gfc_free_constructor (e->value.constructor);
      break;

    case EXPR_SUBSTRING:
      gfc_free (e->value.character.string);
      break;

    case EXPR_NULL:
      break;

    default:
      gfc_internal_error ("free_expr0(): Bad expr type");
    }

  /* Free a shape array.  */
  if (e->shape != NULL)
    {
      for (n = 0; n < e->rank; n++)
	mpz_clear (e->shape[n]);

      gfc_free (e->shape);
    }

  gfc_free_ref_list (e->ref);

  memset (e, '\0', sizeof (gfc_expr));
}


/* Free an expression node and everything beneath it.  */

void
gfc_free_expr (gfc_expr * e)
{

  if (e == NULL)
    return;

  free_expr0 (e);
  gfc_free (e);
}


/* Graft the *src expression onto the *dest subexpression.  */

void
gfc_replace_expr (gfc_expr * dest, gfc_expr * src)
{

  free_expr0 (dest);
  *dest = *src;

  gfc_free (src);
}


/* Try to extract an integer constant from the passed expression node.
   Returns an error message or NULL if the result is set.  It is
   tempting to generate an error and return SUCCESS or FAILURE, but
   failure is OK for some callers.  */

const char *
gfc_extract_int (gfc_expr * expr, int *result)
{

  if (expr->expr_type != EXPR_CONSTANT)
    return _("Constant expression required at %C");

  if (expr->ts.type != BT_INTEGER)
    return _("Integer expression required at %C");

  if ((mpz_cmp_si (expr->value.integer, INT_MAX) > 0)
      || (mpz_cmp_si (expr->value.integer, INT_MIN) < 0))
    {
      return _("Integer value too large in expression at %C");
    }

  *result = (int) mpz_get_si (expr->value.integer);

  return NULL;
}


/* Recursively copy a list of reference structures.  */

static gfc_ref *
copy_ref (gfc_ref * src)
{
  gfc_array_ref *ar;
  gfc_ref *dest;

  if (src == NULL)
    return NULL;

  dest = gfc_get_ref ();
  dest->type = src->type;

  switch (src->type)
    {
    case REF_ARRAY:
      ar = gfc_copy_array_ref (&src->u.ar);
      dest->u.ar = *ar;
      gfc_free (ar);
      break;

    case REF_COMPONENT:
      dest->u.c = src->u.c;
      break;

    case REF_SUBSTRING:
      dest->u.ss = src->u.ss;
      dest->u.ss.start = gfc_copy_expr (src->u.ss.start);
      dest->u.ss.end = gfc_copy_expr (src->u.ss.end);
      break;
    }

  dest->next = copy_ref (src->next);

  return dest;
}


/* Detect whether an expression has any vector index array
   references.  */

int
gfc_has_vector_index (gfc_expr *e)
{
  gfc_ref * ref;
  int i;
  for (ref = e->ref; ref; ref = ref->next)
    if (ref->type == REF_ARRAY)
      for (i = 0; i < ref->u.ar.dimen; i++)
	if (ref->u.ar.dimen_type[i] == DIMEN_VECTOR)
	  return 1;
  return 0;
}


/* Copy a shape array.  */

mpz_t *
gfc_copy_shape (mpz_t * shape, int rank)
{
  mpz_t *new_shape;
  int n;

  if (shape == NULL)
    return NULL;

  new_shape = gfc_get_shape (rank);

  for (n = 0; n < rank; n++)
    mpz_init_set (new_shape[n], shape[n]);

  return new_shape;
}


/* Copy a shape array excluding dimension N, where N is an integer
   constant expression.  Dimensions are numbered in fortran style --
   starting with ONE.

   So, if the original shape array contains R elements
      { s1 ... sN-1  sN  sN+1 ... sR-1 sR}
   the result contains R-1 elements:
      { s1 ... sN-1  sN+1    ...  sR-1}

   If anything goes wrong -- N is not a constant, its value is out
   of range -- or anything else, just returns NULL.
*/

mpz_t *
gfc_copy_shape_excluding (mpz_t * shape, int rank, gfc_expr * dim)
{
  mpz_t *new_shape, *s;
  int i, n;

  if (shape == NULL 
      || rank <= 1
      || dim == NULL
      || dim->expr_type != EXPR_CONSTANT 
      || dim->ts.type != BT_INTEGER)
    return NULL;

  n = mpz_get_si (dim->value.integer);
  n--; /* Convert to zero based index */
  if (n < 0 || n >= rank)
    return NULL;

  s = new_shape = gfc_get_shape (rank-1);

  for (i = 0; i < rank; i++)
    {
      if (i == n)
        continue;
      mpz_init_set (*s, shape[i]);
      s++;
    }

  return new_shape;
}

/* Given an expression pointer, return a copy of the expression.  This
   subroutine is recursive.  */

gfc_expr *
gfc_copy_expr (gfc_expr * p)
{
  gfc_expr *q;
  char *s;

  if (p == NULL)
    return NULL;

  q = gfc_get_expr ();
  *q = *p;

  switch (q->expr_type)
    {
    case EXPR_SUBSTRING:
      s = gfc_getmem (p->value.character.length + 1);
      q->value.character.string = s;

      memcpy (s, p->value.character.string, p->value.character.length + 1);
      break;

    case EXPR_CONSTANT:
      if (p->from_H)
	{
	  s = gfc_getmem (p->value.character.length + 1);
	  q->value.character.string = s;

	  memcpy (s, p->value.character.string,
		  p->value.character.length + 1);
	  break;
	}
      switch (q->ts.type)
	{
	case BT_INTEGER:
	  mpz_init_set (q->value.integer, p->value.integer);
	  break;

	case BT_REAL:
          gfc_set_model_kind (q->ts.kind);
          mpfr_init (q->value.real);
	  mpfr_set (q->value.real, p->value.real, GFC_RND_MODE);
	  break;

	case BT_COMPLEX:
          gfc_set_model_kind (q->ts.kind);
          mpfr_init (q->value.complex.r);
          mpfr_init (q->value.complex.i);
	  mpfr_set (q->value.complex.r, p->value.complex.r, GFC_RND_MODE);
	  mpfr_set (q->value.complex.i, p->value.complex.i, GFC_RND_MODE);
	  break;

	case BT_CHARACTER:
	case BT_HOLLERITH:
	  s = gfc_getmem (p->value.character.length + 1);
	  q->value.character.string = s;

	  memcpy (s, p->value.character.string,
		  p->value.character.length + 1);
	  break;

	case BT_LOGICAL:
	case BT_DERIVED:
	  break;		/* Already done */

	case BT_PROCEDURE:
	case BT_UNKNOWN:
	  gfc_internal_error ("gfc_copy_expr(): Bad expr node");
	  /* Not reached */
	}

      break;

    case EXPR_OP:
      switch (q->value.op.operator)
	{
	case INTRINSIC_NOT:
	case INTRINSIC_UPLUS:
	case INTRINSIC_UMINUS:
	  q->value.op.op1 = gfc_copy_expr (p->value.op.op1);
	  break;

	default:		/* Binary operators */
	  q->value.op.op1 = gfc_copy_expr (p->value.op.op1);
	  q->value.op.op2 = gfc_copy_expr (p->value.op.op2);
	  break;
	}

      break;

    case EXPR_FUNCTION:
      q->value.function.actual =
	gfc_copy_actual_arglist (p->value.function.actual);
      break;

    case EXPR_STRUCTURE:
    case EXPR_ARRAY:
      q->value.constructor = gfc_copy_constructor (p->value.constructor);
      break;

    case EXPR_VARIABLE:
    case EXPR_NULL:
      break;
    }

  q->shape = gfc_copy_shape (p->shape, p->rank);

  q->ref = copy_ref (p->ref);

  return q;
}


/* Return the maximum kind of two expressions.  In general, higher
   kind numbers mean more precision for numeric types.  */

int
gfc_kind_max (gfc_expr * e1, gfc_expr * e2)
{

  return (e1->ts.kind > e2->ts.kind) ? e1->ts.kind : e2->ts.kind;
}


/* Returns nonzero if the type is numeric, zero otherwise.  */

static int
numeric_type (bt type)
{

  return type == BT_COMPLEX || type == BT_REAL || type == BT_INTEGER;
}


/* Returns nonzero if the typespec is a numeric type, zero otherwise.  */

int
gfc_numeric_ts (gfc_typespec * ts)
{

  return numeric_type (ts->type);
}


/* Returns an expression node that is an integer constant.  */

gfc_expr *
gfc_int_expr (int i)
{
  gfc_expr *p;

  p = gfc_get_expr ();

  p->expr_type = EXPR_CONSTANT;
  p->ts.type = BT_INTEGER;
  p->ts.kind = gfc_default_integer_kind;

  p->where = gfc_current_locus;
  mpz_init_set_si (p->value.integer, i);

  return p;
}


/* Returns an expression node that is a logical constant.  */

gfc_expr *
gfc_logical_expr (int i, locus * where)
{
  gfc_expr *p;

  p = gfc_get_expr ();

  p->expr_type = EXPR_CONSTANT;
  p->ts.type = BT_LOGICAL;
  p->ts.kind = gfc_default_logical_kind;

  if (where == NULL)
    where = &gfc_current_locus;
  p->where = *where;
  p->value.logical = i;

  return p;
}


/* Return an expression node with an optional argument list attached.
   A variable number of gfc_expr pointers are strung together in an
   argument list with a NULL pointer terminating the list.  */

gfc_expr *
gfc_build_conversion (gfc_expr * e)
{
  gfc_expr *p;

  p = gfc_get_expr ();
  p->expr_type = EXPR_FUNCTION;
  p->symtree = NULL;
  p->value.function.actual = NULL;

  p->value.function.actual = gfc_get_actual_arglist ();
  p->value.function.actual->expr = e;

  return p;
}


/* Given an expression node with some sort of numeric binary
   expression, insert type conversions required to make the operands
   have the same type.

   The exception is that the operands of an exponential don't have to
   have the same type.  If possible, the base is promoted to the type
   of the exponent.  For example, 1**2.3 becomes 1.0**2.3, but
   1.0**2 stays as it is.  */

void
gfc_type_convert_binary (gfc_expr * e)
{
  gfc_expr *op1, *op2;

  op1 = e->value.op.op1;
  op2 = e->value.op.op2;

  if (op1->ts.type == BT_UNKNOWN || op2->ts.type == BT_UNKNOWN)
    {
      gfc_clear_ts (&e->ts);
      return;
    }

  /* Kind conversions of same type.  */
  if (op1->ts.type == op2->ts.type)
    {

      if (op1->ts.kind == op2->ts.kind)
	{
          /* No type conversions.  */
	  e->ts = op1->ts;
	  goto done;
	}

      if (op1->ts.kind > op2->ts.kind)
	gfc_convert_type (op2, &op1->ts, 2);
      else
	gfc_convert_type (op1, &op2->ts, 2);

      e->ts = op1->ts;
      goto done;
    }

  /* Integer combined with real or complex.  */
  if (op2->ts.type == BT_INTEGER)
    {
      e->ts = op1->ts;

      /* Special case for ** operator.  */
      if (e->value.op.operator == INTRINSIC_POWER)
	goto done;

      gfc_convert_type (e->value.op.op2, &e->ts, 2);
      goto done;
    }

  if (op1->ts.type == BT_INTEGER)
    {
      e->ts = op2->ts;
      gfc_convert_type (e->value.op.op1, &e->ts, 2);
      goto done;
    }

  /* Real combined with complex.  */
  e->ts.type = BT_COMPLEX;
  if (op1->ts.kind > op2->ts.kind)
    e->ts.kind = op1->ts.kind;
  else
    e->ts.kind = op2->ts.kind;
  if (op1->ts.type != BT_COMPLEX || op1->ts.kind != e->ts.kind)
    gfc_convert_type (e->value.op.op1, &e->ts, 2);
  if (op2->ts.type != BT_COMPLEX || op2->ts.kind != e->ts.kind)
    gfc_convert_type (e->value.op.op2, &e->ts, 2);

done:
  return;
}


/* Function to determine if an expression is constant or not.  This
   function expects that the expression has already been simplified.  */

int
gfc_is_constant_expr (gfc_expr * e)
{
  gfc_constructor *c;
  gfc_actual_arglist *arg;
  int rv;

  if (e == NULL)
    return 1;

  switch (e->expr_type)
    {
    case EXPR_OP:
      rv = (gfc_is_constant_expr (e->value.op.op1)
	    && (e->value.op.op2 == NULL
		|| gfc_is_constant_expr (e->value.op.op2)));

      break;

    case EXPR_VARIABLE:
      rv = 0;
      break;

    case EXPR_FUNCTION:
      /* Call to intrinsic with at least one argument.  */
      rv = 0;
      if (e->value.function.isym && e->value.function.actual)
	{
	  for (arg = e->value.function.actual; arg; arg = arg->next)
	    {
	      if (!gfc_is_constant_expr (arg->expr))
		break;
	    }
	  if (arg == NULL)
	    rv = 1;
	}
      break;

    case EXPR_CONSTANT:
    case EXPR_NULL:
      rv = 1;
      break;

    case EXPR_SUBSTRING:
      rv = (gfc_is_constant_expr (e->ref->u.ss.start)
	    && gfc_is_constant_expr (e->ref->u.ss.end));
      break;

    case EXPR_STRUCTURE:
      rv = 0;
      for (c = e->value.constructor; c; c = c->next)
	if (!gfc_is_constant_expr (c->expr))
	  break;

      if (c == NULL)
	rv = 1;
      break;

    case EXPR_ARRAY:
      rv = gfc_constant_ac (e);
      break;

    default:
      gfc_internal_error ("gfc_is_constant_expr(): Unknown expression type");
    }

  return rv;
}


/* Try to collapse intrinsic expressions.  */

static try
simplify_intrinsic_op (gfc_expr * p, int type)
{
  gfc_expr *op1, *op2, *result;

  if (p->value.op.operator == INTRINSIC_USER)
    return SUCCESS;

  op1 = p->value.op.op1;
  op2 = p->value.op.op2;

  if (gfc_simplify_expr (op1, type) == FAILURE)
    return FAILURE;
  if (gfc_simplify_expr (op2, type) == FAILURE)
    return FAILURE;

  if (!gfc_is_constant_expr (op1)
      || (op2 != NULL && !gfc_is_constant_expr (op2)))
    return SUCCESS;

  /* Rip p apart */
  p->value.op.op1 = NULL;
  p->value.op.op2 = NULL;

  switch (p->value.op.operator)
    {
    case INTRINSIC_UPLUS:
      result = gfc_uplus (op1);
      break;

    case INTRINSIC_UMINUS:
      result = gfc_uminus (op1);
      break;

    case INTRINSIC_PLUS:
      result = gfc_add (op1, op2);
      break;

    case INTRINSIC_MINUS:
      result = gfc_subtract (op1, op2);
      break;

    case INTRINSIC_TIMES:
      result = gfc_multiply (op1, op2);
      break;

    case INTRINSIC_DIVIDE:
      result = gfc_divide (op1, op2);
      break;

    case INTRINSIC_POWER:
      result = gfc_power (op1, op2);
      break;

    case INTRINSIC_CONCAT:
      result = gfc_concat (op1, op2);
      break;

    case INTRINSIC_EQ:
      result = gfc_eq (op1, op2);
      break;

    case INTRINSIC_NE:
      result = gfc_ne (op1, op2);
      break;

    case INTRINSIC_GT:
      result = gfc_gt (op1, op2);
      break;

    case INTRINSIC_GE:
      result = gfc_ge (op1, op2);
      break;

    case INTRINSIC_LT:
      result = gfc_lt (op1, op2);
      break;

    case INTRINSIC_LE:
      result = gfc_le (op1, op2);
      break;

    case INTRINSIC_NOT:
      result = gfc_not (op1);
      break;

    case INTRINSIC_AND:
      result = gfc_and (op1, op2);
      break;

    case INTRINSIC_OR:
      result = gfc_or (op1, op2);
      break;

    case INTRINSIC_EQV:
      result = gfc_eqv (op1, op2);
      break;

    case INTRINSIC_NEQV:
      result = gfc_neqv (op1, op2);
      break;

    default:
      gfc_internal_error ("simplify_intrinsic_op(): Bad operator");
    }

  if (result == NULL)
    {
      gfc_free_expr (op1);
      gfc_free_expr (op2);
      return FAILURE;
    }

  gfc_replace_expr (p, result);

  return SUCCESS;
}


/* Subroutine to simplify constructor expressions.  Mutually recursive
   with gfc_simplify_expr().  */

static try
simplify_constructor (gfc_constructor * c, int type)
{

  for (; c; c = c->next)
    {
      if (c->iterator
	  && (gfc_simplify_expr (c->iterator->start, type) == FAILURE
	      || gfc_simplify_expr (c->iterator->end, type) == FAILURE
	      || gfc_simplify_expr (c->iterator->step, type) == FAILURE))
	return FAILURE;

      if (c->expr && gfc_simplify_expr (c->expr, type) == FAILURE)
	return FAILURE;
    }

  return SUCCESS;
}


/* Pull a single array element out of an array constructor.  */

static gfc_constructor *
find_array_element (gfc_constructor * cons, gfc_array_ref * ar)
{
  unsigned long nelemen;
  int i;
  mpz_t delta;
  mpz_t offset;

  mpz_init_set_ui (offset, 0);
  mpz_init (delta);
  for (i = 0; i < ar->dimen; i++)
    {
      if (ar->start[i]->expr_type != EXPR_CONSTANT)
	{
	  cons = NULL;
	  break;
	}
      mpz_sub (delta, ar->start[i]->value.integer,
	       ar->as->lower[i]->value.integer);
      mpz_add (offset, offset, delta);
    }

  if (cons)
    {
      if (mpz_fits_ulong_p (offset))
	{
	  for (nelemen = mpz_get_ui (offset); nelemen > 0; nelemen--)
	    {
	      if (cons->iterator)
		{
		  cons = NULL;
		  break;
		}
	      cons = cons->next;
	    }
	}
      else
	cons = NULL;
    }

  mpz_clear (delta);
  mpz_clear (offset);

  return cons;
}


/* Find a component of a structure constructor.  */

static gfc_constructor *
find_component_ref (gfc_constructor * cons, gfc_ref * ref)
{
  gfc_component *comp;
  gfc_component *pick;

  comp = ref->u.c.sym->components;
  pick = ref->u.c.component;
  while (comp != pick)
    {
      comp = comp->next;
      cons = cons->next;
    }

  return cons;
}


/* Replace an expression with the contents of a constructor, removing
   the subobject reference in the process.  */

static void
remove_subobject_ref (gfc_expr * p, gfc_constructor * cons)
{
  gfc_expr *e;

  e = cons->expr;
  cons->expr = NULL;
  e->ref = p->ref->next;
  p->ref->next =  NULL;
  gfc_replace_expr (p, e);
}


/* Simplify a subobject reference of a constructor.  This occurs when
   parameter variable values are substituted.  */

static try
simplify_const_ref (gfc_expr * p)
{
  gfc_constructor *cons;

  while (p->ref)
    {
      switch (p->ref->type)
	{
	case REF_ARRAY:
	  switch (p->ref->u.ar.type)
	    {
	    case AR_ELEMENT:
	      cons = find_array_element (p->value.constructor, &p->ref->u.ar);
	      if (!cons)
		return SUCCESS;
	      remove_subobject_ref (p, cons);
	      break;

	    case AR_FULL:
	      if (p->ref->next != NULL)
		{
		  /* TODO: Simplify array subobject references.  */
		  return SUCCESS;
		}
		gfc_free_ref_list (p->ref);
		p->ref = NULL;
	      break;

	    default:
	      /* TODO: Simplify array subsections.  */
	      return SUCCESS;
	    }

	  break;

	case REF_COMPONENT:
	  cons = find_component_ref (p->value.constructor, p->ref);
	  remove_subobject_ref (p, cons);
	  break;

	case REF_SUBSTRING:
	  /* TODO: Constant substrings.  */
	  return SUCCESS;
	}
    }

  return SUCCESS;
}


/* Simplify a chain of references.  */

static try
simplify_ref_chain (gfc_ref * ref, int type)
{
  int n;

  for (; ref; ref = ref->next)
    {
      switch (ref->type)
	{
	case REF_ARRAY:
	  for (n = 0; n < ref->u.ar.dimen; n++)
	    {
	      if (gfc_simplify_expr (ref->u.ar.start[n], type)
		    == FAILURE)
		return FAILURE;
	      if (gfc_simplify_expr (ref->u.ar.end[n], type)
		     == FAILURE)
		return FAILURE;
	      if (gfc_simplify_expr (ref->u.ar.stride[n], type)
		     == FAILURE)
		return FAILURE;
	    }
	  break;

	case REF_SUBSTRING:
	  if (gfc_simplify_expr (ref->u.ss.start, type) == FAILURE)
	    return FAILURE;
	  if (gfc_simplify_expr (ref->u.ss.end, type) == FAILURE)
	    return FAILURE;
	  break;

	default:
	  break;
	}
    }
  return SUCCESS;
}


/* Try to substitute the value of a parameter variable.  */
static try
simplify_parameter_variable (gfc_expr * p, int type)
{
  gfc_expr *e;
  try t;

  e = gfc_copy_expr (p->symtree->n.sym->value);
  /* Do not copy subobject refs for constant.  */
  if (e->expr_type != EXPR_CONSTANT && p->ref != NULL)
    e->ref = copy_ref (p->ref);
  t = gfc_simplify_expr (e, type);

  /* Only use the simplification if it eliminated all subobject
     references.  */
  if (t == SUCCESS && ! e->ref)
    gfc_replace_expr (p, e);
  else
    gfc_free_expr (e);

  return t;
}

/* Given an expression, simplify it by collapsing constant
   expressions.  Most simplification takes place when the expression
   tree is being constructed.  If an intrinsic function is simplified
   at some point, we get called again to collapse the result against
   other constants.

   We work by recursively simplifying expression nodes, simplifying
   intrinsic functions where possible, which can lead to further
   constant collapsing.  If an operator has constant operand(s), we
   rip the expression apart, and rebuild it, hoping that it becomes
   something simpler.

   The expression type is defined for:
     0   Basic expression parsing
     1   Simplifying array constructors -- will substitute
         iterator values.
   Returns FAILURE on error, SUCCESS otherwise.
   NOTE: Will return SUCCESS even if the expression can not be simplified.  */

try
gfc_simplify_expr (gfc_expr * p, int type)
{
  gfc_actual_arglist *ap;

  if (p == NULL)
    return SUCCESS;

  switch (p->expr_type)
    {
    case EXPR_CONSTANT:
    case EXPR_NULL:
      break;

    case EXPR_FUNCTION:
      for (ap = p->value.function.actual; ap; ap = ap->next)
	if (gfc_simplify_expr (ap->expr, type) == FAILURE)
	  return FAILURE;

      if (p->value.function.isym != NULL
	  && gfc_intrinsic_func_interface (p, 1) == MATCH_ERROR)
	return FAILURE;

      break;

    case EXPR_SUBSTRING:
      if (simplify_ref_chain (p->ref, type) == FAILURE)
	return FAILURE;

      if (gfc_is_constant_expr (p))
	{
	  char *s;
	  int start, end;

	  gfc_extract_int (p->ref->u.ss.start, &start);
	  start--;  /* Convert from one-based to zero-based.  */
	  gfc_extract_int (p->ref->u.ss.end, &end);
	  s = gfc_getmem (end - start + 1);
	  memcpy (s, p->value.character.string + start, end - start);
	  s[end] = '\0';  /* TODO: C-style string for debugging.  */
	  gfc_free (p->value.character.string);
	  p->value.character.string = s;
	  p->value.character.length = end - start;
	  p->ts.cl = gfc_get_charlen ();
	  p->ts.cl->next = gfc_current_ns->cl_list;
	  gfc_current_ns->cl_list = p->ts.cl;
	  p->ts.cl->length = gfc_int_expr (p->value.character.length);
	  gfc_free_ref_list (p->ref);
	  p->ref = NULL;
	  p->expr_type = EXPR_CONSTANT;
	}
      break;

    case EXPR_OP:
      if (simplify_intrinsic_op (p, type) == FAILURE)
	return FAILURE;
      break;

    case EXPR_VARIABLE:
      /* Only substitute array parameter variables if we are in an
         initialization expression, or we want a subsection.  */
      if (p->symtree->n.sym->attr.flavor == FL_PARAMETER
	  && (gfc_init_expr || p->ref
	      || p->symtree->n.sym->value->expr_type != EXPR_ARRAY))
	{
	  if (simplify_parameter_variable (p, type) == FAILURE)
	    return FAILURE;
	  break;
	}

      if (type == 1)
	{
	  gfc_simplify_iterator_var (p);
	}

      /* Simplify subcomponent references.  */
      if (simplify_ref_chain (p->ref, type) == FAILURE)
	return FAILURE;

      break;

    case EXPR_STRUCTURE:
    case EXPR_ARRAY:
      if (simplify_ref_chain (p->ref, type) == FAILURE)
	return FAILURE;

      if (simplify_constructor (p->value.constructor, type) == FAILURE)
	return FAILURE;

      if (p->expr_type == EXPR_ARRAY)
	  gfc_expand_constructor (p);

      if (simplify_const_ref (p) == FAILURE)
	return FAILURE;

      break;
    }

  return SUCCESS;
}


/* Returns the type of an expression with the exception that iterator
   variables are automatically integers no matter what else they may
   be declared as.  */

static bt
et0 (gfc_expr * e)
{

  if (e->expr_type == EXPR_VARIABLE && gfc_check_iter_variable (e) == SUCCESS)
    return BT_INTEGER;

  return e->ts.type;
}


/* Check an intrinsic arithmetic operation to see if it is consistent
   with some type of expression.  */

static try check_init_expr (gfc_expr *);

static try
check_intrinsic_op (gfc_expr * e, try (*check_function) (gfc_expr *))
{
  gfc_expr *op1 = e->value.op.op1;
  gfc_expr *op2 = e->value.op.op2;

  if ((*check_function) (op1) == FAILURE)
    return FAILURE;

  switch (e->value.op.operator)
    {
    case INTRINSIC_UPLUS:
    case INTRINSIC_UMINUS:
      if (!numeric_type (et0 (op1)))
	goto not_numeric;
      break;

    case INTRINSIC_EQ:
    case INTRINSIC_NE:
    case INTRINSIC_GT:
    case INTRINSIC_GE:
    case INTRINSIC_LT:
    case INTRINSIC_LE:
      if ((*check_function) (op2) == FAILURE)
	return FAILURE;
      
      if (!(et0 (op1) == BT_CHARACTER && et0 (op2) == BT_CHARACTER)
	  && !(numeric_type (et0 (op1)) && numeric_type (et0 (op2))))
	{
	  gfc_error ("Numeric or CHARACTER operands are required in "
		     "expression at %L", &e->where);
         return FAILURE;
	}
      break;

    case INTRINSIC_PLUS:
    case INTRINSIC_MINUS:
    case INTRINSIC_TIMES:
    case INTRINSIC_DIVIDE:
    case INTRINSIC_POWER:
      if ((*check_function) (op2) == FAILURE)
	return FAILURE;

      if (!numeric_type (et0 (op1)) || !numeric_type (et0 (op2)))
	goto not_numeric;

      if (e->value.op.operator == INTRINSIC_POWER
	  && check_function == check_init_expr && et0 (op2) != BT_INTEGER)
	{
	  gfc_error ("Exponent at %L must be INTEGER for an initialization "
		     "expression", &op2->where);
	  return FAILURE;
	}

      break;

    case INTRINSIC_CONCAT:
      if ((*check_function) (op2) == FAILURE)
	return FAILURE;

      if (et0 (op1) != BT_CHARACTER || et0 (op2) != BT_CHARACTER)
	{
	  gfc_error ("Concatenation operator in expression at %L "
		     "must have two CHARACTER operands", &op1->where);
	  return FAILURE;
	}

      if (op1->ts.kind != op2->ts.kind)
	{
	  gfc_error ("Concat operator at %L must concatenate strings of the "
		     "same kind", &e->where);
	  return FAILURE;
	}

      break;

    case INTRINSIC_NOT:
      if (et0 (op1) != BT_LOGICAL)
	{
	  gfc_error (".NOT. operator in expression at %L must have a LOGICAL "
		     "operand", &op1->where);
	  return FAILURE;
	}

      break;

    case INTRINSIC_AND:
    case INTRINSIC_OR:
    case INTRINSIC_EQV:
    case INTRINSIC_NEQV:
      if ((*check_function) (op2) == FAILURE)
	return FAILURE;

      if (et0 (op1) != BT_LOGICAL || et0 (op2) != BT_LOGICAL)
	{
	  gfc_error ("LOGICAL operands are required in expression at %L",
		     &e->where);
	  return FAILURE;
	}

      break;

    default:
      gfc_error ("Only intrinsic operators can be used in expression at %L",
		 &e->where);
      return FAILURE;
    }

  return SUCCESS;

not_numeric:
  gfc_error ("Numeric operands are required in expression at %L", &e->where);

  return FAILURE;
}



/* Certain inquiry functions are specifically allowed to have variable
   arguments, which is an exception to the normal requirement that an
   initialization function have initialization arguments.  We head off
   this problem here.  */

static try
check_inquiry (gfc_expr * e)
{
  const char *name;

  /* FIXME: This should be moved into the intrinsic definitions,
     to eliminate this ugly hack.  */
  static const char * const inquiry_function[] = {
    "digits", "epsilon", "huge", "kind", "len", "maxexponent", "minexponent",
    "precision", "radix", "range", "tiny", "bit_size", "size", "shape",
    "lbound", "ubound", NULL
  };

  int i;

  name = e->symtree->n.sym->name;

  for (i = 0; inquiry_function[i]; i++)
    if (strcmp (inquiry_function[i], name) == 0)
      break;

  if (inquiry_function[i] == NULL)
    return FAILURE;

  e = e->value.function.actual->expr;

  if (e == NULL || e->expr_type != EXPR_VARIABLE)
    return FAILURE;

  /* At this point we have an inquiry function with a variable argument.  The
     type of the variable might be undefined, but we need it now, because the
     arguments of these functions are allowed to be undefined.  */

  if (e->ts.type == BT_UNKNOWN)
    {
      if (e->symtree->n.sym->ts.type == BT_UNKNOWN
	  && gfc_set_default_type (e->symtree->n.sym, 0, gfc_current_ns)
            == FAILURE)
	return FAILURE;

      e->ts = e->symtree->n.sym->ts;
    }

  return SUCCESS;
}


/* Verify that an expression is an initialization expression.  A side
   effect is that the expression tree is reduced to a single constant
   node if all goes well.  This would normally happen when the
   expression is constructed but function references are assumed to be
   intrinsics in the context of initialization expressions.  If
   FAILURE is returned an error message has been generated.  */

static try
check_init_expr (gfc_expr * e)
{
  gfc_actual_arglist *ap;
  match m;
  try t;

  if (e == NULL)
    return SUCCESS;

  switch (e->expr_type)
    {
    case EXPR_OP:
      t = check_intrinsic_op (e, check_init_expr);
      if (t == SUCCESS)
	t = gfc_simplify_expr (e, 0);

      break;

    case EXPR_FUNCTION:
      t = SUCCESS;

      if (check_inquiry (e) != SUCCESS)
	{
	  t = SUCCESS;
	  for (ap = e->value.function.actual; ap; ap = ap->next)
	    if (check_init_expr (ap->expr) == FAILURE)
	      {
		t = FAILURE;
		break;
	      }
	}

      if (t == SUCCESS)
	{
	  m = gfc_intrinsic_func_interface (e, 0);

	  if (m == MATCH_NO)
	    gfc_error ("Function '%s' in initialization expression at %L "
		       "must be an intrinsic function",
                       e->symtree->n.sym->name, &e->where);

	  if (m != MATCH_YES)
	    t = FAILURE;
	}

      break;

    case EXPR_VARIABLE:
      t = SUCCESS;

      if (gfc_check_iter_variable (e) == SUCCESS)
	break;

      if (e->symtree->n.sym->attr.flavor == FL_PARAMETER)
	{
	  t = simplify_parameter_variable (e, 0);
	  break;
	}

      gfc_error ("Variable '%s' at %L cannot appear in an initialization "
		 "expression", e->symtree->n.sym->name, &e->where);
      t = FAILURE;
      break;

    case EXPR_CONSTANT:
    case EXPR_NULL:
      t = SUCCESS;
      break;

    case EXPR_SUBSTRING:
      t = check_init_expr (e->ref->u.ss.start);
      if (t == FAILURE)
	break;

      t = check_init_expr (e->ref->u.ss.end);
      if (t == SUCCESS)
	t = gfc_simplify_expr (e, 0);

      break;

    case EXPR_STRUCTURE:
      t = gfc_check_constructor (e, check_init_expr);
      break;

    case EXPR_ARRAY:
      t = gfc_check_constructor (e, check_init_expr);
      if (t == FAILURE)
	break;

      t = gfc_expand_constructor (e);
      if (t == FAILURE)
	break;

      t = gfc_check_constructor_type (e);
      break;

    default:
      gfc_internal_error ("check_init_expr(): Unknown expression type");
    }

  return t;
}


/* Match an initialization expression.  We work by first matching an
   expression, then reducing it to a constant.  */

match
gfc_match_init_expr (gfc_expr ** result)
{
  gfc_expr *expr;
  match m;
  try t;

  m = gfc_match_expr (&expr);
  if (m != MATCH_YES)
    return m;

  gfc_init_expr = 1;
  t = gfc_resolve_expr (expr);
  if (t == SUCCESS)
    t = check_init_expr (expr);
  gfc_init_expr = 0;

  if (t == FAILURE)
    {
      gfc_free_expr (expr);
      return MATCH_ERROR;
    }

  if (expr->expr_type == EXPR_ARRAY
      && (gfc_check_constructor_type (expr) == FAILURE
	  || gfc_expand_constructor (expr) == FAILURE))
    {
      gfc_free_expr (expr);
      return MATCH_ERROR;
    }

  if (!gfc_is_constant_expr (expr))
    gfc_internal_error ("Initialization expression didn't reduce %C");

  *result = expr;

  return MATCH_YES;
}



static try check_restricted (gfc_expr *);

/* Given an actual argument list, test to see that each argument is a
   restricted expression and optionally if the expression type is
   integer or character.  */

static try
restricted_args (gfc_actual_arglist * a)
{
  for (; a; a = a->next)
    {
      if (check_restricted (a->expr) == FAILURE)
	return FAILURE;
    }

  return SUCCESS;
}


/************* Restricted/specification expressions *************/


/* Make sure a non-intrinsic function is a specification function.  */

static try
external_spec_function (gfc_expr * e)
{
  gfc_symbol *f;

  f = e->value.function.esym;

  if (f->attr.proc == PROC_ST_FUNCTION)
    {
      gfc_error ("Specification function '%s' at %L cannot be a statement "
		 "function", f->name, &e->where);
      return FAILURE;
    }

  if (f->attr.proc == PROC_INTERNAL)
    {
      gfc_error ("Specification function '%s' at %L cannot be an internal "
		 "function", f->name, &e->where);
      return FAILURE;
    }

  if (!f->attr.pure)
    {
      gfc_error ("Specification function '%s' at %L must be PURE", f->name,
		 &e->where);
      return FAILURE;
    }

  if (f->attr.recursive)
    {
      gfc_error ("Specification function '%s' at %L cannot be RECURSIVE",
		 f->name, &e->where);
      return FAILURE;
    }

  return restricted_args (e->value.function.actual);
}


/* Check to see that a function reference to an intrinsic is a
   restricted expression.  */

static try
restricted_intrinsic (gfc_expr * e)
{
  /* TODO: Check constraints on inquiry functions.  7.1.6.2 (7).  */
  if (check_inquiry (e) == SUCCESS)
    return SUCCESS;

  return restricted_args (e->value.function.actual);
}


/* Verify that an expression is a restricted expression.  Like its
   cousin check_init_expr(), an error message is generated if we
   return FAILURE.  */

static try
check_restricted (gfc_expr * e)
{
  gfc_symbol *sym;
  try t;

  if (e == NULL)
    return SUCCESS;

  switch (e->expr_type)
    {
    case EXPR_OP:
      t = check_intrinsic_op (e, check_restricted);
      if (t == SUCCESS)
	t = gfc_simplify_expr (e, 0);

      break;

    case EXPR_FUNCTION:
      t = e->value.function.esym ?
	external_spec_function (e) : restricted_intrinsic (e);

      break;

    case EXPR_VARIABLE:
      sym = e->symtree->n.sym;
      t = FAILURE;

      if (sym->attr.optional)
	{
	  gfc_error ("Dummy argument '%s' at %L cannot be OPTIONAL",
		     sym->name, &e->where);
	  break;
	}

      if (sym->attr.intent == INTENT_OUT)
	{
	  gfc_error ("Dummy argument '%s' at %L cannot be INTENT(OUT)",
		     sym->name, &e->where);
	  break;
	}

      /* gfc_is_formal_arg broadcasts that a formal argument list is being processed
	 in resolve.c(resolve_formal_arglist).  This is done so that host associated
	 dummy array indices are accepted (PR23446).  */
      if (sym->attr.in_common
	  || sym->attr.use_assoc
	  || sym->attr.dummy
	  || sym->ns != gfc_current_ns
	  || (sym->ns->proc_name != NULL
	      && sym->ns->proc_name->attr.flavor == FL_MODULE)
	  || gfc_is_formal_arg ())
	{
	  t = SUCCESS;
	  break;
	}

      gfc_error ("Variable '%s' cannot appear in the expression at %L",
		 sym->name, &e->where);

      break;

    case EXPR_NULL:
    case EXPR_CONSTANT:
      t = SUCCESS;
      break;

    case EXPR_SUBSTRING:
      t = gfc_specification_expr (e->ref->u.ss.start);
      if (t == FAILURE)
	break;

      t = gfc_specification_expr (e->ref->u.ss.end);
      if (t == SUCCESS)
	t = gfc_simplify_expr (e, 0);

      break;

    case EXPR_STRUCTURE:
      t = gfc_check_constructor (e, check_restricted);
      break;

    case EXPR_ARRAY:
      t = gfc_check_constructor (e, check_restricted);
      break;

    default:
      gfc_internal_error ("check_restricted(): Unknown expression type");
    }

  return t;
}


/* Check to see that an expression is a specification expression.  If
   we return FAILURE, an error has been generated.  */

try
gfc_specification_expr (gfc_expr * e)
{

  if (e->ts.type != BT_INTEGER)
    {
      gfc_error ("Expression at %L must be of INTEGER type", &e->where);
      return FAILURE;
    }

  if (e->rank != 0)
    {
      gfc_error ("Expression at %L must be scalar", &e->where);
      return FAILURE;
    }

  if (gfc_simplify_expr (e, 0) == FAILURE)
    return FAILURE;

  return check_restricted (e);
}


/************** Expression conformance checks.  *************/

/* Given two expressions, make sure that the arrays are conformable.  */

try
gfc_check_conformance (const char *optype_msgid,
		       gfc_expr * op1, gfc_expr * op2)
{
  int op1_flag, op2_flag, d;
  mpz_t op1_size, op2_size;
  try t;

  if (op1->rank == 0 || op2->rank == 0)
    return SUCCESS;

  if (op1->rank != op2->rank)
    {
      gfc_error ("Incompatible ranks in %s at %L", _(optype_msgid),
		 &op1->where);
      return FAILURE;
    }

  t = SUCCESS;

  for (d = 0; d < op1->rank; d++)
    {
      op1_flag = gfc_array_dimen_size (op1, d, &op1_size) == SUCCESS;
      op2_flag = gfc_array_dimen_size (op2, d, &op2_size) == SUCCESS;

      if (op1_flag && op2_flag && mpz_cmp (op1_size, op2_size) != 0)
	{
	  gfc_error ("%s at %L has different shape on dimension %d (%d/%d)",
		     _(optype_msgid), &op1->where, d + 1,
		     (int) mpz_get_si (op1_size),
		     (int) mpz_get_si (op2_size));

	  t = FAILURE;
	}

      if (op1_flag)
	mpz_clear (op1_size);
      if (op2_flag)
	mpz_clear (op2_size);

      if (t == FAILURE)
	return FAILURE;
    }

  return SUCCESS;
}


/* Given an assignable expression and an arbitrary expression, make
   sure that the assignment can take place.  */

try
gfc_check_assign (gfc_expr * lvalue, gfc_expr * rvalue, int conform)
{
  gfc_symbol *sym;

  sym = lvalue->symtree->n.sym;

  if (sym->attr.intent == INTENT_IN)
    {
      gfc_error ("Can't assign to INTENT(IN) variable '%s' at %L",
		 sym->name, &lvalue->where);
      return FAILURE;
    }

  if (rvalue->rank != 0 && lvalue->rank != rvalue->rank)
    {
      gfc_error ("Incompatible ranks %d and %d in assignment at %L",
		 lvalue->rank, rvalue->rank, &lvalue->where);
      return FAILURE;
    }

  if (lvalue->ts.type == BT_UNKNOWN)
    {
      gfc_error ("Variable type is UNKNOWN in assignment at %L",
		 &lvalue->where);
      return FAILURE;
    }

   if (rvalue->expr_type == EXPR_NULL)
     {
       gfc_error ("NULL appears on right-hand side in assignment at %L",
		  &rvalue->where);
       return FAILURE;
     }

   if (sym->attr.cray_pointee
       && lvalue->ref != NULL
       && lvalue->ref->u.ar.type != AR_ELEMENT
       && lvalue->ref->u.ar.as->cp_was_assumed)
     {
       gfc_error ("Vector assignment to assumed-size Cray Pointee at %L"
		  " is illegal.", &lvalue->where);
       return FAILURE;
     }

  /* This is possibly a typo: x = f() instead of x => f()  */
  if (gfc_option.warn_surprising 
      && rvalue->expr_type == EXPR_FUNCTION
      && rvalue->symtree->n.sym->attr.pointer)
    gfc_warning ("POINTER valued function appears on right-hand side of "
		 "assignment at %L", &rvalue->where);

  /* Check size of array assignments.  */
  if (lvalue->rank != 0 && rvalue->rank != 0
      && gfc_check_conformance ("Array assignment", lvalue, rvalue) != SUCCESS)
    return FAILURE;

  if (gfc_compare_types (&lvalue->ts, &rvalue->ts))
    return SUCCESS;

  if (!conform)
    {
      /* Numeric can be converted to any other numeric. And Hollerith can be
	 converted to any other type.  */
      if ((gfc_numeric_ts (&lvalue->ts) && gfc_numeric_ts (&rvalue->ts))
	  || rvalue->ts.type == BT_HOLLERITH)
	return SUCCESS;

      if (lvalue->ts.type == BT_LOGICAL && rvalue->ts.type == BT_LOGICAL)
	return SUCCESS;

      gfc_error ("Incompatible types in assignment at %L, %s to %s",
		 &rvalue->where, gfc_typename (&rvalue->ts),
		 gfc_typename (&lvalue->ts));

      return FAILURE;
    }

  return gfc_convert_type (rvalue, &lvalue->ts, 1);
}


/* Check that a pointer assignment is OK.  We first check lvalue, and
   we only check rvalue if it's not an assignment to NULL() or a
   NULLIFY statement.  */

try
gfc_check_pointer_assign (gfc_expr * lvalue, gfc_expr * rvalue)
{
  symbol_attribute attr;
  int is_pure;

  if (lvalue->symtree->n.sym->ts.type == BT_UNKNOWN)
    {
      gfc_error ("Pointer assignment target is not a POINTER at %L",
		 &lvalue->where);
      return FAILURE;
    }

  attr = gfc_variable_attr (lvalue, NULL);
  if (!attr.pointer)
    {
      gfc_error ("Pointer assignment to non-POINTER at %L", &lvalue->where);
      return FAILURE;
    }

  is_pure = gfc_pure (NULL);

  if (is_pure && gfc_impure_variable (lvalue->symtree->n.sym))
    {
      gfc_error ("Bad pointer object in PURE procedure at %L",
		 &lvalue->where);
      return FAILURE;
    }

  /* If rvalue is a NULL() or NULLIFY, we're done. Otherwise the type,
     kind, etc for lvalue and rvalue must match, and rvalue must be a
     pure variable if we're in a pure function.  */
  if (rvalue->expr_type == EXPR_NULL)
    return SUCCESS;

  if (!gfc_compare_types (&lvalue->ts, &rvalue->ts))
    {
      gfc_error ("Different types in pointer assignment at %L",
		 &lvalue->where);
      return FAILURE;
    }

  if (lvalue->ts.kind != rvalue->ts.kind)
    {
      gfc_error ("Different kind type parameters in pointer "
		 "assignment at %L", &lvalue->where);
      return FAILURE;
    }

  attr = gfc_expr_attr (rvalue);
  if (!attr.target && !attr.pointer)
    {
      gfc_error ("Pointer assignment target is neither TARGET "
		 "nor POINTER at %L", &rvalue->where);
      return FAILURE;
    }

  if (is_pure && gfc_impure_variable (rvalue->symtree->n.sym))
    {
      gfc_error ("Bad target in pointer assignment in PURE "
		 "procedure at %L", &rvalue->where);
    }

  if (lvalue->rank != rvalue->rank)
    {
      gfc_error ("Unequal ranks %d and %d in pointer assignment at %L", 
		 lvalue->rank, rvalue->rank, &rvalue->where);
      return FAILURE;
    }

  if (gfc_has_vector_index (rvalue))
    {
      gfc_error ("Pointer assignment with vector subscript "
		 "on rhs at %L", &rvalue->where);
      return FAILURE;
    }

  return SUCCESS;
}


/* Relative of gfc_check_assign() except that the lvalue is a single
   symbol.  Used for initialization assignments.  */

try
gfc_check_assign_symbol (gfc_symbol * sym, gfc_expr * rvalue)
{
  gfc_expr lvalue;
  try r;

  memset (&lvalue, '\0', sizeof (gfc_expr));

  lvalue.expr_type = EXPR_VARIABLE;
  lvalue.ts = sym->ts;
  if (sym->as)
    lvalue.rank = sym->as->rank;
  lvalue.symtree = (gfc_symtree *)gfc_getmem (sizeof (gfc_symtree));
  lvalue.symtree->n.sym = sym;
  lvalue.where = sym->declared_at;

  if (sym->attr.pointer)
    r = gfc_check_pointer_assign (&lvalue, rvalue);
  else
    r = gfc_check_assign (&lvalue, rvalue, 1);

  gfc_free (lvalue.symtree);

  return r;
}


/* Get an expression for a default initializer.  */

gfc_expr *
gfc_default_initializer (gfc_typespec *ts)
{
  gfc_constructor *tail;
  gfc_expr *init;
  gfc_component *c;

  init = NULL;

  /* See if we have a default initializer.  */
  for (c = ts->derived->components; c; c = c->next)
    {
      if (c->initializer && init == NULL)
        init = gfc_get_expr ();
    }

  if (init == NULL)
    return NULL;

  /* Build the constructor.  */
  init->expr_type = EXPR_STRUCTURE;
  init->ts = *ts;
  init->where = ts->derived->declared_at;
  tail = NULL;
  for (c = ts->derived->components; c; c = c->next)
    {
      if (tail == NULL)
        init->value.constructor = tail = gfc_get_constructor ();
      else
        {
          tail->next = gfc_get_constructor ();
          tail = tail->next;
        }

      if (c->initializer)
        tail->expr = gfc_copy_expr (c->initializer);
    }
  return init;
}


/* Given a symbol, create an expression node with that symbol as a
   variable. If the symbol is array valued, setup a reference of the
   whole array.  */

gfc_expr *
gfc_get_variable_expr (gfc_symtree * var)
{
  gfc_expr *e;

  e = gfc_get_expr ();
  e->expr_type = EXPR_VARIABLE;
  e->symtree = var;
  e->ts = var->n.sym->ts;

  if (var->n.sym->as != NULL)
    {
      e->rank = var->n.sym->as->rank;
      e->ref = gfc_get_ref ();
      e->ref->type = REF_ARRAY;
      e->ref->u.ar.type = AR_FULL;
    }

  return e;
}

