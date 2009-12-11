/* Routines for manipulation of expression nodes.
   Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009
   Free Software Foundation, Inc.
   Contributed by Andy Vaught

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
#include "match.h"
#include "target-memory.h" /* for gfc_convert_boz */

/* Get a new expr node.  */

gfc_expr *
gfc_get_expr (void)
{
  gfc_expr *e;

  e = XCNEW (gfc_expr);
  gfc_clear_ts (&e->ts);
  e->shape = NULL;
  e->ref = NULL;
  e->symtree = NULL;
  e->con_by_offset = NULL;
  return e;
}


/* Free an argument list and everything below it.  */

void
gfc_free_actual_arglist (gfc_actual_arglist *a1)
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
gfc_copy_actual_arglist (gfc_actual_arglist *p)
{
  gfc_actual_arglist *head, *tail, *new_arg;

  head = tail = NULL;

  for (; p; p = p->next)
    {
      new_arg = gfc_get_actual_arglist ();
      *new_arg = *p;

      new_arg->expr = gfc_copy_expr (p->expr);
      new_arg->next = NULL;

      if (head == NULL)
	head = new_arg;
      else
	tail->next = new_arg;

      tail = new_arg;
    }

  return head;
}


/* Free a list of reference structures.  */

void
gfc_free_ref_list (gfc_ref *p)
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
free_expr0 (gfc_expr *e)
{
  int n;

  switch (e->expr_type)
    {
    case EXPR_CONSTANT:
      /* Free any parts of the value that need freeing.  */
      switch (e->ts.type)
	{
	case BT_INTEGER:
	  mpz_clear (e->value.integer);
	  break;

	case BT_REAL:
	  mpfr_clear (e->value.real);
	  break;

	case BT_CHARACTER:
	  gfc_free (e->value.character.string);
	  break;

	case BT_COMPLEX:
	  mpc_clear (e->value.complex);
	  break;

	default:
	  break;
	}

      /* Free the representation.  */
      if (e->representation.string)
	gfc_free (e->representation.string);

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

    case EXPR_COMPCALL:
    case EXPR_PPC:
      gfc_free_actual_arglist (e->value.compcall.actual);
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
gfc_free_expr (gfc_expr *e)
{
  if (e == NULL)
    return;
  if (e->con_by_offset)
    splay_tree_delete (e->con_by_offset); 
  free_expr0 (e);
  gfc_free (e);
}


/* Graft the *src expression onto the *dest subexpression.  */

void
gfc_replace_expr (gfc_expr *dest, gfc_expr *src)
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
gfc_extract_int (gfc_expr *expr, int *result)
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

gfc_ref *
gfc_copy_ref (gfc_ref *src)
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

  dest->next = gfc_copy_ref (src->next);

  return dest;
}


/* Detect whether an expression has any vector index array references.  */

int
gfc_has_vector_index (gfc_expr *e)
{
  gfc_ref *ref;
  int i;
  for (ref = e->ref; ref; ref = ref->next)
    if (ref->type == REF_ARRAY)
      for (i = 0; i < ref->u.ar.dimen; i++)
	if (ref->u.ar.dimen_type[i] == DIMEN_VECTOR)
	  return 1;
  return 0;
}


/* Insert a reference to the component of the given name.
   Only to be used with CLASS containers.  */

void
gfc_add_component_ref (gfc_expr *e, const char *name)
{
  gfc_ref **tail = &(e->ref);
  gfc_ref *next = NULL;
  gfc_symbol *derived = e->symtree->n.sym->ts.u.derived;
  while (*tail != NULL)
    {
      if ((*tail)->type == REF_COMPONENT)
	derived = (*tail)->u.c.component->ts.u.derived;
      if ((*tail)->type == REF_ARRAY && (*tail)->next == NULL)
	break;
      tail = &((*tail)->next);
    }
  if (*tail != NULL && strcmp (name, "$data") == 0)
    next = *tail;
  (*tail) = gfc_get_ref();
  (*tail)->next = next;
  (*tail)->type = REF_COMPONENT;
  (*tail)->u.c.sym = derived;
  (*tail)->u.c.component = gfc_find_component (derived, name, true, true);
  gcc_assert((*tail)->u.c.component);
  if (!next)
    e->ts = (*tail)->u.c.component->ts;
}


/* Copy a shape array.  */

mpz_t *
gfc_copy_shape (mpz_t *shape, int rank)
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
   of range -- or anything else, just returns NULL.  */

mpz_t *
gfc_copy_shape_excluding (mpz_t *shape, int rank, gfc_expr *dim)
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
  n--; /* Convert to zero based index.  */
  if (n < 0 || n >= rank)
    return NULL;

  s = new_shape = gfc_get_shape (rank - 1);

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
gfc_copy_expr (gfc_expr *p)
{
  gfc_expr *q;
  gfc_char_t *s;
  char *c;

  if (p == NULL)
    return NULL;

  q = gfc_get_expr ();
  *q = *p;

  switch (q->expr_type)
    {
    case EXPR_SUBSTRING:
      s = gfc_get_wide_string (p->value.character.length + 1);
      q->value.character.string = s;
      memcpy (s, p->value.character.string,
	      (p->value.character.length + 1) * sizeof (gfc_char_t));
      break;

    case EXPR_CONSTANT:
      /* Copy target representation, if it exists.  */
      if (p->representation.string)
	{
	  c = XCNEWVEC (char, p->representation.length + 1);
	  q->representation.string = c;
	  memcpy (c, p->representation.string, (p->representation.length + 1));
	}

      /* Copy the values of any pointer components of p->value.  */
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
	  mpc_init2 (q->value.complex, mpfr_get_default_prec());
	  mpc_set (q->value.complex, p->value.complex, GFC_MPC_RND_MODE);
	  break;

	case BT_CHARACTER:
	  if (p->representation.string)
	    q->value.character.string
	      = gfc_char_to_widechar (q->representation.string);
	  else
	    {
	      s = gfc_get_wide_string (p->value.character.length + 1);
	      q->value.character.string = s;

	      /* This is the case for the C_NULL_CHAR named constant.  */
	      if (p->value.character.length == 0
		  && (p->ts.is_c_interop || p->ts.is_iso_c))
		{
		  *s = '\0';
		  /* Need to set the length to 1 to make sure the NUL
		     terminator is copied.  */
		  q->value.character.length = 1;
		}
	      else
		memcpy (s, p->value.character.string,
			(p->value.character.length + 1) * sizeof (gfc_char_t));
	    }
	  break;

	case BT_HOLLERITH:
	case BT_LOGICAL:
	case BT_DERIVED:
	case BT_CLASS:
	  break;		/* Already done.  */

	case BT_PROCEDURE:
        case BT_VOID:
           /* Should never be reached.  */
	case BT_UNKNOWN:
	  gfc_internal_error ("gfc_copy_expr(): Bad expr node");
	  /* Not reached.  */
	}

      break;

    case EXPR_OP:
      switch (q->value.op.op)
	{
	case INTRINSIC_NOT:
	case INTRINSIC_PARENTHESES:
	case INTRINSIC_UPLUS:
	case INTRINSIC_UMINUS:
	  q->value.op.op1 = gfc_copy_expr (p->value.op.op1);
	  break;

	default:		/* Binary operators.  */
	  q->value.op.op1 = gfc_copy_expr (p->value.op.op1);
	  q->value.op.op2 = gfc_copy_expr (p->value.op.op2);
	  break;
	}

      break;

    case EXPR_FUNCTION:
      q->value.function.actual =
	gfc_copy_actual_arglist (p->value.function.actual);
      break;

    case EXPR_COMPCALL:
    case EXPR_PPC:
      q->value.compcall.actual =
	gfc_copy_actual_arglist (p->value.compcall.actual);
      q->value.compcall.tbp = p->value.compcall.tbp;
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

  q->ref = gfc_copy_ref (p->ref);

  return q;
}


/* Return the maximum kind of two expressions.  In general, higher
   kind numbers mean more precision for numeric types.  */

int
gfc_kind_max (gfc_expr *e1, gfc_expr *e2)
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
gfc_numeric_ts (gfc_typespec *ts)
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
gfc_logical_expr (int i, locus *where)
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
gfc_build_conversion (gfc_expr *e)
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
   have the same type. Conversion warnings are disabled if wconversion
   is set to 0.

   The exception is that the operands of an exponential don't have to
   have the same type.  If possible, the base is promoted to the type
   of the exponent.  For example, 1**2.3 becomes 1.0**2.3, but
   1.0**2 stays as it is.  */

void
gfc_type_convert_binary (gfc_expr *e, int wconversion)
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
	gfc_convert_type_warn (op2, &op1->ts, 2, wconversion);
      else
	gfc_convert_type_warn (op1, &op2->ts, 2, wconversion);

      e->ts = op1->ts;
      goto done;
    }

  /* Integer combined with real or complex.  */
  if (op2->ts.type == BT_INTEGER)
    {
      e->ts = op1->ts;

      /* Special case for ** operator.  */
      if (e->value.op.op == INTRINSIC_POWER)
	goto done;

      gfc_convert_type_warn (e->value.op.op2, &e->ts, 2, wconversion);
      goto done;
    }

  if (op1->ts.type == BT_INTEGER)
    {
      e->ts = op2->ts;
      gfc_convert_type_warn (e->value.op.op1, &e->ts, 2, wconversion);
      goto done;
    }

  /* Real combined with complex.  */
  e->ts.type = BT_COMPLEX;
  if (op1->ts.kind > op2->ts.kind)
    e->ts.kind = op1->ts.kind;
  else
    e->ts.kind = op2->ts.kind;
  if (op1->ts.type != BT_COMPLEX || op1->ts.kind != e->ts.kind)
    gfc_convert_type_warn (e->value.op.op1, &e->ts, 2, wconversion);
  if (op2->ts.type != BT_COMPLEX || op2->ts.kind != e->ts.kind)
    gfc_convert_type_warn (e->value.op.op2, &e->ts, 2, wconversion);

done:
  return;
}


static match
check_specification_function (gfc_expr *e)
{
  gfc_symbol *sym;

  if (!e->symtree)
    return MATCH_NO;

  sym = e->symtree->n.sym;

  /* F95, 7.1.6.2; F2003, 7.1.7  */
  if (sym
      && sym->attr.function
      && sym->attr.pure
      && !sym->attr.intrinsic
      && !sym->attr.recursive
      && sym->attr.proc != PROC_INTERNAL
      && sym->attr.proc != PROC_ST_FUNCTION
      && sym->attr.proc != PROC_UNKNOWN
      && sym->formal == NULL)
    return MATCH_YES;

  return MATCH_NO;
}

/* Function to determine if an expression is constant or not.  This
   function expects that the expression has already been simplified.  */

int
gfc_is_constant_expr (gfc_expr *e)
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
      /* Specification functions are constant.  */
      if (check_specification_function (e) == MATCH_YES)
	{
	  rv = 1;
	  break;
	}

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
      rv = e->ref == NULL || (gfc_is_constant_expr (e->ref->u.ss.start)
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


/* Is true if an array reference is followed by a component or substring
   reference.  */
bool
is_subref_array (gfc_expr * e)
{
  gfc_ref * ref;
  bool seen_array;

  if (e->expr_type != EXPR_VARIABLE)
    return false;

  if (e->symtree->n.sym->attr.subref_array_pointer)
    return true;

  seen_array = false;
  for (ref = e->ref; ref; ref = ref->next)
    {
      if (ref->type == REF_ARRAY
	    && ref->u.ar.type != AR_ELEMENT)
	seen_array = true;

      if (seen_array
	    && ref->type != REF_ARRAY)
	return seen_array;
    }
  return false;
}


/* Try to collapse intrinsic expressions.  */

static gfc_try
simplify_intrinsic_op (gfc_expr *p, int type)
{
  gfc_intrinsic_op op;
  gfc_expr *op1, *op2, *result;

  if (p->value.op.op == INTRINSIC_USER)
    return SUCCESS;

  op1 = p->value.op.op1;
  op2 = p->value.op.op2;
  op  = p->value.op.op;

  if (gfc_simplify_expr (op1, type) == FAILURE)
    return FAILURE;
  if (gfc_simplify_expr (op2, type) == FAILURE)
    return FAILURE;

  if (!gfc_is_constant_expr (op1)
      || (op2 != NULL && !gfc_is_constant_expr (op2)))
    return SUCCESS;

  /* Rip p apart.  */
  p->value.op.op1 = NULL;
  p->value.op.op2 = NULL;

  switch (op)
    {
    case INTRINSIC_PARENTHESES:
      result = gfc_parentheses (op1);
      break;

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
    case INTRINSIC_EQ_OS:
      result = gfc_eq (op1, op2, op);
      break;

    case INTRINSIC_NE:
    case INTRINSIC_NE_OS:
      result = gfc_ne (op1, op2, op);
      break;

    case INTRINSIC_GT:
    case INTRINSIC_GT_OS:
      result = gfc_gt (op1, op2, op);
      break;

    case INTRINSIC_GE:
    case INTRINSIC_GE_OS:
      result = gfc_ge (op1, op2, op);
      break;

    case INTRINSIC_LT:
    case INTRINSIC_LT_OS:
      result = gfc_lt (op1, op2, op);
      break;

    case INTRINSIC_LE:
    case INTRINSIC_LE_OS:
      result = gfc_le (op1, op2, op);
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

  result->rank = p->rank;
  result->where = p->where;
  gfc_replace_expr (p, result);

  return SUCCESS;
}


/* Subroutine to simplify constructor expressions.  Mutually recursive
   with gfc_simplify_expr().  */

static gfc_try
simplify_constructor (gfc_constructor *c, int type)
{
  gfc_expr *p;

  for (; c; c = c->next)
    {
      if (c->iterator
	  && (gfc_simplify_expr (c->iterator->start, type) == FAILURE
	      || gfc_simplify_expr (c->iterator->end, type) == FAILURE
	      || gfc_simplify_expr (c->iterator->step, type) == FAILURE))
	return FAILURE;

      if (c->expr)
	{
	  /* Try and simplify a copy.  Replace the original if successful
	     but keep going through the constructor at all costs.  Not
	     doing so can make a dog's dinner of complicated things.  */
	  p = gfc_copy_expr (c->expr);

	  if (gfc_simplify_expr (p, type) == FAILURE)
	    {
	      gfc_free_expr (p);
	      continue;
	    }

	  gfc_replace_expr (c->expr, p);
	}
    }

  return SUCCESS;
}


/* Pull a single array element out of an array constructor.  */

static gfc_try
find_array_element (gfc_constructor *cons, gfc_array_ref *ar,
		    gfc_constructor **rval)
{
  unsigned long nelemen;
  int i;
  mpz_t delta;
  mpz_t offset;
  mpz_t span;
  mpz_t tmp;
  gfc_expr *e;
  gfc_try t;

  t = SUCCESS;
  e = NULL;

  mpz_init_set_ui (offset, 0);
  mpz_init (delta);
  mpz_init (tmp);
  mpz_init_set_ui (span, 1);
  for (i = 0; i < ar->dimen; i++)
    {
      if (gfc_reduce_init_expr (ar->as->lower[i]) == FAILURE
	  || gfc_reduce_init_expr (ar->as->upper[i]) == FAILURE)
	{
	  t = FAILURE;
	  cons = NULL;
	  goto depart;
	}

      e = gfc_copy_expr (ar->start[i]);
      if (e->expr_type != EXPR_CONSTANT)
	{
	  cons = NULL;
	  goto depart;
	}

      gcc_assert (ar->as->upper[i]->expr_type == EXPR_CONSTANT
		  && ar->as->lower[i]->expr_type == EXPR_CONSTANT);

      /* Check the bounds.  */
      if ((ar->as->upper[i]
	   && mpz_cmp (e->value.integer,
		       ar->as->upper[i]->value.integer) > 0)
	  || (mpz_cmp (e->value.integer,
		       ar->as->lower[i]->value.integer) < 0))
	{
	  gfc_error ("Index in dimension %d is out of bounds "
		     "at %L", i + 1, &ar->c_where[i]);
	  cons = NULL;
	  t = FAILURE;
	  goto depart;
	}

      mpz_sub (delta, e->value.integer, ar->as->lower[i]->value.integer);
      mpz_mul (delta, delta, span);
      mpz_add (offset, offset, delta);

      mpz_set_ui (tmp, 1);
      mpz_add (tmp, tmp, ar->as->upper[i]->value.integer);
      mpz_sub (tmp, tmp, ar->as->lower[i]->value.integer);
      mpz_mul (span, span, tmp);
    }

  for (nelemen = mpz_get_ui (offset); nelemen > 0; nelemen--)
    {
      if (cons)
	{
	  if (cons->iterator)
	    {
	      cons = NULL;
	      goto depart;
	    }
	  cons = cons->next;
	}
    }

depart:
  mpz_clear (delta);
  mpz_clear (offset);
  mpz_clear (span);
  mpz_clear (tmp);
  if (e)
    gfc_free_expr (e);
  *rval = cons;
  return t;
}


/* Find a component of a structure constructor.  */

static gfc_constructor *
find_component_ref (gfc_constructor *cons, gfc_ref *ref)
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
remove_subobject_ref (gfc_expr *p, gfc_constructor *cons)
{
  gfc_expr *e;

  e = cons->expr;
  cons->expr = NULL;
  e->ref = p->ref->next;
  p->ref->next =  NULL;
  gfc_replace_expr (p, e);
}


/* Pull an array section out of an array constructor.  */

static gfc_try
find_array_section (gfc_expr *expr, gfc_ref *ref)
{
  int idx;
  int rank;
  int d;
  int shape_i;
  long unsigned one = 1;
  bool incr_ctr;
  mpz_t start[GFC_MAX_DIMENSIONS];
  mpz_t end[GFC_MAX_DIMENSIONS];
  mpz_t stride[GFC_MAX_DIMENSIONS];
  mpz_t delta[GFC_MAX_DIMENSIONS];
  mpz_t ctr[GFC_MAX_DIMENSIONS];
  mpz_t delta_mpz;
  mpz_t tmp_mpz;
  mpz_t nelts;
  mpz_t ptr;
  mpz_t index;
  gfc_constructor *cons;
  gfc_constructor *base;
  gfc_expr *begin;
  gfc_expr *finish;
  gfc_expr *step;
  gfc_expr *upper;
  gfc_expr *lower;
  gfc_constructor *vecsub[GFC_MAX_DIMENSIONS], *c;
  gfc_try t;

  t = SUCCESS;

  base = expr->value.constructor;
  expr->value.constructor = NULL;

  rank = ref->u.ar.as->rank;

  if (expr->shape == NULL)
    expr->shape = gfc_get_shape (rank);

  mpz_init_set_ui (delta_mpz, one);
  mpz_init_set_ui (nelts, one);
  mpz_init (tmp_mpz);

  /* Do the initialization now, so that we can cleanup without
     keeping track of where we were.  */
  for (d = 0; d < rank; d++)
    {
      mpz_init (delta[d]);
      mpz_init (start[d]);
      mpz_init (end[d]);
      mpz_init (ctr[d]);
      mpz_init (stride[d]);
      vecsub[d] = NULL;
    }

  /* Build the counters to clock through the array reference.  */
  shape_i = 0;
  for (d = 0; d < rank; d++)
    {
      /* Make this stretch of code easier on the eye!  */
      begin = ref->u.ar.start[d];
      finish = ref->u.ar.end[d];
      step = ref->u.ar.stride[d];
      lower = ref->u.ar.as->lower[d];
      upper = ref->u.ar.as->upper[d];

      if (ref->u.ar.dimen_type[d] == DIMEN_VECTOR)  /* Vector subscript.  */
	{
	  gcc_assert (begin);

	  if (begin->expr_type != EXPR_ARRAY || !gfc_is_constant_expr (begin))
	    {
	      t = FAILURE;
	      goto cleanup;
	    }

	  gcc_assert (begin->rank == 1);
	  /* Zero-sized arrays have no shape and no elements, stop early.  */
	  if (!begin->shape) 
	    {
	      mpz_init_set_ui (nelts, 0);
	      break;
	    }

	  vecsub[d] = begin->value.constructor;
	  mpz_set (ctr[d], vecsub[d]->expr->value.integer);
	  mpz_mul (nelts, nelts, begin->shape[0]);
	  mpz_set (expr->shape[shape_i++], begin->shape[0]);

	  /* Check bounds.  */
	  for (c = vecsub[d]; c; c = c->next)
	    {
	      if (mpz_cmp (c->expr->value.integer, upper->value.integer) > 0
		  || mpz_cmp (c->expr->value.integer,
			      lower->value.integer) < 0)
		{
		  gfc_error ("index in dimension %d is out of bounds "
			     "at %L", d + 1, &ref->u.ar.c_where[d]);
		  t = FAILURE;
		  goto cleanup;
		}
	    }
	}
      else
	{
	  if ((begin && begin->expr_type != EXPR_CONSTANT)
	      || (finish && finish->expr_type != EXPR_CONSTANT)
	      || (step && step->expr_type != EXPR_CONSTANT))
	    {
	      t = FAILURE;
	      goto cleanup;
	    }

	  /* Obtain the stride.  */
	  if (step)
	    mpz_set (stride[d], step->value.integer);
	  else
	    mpz_set_ui (stride[d], one);

	  if (mpz_cmp_ui (stride[d], 0) == 0)
	    mpz_set_ui (stride[d], one);

	  /* Obtain the start value for the index.  */
	  if (begin)
	    mpz_set (start[d], begin->value.integer);
	  else
	    mpz_set (start[d], lower->value.integer);

	  mpz_set (ctr[d], start[d]);

	  /* Obtain the end value for the index.  */
	  if (finish)
	    mpz_set (end[d], finish->value.integer);
	  else
	    mpz_set (end[d], upper->value.integer);

	  /* Separate 'if' because elements sometimes arrive with
	     non-null end.  */
	  if (ref->u.ar.dimen_type[d] == DIMEN_ELEMENT)
	    mpz_set (end [d], begin->value.integer);

	  /* Check the bounds.  */
	  if (mpz_cmp (ctr[d], upper->value.integer) > 0
	      || mpz_cmp (end[d], upper->value.integer) > 0
	      || mpz_cmp (ctr[d], lower->value.integer) < 0
	      || mpz_cmp (end[d], lower->value.integer) < 0)
	    {
	      gfc_error ("index in dimension %d is out of bounds "
			 "at %L", d + 1, &ref->u.ar.c_where[d]);
	      t = FAILURE;
	      goto cleanup;
	    }

	  /* Calculate the number of elements and the shape.  */
	  mpz_set (tmp_mpz, stride[d]);
	  mpz_add (tmp_mpz, end[d], tmp_mpz);
	  mpz_sub (tmp_mpz, tmp_mpz, ctr[d]);
	  mpz_div (tmp_mpz, tmp_mpz, stride[d]);
	  mpz_mul (nelts, nelts, tmp_mpz);

	  /* An element reference reduces the rank of the expression; don't
	     add anything to the shape array.  */
	  if (ref->u.ar.dimen_type[d] != DIMEN_ELEMENT) 
	    mpz_set (expr->shape[shape_i++], tmp_mpz);
	}

      /* Calculate the 'stride' (=delta) for conversion of the
	 counter values into the index along the constructor.  */
      mpz_set (delta[d], delta_mpz);
      mpz_sub (tmp_mpz, upper->value.integer, lower->value.integer);
      mpz_add_ui (tmp_mpz, tmp_mpz, one);
      mpz_mul (delta_mpz, delta_mpz, tmp_mpz);
    }

  mpz_init (index);
  mpz_init (ptr);
  cons = base;

  /* Now clock through the array reference, calculating the index in
     the source constructor and transferring the elements to the new
     constructor.  */  
  for (idx = 0; idx < (int) mpz_get_si (nelts); idx++)
    {
      if (ref->u.ar.offset)
	mpz_set (ptr, ref->u.ar.offset->value.integer);
      else
	mpz_init_set_ui (ptr, 0);

      incr_ctr = true;
      for (d = 0; d < rank; d++)
	{
	  mpz_set (tmp_mpz, ctr[d]);
	  mpz_sub (tmp_mpz, tmp_mpz, ref->u.ar.as->lower[d]->value.integer);
	  mpz_mul (tmp_mpz, tmp_mpz, delta[d]);
	  mpz_add (ptr, ptr, tmp_mpz);

	  if (!incr_ctr) continue;

	  if (ref->u.ar.dimen_type[d] == DIMEN_VECTOR) /* Vector subscript.  */
	    {
	      gcc_assert(vecsub[d]);

	      if (!vecsub[d]->next)
		vecsub[d] = ref->u.ar.start[d]->value.constructor;
	      else
		{
		  vecsub[d] = vecsub[d]->next;
		  incr_ctr = false;
		}
	      mpz_set (ctr[d], vecsub[d]->expr->value.integer);
	    }
	  else
	    {
	      mpz_add (ctr[d], ctr[d], stride[d]); 

	      if (mpz_cmp_ui (stride[d], 0) > 0
		  ? mpz_cmp (ctr[d], end[d]) > 0
		  : mpz_cmp (ctr[d], end[d]) < 0)
		mpz_set (ctr[d], start[d]);
	      else
		incr_ctr = false;
	    }
	}

      /* There must be a better way of dealing with negative strides
	 than resetting the index and the constructor pointer!  */ 
      if (mpz_cmp (ptr, index) < 0)
	{
	  mpz_set_ui (index, 0);
	  cons = base;
	}

      while (cons && cons->next && mpz_cmp (ptr, index) > 0)
	{
	  mpz_add_ui (index, index, one);
	  cons = cons->next;
	}

      gfc_append_constructor (expr, gfc_copy_expr (cons->expr));
    }

  mpz_clear (ptr);
  mpz_clear (index);

cleanup:

  mpz_clear (delta_mpz);
  mpz_clear (tmp_mpz);
  mpz_clear (nelts);
  for (d = 0; d < rank; d++)
    {
      mpz_clear (delta[d]);
      mpz_clear (start[d]);
      mpz_clear (end[d]);
      mpz_clear (ctr[d]);
      mpz_clear (stride[d]);
    }
  gfc_free_constructor (base);
  return t;
}

/* Pull a substring out of an expression.  */

static gfc_try
find_substring_ref (gfc_expr *p, gfc_expr **newp)
{
  int end;
  int start;
  int length;
  gfc_char_t *chr;

  if (p->ref->u.ss.start->expr_type != EXPR_CONSTANT
      || p->ref->u.ss.end->expr_type != EXPR_CONSTANT)
    return FAILURE;

  *newp = gfc_copy_expr (p);
  gfc_free ((*newp)->value.character.string);

  end = (int) mpz_get_ui (p->ref->u.ss.end->value.integer);
  start = (int) mpz_get_ui (p->ref->u.ss.start->value.integer);
  length = end - start + 1;

  chr = (*newp)->value.character.string = gfc_get_wide_string (length + 1);
  (*newp)->value.character.length = length;
  memcpy (chr, &p->value.character.string[start - 1],
	  length * sizeof (gfc_char_t));
  chr[length] = '\0';
  return SUCCESS;
}



/* Simplify a subobject reference of a constructor.  This occurs when
   parameter variable values are substituted.  */

static gfc_try
simplify_const_ref (gfc_expr *p)
{
  gfc_constructor *cons;
  gfc_expr *newp;

  while (p->ref)
    {
      switch (p->ref->type)
	{
	case REF_ARRAY:
	  switch (p->ref->u.ar.type)
	    {
	    case AR_ELEMENT:
	      if (find_array_element (p->value.constructor, &p->ref->u.ar,
				      &cons) == FAILURE)
		return FAILURE;

	      if (!cons)
		return SUCCESS;

	      remove_subobject_ref (p, cons);
	      break;

	    case AR_SECTION:
	      if (find_array_section (p, p->ref) == FAILURE)
		return FAILURE;
	      p->ref->u.ar.type = AR_FULL;

	    /* Fall through.  */

	    case AR_FULL:
	      if (p->ref->next != NULL
		  && (p->ts.type == BT_CHARACTER || p->ts.type == BT_DERIVED))
		{
		  cons = p->value.constructor;
		  for (; cons; cons = cons->next)
		    {
		      cons->expr->ref = gfc_copy_ref (p->ref->next);
		      if (simplify_const_ref (cons->expr) == FAILURE)
			return FAILURE;
		    }

		  /* If this is a CHARACTER array and we possibly took a
		     substring out of it, update the type-spec's character
		     length according to the first element (as all should have
		     the same length).  */
		  if (p->ts.type == BT_CHARACTER)
		    {
		      int string_len;

		      gcc_assert (p->ref->next);
		      gcc_assert (!p->ref->next->next);
		      gcc_assert (p->ref->next->type == REF_SUBSTRING);

		      if (p->value.constructor)
			{
			  const gfc_expr* first = p->value.constructor->expr;
			  gcc_assert (first->expr_type == EXPR_CONSTANT);
			  gcc_assert (first->ts.type == BT_CHARACTER);
			  string_len = first->value.character.length;
			}
		      else
			string_len = 0;

		      if (!p->ts.u.cl)
			p->ts.u.cl = gfc_new_charlen (p->symtree->n.sym->ns,
						      NULL);
		      else
			gfc_free_expr (p->ts.u.cl->length);

		      p->ts.u.cl->length = gfc_int_expr (string_len);
		    }
		}
	      gfc_free_ref_list (p->ref);
	      p->ref = NULL;
	      break;

	    default:
	      return SUCCESS;
	    }

	  break;

	case REF_COMPONENT:
	  cons = find_component_ref (p->value.constructor, p->ref);
	  remove_subobject_ref (p, cons);
	  break;

	case REF_SUBSTRING:
  	  if (find_substring_ref (p, &newp) == FAILURE)
	    return FAILURE;

	  gfc_replace_expr (p, newp);
	  gfc_free_ref_list (p->ref);
	  p->ref = NULL;
	  break;
	}
    }

  return SUCCESS;
}


/* Simplify a chain of references.  */

static gfc_try
simplify_ref_chain (gfc_ref *ref, int type)
{
  int n;

  for (; ref; ref = ref->next)
    {
      switch (ref->type)
	{
	case REF_ARRAY:
	  for (n = 0; n < ref->u.ar.dimen; n++)
	    {
	      if (gfc_simplify_expr (ref->u.ar.start[n], type) == FAILURE)
		return FAILURE;
	      if (gfc_simplify_expr (ref->u.ar.end[n], type) == FAILURE)
		return FAILURE;
	      if (gfc_simplify_expr (ref->u.ar.stride[n], type) == FAILURE)
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

static gfc_try
simplify_parameter_variable (gfc_expr *p, int type)
{
  gfc_expr *e;
  gfc_try t;

  e = gfc_copy_expr (p->symtree->n.sym->value);
  if (e == NULL)
    return FAILURE;

  e->rank = p->rank;

  /* Do not copy subobject refs for constant.  */
  if (e->expr_type != EXPR_CONSTANT && p->ref != NULL)
    e->ref = gfc_copy_ref (p->ref);
  t = gfc_simplify_expr (e, type);

  /* Only use the simplification if it eliminated all subobject references.  */
  if (t == SUCCESS && !e->ref)
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

gfc_try
gfc_simplify_expr (gfc_expr *p, int type)
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
	  gfc_char_t *s;
	  int start, end;

	  start = 0;
	  if (p->ref && p->ref->u.ss.start)
	    {
	      gfc_extract_int (p->ref->u.ss.start, &start);
	      start--;  /* Convert from one-based to zero-based.  */
	    }

	  end = p->value.character.length;
	  if (p->ref && p->ref->u.ss.end)
	    gfc_extract_int (p->ref->u.ss.end, &end);

	  s = gfc_get_wide_string (end - start + 2);
	  memcpy (s, p->value.character.string + start,
		  (end - start) * sizeof (gfc_char_t));
	  s[end - start + 1] = '\0';  /* TODO: C-style string.  */
	  gfc_free (p->value.character.string);
	  p->value.character.string = s;
	  p->value.character.length = end - start;
	  p->ts.u.cl = gfc_new_charlen (gfc_current_ns, NULL);
	  p->ts.u.cl->length = gfc_int_expr (p->value.character.length);
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

      if (p->expr_type == EXPR_ARRAY && p->ref && p->ref->type == REF_ARRAY
	  && p->ref->u.ar.type == AR_FULL)
	  gfc_expand_constructor (p);

      if (simplify_const_ref (p) == FAILURE)
	return FAILURE;

      break;

    case EXPR_COMPCALL:
    case EXPR_PPC:
      gcc_unreachable ();
      break;
    }

  return SUCCESS;
}


/* Returns the type of an expression with the exception that iterator
   variables are automatically integers no matter what else they may
   be declared as.  */

static bt
et0 (gfc_expr *e)
{
  if (e->expr_type == EXPR_VARIABLE && gfc_check_iter_variable (e) == SUCCESS)
    return BT_INTEGER;

  return e->ts.type;
}


/* Check an intrinsic arithmetic operation to see if it is consistent
   with some type of expression.  */

static gfc_try check_init_expr (gfc_expr *);


/* Scalarize an expression for an elemental intrinsic call.  */

static gfc_try
scalarize_intrinsic_call (gfc_expr *e)
{
  gfc_actual_arglist *a, *b;
  gfc_constructor *args[5], *ctor, *new_ctor;
  gfc_expr *expr, *old;
  int n, i, rank[5], array_arg;

  /* Find which, if any, arguments are arrays.  Assume that the old
     expression carries the type information and that the first arg
     that is an array expression carries all the shape information.*/
  n = array_arg = 0;
  a = e->value.function.actual;
  for (; a; a = a->next)
    {
      n++;
      if (a->expr->expr_type != EXPR_ARRAY)
	continue;
      array_arg = n;
      expr = gfc_copy_expr (a->expr);
      break;
    }

  if (!array_arg)
    return FAILURE;

  old = gfc_copy_expr (e);

  gfc_free_constructor (expr->value.constructor);
  expr->value.constructor = NULL;

  expr->ts = old->ts;
  expr->where = old->where;
  expr->expr_type = EXPR_ARRAY;

  /* Copy the array argument constructors into an array, with nulls
     for the scalars.  */
  n = 0;
  a = old->value.function.actual;
  for (; a; a = a->next)
    {
      /* Check that this is OK for an initialization expression.  */
      if (a->expr && check_init_expr (a->expr) == FAILURE)
	goto cleanup;

      rank[n] = 0;
      if (a->expr && a->expr->rank && a->expr->expr_type == EXPR_VARIABLE)
	{
	  rank[n] = a->expr->rank;
	  ctor = a->expr->symtree->n.sym->value->value.constructor;
	  args[n] = gfc_copy_constructor (ctor);
	}
      else if (a->expr && a->expr->expr_type == EXPR_ARRAY)
	{
	  if (a->expr->rank)
	    rank[n] = a->expr->rank;
	  else
	    rank[n] = 1;
	  args[n] = gfc_copy_constructor (a->expr->value.constructor);
	}
      else
	args[n] = NULL;
      n++;
    }


  /* Using the array argument as the master, step through the array
     calling the function for each element and advancing the array
     constructors together.  */
  ctor = args[array_arg - 1];
  new_ctor = NULL;
  for (; ctor; ctor = ctor->next)
    {
	  if (expr->value.constructor == NULL)
	    expr->value.constructor
		= new_ctor = gfc_get_constructor ();
	  else
	    {
	      new_ctor->next = gfc_get_constructor ();
	      new_ctor = new_ctor->next;
	    }
	  new_ctor->expr = gfc_copy_expr (old);
	  gfc_free_actual_arglist (new_ctor->expr->value.function.actual);
	  a = NULL;
	  b = old->value.function.actual;
	  for (i = 0; i < n; i++)
	    {
	      if (a == NULL)
		new_ctor->expr->value.function.actual
			= a = gfc_get_actual_arglist ();
	      else
		{
		  a->next = gfc_get_actual_arglist ();
		  a = a->next;
		}
	      if (args[i])
		a->expr = gfc_copy_expr (args[i]->expr);
	      else
		a->expr = gfc_copy_expr (b->expr);

	      b = b->next;
	    }

	  /* Simplify the function calls.  If the simplification fails, the
	     error will be flagged up down-stream or the library will deal
	     with it.  */
	  gfc_simplify_expr (new_ctor->expr, 0);

	  for (i = 0; i < n; i++)
	    if (args[i])
	      args[i] = args[i]->next;

	  for (i = 1; i < n; i++)
	    if (rank[i] && ((args[i] != NULL && args[array_arg - 1] == NULL)
			 || (args[i] == NULL && args[array_arg - 1] != NULL)))
	      goto compliance;
    }

  free_expr0 (e);
  *e = *expr;
  gfc_free_expr (old);
  return SUCCESS;

compliance:
  gfc_error_now ("elemental function arguments at %C are not compliant");

cleanup:
  gfc_free_expr (expr);
  gfc_free_expr (old);
  return FAILURE;
}


static gfc_try
check_intrinsic_op (gfc_expr *e, gfc_try (*check_function) (gfc_expr *))
{
  gfc_expr *op1 = e->value.op.op1;
  gfc_expr *op2 = e->value.op.op2;

  if ((*check_function) (op1) == FAILURE)
    return FAILURE;

  switch (e->value.op.op)
    {
    case INTRINSIC_UPLUS:
    case INTRINSIC_UMINUS:
      if (!numeric_type (et0 (op1)))
	goto not_numeric;
      break;

    case INTRINSIC_EQ:
    case INTRINSIC_EQ_OS:
    case INTRINSIC_NE:
    case INTRINSIC_NE_OS:
    case INTRINSIC_GT:
    case INTRINSIC_GT_OS:
    case INTRINSIC_GE:
    case INTRINSIC_GE_OS:
    case INTRINSIC_LT:
    case INTRINSIC_LT_OS:
    case INTRINSIC_LE:
    case INTRINSIC_LE_OS:
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

    case INTRINSIC_PARENTHESES:
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

/* F2003, 7.1.7 (3): In init expression, allocatable components
   must not be data-initialized.  */
static gfc_try
check_alloc_comp_init (gfc_expr *e)
{
  gfc_component *c;
  gfc_constructor *ctor;

  gcc_assert (e->expr_type == EXPR_STRUCTURE);
  gcc_assert (e->ts.type == BT_DERIVED);

  for (c = e->ts.u.derived->components, ctor = e->value.constructor;
       c; c = c->next, ctor = ctor->next)
    {
      if (c->attr.allocatable
          && ctor->expr->expr_type != EXPR_NULL)
        {
	  gfc_error("Invalid initialization expression for ALLOCATABLE "
	            "component '%s' in structure constructor at %L",
	            c->name, &ctor->expr->where);
	  return FAILURE;
	}
    }

  return SUCCESS;
}

static match
check_init_expr_arguments (gfc_expr *e)
{
  gfc_actual_arglist *ap;

  for (ap = e->value.function.actual; ap; ap = ap->next)
    if (check_init_expr (ap->expr) == FAILURE)
      return MATCH_ERROR;

  return MATCH_YES;
}

static gfc_try check_restricted (gfc_expr *);

/* F95, 7.1.6.1, Initialization expressions, (7)
   F2003, 7.1.7 Initialization expression, (8)  */

static match
check_inquiry (gfc_expr *e, int not_restricted)
{
  const char *name;
  const char *const *functions;

  static const char *const inquiry_func_f95[] = {
    "lbound", "shape", "size", "ubound",
    "bit_size", "len", "kind",
    "digits", "epsilon", "huge", "maxexponent", "minexponent",
    "precision", "radix", "range", "tiny",
    NULL
  };

  static const char *const inquiry_func_f2003[] = {
    "lbound", "shape", "size", "ubound",
    "bit_size", "len", "kind",
    "digits", "epsilon", "huge", "maxexponent", "minexponent",
    "precision", "radix", "range", "tiny",
    "new_line", NULL
  };

  int i;
  gfc_actual_arglist *ap;

  if (!e->value.function.isym
      || !e->value.function.isym->inquiry)
    return MATCH_NO;

  /* An undeclared parameter will get us here (PR25018).  */
  if (e->symtree == NULL)
    return MATCH_NO;

  name = e->symtree->n.sym->name;

  functions = (gfc_option.warn_std & GFC_STD_F2003) 
		? inquiry_func_f2003 : inquiry_func_f95;

  for (i = 0; functions[i]; i++)
    if (strcmp (functions[i], name) == 0)
      break;

  if (functions[i] == NULL)
    return MATCH_ERROR;

  /* At this point we have an inquiry function with a variable argument.  The
     type of the variable might be undefined, but we need it now, because the
     arguments of these functions are not allowed to be undefined.  */

  for (ap = e->value.function.actual; ap; ap = ap->next)
    {
      if (!ap->expr)
	continue;

      if (ap->expr->ts.type == BT_UNKNOWN)
	{
	  if (ap->expr->symtree->n.sym->ts.type == BT_UNKNOWN
	      && gfc_set_default_type (ap->expr->symtree->n.sym, 0, gfc_current_ns)
	      == FAILURE)
	    return MATCH_NO;

	  ap->expr->ts = ap->expr->symtree->n.sym->ts;
	}

	/* Assumed character length will not reduce to a constant expression
	   with LEN, as required by the standard.  */
	if (i == 5 && not_restricted
	    && ap->expr->symtree->n.sym->ts.type == BT_CHARACTER
	    && ap->expr->symtree->n.sym->ts.u.cl->length == NULL)
	  {
	    gfc_error ("Assumed character length variable '%s' in constant "
		       "expression at %L", e->symtree->n.sym->name, &e->where);
	      return MATCH_ERROR;
	  }
	else if (not_restricted && check_init_expr (ap->expr) == FAILURE)
	  return MATCH_ERROR;

	if (not_restricted == 0
	      && ap->expr->expr_type != EXPR_VARIABLE
	      && check_restricted (ap->expr) == FAILURE)
	  return MATCH_ERROR;
    }

  return MATCH_YES;
}


/* F95, 7.1.6.1, Initialization expressions, (5)
   F2003, 7.1.7 Initialization expression, (5)  */

static match
check_transformational (gfc_expr *e)
{
  static const char * const trans_func_f95[] = {
    "repeat", "reshape", "selected_int_kind",
    "selected_real_kind", "transfer", "trim", NULL
  };

  static const char * const trans_func_f2003[] =  {
    "all", "any", "count", "dot_product", "matmul", "null", "pack",
    "product", "repeat", "reshape", "selected_char_kind", "selected_int_kind",
    "selected_real_kind", "spread", "sum", "transfer", "transpose",
    "trim", "unpack", NULL
  };

  int i;
  const char *name;
  const char *const *functions;

  if (!e->value.function.isym
      || !e->value.function.isym->transformational)
    return MATCH_NO;

  name = e->symtree->n.sym->name;

  functions = (gfc_option.allow_std & GFC_STD_F2003) 
		? trans_func_f2003 : trans_func_f95;

  /* NULL() is dealt with below.  */
  if (strcmp ("null", name) == 0)
    return MATCH_NO;

  for (i = 0; functions[i]; i++)
    if (strcmp (functions[i], name) == 0)
       break;

  if (functions[i] == NULL)
    {
      gfc_error("transformational intrinsic '%s' at %L is not permitted "
		"in an initialization expression", name, &e->where);
      return MATCH_ERROR;
    }

  return check_init_expr_arguments (e);
}


/* F95, 7.1.6.1, Initialization expressions, (6)
   F2003, 7.1.7 Initialization expression, (6)  */

static match
check_null (gfc_expr *e)
{
  if (strcmp ("null", e->symtree->n.sym->name) != 0)
    return MATCH_NO;

  return check_init_expr_arguments (e);
}


static match
check_elemental (gfc_expr *e)
{
  if (!e->value.function.isym
      || !e->value.function.isym->elemental)
    return MATCH_NO;

  if (e->ts.type != BT_INTEGER
      && e->ts.type != BT_CHARACTER
      && gfc_notify_std (GFC_STD_F2003, "Extension: Evaluation of "
			"nonstandard initialization expression at %L",
			&e->where) == FAILURE)
    return MATCH_ERROR;

  return check_init_expr_arguments (e);
}


static match
check_conversion (gfc_expr *e)
{
  if (!e->value.function.isym
      || !e->value.function.isym->conversion)
    return MATCH_NO;

  return check_init_expr_arguments (e);
}


/* Verify that an expression is an initialization expression.  A side
   effect is that the expression tree is reduced to a single constant
   node if all goes well.  This would normally happen when the
   expression is constructed but function references are assumed to be
   intrinsics in the context of initialization expressions.  If
   FAILURE is returned an error message has been generated.  */

static gfc_try
check_init_expr (gfc_expr *e)
{
  match m;
  gfc_try t;

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
      t = FAILURE;

      if ((m = check_specification_function (e)) != MATCH_YES)
	{
	  gfc_intrinsic_sym* isym;
          gfc_symbol* sym;

          sym = e->symtree->n.sym;
	  if (!gfc_is_intrinsic (sym, 0, e->where)
              || (m = gfc_intrinsic_func_interface (e, 0)) != MATCH_YES)
	    {
	      gfc_error ("Function '%s' in initialization expression at %L "
			 "must be an intrinsic or a specification function",
			 e->symtree->n.sym->name, &e->where);
	      break;
	    }

	  if ((m = check_conversion (e)) == MATCH_NO
	      && (m = check_inquiry (e, 1)) == MATCH_NO
	      && (m = check_null (e)) == MATCH_NO
	      && (m = check_transformational (e)) == MATCH_NO
	      && (m = check_elemental (e)) == MATCH_NO)
	    {
	      gfc_error ("Intrinsic function '%s' at %L is not permitted "
			 "in an initialization expression",
			 e->symtree->n.sym->name, &e->where);
	      m = MATCH_ERROR;
	    }

	  /* Try to scalarize an elemental intrinsic function that has an
	     array argument.  */
          isym = gfc_find_function (e->symtree->n.sym->name);
	  if (isym && isym->elemental
		&& (t = scalarize_intrinsic_call (e)) == SUCCESS)
	    break;
	}

      if (m == MATCH_YES)
	t = gfc_simplify_expr (e, 0);

      break;

    case EXPR_VARIABLE:
      t = SUCCESS;

      if (gfc_check_iter_variable (e) == SUCCESS)
	break;

      if (e->symtree->n.sym->attr.flavor == FL_PARAMETER)
	{
	  /* A PARAMETER shall not be used to define itself, i.e.
		REAL, PARAMETER :: x = transfer(0, x)
	     is invalid.  */
	  if (!e->symtree->n.sym->value)
	    {
	      gfc_error("PARAMETER '%s' is used at %L before its definition "
			"is complete", e->symtree->n.sym->name, &e->where);
	      t = FAILURE;
	    }
	  else
	    t = simplify_parameter_variable (e, 0);

	  break;
	}

      if (gfc_in_match_data ())
	break;

      t = FAILURE;

      if (e->symtree->n.sym->as)
	{
	  switch (e->symtree->n.sym->as->type)
	    {
	      case AS_ASSUMED_SIZE:
		gfc_error ("Assumed size array '%s' at %L is not permitted "
			   "in an initialization expression",
			   e->symtree->n.sym->name, &e->where);
		break;

	      case AS_ASSUMED_SHAPE:
		gfc_error ("Assumed shape array '%s' at %L is not permitted "
			   "in an initialization expression",
			   e->symtree->n.sym->name, &e->where);
		break;

	      case AS_DEFERRED:
		gfc_error ("Deferred array '%s' at %L is not permitted "
			   "in an initialization expression",
			   e->symtree->n.sym->name, &e->where);
		break;

	      case AS_EXPLICIT:
		gfc_error ("Array '%s' at %L is a variable, which does "
			   "not reduce to a constant expression",
			   e->symtree->n.sym->name, &e->where);
		break;

	      default:
		gcc_unreachable();
	  }
	}
      else
	gfc_error ("Parameter '%s' at %L has not been declared or is "
		   "a variable, which does not reduce to a constant "
		   "expression", e->symtree->n.sym->name, &e->where);

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
      t = e->ts.is_iso_c ? SUCCESS : FAILURE;
      if (t == SUCCESS)
	break;

      t = check_alloc_comp_init (e);
      if (t == FAILURE)
	break;

      t = gfc_check_constructor (e, check_init_expr);
      if (t == FAILURE)
	break;

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

/* Reduces a general expression to an initialization expression (a constant).
   This used to be part of gfc_match_init_expr.
   Note that this function doesn't free the given expression on FAILURE.  */

gfc_try
gfc_reduce_init_expr (gfc_expr *expr)
{
  gfc_try t;

  gfc_init_expr = 1;
  t = gfc_resolve_expr (expr);
  if (t == SUCCESS)
    t = check_init_expr (expr);
  gfc_init_expr = 0;

  if (t == FAILURE)
    return FAILURE;

  if (expr->expr_type == EXPR_ARRAY
      && (gfc_check_constructor_type (expr) == FAILURE
      || gfc_expand_constructor (expr) == FAILURE))
    return FAILURE;

  /* Not all inquiry functions are simplified to constant expressions
     so it is necessary to call check_inquiry again.  */ 
  if (!gfc_is_constant_expr (expr) && check_inquiry (expr, 1) != MATCH_YES
      && !gfc_in_match_data ())
    {
      gfc_error ("Initialization expression didn't reduce %C");
      return FAILURE;
    }

  return SUCCESS;
}


/* Match an initialization expression.  We work by first matching an
   expression, then reducing it to a constant.  The reducing it to 
   constant part requires a global variable to flag the prohibition
   of a non-integer exponent in -std=f95 mode.  */

bool init_flag = false;

match
gfc_match_init_expr (gfc_expr **result)
{
  gfc_expr *expr;
  match m;
  gfc_try t;

  expr = NULL;

  init_flag = true;

  m = gfc_match_expr (&expr);
  if (m != MATCH_YES)
    {
      init_flag = false;
      return m;
    }

  t = gfc_reduce_init_expr (expr);
  if (t != SUCCESS)
    {
      gfc_free_expr (expr);
      init_flag = false;
      return MATCH_ERROR;
    }

  *result = expr;
  init_flag = false;

  return MATCH_YES;
}


/* Given an actual argument list, test to see that each argument is a
   restricted expression and optionally if the expression type is
   integer or character.  */

static gfc_try
restricted_args (gfc_actual_arglist *a)
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

static gfc_try
external_spec_function (gfc_expr *e)
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

  if (!f->attr.pure && !f->attr.elemental)
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

static gfc_try
restricted_intrinsic (gfc_expr *e)
{
  /* TODO: Check constraints on inquiry functions.  7.1.6.2 (7).  */
  if (check_inquiry (e, 0) == MATCH_YES)
    return SUCCESS;

  return restricted_args (e->value.function.actual);
}


/* Check the expressions of an actual arglist.  Used by check_restricted.  */

static gfc_try
check_arglist (gfc_actual_arglist* arg, gfc_try (*checker) (gfc_expr*))
{
  for (; arg; arg = arg->next)
    if (checker (arg->expr) == FAILURE)
      return FAILURE;

  return SUCCESS;
}


/* Check the subscription expressions of a reference chain with a checking
   function; used by check_restricted.  */

static gfc_try
check_references (gfc_ref* ref, gfc_try (*checker) (gfc_expr*))
{
  int dim;

  if (!ref)
    return SUCCESS;

  switch (ref->type)
    {
    case REF_ARRAY:
      for (dim = 0; dim != ref->u.ar.dimen; ++dim)
	{
	  if (checker (ref->u.ar.start[dim]) == FAILURE)
	    return FAILURE;
	  if (checker (ref->u.ar.end[dim]) == FAILURE)
	    return FAILURE;
	  if (checker (ref->u.ar.stride[dim]) == FAILURE)
	    return FAILURE;
	}
      break;

    case REF_COMPONENT:
      /* Nothing needed, just proceed to next reference.  */
      break;

    case REF_SUBSTRING:
      if (checker (ref->u.ss.start) == FAILURE)
	return FAILURE;
      if (checker (ref->u.ss.end) == FAILURE)
	return FAILURE;
      break;

    default:
      gcc_unreachable ();
      break;
    }

  return check_references (ref->next, checker);
}


/* Verify that an expression is a restricted expression.  Like its
   cousin check_init_expr(), an error message is generated if we
   return FAILURE.  */

static gfc_try
check_restricted (gfc_expr *e)
{
  gfc_symbol* sym;
  gfc_try t;

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
      if (e->value.function.esym)
	{
	  t = check_arglist (e->value.function.actual, &check_restricted);
	  if (t == SUCCESS)
	    t = external_spec_function (e);
	}
      else
	{
	  if (e->value.function.isym && e->value.function.isym->inquiry)
	    t = SUCCESS;
	  else
	    t = check_arglist (e->value.function.actual, &check_restricted);

	  if (t == SUCCESS)
	    t = restricted_intrinsic (e);
	}
      break;

    case EXPR_VARIABLE:
      sym = e->symtree->n.sym;
      t = FAILURE;

      /* If a dummy argument appears in a context that is valid for a
	 restricted expression in an elemental procedure, it will have
	 already been simplified away once we get here.  Therefore we
	 don't need to jump through hoops to distinguish valid from
	 invalid cases.  */
      if (sym->attr.dummy && sym->ns == gfc_current_ns
	  && sym->ns->proc_name && sym->ns->proc_name->attr.elemental)
	{
	  gfc_error ("Dummy argument '%s' not allowed in expression at %L",
		     sym->name, &e->where);
	  break;
	}

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

      /* Check reference chain if any.  */
      if (check_references (e->ref, &check_restricted) == FAILURE)
	break;

      /* gfc_is_formal_arg broadcasts that a formal argument list is being
	 processed in resolve.c(resolve_formal_arglist).  This is done so
	 that host associated dummy array indices are accepted (PR23446).
	 This mechanism also does the same for the specification expressions
	 of array-valued functions.  */
      if (e->error
	    || sym->attr.in_common
	    || sym->attr.use_assoc
	    || sym->attr.dummy
	    || sym->attr.implied_index
	    || sym->attr.flavor == FL_PARAMETER
	    || (sym->ns && sym->ns == gfc_current_ns->parent)
	    || (sym->ns && gfc_current_ns->parent
		  && sym->ns == gfc_current_ns->parent->parent)
	    || (sym->ns->proc_name != NULL
		  && sym->ns->proc_name->attr.flavor == FL_MODULE)
	    || (gfc_is_formal_arg () && (sym->ns == gfc_current_ns)))
	{
	  t = SUCCESS;
	  break;
	}

      gfc_error ("Variable '%s' cannot appear in the expression at %L",
		 sym->name, &e->where);
      /* Prevent a repetition of the error.  */
      e->error = 1;
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

gfc_try
gfc_specification_expr (gfc_expr *e)
{

  if (e == NULL)
    return SUCCESS;

  if (e->ts.type != BT_INTEGER)
    {
      gfc_error ("Expression at %L must be of INTEGER type, found %s",
		 &e->where, gfc_basic_typename (e->ts.type));
      return FAILURE;
    }

  if (e->expr_type == EXPR_FUNCTION
	  && !e->value.function.isym
	  && !e->value.function.esym
	  && !gfc_pure (e->symtree->n.sym))
    {
      gfc_error ("Function '%s' at %L must be PURE",
		 e->symtree->n.sym->name, &e->where);
      /* Prevent repeat error messages.  */
      e->symtree->n.sym->attr.pure = 1;
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

gfc_try
gfc_check_conformance (gfc_expr *op1, gfc_expr *op2, const char *optype_msgid, ...)
{
  int op1_flag, op2_flag, d;
  mpz_t op1_size, op2_size;
  gfc_try t;

  va_list argp;
  char buffer[240];

  if (op1->rank == 0 || op2->rank == 0)
    return SUCCESS;

  va_start (argp, optype_msgid);
  vsnprintf (buffer, 240, optype_msgid, argp);
  va_end (argp);

  if (op1->rank != op2->rank)
    {
      gfc_error ("Incompatible ranks in %s (%d and %d) at %L", _(buffer),
		 op1->rank, op2->rank, &op1->where);
      return FAILURE;
    }

  t = SUCCESS;

  for (d = 0; d < op1->rank; d++)
    {
      op1_flag = gfc_array_dimen_size (op1, d, &op1_size) == SUCCESS;
      op2_flag = gfc_array_dimen_size (op2, d, &op2_size) == SUCCESS;

      if (op1_flag && op2_flag && mpz_cmp (op1_size, op2_size) != 0)
	{
	  gfc_error ("Different shape for %s at %L on dimension %d "
		     "(%d and %d)", _(buffer), &op1->where, d + 1,
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

gfc_try
gfc_check_assign (gfc_expr *lvalue, gfc_expr *rvalue, int conform)
{
  gfc_symbol *sym;
  gfc_ref *ref;
  int has_pointer;

  sym = lvalue->symtree->n.sym;

  /* Check INTENT(IN), unless the object itself is the component or
     sub-component of a pointer.  */
  has_pointer = sym->attr.pointer;

  for (ref = lvalue->ref; ref; ref = ref->next)
    if (ref->type == REF_COMPONENT && ref->u.c.component->attr.pointer)
      {
	has_pointer = 1;
	break;
      }

  if (!has_pointer && sym->attr.intent == INTENT_IN)
    {
      gfc_error ("Cannot assign to INTENT(IN) variable '%s' at %L",
		 sym->name, &lvalue->where);
      return FAILURE;
    }

  /* 12.5.2.2, Note 12.26: The result variable is very similar to any other
     variable local to a function subprogram.  Its existence begins when
     execution of the function is initiated and ends when execution of the
     function is terminated...
     Therefore, the left hand side is no longer a variable, when it is:  */
  if (sym->attr.flavor == FL_PROCEDURE && sym->attr.proc != PROC_ST_FUNCTION
      && !sym->attr.external)
    {
      bool bad_proc;
      bad_proc = false;

      /* (i) Use associated;  */
      if (sym->attr.use_assoc)
	bad_proc = true;

      /* (ii) The assignment is in the main program; or  */
      if (gfc_current_ns->proc_name->attr.is_main_program)
	bad_proc = true;

      /* (iii) A module or internal procedure...  */
      if ((gfc_current_ns->proc_name->attr.proc == PROC_INTERNAL
	   || gfc_current_ns->proc_name->attr.proc == PROC_MODULE)
	  && gfc_current_ns->parent
	  && (!(gfc_current_ns->parent->proc_name->attr.function
		|| gfc_current_ns->parent->proc_name->attr.subroutine)
	      || gfc_current_ns->parent->proc_name->attr.is_main_program))
	{
	  /* ... that is not a function...  */ 
	  if (!gfc_current_ns->proc_name->attr.function)
	    bad_proc = true;

	  /* ... or is not an entry and has a different name.  */
	  if (!sym->attr.entry && sym->name != gfc_current_ns->proc_name->name)
	    bad_proc = true;
	}

      /* (iv) Host associated and not the function symbol or the
	      parent result.  This picks up sibling references, which
	      cannot be entries.  */
      if (!sym->attr.entry
	    && sym->ns == gfc_current_ns->parent
	    && sym != gfc_current_ns->proc_name
	    && sym != gfc_current_ns->parent->proc_name->result)
	bad_proc = true;

      if (bad_proc)
	{
	  gfc_error ("'%s' at %L is not a VALUE", sym->name, &lvalue->where);
	  return FAILURE;
	}
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
      if (has_pointer && (ref == NULL || ref->next == NULL)
	  && lvalue->symtree->n.sym->attr.data)
        return SUCCESS;
      else
	{
	  gfc_error ("NULL appears on right-hand side in assignment at %L",
		     &rvalue->where);
	  return FAILURE;
	}
    }

   if (sym->attr.cray_pointee
       && lvalue->ref != NULL
       && lvalue->ref->u.ar.type == AR_FULL
       && lvalue->ref->u.ar.as->cp_was_assumed)
     {
       gfc_error ("Vector assignment to assumed-size Cray Pointee at %L "
		  "is illegal", &lvalue->where);
       return FAILURE;
     }

  /* This is possibly a typo: x = f() instead of x => f().  */
  if (gfc_option.warn_surprising 
      && rvalue->expr_type == EXPR_FUNCTION
      && rvalue->symtree->n.sym->attr.pointer)
    gfc_warning ("POINTER valued function appears on right-hand side of "
		 "assignment at %L", &rvalue->where);

  /* Check size of array assignments.  */
  if (lvalue->rank != 0 && rvalue->rank != 0
      && gfc_check_conformance (lvalue, rvalue, "array assignment") != SUCCESS)
    return FAILURE;

  if (rvalue->is_boz && lvalue->ts.type != BT_INTEGER
      && lvalue->symtree->n.sym->attr.data
      && gfc_notify_std (GFC_STD_GNU, "Extension: BOZ literal at %L used to "
                         "initialize non-integer variable '%s'",
			 &rvalue->where, lvalue->symtree->n.sym->name)
	 == FAILURE)
    return FAILURE;
  else if (rvalue->is_boz && !lvalue->symtree->n.sym->attr.data
      && gfc_notify_std (GFC_STD_GNU, "Extension: BOZ literal at %L outside "
			 "a DATA statement and outside INT/REAL/DBLE/CMPLX",
			 &rvalue->where) == FAILURE)
    return FAILURE;

  /* Handle the case of a BOZ literal on the RHS.  */
  if (rvalue->is_boz && lvalue->ts.type != BT_INTEGER)
    {
      int rc;
      if (gfc_option.warn_surprising)
        gfc_warning ("BOZ literal at %L is bitwise transferred "
                     "non-integer symbol '%s'", &rvalue->where,
                     lvalue->symtree->n.sym->name);
      if (!gfc_convert_boz (rvalue, &lvalue->ts))
	return FAILURE;
      if ((rc = gfc_range_check (rvalue)) != ARITH_OK)
	{
	  if (rc == ARITH_UNDERFLOW)
	    gfc_error ("Arithmetic underflow of bit-wise transferred BOZ at %L"
		       ". This check can be disabled with the option "
		       "-fno-range-check", &rvalue->where);
	  else if (rc == ARITH_OVERFLOW)
	    gfc_error ("Arithmetic overflow of bit-wise transferred BOZ at %L"
		       ". This check can be disabled with the option "
		       "-fno-range-check", &rvalue->where);
	  else if (rc == ARITH_NAN)
	    gfc_error ("Arithmetic NaN of bit-wise transferred BOZ at %L"
		       ". This check can be disabled with the option "
		       "-fno-range-check", &rvalue->where);
	  return FAILURE;
	}
    }

  if (gfc_compare_types (&lvalue->ts, &rvalue->ts))
    return SUCCESS;

  /* Only DATA Statements come here.  */
  if (!conform)
    {
      /* Numeric can be converted to any other numeric. And Hollerith can be
	 converted to any other type.  */
      if ((gfc_numeric_ts (&lvalue->ts) && gfc_numeric_ts (&rvalue->ts))
	  || rvalue->ts.type == BT_HOLLERITH)
	return SUCCESS;

      if (lvalue->ts.type == BT_LOGICAL && rvalue->ts.type == BT_LOGICAL)
	return SUCCESS;

      gfc_error ("Incompatible types in DATA statement at %L; attempted "
		 "conversion of %s to %s", &lvalue->where,
		 gfc_typename (&rvalue->ts), gfc_typename (&lvalue->ts));

      return FAILURE;
    }

  /* Assignment is the only case where character variables of different
     kind values can be converted into one another.  */
  if (lvalue->ts.type == BT_CHARACTER && rvalue->ts.type == BT_CHARACTER)
    {
      if (lvalue->ts.kind != rvalue->ts.kind)
	gfc_convert_chartype (rvalue, &lvalue->ts);

      return SUCCESS;
    }

  return gfc_convert_type (rvalue, &lvalue->ts, 1);
}


/* Check that a pointer assignment is OK.  We first check lvalue, and
   we only check rvalue if it's not an assignment to NULL() or a
   NULLIFY statement.  */

gfc_try
gfc_check_pointer_assign (gfc_expr *lvalue, gfc_expr *rvalue)
{
  symbol_attribute attr;
  gfc_ref *ref;
  int is_pure;
  int pointer, check_intent_in, proc_pointer;

  if (lvalue->symtree->n.sym->ts.type == BT_UNKNOWN
      && !lvalue->symtree->n.sym->attr.proc_pointer)
    {
      gfc_error ("Pointer assignment target is not a POINTER at %L",
		 &lvalue->where);
      return FAILURE;
    }

  if (lvalue->symtree->n.sym->attr.flavor == FL_PROCEDURE
      && lvalue->symtree->n.sym->attr.use_assoc
      && !lvalue->symtree->n.sym->attr.proc_pointer)
    {
      gfc_error ("'%s' in the pointer assignment at %L cannot be an "
		 "l-value since it is a procedure",
		 lvalue->symtree->n.sym->name, &lvalue->where);
      return FAILURE;
    }


  /* Check INTENT(IN), unless the object itself is the component or
     sub-component of a pointer.  */
  check_intent_in = 1;
  pointer = lvalue->symtree->n.sym->attr.pointer;
  proc_pointer = lvalue->symtree->n.sym->attr.proc_pointer;

  for (ref = lvalue->ref; ref; ref = ref->next)
    {
      if (pointer)
	check_intent_in = 0;

      if (ref->type == REF_COMPONENT)
	{
	  pointer = ref->u.c.component->attr.pointer;
	  proc_pointer = ref->u.c.component->attr.proc_pointer;
	}

      if (ref->type == REF_ARRAY && ref->next == NULL)
	{
	  if (ref->u.ar.type == AR_FULL)
	    break;

	  if (ref->u.ar.type != AR_SECTION)
	    {
	      gfc_error ("Expected bounds specification for '%s' at %L",
			 lvalue->symtree->n.sym->name, &lvalue->where);
	      return FAILURE;
	    }

	  if (gfc_notify_std (GFC_STD_F2003,"Fortran 2003: Bounds "
			      "specification for '%s' in pointer assignment "
                              "at %L", lvalue->symtree->n.sym->name,
			      &lvalue->where) == FAILURE)
            return FAILURE;

	  gfc_error ("Pointer bounds remapping at %L is not yet implemented "
		     "in gfortran", &lvalue->where);
	  /* TODO: See PR 29785. Add checks that all lbounds are specified and
	     either never or always the upper-bound; strides shall not be
	     present.  */
	  return FAILURE;
	}
    }

  if (check_intent_in && lvalue->symtree->n.sym->attr.intent == INTENT_IN)
    {
      gfc_error ("Cannot assign to INTENT(IN) variable '%s' at %L",
		 lvalue->symtree->n.sym->name, &lvalue->where);
      return FAILURE;
    }

  if (!pointer && !proc_pointer
	&& !(lvalue->ts.type == BT_CLASS
		&& lvalue->ts.u.derived->components->attr.pointer))
    {
      gfc_error ("Pointer assignment to non-POINTER at %L", &lvalue->where);
      return FAILURE;
    }

  is_pure = gfc_pure (NULL);

  if (is_pure && gfc_impure_variable (lvalue->symtree->n.sym)
	&& lvalue->symtree->n.sym->value != rvalue)
    {
      gfc_error ("Bad pointer object in PURE procedure at %L", &lvalue->where);
      return FAILURE;
    }

  /* If rvalue is a NULL() or NULLIFY, we're done. Otherwise the type,
     kind, etc for lvalue and rvalue must match, and rvalue must be a
     pure variable if we're in a pure function.  */
  if (rvalue->expr_type == EXPR_NULL && rvalue->ts.type == BT_UNKNOWN)
    return SUCCESS;

  /* Checks on rvalue for procedure pointer assignments.  */
  if (proc_pointer)
    {
      char err[200];
      gfc_symbol *s1,*s2;
      gfc_component *comp;
      const char *name;

      attr = gfc_expr_attr (rvalue);
      if (!((rvalue->expr_type == EXPR_NULL)
	    || (rvalue->expr_type == EXPR_FUNCTION && attr.proc_pointer)
	    || (rvalue->expr_type == EXPR_VARIABLE && attr.proc_pointer)
	    || (rvalue->expr_type == EXPR_VARIABLE
		&& attr.flavor == FL_PROCEDURE)))
	{
	  gfc_error ("Invalid procedure pointer assignment at %L",
		     &rvalue->where);
	  return FAILURE;
	}
      if (attr.abstract)
	{
	  gfc_error ("Abstract interface '%s' is invalid "
		     "in procedure pointer assignment at %L",
		     rvalue->symtree->name, &rvalue->where);
	  return FAILURE;
	}
      /* Check for C727.  */
      if (attr.flavor == FL_PROCEDURE)
	{
	  if (attr.proc == PROC_ST_FUNCTION)
	    {
	      gfc_error ("Statement function '%s' is invalid "
			 "in procedure pointer assignment at %L",
			 rvalue->symtree->name, &rvalue->where);
	      return FAILURE;
	    }
	  if (attr.proc == PROC_INTERNAL &&
	      gfc_notify_std (GFC_STD_F2008, "Internal procedure '%s' is "
			      "invalid in procedure pointer assignment at %L",
			      rvalue->symtree->name, &rvalue->where) == FAILURE)
	    return FAILURE;
	}

      /* Ensure that the calling convention is the same. As other attributes
	 such as DLLEXPORT may differ, one explicitly only tests for the
	 calling conventions.  */
      if (rvalue->expr_type == EXPR_VARIABLE
	  && lvalue->symtree->n.sym->attr.ext_attr
	       != rvalue->symtree->n.sym->attr.ext_attr)
	{
	  symbol_attribute calls;

	  calls.ext_attr = 0;
	  gfc_add_ext_attribute (&calls, EXT_ATTR_CDECL, NULL);
	  gfc_add_ext_attribute (&calls, EXT_ATTR_STDCALL, NULL);
	  gfc_add_ext_attribute (&calls, EXT_ATTR_FASTCALL, NULL);

	  if ((calls.ext_attr & lvalue->symtree->n.sym->attr.ext_attr)
	      != (calls.ext_attr & rvalue->symtree->n.sym->attr.ext_attr))
	    {
	      gfc_error ("Mismatch in the procedure pointer assignment "
			 "at %L: mismatch in the calling convention",
			 &rvalue->where);
	  return FAILURE;
	    }
	}

      if (gfc_is_proc_ptr_comp (lvalue, &comp))
	s1 = comp->ts.interface;
      else
	s1 = lvalue->symtree->n.sym;

      if (gfc_is_proc_ptr_comp (rvalue, &comp))
	{
	  s2 = comp->ts.interface;
	  name = comp->name;
	}
      else if (rvalue->expr_type == EXPR_FUNCTION)
	{
	  s2 = rvalue->symtree->n.sym->result;
	  name = rvalue->symtree->n.sym->result->name;
	}
      else
	{
	  s2 = rvalue->symtree->n.sym;
	  name = rvalue->symtree->n.sym->name;
	}

      if (s1 && s2 && !gfc_compare_interfaces (s1, s2, name, 0, 1,
					       err, sizeof(err)))
	{
	  gfc_error ("Interface mismatch in procedure pointer assignment "
		     "at %L: %s", &rvalue->where, err);
	  return FAILURE;
	}

      return SUCCESS;
    }

  if (!gfc_compare_types (&lvalue->ts, &rvalue->ts))
    {
      gfc_error ("Different types in pointer assignment at %L; attempted "
		 "assignment of %s to %s", &lvalue->where, 
		 gfc_typename (&rvalue->ts), gfc_typename (&lvalue->ts));
      return FAILURE;
    }

  if (lvalue->ts.type != BT_CLASS && lvalue->ts.kind != rvalue->ts.kind)
    {
      gfc_error ("Different kind type parameters in pointer "
		 "assignment at %L", &lvalue->where);
      return FAILURE;
    }

  if (lvalue->rank != rvalue->rank)
    {
      gfc_error ("Different ranks in pointer assignment at %L",
		 &lvalue->where);
      return FAILURE;
    }

  /* Now punt if we are dealing with a NULLIFY(X) or X = NULL(X).  */
  if (rvalue->expr_type == EXPR_NULL)
    return SUCCESS;

  if (lvalue->ts.type == BT_CHARACTER)
    {
      gfc_try t = gfc_check_same_strlen (lvalue, rvalue, "pointer assignment");
      if (t == FAILURE)
	return FAILURE;
    }

  if (rvalue->expr_type == EXPR_VARIABLE && is_subref_array (rvalue))
    lvalue->symtree->n.sym->attr.subref_array_pointer = 1;

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

  if (gfc_has_vector_index (rvalue))
    {
      gfc_error ("Pointer assignment with vector subscript "
		 "on rhs at %L", &rvalue->where);
      return FAILURE;
    }

  if (attr.is_protected && attr.use_assoc
      && !(attr.pointer || attr.proc_pointer))
    {
      gfc_error ("Pointer assignment target has PROTECTED "
		 "attribute at %L", &rvalue->where);
      return FAILURE;
    }

  return SUCCESS;
}


/* Relative of gfc_check_assign() except that the lvalue is a single
   symbol.  Used for initialization assignments.  */

gfc_try
gfc_check_assign_symbol (gfc_symbol *sym, gfc_expr *rvalue)
{
  gfc_expr lvalue;
  gfc_try r;

  memset (&lvalue, '\0', sizeof (gfc_expr));

  lvalue.expr_type = EXPR_VARIABLE;
  lvalue.ts = sym->ts;
  if (sym->as)
    lvalue.rank = sym->as->rank;
  lvalue.symtree = (gfc_symtree *) gfc_getmem (sizeof (gfc_symtree));
  lvalue.symtree->n.sym = sym;
  lvalue.where = sym->declared_at;

  if (sym->attr.pointer || sym->attr.proc_pointer
      || (sym->ts.type == BT_CLASS 
	  && sym->ts.u.derived->components->attr.pointer
	  && rvalue->expr_type == EXPR_NULL))
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

  /* See if we have a default initializer.  */
  for (c = ts->u.derived->components; c; c = c->next)
    if (c->initializer || c->attr.allocatable)
      break;

  if (!c)
    return NULL;

  /* Build the constructor.  */
  init = gfc_get_expr ();
  init->expr_type = EXPR_STRUCTURE;
  init->ts = *ts;
  init->where = ts->u.derived->declared_at;

  tail = NULL;
  for (c = ts->u.derived->components; c; c = c->next)
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

      if (c->attr.allocatable)
	{
	  tail->expr = gfc_get_expr ();
	  tail->expr->expr_type = EXPR_NULL;
	  tail->expr->ts = c->ts;
	}
    }
  return init;
}


/* Given a symbol, create an expression node with that symbol as a
   variable. If the symbol is array valued, setup a reference of the
   whole array.  */

gfc_expr *
gfc_get_variable_expr (gfc_symtree *var)
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


/* General expression traversal function.  */

bool
gfc_traverse_expr (gfc_expr *expr, gfc_symbol *sym,
		   bool (*func)(gfc_expr *, gfc_symbol *, int*),
		   int f)
{
  gfc_array_ref ar;
  gfc_ref *ref;
  gfc_actual_arglist *args;
  gfc_constructor *c;
  int i;

  if (!expr)
    return false;

  if ((*func) (expr, sym, &f))
    return true;

  if (expr->ts.type == BT_CHARACTER
	&& expr->ts.u.cl
	&& expr->ts.u.cl->length
	&& expr->ts.u.cl->length->expr_type != EXPR_CONSTANT
	&& gfc_traverse_expr (expr->ts.u.cl->length, sym, func, f))
    return true;

  switch (expr->expr_type)
    {
    case EXPR_FUNCTION:
      for (args = expr->value.function.actual; args; args = args->next)
	{
	  if (gfc_traverse_expr (args->expr, sym, func, f))
	    return true;
	}
      break;

    case EXPR_VARIABLE:
    case EXPR_CONSTANT:
    case EXPR_NULL:
    case EXPR_SUBSTRING:
      break;

    case EXPR_STRUCTURE:
    case EXPR_ARRAY:
      for (c = expr->value.constructor; c; c = c->next)
	{
	  if (gfc_traverse_expr (c->expr, sym, func, f))
	    return true;
	  if (c->iterator)
	    {
	      if (gfc_traverse_expr (c->iterator->var, sym, func, f))
		return true;
	      if (gfc_traverse_expr (c->iterator->start, sym, func, f))
		return true;
	      if (gfc_traverse_expr (c->iterator->end, sym, func, f))
		return true;
	      if (gfc_traverse_expr (c->iterator->step, sym, func, f))
		return true;
	    }
	}
      break;

    case EXPR_OP:
      if (gfc_traverse_expr (expr->value.op.op1, sym, func, f))
	return true;
      if (gfc_traverse_expr (expr->value.op.op2, sym, func, f))
	return true;
      break;

    default:
      gcc_unreachable ();
      break;
    }

  ref = expr->ref;
  while (ref != NULL)
    {
      switch (ref->type)
	{
	case  REF_ARRAY:
	  ar = ref->u.ar;
	  for (i = 0; i < GFC_MAX_DIMENSIONS; i++)
	    {
	      if (gfc_traverse_expr (ar.start[i], sym, func, f))
		return true;
	      if (gfc_traverse_expr (ar.end[i], sym, func, f))
		return true;
	      if (gfc_traverse_expr (ar.stride[i], sym, func, f))
		return true;
	    }
	  break;

	case REF_SUBSTRING:
	  if (gfc_traverse_expr (ref->u.ss.start, sym, func, f))
	    return true;
	  if (gfc_traverse_expr (ref->u.ss.end, sym, func, f))
	    return true;
	  break;

	case REF_COMPONENT:
	  if (ref->u.c.component->ts.type == BT_CHARACTER
		&& ref->u.c.component->ts.u.cl
		&& ref->u.c.component->ts.u.cl->length
		&& ref->u.c.component->ts.u.cl->length->expr_type
		     != EXPR_CONSTANT
		&& gfc_traverse_expr (ref->u.c.component->ts.u.cl->length,
				      sym, func, f))
	    return true;

	  if (ref->u.c.component->as)
	    for (i = 0; i < ref->u.c.component->as->rank; i++)
	      {
		if (gfc_traverse_expr (ref->u.c.component->as->lower[i],
				       sym, func, f))
		  return true;
		if (gfc_traverse_expr (ref->u.c.component->as->upper[i],
				       sym, func, f))
		  return true;
	      }
	  break;

	default:
	  gcc_unreachable ();
	}
      ref = ref->next;
    }
  return false;
}

/* Traverse expr, marking all EXPR_VARIABLE symbols referenced.  */

static bool
expr_set_symbols_referenced (gfc_expr *expr,
			     gfc_symbol *sym ATTRIBUTE_UNUSED,
			     int *f ATTRIBUTE_UNUSED)
{
  if (expr->expr_type != EXPR_VARIABLE)
    return false;
  gfc_set_sym_referenced (expr->symtree->n.sym);
  return false;
}

void
gfc_expr_set_symbols_referenced (gfc_expr *expr)
{
  gfc_traverse_expr (expr, NULL, expr_set_symbols_referenced, 0);
}


/* Determine if an expression is a procedure pointer component. If yes, the
   argument 'comp' will point to the component (provided that 'comp' was
   provided).  */

bool
gfc_is_proc_ptr_comp (gfc_expr *expr, gfc_component **comp)
{
  gfc_ref *ref;
  bool ppc = false;

  if (!expr || !expr->ref)
    return false;

  ref = expr->ref;
  while (ref->next)
    ref = ref->next;

  if (ref->type == REF_COMPONENT)
    {
      ppc = ref->u.c.component->attr.proc_pointer;
      if (ppc && comp)
	*comp = ref->u.c.component;
    }

  return ppc;
}


/* Walk an expression tree and check each variable encountered for being typed.
   If strict is not set, a top-level variable is tolerated untyped in -std=gnu
   mode as is a basic arithmetic expression using those; this is for things in
   legacy-code like:

     INTEGER :: arr(n), n
     INTEGER :: arr(n + 1), n

   The namespace is needed for IMPLICIT typing.  */

static gfc_namespace* check_typed_ns;

static bool
expr_check_typed_help (gfc_expr* e, gfc_symbol* sym ATTRIBUTE_UNUSED,
                       int* f ATTRIBUTE_UNUSED)
{
  gfc_try t;

  if (e->expr_type != EXPR_VARIABLE)
    return false;

  gcc_assert (e->symtree);
  t = gfc_check_symbol_typed (e->symtree->n.sym, check_typed_ns,
                              true, e->where);

  return (t == FAILURE);
}

gfc_try
gfc_expr_check_typed (gfc_expr* e, gfc_namespace* ns, bool strict)
{
  bool error_found;

  /* If this is a top-level variable or EXPR_OP, do the check with strict given
     to us.  */
  if (!strict)
    {
      if (e->expr_type == EXPR_VARIABLE && !e->ref)
	return gfc_check_symbol_typed (e->symtree->n.sym, ns, strict, e->where);

      if (e->expr_type == EXPR_OP)
	{
	  gfc_try t = SUCCESS;

	  gcc_assert (e->value.op.op1);
	  t = gfc_expr_check_typed (e->value.op.op1, ns, strict);

	  if (t == SUCCESS && e->value.op.op2)
	    t = gfc_expr_check_typed (e->value.op.op2, ns, strict);

	  return t;
	}
    }

  /* Otherwise, walk the expression and do it strictly.  */
  check_typed_ns = ns;
  error_found = gfc_traverse_expr (e, NULL, &expr_check_typed_help, 0);

  return error_found ? FAILURE : SUCCESS;
}

/* Walk an expression tree and replace all symbols with a corresponding symbol
   in the formal_ns of "sym". Needed for copying interfaces in PROCEDURE
   statements. The boolean return value is required by gfc_traverse_expr.  */

static bool
replace_symbol (gfc_expr *expr, gfc_symbol *sym, int *i ATTRIBUTE_UNUSED)
{
  if ((expr->expr_type == EXPR_VARIABLE 
       || (expr->expr_type == EXPR_FUNCTION
	   && !gfc_is_intrinsic (expr->symtree->n.sym, 0, expr->where)))
      && expr->symtree->n.sym->ns == sym->ts.interface->formal_ns)
    {
      gfc_symtree *stree;
      gfc_namespace *ns = sym->formal_ns;
      /* Don't use gfc_get_symtree as we prefer to fail badly if we don't find
	 the symtree rather than create a new one (and probably fail later).  */
      stree = gfc_find_symtree (ns ? ns->sym_root : gfc_current_ns->sym_root,
		      		expr->symtree->n.sym->name);
      gcc_assert (stree);
      stree->n.sym->attr = expr->symtree->n.sym->attr;
      expr->symtree = stree;
    }
  return false;
}

void
gfc_expr_replace_symbols (gfc_expr *expr, gfc_symbol *dest)
{
  gfc_traverse_expr (expr, dest, &replace_symbol, 0);
}

/* The following is analogous to 'replace_symbol', and needed for copying
   interfaces for procedure pointer components. The argument 'sym' must formally
   be a gfc_symbol, so that the function can be passed to gfc_traverse_expr.
   However, it gets actually passed a gfc_component (i.e. the procedure pointer
   component in whose formal_ns the arguments have to be).  */

static bool
replace_comp (gfc_expr *expr, gfc_symbol *sym, int *i ATTRIBUTE_UNUSED)
{
  gfc_component *comp;
  comp = (gfc_component *)sym;
  if ((expr->expr_type == EXPR_VARIABLE 
       || (expr->expr_type == EXPR_FUNCTION
	   && !gfc_is_intrinsic (expr->symtree->n.sym, 0, expr->where)))
      && expr->symtree->n.sym->ns == comp->ts.interface->formal_ns)
    {
      gfc_symtree *stree;
      gfc_namespace *ns = comp->formal_ns;
      /* Don't use gfc_get_symtree as we prefer to fail badly if we don't find
	 the symtree rather than create a new one (and probably fail later).  */
      stree = gfc_find_symtree (ns ? ns->sym_root : gfc_current_ns->sym_root,
		      		expr->symtree->n.sym->name);
      gcc_assert (stree);
      stree->n.sym->attr = expr->symtree->n.sym->attr;
      expr->symtree = stree;
    }
  return false;
}

void
gfc_expr_replace_comp (gfc_expr *expr, gfc_component *dest)
{
  gfc_traverse_expr (expr, (gfc_symbol *)dest, &replace_comp, 0);
}

