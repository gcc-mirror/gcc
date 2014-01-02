/* Routines for manipulation of expression nodes.
   Copyright (C) 2000-2014 Free Software Foundation, Inc.
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
#include "coretypes.h"
#include "gfortran.h"
#include "arith.h"
#include "match.h"
#include "target-memory.h" /* for gfc_convert_boz */
#include "constructor.h"


/* The following set of functions provide access to gfc_expr* of
   various types - actual all but EXPR_FUNCTION and EXPR_VARIABLE.

   There are two functions available elsewhere that provide
   slightly different flavours of variables.  Namely:
     expr.c (gfc_get_variable_expr)
     symbol.c (gfc_lval_expr_from_sym)
   TODO: Merge these functions, if possible.  */

/* Get a new expression node.  */

gfc_expr *
gfc_get_expr (void)
{
  gfc_expr *e;

  e = XCNEW (gfc_expr);
  gfc_clear_ts (&e->ts);
  e->shape = NULL;
  e->ref = NULL;
  e->symtree = NULL;
  return e;
}


/* Get a new expression node that is an array constructor
   of given type and kind.  */

gfc_expr *
gfc_get_array_expr (bt type, int kind, locus *where)
{
  gfc_expr *e;

  e = gfc_get_expr ();
  e->expr_type = EXPR_ARRAY;
  e->value.constructor = NULL;
  e->rank = 1;
  e->shape = NULL;

  e->ts.type = type;
  e->ts.kind = kind;
  if (where)
    e->where = *where;

  return e;
}


/* Get a new expression node that is the NULL expression.  */

gfc_expr *
gfc_get_null_expr (locus *where)
{
  gfc_expr *e;

  e = gfc_get_expr ();
  e->expr_type = EXPR_NULL;
  e->ts.type = BT_UNKNOWN;

  if (where)
    e->where = *where;

  return e;
}


/* Get a new expression node that is an operator expression node.  */

gfc_expr *
gfc_get_operator_expr (locus *where, gfc_intrinsic_op op,
                      gfc_expr *op1, gfc_expr *op2)
{
  gfc_expr *e;

  e = gfc_get_expr ();
  e->expr_type = EXPR_OP;
  e->value.op.op = op;
  e->value.op.op1 = op1;
  e->value.op.op2 = op2;

  if (where)
    e->where = *where;

  return e;
}


/* Get a new expression node that is an structure constructor
   of given type and kind.  */

gfc_expr *
gfc_get_structure_constructor_expr (bt type, int kind, locus *where)
{
  gfc_expr *e;

  e = gfc_get_expr ();
  e->expr_type = EXPR_STRUCTURE;
  e->value.constructor = NULL;

  e->ts.type = type;
  e->ts.kind = kind;
  if (where)
    e->where = *where;

  return e;
}


/* Get a new expression node that is an constant of given type and kind.  */

gfc_expr *
gfc_get_constant_expr (bt type, int kind, locus *where)
{
  gfc_expr *e;

  if (!where)
    gfc_internal_error ("gfc_get_constant_expr(): locus 'where' cannot be NULL");

  e = gfc_get_expr ();

  e->expr_type = EXPR_CONSTANT;
  e->ts.type = type;
  e->ts.kind = kind;
  e->where = *where;

  switch (type)
    {
    case BT_INTEGER:
      mpz_init (e->value.integer);
      break;

    case BT_REAL:
      gfc_set_model_kind (kind);
      mpfr_init (e->value.real);
      break;

    case BT_COMPLEX:
      gfc_set_model_kind (kind);
      mpc_init2 (e->value.complex, mpfr_get_default_prec());
      break;

    default:
      break;
    }

  return e;
}


/* Get a new expression node that is an string constant.
   If no string is passed, a string of len is allocated,
   blanked and null-terminated.  */

gfc_expr *
gfc_get_character_expr (int kind, locus *where, const char *src, int len)
{
  gfc_expr *e;
  gfc_char_t *dest;

  if (!src)
    {
      dest = gfc_get_wide_string (len + 1);
      gfc_wide_memset (dest, ' ', len);
      dest[len] = '\0';
    }
  else
    dest = gfc_char_to_widechar (src);

  e = gfc_get_constant_expr (BT_CHARACTER, kind,
                            where ? where : &gfc_current_locus);
  e->value.character.string = dest;
  e->value.character.length = len;

  return e;
}


/* Get a new expression node that is an integer constant.  */

gfc_expr *
gfc_get_int_expr (int kind, locus *where, int value)
{
  gfc_expr *p;
  p = gfc_get_constant_expr (BT_INTEGER, kind,
			     where ? where : &gfc_current_locus);

  mpz_set_si (p->value.integer, value);

  return p;
}


/* Get a new expression node that is a logical constant.  */

gfc_expr *
gfc_get_logical_expr (int kind, locus *where, bool value)
{
  gfc_expr *p;
  p = gfc_get_constant_expr (BT_LOGICAL, kind,
			     where ? where : &gfc_current_locus);

  p->value.logical = value;

  return p;
}


gfc_expr *
gfc_get_iokind_expr (locus *where, io_kind k)
{
  gfc_expr *e;

  /* Set the types to something compatible with iokind. This is needed to
     get through gfc_free_expr later since iokind really has no Basic Type,
     BT, of its own.  */

  e = gfc_get_expr ();
  e->expr_type = EXPR_CONSTANT;
  e->ts.type = BT_LOGICAL;
  e->value.iokind = k;
  e->where = *where;

  return e;
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
	case BT_ASSUMED:
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
      q->value.constructor = gfc_constructor_copy (p->value.constructor);
      break;

    case EXPR_VARIABLE:
    case EXPR_NULL:
      break;
    }

  q->shape = gfc_copy_shape (p->shape, p->rank);

  q->ref = gfc_copy_ref (p->ref);

  return q;
}


void
gfc_clear_shape (mpz_t *shape, int rank)
{
  int i;

  for (i = 0; i < rank; i++)
    mpz_clear (shape[i]);
}


void
gfc_free_shape (mpz_t **shape, int rank)
{
  if (*shape == NULL)
    return;

  gfc_clear_shape (*shape, rank);
  free (*shape);
  *shape = NULL;
}


/* Workhorse function for gfc_free_expr() that frees everything
   beneath an expression node, but not the node itself.  This is
   useful when we want to simplify a node and replace it with
   something else or the expression node belongs to another structure.  */

static void
free_expr0 (gfc_expr *e)
{
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
	  free (e->value.character.string);
	  break;

	case BT_COMPLEX:
	  mpc_clear (e->value.complex);
	  break;

	default:
	  break;
	}

      /* Free the representation.  */
      free (e->representation.string);

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
      gfc_constructor_free (e->value.constructor);
      break;

    case EXPR_SUBSTRING:
      free (e->value.character.string);
      break;

    case EXPR_NULL:
      break;

    default:
      gfc_internal_error ("free_expr0(): Bad expr type");
    }

  /* Free a shape array.  */
  gfc_free_shape (&e->shape, e->rank);

  gfc_free_ref_list (e->ref);

  memset (e, '\0', sizeof (gfc_expr));
}


/* Free an expression node and everything beneath it.  */

void
gfc_free_expr (gfc_expr *e)
{
  if (e == NULL)
    return;
  free_expr0 (e);
  free (e);
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
      free (a1);
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

      free (p);
    }
}


/* Graft the *src expression onto the *dest subexpression.  */

void
gfc_replace_expr (gfc_expr *dest, gfc_expr *src)
{
  free_expr0 (dest);
  *dest = *src;
  free (src);
}


/* Try to extract an integer constant from the passed expression node.
   Returns an error message or NULL if the result is set.  It is
   tempting to generate an error and return true or false, but
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
      free (ar);
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
   constant expression.  Dimensions are numbered in Fortran style --
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


/* Function to determine if an expression is constant or not.  This
   function expects that the expression has already been simplified.  */

int
gfc_is_constant_expr (gfc_expr *e)
{
  gfc_constructor *c;
  gfc_actual_arglist *arg;
  gfc_symbol *sym;

  if (e == NULL)
    return 1;

  switch (e->expr_type)
    {
    case EXPR_OP:
      return (gfc_is_constant_expr (e->value.op.op1)
	      && (e->value.op.op2 == NULL
		  || gfc_is_constant_expr (e->value.op.op2)));

    case EXPR_VARIABLE:
      return 0;

    case EXPR_FUNCTION:
    case EXPR_PPC:
    case EXPR_COMPCALL:
      gcc_assert (e->symtree || e->value.function.esym
		  || e->value.function.isym);

      /* Call to intrinsic with at least one argument.  */
      if (e->value.function.isym && e->value.function.actual)
	{
	  for (arg = e->value.function.actual; arg; arg = arg->next)
	    if (!gfc_is_constant_expr (arg->expr))
	      return 0;
	}

      /* Specification functions are constant.  */
      /* F95, 7.1.6.2; F2003, 7.1.7  */
      sym = NULL;
      if (e->symtree)
	sym = e->symtree->n.sym;
      if (e->value.function.esym)
	sym = e->value.function.esym;

      if (sym
	  && sym->attr.function
	  && sym->attr.pure
	  && !sym->attr.intrinsic
	  && !sym->attr.recursive
	  && sym->attr.proc != PROC_INTERNAL
	  && sym->attr.proc != PROC_ST_FUNCTION
	  && sym->attr.proc != PROC_UNKNOWN
	  && gfc_sym_get_dummy_args (sym) == NULL)
	return 1;

      if (e->value.function.isym
	  && (e->value.function.isym->elemental
	      || e->value.function.isym->pure
	      || e->value.function.isym->inquiry
	      || e->value.function.isym->transformational))
	return 1;

      return 0;

    case EXPR_CONSTANT:
    case EXPR_NULL:
      return 1;

    case EXPR_SUBSTRING:
      return e->ref == NULL || (gfc_is_constant_expr (e->ref->u.ss.start)
				&& gfc_is_constant_expr (e->ref->u.ss.end));

    case EXPR_ARRAY:
    case EXPR_STRUCTURE:
      c = gfc_constructor_first (e->value.constructor);
      if ((e->expr_type == EXPR_ARRAY) && c && c->iterator)
        return gfc_constant_ac (e);

      for (; c; c = gfc_constructor_next (c))
	if (!gfc_is_constant_expr (c->expr))
	  return 0;

      return 1;


    default:
      gfc_internal_error ("gfc_is_constant_expr(): Unknown expression type");
      return 0;
    }
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

static bool
simplify_intrinsic_op (gfc_expr *p, int type)
{
  gfc_intrinsic_op op;
  gfc_expr *op1, *op2, *result;

  if (p->value.op.op == INTRINSIC_USER)
    return true;

  op1 = p->value.op.op1;
  op2 = p->value.op.op2;
  op  = p->value.op.op;

  if (!gfc_simplify_expr (op1, type))
    return false;
  if (!gfc_simplify_expr (op2, type))
    return false;

  if (!gfc_is_constant_expr (op1)
      || (op2 != NULL && !gfc_is_constant_expr (op2)))
    return true;

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
      return false;
    }

  result->rank = p->rank;
  result->where = p->where;
  gfc_replace_expr (p, result);

  return true;
}


/* Subroutine to simplify constructor expressions.  Mutually recursive
   with gfc_simplify_expr().  */

static bool
simplify_constructor (gfc_constructor_base base, int type)
{
  gfc_constructor *c;
  gfc_expr *p;

  for (c = gfc_constructor_first (base); c; c = gfc_constructor_next (c))
    {
      if (c->iterator
	  && (!gfc_simplify_expr(c->iterator->start, type)
	      || !gfc_simplify_expr (c->iterator->end, type)
	      || !gfc_simplify_expr (c->iterator->step, type)))
	return false;

      if (c->expr)
	{
	  /* Try and simplify a copy.  Replace the original if successful
	     but keep going through the constructor at all costs.  Not
	     doing so can make a dog's dinner of complicated things.  */
	  p = gfc_copy_expr (c->expr);

	  if (!gfc_simplify_expr (p, type))
	    {
	      gfc_free_expr (p);
	      continue;
	    }

	  gfc_replace_expr (c->expr, p);
	}
    }

  return true;
}


/* Pull a single array element out of an array constructor.  */

static bool
find_array_element (gfc_constructor_base base, gfc_array_ref *ar,
		    gfc_constructor **rval)
{
  unsigned long nelemen;
  int i;
  mpz_t delta;
  mpz_t offset;
  mpz_t span;
  mpz_t tmp;
  gfc_constructor *cons;
  gfc_expr *e;
  bool t;

  t = true;
  e = NULL;

  mpz_init_set_ui (offset, 0);
  mpz_init (delta);
  mpz_init (tmp);
  mpz_init_set_ui (span, 1);
  for (i = 0; i < ar->dimen; i++)
    {
      if (!gfc_reduce_init_expr (ar->as->lower[i])
	  || !gfc_reduce_init_expr (ar->as->upper[i]))
	{
	  t = false;
	  cons = NULL;
	  goto depart;
	}

      e = ar->start[i];
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
	  t = false;
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

  for (cons = gfc_constructor_first (base), nelemen = mpz_get_ui (offset);
       cons && nelemen > 0; cons = gfc_constructor_next (cons), nelemen--)
    {
      if (cons->iterator)
	{
	  cons = NULL;
	  goto depart;
	}
    }

depart:
  mpz_clear (delta);
  mpz_clear (offset);
  mpz_clear (span);
  mpz_clear (tmp);
  *rval = cons;
  return t;
}


/* Find a component of a structure constructor.  */

static gfc_constructor *
find_component_ref (gfc_constructor_base base, gfc_ref *ref)
{
  gfc_component *comp;
  gfc_component *pick;
  gfc_constructor *c = gfc_constructor_first (base);

  comp = ref->u.c.sym->components;
  pick = ref->u.c.component;
  while (comp != pick)
    {
      comp = comp->next;
      c = gfc_constructor_next (c);
    }

  return c;
}


/* Replace an expression with the contents of a constructor, removing
   the subobject reference in the process.  */

static void
remove_subobject_ref (gfc_expr *p, gfc_constructor *cons)
{
  gfc_expr *e;

  if (cons)
    {
      e = cons->expr;
      cons->expr = NULL;
    }
  else
    e = gfc_copy_expr (p);
  e->ref = p->ref->next;
  p->ref->next =  NULL;
  gfc_replace_expr (p, e);
}


/* Pull an array section out of an array constructor.  */

static bool
find_array_section (gfc_expr *expr, gfc_ref *ref)
{
  int idx;
  int rank;
  int d;
  int shape_i;
  int limit;
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
  gfc_constructor_base base;
  gfc_constructor *cons, *vecsub[GFC_MAX_DIMENSIONS];
  gfc_expr *begin;
  gfc_expr *finish;
  gfc_expr *step;
  gfc_expr *upper;
  gfc_expr *lower;
  bool t;

  t = true;

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
	  gfc_constructor *ci;
	  gcc_assert (begin);

	  if (begin->expr_type != EXPR_ARRAY || !gfc_is_constant_expr (begin))
	    {
	      t = false;
	      goto cleanup;
	    }

	  gcc_assert (begin->rank == 1);
	  /* Zero-sized arrays have no shape and no elements, stop early.  */
	  if (!begin->shape)
	    {
	      mpz_init_set_ui (nelts, 0);
	      break;
	    }

	  vecsub[d] = gfc_constructor_first (begin->value.constructor);
	  mpz_set (ctr[d], vecsub[d]->expr->value.integer);
	  mpz_mul (nelts, nelts, begin->shape[0]);
	  mpz_set (expr->shape[shape_i++], begin->shape[0]);

	  /* Check bounds.  */
	  for (ci = vecsub[d]; ci; ci = gfc_constructor_next (ci))
	    {
	      if (mpz_cmp (ci->expr->value.integer, upper->value.integer) > 0
		  || mpz_cmp (ci->expr->value.integer,
			      lower->value.integer) < 0)
		{
		  gfc_error ("index in dimension %d is out of bounds "
			     "at %L", d + 1, &ref->u.ar.c_where[d]);
		  t = false;
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
	      t = false;
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
	      t = false;
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

  mpz_init (ptr);
  cons = gfc_constructor_first (base);

  /* Now clock through the array reference, calculating the index in
     the source constructor and transferring the elements to the new
     constructor.  */
  for (idx = 0; idx < (int) mpz_get_si (nelts); idx++)
    {
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

	      if (!gfc_constructor_next (vecsub[d]))
		vecsub[d] = gfc_constructor_first (ref->u.ar.start[d]->value.constructor);
	      else
		{
		  vecsub[d] = gfc_constructor_next (vecsub[d]);
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

      limit = mpz_get_ui (ptr);
      if (limit >= gfc_option.flag_max_array_constructor)
        {
	  gfc_error ("The number of elements in the array constructor "
		     "at %L requires an increase of the allowed %d "
		     "upper limit.   See -fmax-array-constructor "
		     "option", &expr->where,
		     gfc_option.flag_max_array_constructor);
	  return false;
	}

      cons = gfc_constructor_lookup (base, limit);
      gcc_assert (cons);
      gfc_constructor_append_expr (&expr->value.constructor,
				   gfc_copy_expr (cons->expr), NULL);
    }

  mpz_clear (ptr);

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
  gfc_constructor_free (base);
  return t;
}

/* Pull a substring out of an expression.  */

static bool
find_substring_ref (gfc_expr *p, gfc_expr **newp)
{
  int end;
  int start;
  int length;
  gfc_char_t *chr;

  if (p->ref->u.ss.start->expr_type != EXPR_CONSTANT
      || p->ref->u.ss.end->expr_type != EXPR_CONSTANT)
    return false;

  *newp = gfc_copy_expr (p);
  free ((*newp)->value.character.string);

  end = (int) mpz_get_ui (p->ref->u.ss.end->value.integer);
  start = (int) mpz_get_ui (p->ref->u.ss.start->value.integer);
  length = end - start + 1;

  chr = (*newp)->value.character.string = gfc_get_wide_string (length + 1);
  (*newp)->value.character.length = length;
  memcpy (chr, &p->value.character.string[start - 1],
	  length * sizeof (gfc_char_t));
  chr[length] = '\0';
  return true;
}



/* Simplify a subobject reference of a constructor.  This occurs when
   parameter variable values are substituted.  */

static bool
simplify_const_ref (gfc_expr *p)
{
  gfc_constructor *cons, *c;
  gfc_expr *newp;
  gfc_ref *last_ref;

  while (p->ref)
    {
      switch (p->ref->type)
	{
	case REF_ARRAY:
	  switch (p->ref->u.ar.type)
	    {
	    case AR_ELEMENT:
	      /* <type/kind spec>, parameter :: x(<int>) = scalar_expr
		 will generate this.  */
	      if (p->expr_type != EXPR_ARRAY)
		{
		  remove_subobject_ref (p, NULL);
		  break;
		}
	      if (!find_array_element (p->value.constructor, &p->ref->u.ar, &cons))
		return false;

	      if (!cons)
		return true;

	      remove_subobject_ref (p, cons);
	      break;

	    case AR_SECTION:
	      if (!find_array_section (p, p->ref))
		return false;
	      p->ref->u.ar.type = AR_FULL;

	    /* Fall through.  */

	    case AR_FULL:
	      if (p->ref->next != NULL
		  && (p->ts.type == BT_CHARACTER || p->ts.type == BT_DERIVED))
		{
		  for (c = gfc_constructor_first (p->value.constructor);
		       c; c = gfc_constructor_next (c))
		    {
		      c->expr->ref = gfc_copy_ref (p->ref->next);
		      if (!simplify_const_ref (c->expr))
			return false;
		    }

		  if (p->ts.type == BT_DERIVED
			&& p->ref->next
			&& (c = gfc_constructor_first (p->value.constructor)))
		    {
		      /* There may have been component references.  */
		      p->ts = c->expr->ts;
		    }

		  last_ref = p->ref;
		  for (; last_ref->next; last_ref = last_ref->next) {};

		  if (p->ts.type == BT_CHARACTER
			&& last_ref->type == REF_SUBSTRING)
		    {
		      /* If this is a CHARACTER array and we possibly took
			 a substring out of it, update the type-spec's
			 character length according to the first element
			 (as all should have the same length).  */
		      int string_len;
		      if ((c = gfc_constructor_first (p->value.constructor)))
			{
			  const gfc_expr* first = c->expr;
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

		      p->ts.u.cl->length
			= gfc_get_int_expr (gfc_default_integer_kind,
					    NULL, string_len);
		    }
		}
	      gfc_free_ref_list (p->ref);
	      p->ref = NULL;
	      break;

	    default:
	      return true;
	    }

	  break;

	case REF_COMPONENT:
	  cons = find_component_ref (p->value.constructor, p->ref);
	  remove_subobject_ref (p, cons);
	  break;

	case REF_SUBSTRING:
  	  if (!find_substring_ref (p, &newp))
	    return false;

	  gfc_replace_expr (p, newp);
	  gfc_free_ref_list (p->ref);
	  p->ref = NULL;
	  break;
	}
    }

  return true;
}


/* Simplify a chain of references.  */

static bool
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
	      if (!gfc_simplify_expr (ref->u.ar.start[n], type))
		return false;
	      if (!gfc_simplify_expr (ref->u.ar.end[n], type))
		return false;
	      if (!gfc_simplify_expr (ref->u.ar.stride[n], type))
		return false;
	    }
	  break;

	case REF_SUBSTRING:
	  if (!gfc_simplify_expr (ref->u.ss.start, type))
	    return false;
	  if (!gfc_simplify_expr (ref->u.ss.end, type))
	    return false;
	  break;

	default:
	  break;
	}
    }
  return true;
}


/* Try to substitute the value of a parameter variable.  */

static bool
simplify_parameter_variable (gfc_expr *p, int type)
{
  gfc_expr *e;
  bool t;

  e = gfc_copy_expr (p->symtree->n.sym->value);
  if (e == NULL)
    return false;

  e->rank = p->rank;

  /* Do not copy subobject refs for constant.  */
  if (e->expr_type != EXPR_CONSTANT && p->ref != NULL)
    e->ref = gfc_copy_ref (p->ref);
  t = gfc_simplify_expr (e, type);

  /* Only use the simplification if it eliminated all subobject references.  */
  if (t && !e->ref)
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
   Returns false on error, true otherwise.
   NOTE: Will return true even if the expression can not be simplified.  */

bool
gfc_simplify_expr (gfc_expr *p, int type)
{
  gfc_actual_arglist *ap;

  if (p == NULL)
    return true;

  switch (p->expr_type)
    {
    case EXPR_CONSTANT:
    case EXPR_NULL:
      break;

    case EXPR_FUNCTION:
      for (ap = p->value.function.actual; ap; ap = ap->next)
	if (!gfc_simplify_expr (ap->expr, type))
	  return false;

      if (p->value.function.isym != NULL
	  && gfc_intrinsic_func_interface (p, 1) == MATCH_ERROR)
	return false;

      break;

    case EXPR_SUBSTRING:
      if (!simplify_ref_chain (p->ref, type))
	return false;

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

	  if (end < start)
	    end = start;

	  s = gfc_get_wide_string (end - start + 2);
	  memcpy (s, p->value.character.string + start,
		  (end - start) * sizeof (gfc_char_t));
	  s[end - start + 1] = '\0';  /* TODO: C-style string.  */
	  free (p->value.character.string);
	  p->value.character.string = s;
	  p->value.character.length = end - start;
	  p->ts.u.cl = gfc_new_charlen (gfc_current_ns, NULL);
	  p->ts.u.cl->length = gfc_get_int_expr (gfc_default_integer_kind,
						 NULL,
						 p->value.character.length);
	  gfc_free_ref_list (p->ref);
	  p->ref = NULL;
	  p->expr_type = EXPR_CONSTANT;
	}
      break;

    case EXPR_OP:
      if (!simplify_intrinsic_op (p, type))
	return false;
      break;

    case EXPR_VARIABLE:
      /* Only substitute array parameter variables if we are in an
	 initialization expression, or we want a subsection.  */
      if (p->symtree->n.sym->attr.flavor == FL_PARAMETER
	  && (gfc_init_expr_flag || p->ref
	      || p->symtree->n.sym->value->expr_type != EXPR_ARRAY))
	{
	  if (!simplify_parameter_variable (p, type))
	    return false;
	  break;
	}

      if (type == 1)
	{
	  gfc_simplify_iterator_var (p);
	}

      /* Simplify subcomponent references.  */
      if (!simplify_ref_chain (p->ref, type))
	return false;

      break;

    case EXPR_STRUCTURE:
    case EXPR_ARRAY:
      if (!simplify_ref_chain (p->ref, type))
	return false;

      if (!simplify_constructor (p->value.constructor, type))
	return false;

      if (p->expr_type == EXPR_ARRAY && p->ref && p->ref->type == REF_ARRAY
	  && p->ref->u.ar.type == AR_FULL)
	  gfc_expand_constructor (p, false);

      if (!simplify_const_ref (p))
	return false;

      break;

    case EXPR_COMPCALL:
    case EXPR_PPC:
      break;
    }

  return true;
}


/* Returns the type of an expression with the exception that iterator
   variables are automatically integers no matter what else they may
   be declared as.  */

static bt
et0 (gfc_expr *e)
{
  if (e->expr_type == EXPR_VARIABLE && gfc_check_iter_variable (e))
    return BT_INTEGER;

  return e->ts.type;
}


/* Scalarize an expression for an elemental intrinsic call.  */

static bool
scalarize_intrinsic_call (gfc_expr *e)
{
  gfc_actual_arglist *a, *b;
  gfc_constructor_base ctor;
  gfc_constructor *args[5];
  gfc_constructor *ci, *new_ctor;
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
    return false;

  old = gfc_copy_expr (e);

  gfc_constructor_free (expr->value.constructor);
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
      if (a->expr && !gfc_check_init_expr (a->expr))
	goto cleanup;

      rank[n] = 0;
      if (a->expr && a->expr->rank && a->expr->expr_type == EXPR_VARIABLE)
	{
	  rank[n] = a->expr->rank;
	  ctor = a->expr->symtree->n.sym->value->value.constructor;
	  args[n] = gfc_constructor_first (ctor);
	}
      else if (a->expr && a->expr->expr_type == EXPR_ARRAY)
	{
	  if (a->expr->rank)
	    rank[n] = a->expr->rank;
	  else
	    rank[n] = 1;
	  ctor = gfc_constructor_copy (a->expr->value.constructor);
	  args[n] = gfc_constructor_first (ctor);
	}
      else
	args[n] = NULL;

      n++;
    }


  /* Using the array argument as the master, step through the array
     calling the function for each element and advancing the array
     constructors together.  */
  for (ci = args[array_arg - 1]; ci; ci = gfc_constructor_next (ci))
    {
      new_ctor = gfc_constructor_append_expr (&expr->value.constructor,
					      gfc_copy_expr (old), NULL);

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
	  args[i] = gfc_constructor_next (args[i]);

      for (i = 1; i < n; i++)
	if (rank[i] && ((args[i] != NULL && args[array_arg - 1] == NULL)
			|| (args[i] == NULL && args[array_arg - 1] != NULL)))
	  goto compliance;
    }

  free_expr0 (e);
  *e = *expr;
  /* Free "expr" but not the pointers it contains.  */
  free (expr);
  gfc_free_expr (old);
  return true;

compliance:
  gfc_error_now ("elemental function arguments at %C are not compliant");

cleanup:
  gfc_free_expr (expr);
  gfc_free_expr (old);
  return false;
}


static bool
check_intrinsic_op (gfc_expr *e, bool (*check_function) (gfc_expr *))
{
  gfc_expr *op1 = e->value.op.op1;
  gfc_expr *op2 = e->value.op.op2;

  if (!(*check_function)(op1))
    return false;

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
      if (!(*check_function)(op2))
	return false;

      if (!(et0 (op1) == BT_CHARACTER && et0 (op2) == BT_CHARACTER)
	  && !(numeric_type (et0 (op1)) && numeric_type (et0 (op2))))
	{
	  gfc_error ("Numeric or CHARACTER operands are required in "
		     "expression at %L", &e->where);
	 return false;
	}
      break;

    case INTRINSIC_PLUS:
    case INTRINSIC_MINUS:
    case INTRINSIC_TIMES:
    case INTRINSIC_DIVIDE:
    case INTRINSIC_POWER:
      if (!(*check_function)(op2))
	return false;

      if (!numeric_type (et0 (op1)) || !numeric_type (et0 (op2)))
	goto not_numeric;

      break;

    case INTRINSIC_CONCAT:
      if (!(*check_function)(op2))
	return false;

      if (et0 (op1) != BT_CHARACTER || et0 (op2) != BT_CHARACTER)
	{
	  gfc_error ("Concatenation operator in expression at %L "
		     "must have two CHARACTER operands", &op1->where);
	  return false;
	}

      if (op1->ts.kind != op2->ts.kind)
	{
	  gfc_error ("Concat operator at %L must concatenate strings of the "
		     "same kind", &e->where);
	  return false;
	}

      break;

    case INTRINSIC_NOT:
      if (et0 (op1) != BT_LOGICAL)
	{
	  gfc_error (".NOT. operator in expression at %L must have a LOGICAL "
		     "operand", &op1->where);
	  return false;
	}

      break;

    case INTRINSIC_AND:
    case INTRINSIC_OR:
    case INTRINSIC_EQV:
    case INTRINSIC_NEQV:
      if (!(*check_function)(op2))
	return false;

      if (et0 (op1) != BT_LOGICAL || et0 (op2) != BT_LOGICAL)
	{
	  gfc_error ("LOGICAL operands are required in expression at %L",
		     &e->where);
	  return false;
	}

      break;

    case INTRINSIC_PARENTHESES:
      break;

    default:
      gfc_error ("Only intrinsic operators can be used in expression at %L",
		 &e->where);
      return false;
    }

  return true;

not_numeric:
  gfc_error ("Numeric operands are required in expression at %L", &e->where);

  return false;
}

/* F2003, 7.1.7 (3): In init expression, allocatable components
   must not be data-initialized.  */
static bool
check_alloc_comp_init (gfc_expr *e)
{
  gfc_component *comp;
  gfc_constructor *ctor;

  gcc_assert (e->expr_type == EXPR_STRUCTURE);
  gcc_assert (e->ts.type == BT_DERIVED);

  for (comp = e->ts.u.derived->components,
       ctor = gfc_constructor_first (e->value.constructor);
       comp; comp = comp->next, ctor = gfc_constructor_next (ctor))
    {
      if (comp->attr.allocatable
          && ctor->expr->expr_type != EXPR_NULL)
        {
	  gfc_error("Invalid initialization expression for ALLOCATABLE "
	            "component '%s' in structure constructor at %L",
	            comp->name, &ctor->expr->where);
	  return false;
	}
    }

  return true;
}

static match
check_init_expr_arguments (gfc_expr *e)
{
  gfc_actual_arglist *ap;

  for (ap = e->value.function.actual; ap; ap = ap->next)
    if (!gfc_check_init_expr (ap->expr))
      return MATCH_ERROR;

  return MATCH_YES;
}

static bool check_restricted (gfc_expr *);

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

  int i = 0;
  gfc_actual_arglist *ap;

  if (!e->value.function.isym
      || !e->value.function.isym->inquiry)
    return MATCH_NO;

  /* An undeclared parameter will get us here (PR25018).  */
  if (e->symtree == NULL)
    return MATCH_NO;

  if (e->symtree->n.sym->from_intmod)
    {
      if (e->symtree->n.sym->from_intmod == INTMOD_ISO_FORTRAN_ENV
	  && e->symtree->n.sym->intmod_sym_id != ISOFORTRAN_COMPILER_OPTIONS
	  && e->symtree->n.sym->intmod_sym_id != ISOFORTRAN_COMPILER_VERSION)
	return MATCH_NO;

      if (e->symtree->n.sym->from_intmod == INTMOD_ISO_C_BINDING
	  && e->symtree->n.sym->intmod_sym_id != ISOCBINDING_C_SIZEOF)
	return MATCH_NO;
    }
  else
    {
      name = e->symtree->n.sym->name;

      functions = (gfc_option.warn_std & GFC_STD_F2003)
		? inquiry_func_f2003 : inquiry_func_f95;

      for (i = 0; functions[i]; i++)
	if (strcmp (functions[i], name) == 0)
	  break;

	if (functions[i] == NULL)
	  return MATCH_ERROR;
    }

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
	      && !gfc_set_default_type (ap->expr->symtree->n.sym, 0, gfc_current_ns))
	    return MATCH_NO;

	  ap->expr->ts = ap->expr->symtree->n.sym->ts;
	}

	/* Assumed character length will not reduce to a constant expression
	   with LEN, as required by the standard.  */
	if (i == 5 && not_restricted
	    && ap->expr->symtree->n.sym->ts.type == BT_CHARACTER
	    && (ap->expr->symtree->n.sym->ts.u.cl->length == NULL
		|| ap->expr->symtree->n.sym->ts.deferred))
	  {
	    gfc_error ("Assumed or deferred character length variable '%s' "
			" in constant expression at %L",
			ap->expr->symtree->n.sym->name,
			&ap->expr->where);
	      return MATCH_ERROR;
	  }
	else if (not_restricted && !gfc_check_init_expr (ap->expr))
	  return MATCH_ERROR;

	if (not_restricted == 0
	      && ap->expr->expr_type != EXPR_VARIABLE
	      && !check_restricted (ap->expr))
	  return MATCH_ERROR;

	if (not_restricted == 0
	    && ap->expr->expr_type == EXPR_VARIABLE
	    && ap->expr->symtree->n.sym->attr.dummy
	    && ap->expr->symtree->n.sym->attr.optional)
	  return MATCH_NO;
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
      && !gfc_notify_std (GFC_STD_F2003, "Evaluation of nonstandard "
			  "initialization expression at %L", &e->where))
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
   false is returned an error message has been generated.  */

bool
gfc_check_init_expr (gfc_expr *e)
{
  match m;
  bool t;

  if (e == NULL)
    return true;

  switch (e->expr_type)
    {
    case EXPR_OP:
      t = check_intrinsic_op (e, gfc_check_init_expr);
      if (t)
	t = gfc_simplify_expr (e, 0);

      break;

    case EXPR_FUNCTION:
      t = false;

      {
	gfc_intrinsic_sym* isym;
	gfc_symbol* sym;

	sym = e->symtree->n.sym;
	if (!gfc_is_intrinsic (sym, 0, e->where)
	    || (m = gfc_intrinsic_func_interface (e, 0)) != MATCH_YES)
	  {
	    gfc_error ("Function '%s' in initialization expression at %L "
		       "must be an intrinsic function",
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

	if (m == MATCH_ERROR)
	  return false;

	/* Try to scalarize an elemental intrinsic function that has an
	   array argument.  */
	isym = gfc_find_function (e->symtree->n.sym->name);
	if (isym && isym->elemental
	    && (t = scalarize_intrinsic_call(e)))
	  break;
      }

      if (m == MATCH_YES)
	t = gfc_simplify_expr (e, 0);

      break;

    case EXPR_VARIABLE:
      t = true;

      if (gfc_check_iter_variable (e))
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
	      t = false;
	    }
	  else
	    t = simplify_parameter_variable (e, 0);

	  break;
	}

      if (gfc_in_match_data ())
	break;

      t = false;

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
      t = true;
      break;

    case EXPR_SUBSTRING:
      t = gfc_check_init_expr (e->ref->u.ss.start);
      if (!t)
	break;

      t = gfc_check_init_expr (e->ref->u.ss.end);
      if (t)
	t = gfc_simplify_expr (e, 0);

      break;

    case EXPR_STRUCTURE:
      t = e->ts.is_iso_c ? true : false;
      if (t)
	break;

      t = check_alloc_comp_init (e);
      if (!t)
	break;

      t = gfc_check_constructor (e, gfc_check_init_expr);
      if (!t)
	break;

      break;

    case EXPR_ARRAY:
      t = gfc_check_constructor (e, gfc_check_init_expr);
      if (!t)
	break;

      t = gfc_expand_constructor (e, true);
      if (!t)
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
   Note that this function doesn't free the given expression on false.  */

bool
gfc_reduce_init_expr (gfc_expr *expr)
{
  bool t;

  gfc_init_expr_flag = true;
  t = gfc_resolve_expr (expr);
  if (t)
    t = gfc_check_init_expr (expr);
  gfc_init_expr_flag = false;

  if (!t)
    return false;

  if (expr->expr_type == EXPR_ARRAY)
    {
      if (!gfc_check_constructor_type (expr))
	return false;
      if (!gfc_expand_constructor (expr, true))
	return false;
    }

  return true;
}


/* Match an initialization expression.  We work by first matching an
   expression, then reducing it to a constant.  */

match
gfc_match_init_expr (gfc_expr **result)
{
  gfc_expr *expr;
  match m;
  bool t;

  expr = NULL;

  gfc_init_expr_flag = true;

  m = gfc_match_expr (&expr);
  if (m != MATCH_YES)
    {
      gfc_init_expr_flag = false;
      return m;
    }

  t = gfc_reduce_init_expr (expr);
  if (!t)
    {
      gfc_free_expr (expr);
      gfc_init_expr_flag = false;
      return MATCH_ERROR;
    }

  *result = expr;
  gfc_init_expr_flag = false;

  return MATCH_YES;
}


/* Given an actual argument list, test to see that each argument is a
   restricted expression and optionally if the expression type is
   integer or character.  */

static bool
restricted_args (gfc_actual_arglist *a)
{
  for (; a; a = a->next)
    {
      if (!check_restricted (a->expr))
	return false;
    }

  return true;
}


/************* Restricted/specification expressions *************/


/* Make sure a non-intrinsic function is a specification function.  */

static bool
external_spec_function (gfc_expr *e)
{
  gfc_symbol *f;

  f = e->value.function.esym;

  if (f->attr.proc == PROC_ST_FUNCTION)
    {
      gfc_error ("Specification function '%s' at %L cannot be a statement "
		 "function", f->name, &e->where);
      return false;
    }

  if (f->attr.proc == PROC_INTERNAL)
    {
      gfc_error ("Specification function '%s' at %L cannot be an internal "
		 "function", f->name, &e->where);
      return false;
    }

  if (!f->attr.pure && !f->attr.elemental)
    {
      gfc_error ("Specification function '%s' at %L must be PURE", f->name,
		 &e->where);
      return false;
    }

  if (f->attr.recursive)
    {
      gfc_error ("Specification function '%s' at %L cannot be RECURSIVE",
		 f->name, &e->where);
      return false;
    }

  return restricted_args (e->value.function.actual);
}


/* Check to see that a function reference to an intrinsic is a
   restricted expression.  */

static bool
restricted_intrinsic (gfc_expr *e)
{
  /* TODO: Check constraints on inquiry functions.  7.1.6.2 (7).  */
  if (check_inquiry (e, 0) == MATCH_YES)
    return true;

  return restricted_args (e->value.function.actual);
}


/* Check the expressions of an actual arglist.  Used by check_restricted.  */

static bool
check_arglist (gfc_actual_arglist* arg, bool (*checker) (gfc_expr*))
{
  for (; arg; arg = arg->next)
    if (!checker (arg->expr))
      return false;

  return true;
}


/* Check the subscription expressions of a reference chain with a checking
   function; used by check_restricted.  */

static bool
check_references (gfc_ref* ref, bool (*checker) (gfc_expr*))
{
  int dim;

  if (!ref)
    return true;

  switch (ref->type)
    {
    case REF_ARRAY:
      for (dim = 0; dim != ref->u.ar.dimen; ++dim)
	{
	  if (!checker (ref->u.ar.start[dim]))
	    return false;
	  if (!checker (ref->u.ar.end[dim]))
	    return false;
	  if (!checker (ref->u.ar.stride[dim]))
	    return false;
	}
      break;

    case REF_COMPONENT:
      /* Nothing needed, just proceed to next reference.  */
      break;

    case REF_SUBSTRING:
      if (!checker (ref->u.ss.start))
	return false;
      if (!checker (ref->u.ss.end))
	return false;
      break;

    default:
      gcc_unreachable ();
      break;
    }

  return check_references (ref->next, checker);
}


/* Verify that an expression is a restricted expression.  Like its
   cousin check_init_expr(), an error message is generated if we
   return false.  */

static bool
check_restricted (gfc_expr *e)
{
  gfc_symbol* sym;
  bool t;

  if (e == NULL)
    return true;

  switch (e->expr_type)
    {
    case EXPR_OP:
      t = check_intrinsic_op (e, check_restricted);
      if (t)
	t = gfc_simplify_expr (e, 0);

      break;

    case EXPR_FUNCTION:
      if (e->value.function.esym)
	{
	  t = check_arglist (e->value.function.actual, &check_restricted);
	  if (t)
	    t = external_spec_function (e);
	}
      else
	{
	  if (e->value.function.isym && e->value.function.isym->inquiry)
	    t = true;
	  else
	    t = check_arglist (e->value.function.actual, &check_restricted);

	  if (t)
	    t = restricted_intrinsic (e);
	}
      break;

    case EXPR_VARIABLE:
      sym = e->symtree->n.sym;
      t = false;

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
      if (!check_references (e->ref, &check_restricted))
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
	  t = true;
	  break;
	}

      gfc_error ("Variable '%s' cannot appear in the expression at %L",
		 sym->name, &e->where);
      /* Prevent a repetition of the error.  */
      e->error = 1;
      break;

    case EXPR_NULL:
    case EXPR_CONSTANT:
      t = true;
      break;

    case EXPR_SUBSTRING:
      t = gfc_specification_expr (e->ref->u.ss.start);
      if (!t)
	break;

      t = gfc_specification_expr (e->ref->u.ss.end);
      if (t)
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
   we return false, an error has been generated.  */

bool
gfc_specification_expr (gfc_expr *e)
{
  gfc_component *comp;

  if (e == NULL)
    return true;

  if (e->ts.type != BT_INTEGER)
    {
      gfc_error ("Expression at %L must be of INTEGER type, found %s",
		 &e->where, gfc_basic_typename (e->ts.type));
      return false;
    }

  comp = gfc_get_proc_ptr_comp (e);
  if (e->expr_type == EXPR_FUNCTION
      && !e->value.function.isym
      && !e->value.function.esym
      && !gfc_pure (e->symtree->n.sym)
      && (!comp || !comp->attr.pure))
    {
      gfc_error ("Function '%s' at %L must be PURE",
		 e->symtree->n.sym->name, &e->where);
      /* Prevent repeat error messages.  */
      e->symtree->n.sym->attr.pure = 1;
      return false;
    }

  if (e->rank != 0)
    {
      gfc_error ("Expression at %L must be scalar", &e->where);
      return false;
    }

  if (!gfc_simplify_expr (e, 0))
    return false;

  return check_restricted (e);
}


/************** Expression conformance checks.  *************/

/* Given two expressions, make sure that the arrays are conformable.  */

bool
gfc_check_conformance (gfc_expr *op1, gfc_expr *op2, const char *optype_msgid, ...)
{
  int op1_flag, op2_flag, d;
  mpz_t op1_size, op2_size;
  bool t;

  va_list argp;
  char buffer[240];

  if (op1->rank == 0 || op2->rank == 0)
    return true;

  va_start (argp, optype_msgid);
  vsnprintf (buffer, 240, optype_msgid, argp);
  va_end (argp);

  if (op1->rank != op2->rank)
    {
      gfc_error ("Incompatible ranks in %s (%d and %d) at %L", _(buffer),
		 op1->rank, op2->rank, &op1->where);
      return false;
    }

  t = true;

  for (d = 0; d < op1->rank; d++)
    {
      op1_flag = gfc_array_dimen_size(op1, d, &op1_size);
      op2_flag = gfc_array_dimen_size(op2, d, &op2_size);

      if (op1_flag && op2_flag && mpz_cmp (op1_size, op2_size) != 0)
	{
	  gfc_error ("Different shape for %s at %L on dimension %d "
		     "(%d and %d)", _(buffer), &op1->where, d + 1,
		     (int) mpz_get_si (op1_size),
		     (int) mpz_get_si (op2_size));

	  t = false;
	}

      if (op1_flag)
	mpz_clear (op1_size);
      if (op2_flag)
	mpz_clear (op2_size);

      if (!t)
	return false;
    }

  return true;
}


/* Given an assignable expression and an arbitrary expression, make
   sure that the assignment can take place.  */

bool
gfc_check_assign (gfc_expr *lvalue, gfc_expr *rvalue, int conform)
{
  gfc_symbol *sym;
  gfc_ref *ref;
  int has_pointer;

  sym = lvalue->symtree->n.sym;

  /* See if this is the component or subcomponent of a pointer.  */
  has_pointer = sym->attr.pointer;
  for (ref = lvalue->ref; ref; ref = ref->next)
    if (ref->type == REF_COMPONENT && ref->u.c.component->attr.pointer)
      {
	has_pointer = 1;
	break;
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
	  return false;
	}
    }

  if (rvalue->rank != 0 && lvalue->rank != rvalue->rank)
    {
      gfc_error ("Incompatible ranks %d and %d in assignment at %L",
		 lvalue->rank, rvalue->rank, &lvalue->where);
      return false;
    }

  if (lvalue->ts.type == BT_UNKNOWN)
    {
      gfc_error ("Variable type is UNKNOWN in assignment at %L",
		 &lvalue->where);
      return false;
    }

  if (rvalue->expr_type == EXPR_NULL)
    {
      if (has_pointer && (ref == NULL || ref->next == NULL)
	  && lvalue->symtree->n.sym->attr.data)
        return true;
      else
	{
	  gfc_error ("NULL appears on right-hand side in assignment at %L",
		     &rvalue->where);
	  return false;
	}
    }

  /* This is possibly a typo: x = f() instead of x => f().  */
  if (gfc_option.warn_surprising
      && rvalue->expr_type == EXPR_FUNCTION && gfc_expr_attr (rvalue).pointer)
    gfc_warning ("POINTER-valued function appears on right-hand side of "
		 "assignment at %L", &rvalue->where);

  /* Check size of array assignments.  */
  if (lvalue->rank != 0 && rvalue->rank != 0
      && !gfc_check_conformance (lvalue, rvalue, "array assignment"))
    return false;

  if (rvalue->is_boz && lvalue->ts.type != BT_INTEGER
      && lvalue->symtree->n.sym->attr.data
      && !gfc_notify_std (GFC_STD_GNU, "BOZ literal at %L used to "
			  "initialize non-integer variable '%s'", 
			  &rvalue->where, lvalue->symtree->n.sym->name))
    return false;
  else if (rvalue->is_boz && !lvalue->symtree->n.sym->attr.data
      && !gfc_notify_std (GFC_STD_GNU, "BOZ literal at %L outside "
			  "a DATA statement and outside INT/REAL/DBLE/CMPLX",
			  &rvalue->where))
    return false;

  /* Handle the case of a BOZ literal on the RHS.  */
  if (rvalue->is_boz && lvalue->ts.type != BT_INTEGER)
    {
      int rc;
      if (gfc_option.warn_surprising)
        gfc_warning ("BOZ literal at %L is bitwise transferred "
                     "non-integer symbol '%s'", &rvalue->where,
                     lvalue->symtree->n.sym->name);
      if (!gfc_convert_boz (rvalue, &lvalue->ts))
	return false;
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
	  return false;
	}
    }

  /*  Warn about type-changing conversions for REAL or COMPLEX constants.
      If lvalue and rvalue are mixed REAL and complex, gfc_compare_types
      will warn anyway, so there is no need to to so here.  */

  if (rvalue->expr_type == EXPR_CONSTANT && lvalue->ts.type == rvalue->ts.type
      && (lvalue->ts.type == BT_REAL || lvalue->ts.type == BT_COMPLEX))
    {
      if (lvalue->ts.kind < rvalue->ts.kind && gfc_option.gfc_warn_conversion)
	{
	  /* As a special bonus, don't warn about REAL rvalues which are not
	     changed by the conversion if -Wconversion is specified.  */
	  if (rvalue->ts.type == BT_REAL && mpfr_number_p (rvalue->value.real))
	    {
	      /* Calculate the difference between the constant and the rounded
		 value and check it against zero.  */
	      mpfr_t rv, diff;
	      gfc_set_model_kind (lvalue->ts.kind);
	      mpfr_init (rv);
	      gfc_set_model_kind (rvalue->ts.kind);
	      mpfr_init (diff);

	      mpfr_set (rv, rvalue->value.real, GFC_RND_MODE);
	      mpfr_sub (diff, rv, rvalue->value.real, GFC_RND_MODE);

	      if (!mpfr_zero_p (diff))
		gfc_warning ("Change of value in conversion from "
			     " %s to %s at %L", gfc_typename (&rvalue->ts),
			     gfc_typename (&lvalue->ts), &rvalue->where);

	      mpfr_clear (rv);
	      mpfr_clear (diff);
	    }
	  else
	    gfc_warning ("Possible change of value in conversion from %s "
			 "to %s at %L",gfc_typename (&rvalue->ts),
			 gfc_typename (&lvalue->ts), &rvalue->where);

	}
      else if (gfc_option.warn_conversion_extra
	       && lvalue->ts.kind > rvalue->ts.kind)
	{
	  gfc_warning ("Conversion from %s to %s at %L",
		       gfc_typename (&rvalue->ts),
		       gfc_typename (&lvalue->ts), &rvalue->where);
	}
    }

  if (gfc_compare_types (&lvalue->ts, &rvalue->ts))
    return true;

  /* Only DATA Statements come here.  */
  if (!conform)
    {
      /* Numeric can be converted to any other numeric. And Hollerith can be
	 converted to any other type.  */
      if ((gfc_numeric_ts (&lvalue->ts) && gfc_numeric_ts (&rvalue->ts))
	  || rvalue->ts.type == BT_HOLLERITH)
	return true;

      if (lvalue->ts.type == BT_LOGICAL && rvalue->ts.type == BT_LOGICAL)
	return true;

      gfc_error ("Incompatible types in DATA statement at %L; attempted "
		 "conversion of %s to %s", &lvalue->where,
		 gfc_typename (&rvalue->ts), gfc_typename (&lvalue->ts));

      return false;
    }

  /* Assignment is the only case where character variables of different
     kind values can be converted into one another.  */
  if (lvalue->ts.type == BT_CHARACTER && rvalue->ts.type == BT_CHARACTER)
    {
      if (lvalue->ts.kind != rvalue->ts.kind)
	gfc_convert_chartype (rvalue, &lvalue->ts);

      return true;
    }

  return gfc_convert_type (rvalue, &lvalue->ts, 1);
}


/* Check that a pointer assignment is OK.  We first check lvalue, and
   we only check rvalue if it's not an assignment to NULL() or a
   NULLIFY statement.  */

bool
gfc_check_pointer_assign (gfc_expr *lvalue, gfc_expr *rvalue)
{
  symbol_attribute attr, lhs_attr;
  gfc_ref *ref;
  bool is_pure, is_implicit_pure, rank_remap;
  int proc_pointer;

  lhs_attr = gfc_expr_attr (lvalue);
  if (lvalue->ts.type == BT_UNKNOWN && !lhs_attr.proc_pointer)
    {
      gfc_error ("Pointer assignment target is not a POINTER at %L",
		 &lvalue->where);
      return false;
    }

  if (lhs_attr.flavor == FL_PROCEDURE && lhs_attr.use_assoc
      && !lhs_attr.proc_pointer)
    {
      gfc_error ("'%s' in the pointer assignment at %L cannot be an "
		 "l-value since it is a procedure",
		 lvalue->symtree->n.sym->name, &lvalue->where);
      return false;
    }

  proc_pointer = lvalue->symtree->n.sym->attr.proc_pointer;

  rank_remap = false;
  for (ref = lvalue->ref; ref; ref = ref->next)
    {
      if (ref->type == REF_COMPONENT)
	proc_pointer = ref->u.c.component->attr.proc_pointer;

      if (ref->type == REF_ARRAY && ref->next == NULL)
	{
	  int dim;

	  if (ref->u.ar.type == AR_FULL)
	    break;

	  if (ref->u.ar.type != AR_SECTION)
	    {
	      gfc_error ("Expected bounds specification for '%s' at %L",
			 lvalue->symtree->n.sym->name, &lvalue->where);
	      return false;
	    }

	  if (!gfc_notify_std (GFC_STD_F2003, "Bounds specification "
			       "for '%s' in pointer assignment at %L", 
			       lvalue->symtree->n.sym->name, &lvalue->where))
	    return false;

	  /* When bounds are given, all lbounds are necessary and either all
	     or none of the upper bounds; no strides are allowed.  If the
	     upper bounds are present, we may do rank remapping.  */
	  for (dim = 0; dim < ref->u.ar.dimen; ++dim)
	    {
	      if (!ref->u.ar.start[dim]
		  || ref->u.ar.dimen_type[dim] != DIMEN_RANGE)
		{
		  gfc_error ("Lower bound has to be present at %L",
			     &lvalue->where);
		  return false;
		}
	      if (ref->u.ar.stride[dim])
		{
		  gfc_error ("Stride must not be present at %L",
			     &lvalue->where);
		  return false;
		}

	      if (dim == 0)
		rank_remap = (ref->u.ar.end[dim] != NULL);
	      else
		{
		  if ((rank_remap && !ref->u.ar.end[dim])
		      || (!rank_remap && ref->u.ar.end[dim]))
		    {
		      gfc_error ("Either all or none of the upper bounds"
				 " must be specified at %L", &lvalue->where);
		      return false;
		    }
		}
	    }
	}
    }

  is_pure = gfc_pure (NULL);
  is_implicit_pure = gfc_implicit_pure (NULL);

  /* If rvalue is a NULL() or NULLIFY, we're done. Otherwise the type,
     kind, etc for lvalue and rvalue must match, and rvalue must be a
     pure variable if we're in a pure function.  */
  if (rvalue->expr_type == EXPR_NULL && rvalue->ts.type == BT_UNKNOWN)
    return true;

  /* F2008, C723 (pointer) and C726 (proc-pointer); for PURE also C1283.  */
  if (lvalue->expr_type == EXPR_VARIABLE
      && gfc_is_coindexed (lvalue))
    {
      gfc_ref *ref;
      for (ref = lvalue->ref; ref; ref = ref->next)
	if (ref->type == REF_ARRAY && ref->u.ar.codimen)
	  {
	    gfc_error ("Pointer object at %L shall not have a coindex",
		       &lvalue->where);
	    return false;
	  }
    }

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
	  return false;
	}
      if (rvalue->expr_type == EXPR_VARIABLE && !attr.proc_pointer)
	{
      	  /* Check for intrinsics.  */
	  gfc_symbol *sym = rvalue->symtree->n.sym;
	  if (!sym->attr.intrinsic
	      && (gfc_is_intrinsic (sym, 0, sym->declared_at)
		  || gfc_is_intrinsic (sym, 1, sym->declared_at)))
	    {
	      sym->attr.intrinsic = 1;
	      gfc_resolve_intrinsic (sym, &rvalue->where);
	      attr = gfc_expr_attr (rvalue);
	    }
	  /* Check for result of embracing function.  */
	  if (sym->attr.function && sym->result == sym)
	    {
	      gfc_namespace *ns;

	      for (ns = gfc_current_ns; ns; ns = ns->parent)
		if (sym == ns->proc_name)
		  {
		    gfc_error ("Function result '%s' is invalid as proc-target "
			       "in procedure pointer assignment at %L",
			       sym->name, &rvalue->where);
		    return false;
		  }
	    }
	}
      if (attr.abstract)
	{
	  gfc_error ("Abstract interface '%s' is invalid "
		     "in procedure pointer assignment at %L",
		     rvalue->symtree->name, &rvalue->where);
	  return false;
	}
      /* Check for F08:C729.  */
      if (attr.flavor == FL_PROCEDURE)
	{
	  if (attr.proc == PROC_ST_FUNCTION)
	    {
	      gfc_error ("Statement function '%s' is invalid "
			 "in procedure pointer assignment at %L",
			 rvalue->symtree->name, &rvalue->where);
	      return false;
	    }
	  if (attr.proc == PROC_INTERNAL &&
	      !gfc_notify_std(GFC_STD_F2008, "Internal procedure '%s' "
			      "is invalid in procedure pointer assignment "
			      "at %L", rvalue->symtree->name, &rvalue->where))
	    return false;
	  if (attr.intrinsic && gfc_intrinsic_actual_ok (rvalue->symtree->name,
							 attr.subroutine) == 0)
	    {
	      gfc_error ("Intrinsic '%s' at %L is invalid in procedure pointer "
			 "assignment", rvalue->symtree->name, &rvalue->where);
	      return false;
	    }
	}
      /* Check for F08:C730.  */
      if (attr.elemental && !attr.intrinsic)
	{
	  gfc_error ("Nonintrinsic elemental procedure '%s' is invalid "
		     "in procedure pointer assignment at %L",
		     rvalue->symtree->name, &rvalue->where);
	  return false;
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
	  return false;
	    }
	}

      comp = gfc_get_proc_ptr_comp (lvalue);
      if (comp)
	s1 = comp->ts.interface;
      else
	{
	  s1 = lvalue->symtree->n.sym;
	  if (s1->ts.interface)
	    s1 = s1->ts.interface;
	}

      comp = gfc_get_proc_ptr_comp (rvalue);
      if (comp)
	{
	  if (rvalue->expr_type == EXPR_FUNCTION)
	    {
	      s2 = comp->ts.interface->result;
	      name = s2->name;
	    }
	  else
	    {
	      s2 = comp->ts.interface;
	      name = comp->name;
	    }
	}
      else if (rvalue->expr_type == EXPR_FUNCTION)
	{
	  if (rvalue->value.function.esym)
	    s2 = rvalue->value.function.esym->result;
	  else
	    s2 = rvalue->symtree->n.sym->result;

	  name = s2->name;
	}
      else
	{
	  s2 = rvalue->symtree->n.sym;
	  name = s2->name;
	}

      if (s2 && s2->attr.proc_pointer && s2->ts.interface)
	s2 = s2->ts.interface;

      if (s1 == s2 || !s1 || !s2)
	return true;

      /* F08:7.2.2.4 (4)  */
      if (s1->attr.if_source == IFSRC_UNKNOWN
	  && gfc_explicit_interface_required (s2, err, sizeof(err)))
	{
	  gfc_error ("Explicit interface required for '%s' at %L: %s",
		     s1->name, &lvalue->where, err);
	  return false;
	}
      if (s2->attr.if_source == IFSRC_UNKNOWN
	  && gfc_explicit_interface_required (s1, err, sizeof(err)))
	{
	  gfc_error ("Explicit interface required for '%s' at %L: %s",
		     s2->name, &rvalue->where, err);
	  return false;
	}

      if (!gfc_compare_interfaces (s1, s2, name, 0, 1,
				   err, sizeof(err), NULL, NULL))
	{
	  gfc_error ("Interface mismatch in procedure pointer assignment "
		     "at %L: %s", &rvalue->where, err);
	  return false;
	}

      return true;
    }

  if (!gfc_compare_types (&lvalue->ts, &rvalue->ts))
    {
      /* Check for F03:C717.  */
      if (UNLIMITED_POLY (rvalue)
	  && !(UNLIMITED_POLY (lvalue)
	       || (lvalue->ts.type == BT_DERIVED
		   && (lvalue->ts.u.derived->attr.is_bind_c
		       || lvalue->ts.u.derived->attr.sequence))))
	gfc_error ("Data-pointer-object &L must be unlimited "
		   "polymorphic, a sequence derived type or of a "
		   "type with the BIND attribute assignment at %L "
		   "to be compatible with an unlimited polymorphic "
		   "target", &lvalue->where);
      else
	gfc_error ("Different types in pointer assignment at %L; "
		   "attempted assignment of %s to %s", &lvalue->where,
		   gfc_typename (&rvalue->ts),
		   gfc_typename (&lvalue->ts));
      return false;
    }

  if (lvalue->ts.type != BT_CLASS && lvalue->ts.kind != rvalue->ts.kind)
    {
      gfc_error ("Different kind type parameters in pointer "
		 "assignment at %L", &lvalue->where);
      return false;
    }

  if (lvalue->rank != rvalue->rank && !rank_remap)
    {
      gfc_error ("Different ranks in pointer assignment at %L", &lvalue->where);
      return false;
    }

  /* Make sure the vtab is present.  */
  if (lvalue->ts.type == BT_CLASS && !UNLIMITED_POLY (rvalue))
    gfc_find_vtab (&rvalue->ts);

  /* Check rank remapping.  */
  if (rank_remap)
    {
      mpz_t lsize, rsize;

      /* If this can be determined, check that the target must be at least as
	 large as the pointer assigned to it is.  */
      if (gfc_array_size (lvalue, &lsize)
	  && gfc_array_size (rvalue, &rsize)
	  && mpz_cmp (rsize, lsize) < 0)
	{
	  gfc_error ("Rank remapping target is smaller than size of the"
		     " pointer (%ld < %ld) at %L",
		     mpz_get_si (rsize), mpz_get_si (lsize),
		     &lvalue->where);
	  return false;
	}

      /* The target must be either rank one or it must be simply contiguous
	 and F2008 must be allowed.  */
      if (rvalue->rank != 1)
	{
	  if (!gfc_is_simply_contiguous (rvalue, true))
	    {
	      gfc_error ("Rank remapping target must be rank 1 or"
			 " simply contiguous at %L", &rvalue->where);
	      return false;
	    }
	  if (!gfc_notify_std (GFC_STD_F2008, "Rank remapping target is not "
			       "rank 1 at %L", &rvalue->where))
	    return false;
	}
    }

  /* Now punt if we are dealing with a NULLIFY(X) or X = NULL(X).  */
  if (rvalue->expr_type == EXPR_NULL)
    return true;

  if (lvalue->ts.type == BT_CHARACTER)
    {
      bool t = gfc_check_same_strlen (lvalue, rvalue, "pointer assignment");
      if (!t)
	return false;
    }

  if (rvalue->expr_type == EXPR_VARIABLE && is_subref_array (rvalue))
    lvalue->symtree->n.sym->attr.subref_array_pointer = 1;

  attr = gfc_expr_attr (rvalue);

  if (rvalue->expr_type == EXPR_FUNCTION && !attr.pointer)
    {
      gfc_error ("Target expression in pointer assignment "
		 "at %L must deliver a pointer result",
		 &rvalue->where);
      return false;
    }

  if (!attr.target && !attr.pointer)
    {
      gfc_error ("Pointer assignment target is neither TARGET "
		 "nor POINTER at %L", &rvalue->where);
      return false;
    }

  if (is_pure && gfc_impure_variable (rvalue->symtree->n.sym))
    {
      gfc_error ("Bad target in pointer assignment in PURE "
		 "procedure at %L", &rvalue->where);
    }

  if (is_implicit_pure && gfc_impure_variable (rvalue->symtree->n.sym))
    gfc_current_ns->proc_name->attr.implicit_pure = 0;


  if (gfc_has_vector_index (rvalue))
    {
      gfc_error ("Pointer assignment with vector subscript "
		 "on rhs at %L", &rvalue->where);
      return false;
    }

  if (attr.is_protected && attr.use_assoc
      && !(attr.pointer || attr.proc_pointer))
    {
      gfc_error ("Pointer assignment target has PROTECTED "
		 "attribute at %L", &rvalue->where);
      return false;
    }

  /* F2008, C725. For PURE also C1283.  */
  if (rvalue->expr_type == EXPR_VARIABLE
      && gfc_is_coindexed (rvalue))
    {
      gfc_ref *ref;
      for (ref = rvalue->ref; ref; ref = ref->next)
	if (ref->type == REF_ARRAY && ref->u.ar.codimen)
	  {
	    gfc_error ("Data target at %L shall not have a coindex",
		       &rvalue->where);
	    return false;
	  }
    }

  /* Warn if it is the LHS pointer may lives longer than the RHS target.  */
  if (gfc_option.warn_target_lifetime
      && rvalue->expr_type == EXPR_VARIABLE
      && !rvalue->symtree->n.sym->attr.save
      && !attr.pointer && !rvalue->symtree->n.sym->attr.host_assoc
      && !rvalue->symtree->n.sym->attr.in_common
      && !rvalue->symtree->n.sym->attr.use_assoc
      && !rvalue->symtree->n.sym->attr.dummy)
    {
      bool warn;
      gfc_namespace *ns;

      warn = lvalue->symtree->n.sym->attr.dummy
	     || lvalue->symtree->n.sym->attr.result
	     || lvalue->symtree->n.sym->attr.function
	     || (lvalue->symtree->n.sym->attr.host_assoc
		 && lvalue->symtree->n.sym->ns
		    != rvalue->symtree->n.sym->ns)
	     || lvalue->symtree->n.sym->attr.use_assoc
	     || lvalue->symtree->n.sym->attr.in_common;

      if (rvalue->symtree->n.sym->ns->proc_name
	  && rvalue->symtree->n.sym->ns->proc_name->attr.flavor != FL_PROCEDURE
	  && rvalue->symtree->n.sym->ns->proc_name->attr.flavor != FL_PROGRAM)
       for (ns = rvalue->symtree->n.sym->ns;
	    ns && ns->proc_name && ns->proc_name->attr.flavor != FL_PROCEDURE;
	    ns = ns->parent)
	if (ns->parent == lvalue->symtree->n.sym->ns)
	  {
	    warn = true;
	    break;
	  }

      if (warn)
	gfc_warning ("Pointer at %L in pointer assignment might outlive the "
		     "pointer target", &lvalue->where);
    }

  return true;
}


/* Relative of gfc_check_assign() except that the lvalue is a single
   symbol.  Used for initialization assignments.  */

bool
gfc_check_assign_symbol (gfc_symbol *sym, gfc_component *comp, gfc_expr *rvalue)
{
  gfc_expr lvalue;
  bool r;
  bool pointer, proc_pointer;

  memset (&lvalue, '\0', sizeof (gfc_expr));

  lvalue.expr_type = EXPR_VARIABLE;
  lvalue.ts = sym->ts;
  if (sym->as)
    lvalue.rank = sym->as->rank;
  lvalue.symtree = XCNEW (gfc_symtree);
  lvalue.symtree->n.sym = sym;
  lvalue.where = sym->declared_at;

  if (comp)
    {
      lvalue.ref = gfc_get_ref ();
      lvalue.ref->type = REF_COMPONENT;
      lvalue.ref->u.c.component = comp;
      lvalue.ref->u.c.sym = sym;
      lvalue.ts = comp->ts;
      lvalue.rank = comp->as ? comp->as->rank : 0;
      lvalue.where = comp->loc;
      pointer = comp->ts.type == BT_CLASS &&  CLASS_DATA (comp)
		? CLASS_DATA (comp)->attr.class_pointer : comp->attr.pointer;
      proc_pointer = comp->attr.proc_pointer;
    }
  else
    {
      pointer = sym->ts.type == BT_CLASS &&  CLASS_DATA (sym)
		? CLASS_DATA (sym)->attr.class_pointer : sym->attr.pointer;
      proc_pointer = sym->attr.proc_pointer;
    }

  if (pointer || proc_pointer)
    r = gfc_check_pointer_assign (&lvalue, rvalue);
  else
    r = gfc_check_assign (&lvalue, rvalue, 1);

  free (lvalue.symtree);
  free (lvalue.ref);

  if (!r)
    return r;

  if (pointer && rvalue->expr_type != EXPR_NULL)
    {
      /* F08:C461. Additional checks for pointer initialization.  */
      symbol_attribute attr;
      attr = gfc_expr_attr (rvalue);
      if (attr.allocatable)
	{
	  gfc_error ("Pointer initialization target at %L "
	             "must not be ALLOCATABLE", &rvalue->where);
	  return false;
	}
      if (!attr.target || attr.pointer)
	{
	  gfc_error ("Pointer initialization target at %L "
		     "must have the TARGET attribute", &rvalue->where);
	  return false;
	}

      if (!attr.save && rvalue->expr_type == EXPR_VARIABLE
	  && rvalue->symtree->n.sym->ns->proc_name
	  && rvalue->symtree->n.sym->ns->proc_name->attr.is_main_program)
	{
	  rvalue->symtree->n.sym->ns->proc_name->attr.save = SAVE_IMPLICIT;
	  attr.save = SAVE_IMPLICIT;
	}

      if (!attr.save)
	{
	  gfc_error ("Pointer initialization target at %L "
		     "must have the SAVE attribute", &rvalue->where);
	  return false;
	}
    }

  if (proc_pointer && rvalue->expr_type != EXPR_NULL)
    {
      /* F08:C1220. Additional checks for procedure pointer initialization.  */
      symbol_attribute attr = gfc_expr_attr (rvalue);
      if (attr.proc_pointer)
	{
	  gfc_error ("Procedure pointer initialization target at %L "
		     "may not be a procedure pointer", &rvalue->where);
	  return false;
	}
    }

  return true;
}


/* Check for default initializer; sym->value is not enough
   as it is also set for EXPR_NULL of allocatables.  */

bool
gfc_has_default_initializer (gfc_symbol *der)
{
  gfc_component *c;

  gcc_assert (der->attr.flavor == FL_DERIVED);
  for (c = der->components; c; c = c->next)
    if (c->ts.type == BT_DERIVED)
      {
        if (!c->attr.pointer
	     && gfc_has_default_initializer (c->ts.u.derived))
	  return true;
	if (c->attr.pointer && c->initializer)
	  return true;
      }
    else
      {
        if (c->initializer)
	  return true;
      }

  return false;
}


/* Get an expression for a default initializer.  */

gfc_expr *
gfc_default_initializer (gfc_typespec *ts)
{
  gfc_expr *init;
  gfc_component *comp;

  /* See if we have a default initializer in this, but not in nested
     types (otherwise we could use gfc_has_default_initializer()).  */
  for (comp = ts->u.derived->components; comp; comp = comp->next)
    if (comp->initializer || comp->attr.allocatable
	|| (comp->ts.type == BT_CLASS && CLASS_DATA (comp)
	    && CLASS_DATA (comp)->attr.allocatable))
      break;

  if (!comp)
    return NULL;

  init = gfc_get_structure_constructor_expr (ts->type, ts->kind,
					     &ts->u.derived->declared_at);
  init->ts = *ts;

  for (comp = ts->u.derived->components; comp; comp = comp->next)
    {
      gfc_constructor *ctor = gfc_constructor_get();

      if (comp->initializer)
	{
	  ctor->expr = gfc_copy_expr (comp->initializer);
	  if ((comp->ts.type != comp->initializer->ts.type
	       || comp->ts.kind != comp->initializer->ts.kind)
	      && !comp->attr.pointer && !comp->attr.proc_pointer)
	    gfc_convert_type_warn (ctor->expr, &comp->ts, 2, false);
	}

      if (comp->attr.allocatable
	  || (comp->ts.type == BT_CLASS && CLASS_DATA (comp)->attr.allocatable))
	{
	  ctor->expr = gfc_get_expr ();
	  ctor->expr->expr_type = EXPR_NULL;
	  ctor->expr->ts = comp->ts;
	}

      gfc_constructor_append (&init->value.constructor, ctor);
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

  if ((var->n.sym->as != NULL && var->n.sym->ts.type != BT_CLASS)
      || (var->n.sym->ts.type == BT_CLASS && CLASS_DATA (var->n.sym)
	  && CLASS_DATA (var->n.sym)->as))
    {
      e->rank = var->n.sym->ts.type == BT_CLASS
		? CLASS_DATA (var->n.sym)->as->rank : var->n.sym->as->rank;
      e->ref = gfc_get_ref ();
      e->ref->type = REF_ARRAY;
      e->ref->u.ar.type = AR_FULL;
      e->ref->u.ar.as = gfc_copy_array_spec (var->n.sym->ts.type == BT_CLASS
					     ? CLASS_DATA (var->n.sym)->as
					     : var->n.sym->as);
    }

  return e;
}


/* Adds a full array reference to an expression, as needed.  */

void
gfc_add_full_array_ref (gfc_expr *e, gfc_array_spec *as)
{
  gfc_ref *ref;
  for (ref = e->ref; ref; ref = ref->next)
    if (!ref->next)
      break;
  if (ref)
    {
      ref->next = gfc_get_ref ();
      ref = ref->next;
    }
  else
    {
      e->ref = gfc_get_ref ();
      ref = e->ref;
    }
  ref->type = REF_ARRAY;
  ref->u.ar.type = AR_FULL;
  ref->u.ar.dimen = e->rank;
  ref->u.ar.where = e->where;
  ref->u.ar.as = as;
}


gfc_expr *
gfc_lval_expr_from_sym (gfc_symbol *sym)
{
  gfc_expr *lval;
  lval = gfc_get_expr ();
  lval->expr_type = EXPR_VARIABLE;
  lval->where = sym->declared_at;
  lval->ts = sym->ts;
  lval->symtree = gfc_find_symtree (sym->ns->sym_root, sym->name);

  /* It will always be a full array.  */
  lval->rank = sym->as ? sym->as->rank : 0;
  if (lval->rank)
    gfc_add_full_array_ref (lval, sym->ts.type == BT_CLASS ?
			    CLASS_DATA (sym)->as : sym->as);
  return lval;
}


/* Returns the array_spec of a full array expression.  A NULL is
   returned otherwise.  */
gfc_array_spec *
gfc_get_full_arrayspec_from_expr (gfc_expr *expr)
{
  gfc_array_spec *as;
  gfc_ref *ref;

  if (expr->rank == 0)
    return NULL;

  /* Follow any component references.  */
  if (expr->expr_type == EXPR_VARIABLE
      || expr->expr_type == EXPR_CONSTANT)
    {
      as = expr->symtree->n.sym->as;
      for (ref = expr->ref; ref; ref = ref->next)
	{
	  switch (ref->type)
	    {
	    case REF_COMPONENT:
	      as = ref->u.c.component->as;
	      continue;

	    case REF_SUBSTRING:
	      continue;

	    case REF_ARRAY:
	      {
		switch (ref->u.ar.type)
		  {
		  case AR_ELEMENT:
		  case AR_SECTION:
		  case AR_UNKNOWN:
		    as = NULL;
		    continue;

		  case AR_FULL:
		    break;
		  }
		break;
	      }
	    }
	}
    }
  else
    as = NULL;

  return as;
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
    case EXPR_PPC:
    case EXPR_COMPCALL:
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
      for (c = gfc_constructor_first (expr->value.constructor);
	   c; c = gfc_constructor_next (c))
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
	    for (i = 0; i < ref->u.c.component->as->rank
			    + ref->u.c.component->as->corank; i++)
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


/* Determine if an expression is a procedure pointer component and return
   the component in that case.  Otherwise return NULL.  */

gfc_component *
gfc_get_proc_ptr_comp (gfc_expr *expr)
{
  gfc_ref *ref;

  if (!expr || !expr->ref)
    return NULL;

  ref = expr->ref;
  while (ref->next)
    ref = ref->next;

  if (ref->type == REF_COMPONENT
      && ref->u.c.component->attr.proc_pointer)
    return ref->u.c.component;

  return NULL;
}


/* Determine if an expression is a procedure pointer component.  */

bool
gfc_is_proc_ptr_comp (gfc_expr *expr)
{
  return (gfc_get_proc_ptr_comp (expr) != NULL);
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
  bool t;

  if (e->expr_type != EXPR_VARIABLE)
    return false;

  gcc_assert (e->symtree);
  t = gfc_check_symbol_typed (e->symtree->n.sym, check_typed_ns,
                              true, e->where);

  return (!t);
}

bool
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
	  bool t = true;

	  gcc_assert (e->value.op.op1);
	  t = gfc_expr_check_typed (e->value.op.op1, ns, strict);

	  if (t && e->value.op.op2)
	    t = gfc_expr_check_typed (e->value.op.op2, ns, strict);

	  return t;
	}
    }

  /* Otherwise, walk the expression and do it strictly.  */
  check_typed_ns = ns;
  error_found = gfc_traverse_expr (e, NULL, &expr_check_typed_help, 0);

  return error_found ? false : true;
}


bool
gfc_ref_this_image (gfc_ref *ref)
{
  int n;

  gcc_assert (ref->type == REF_ARRAY && ref->u.ar.codimen > 0);

  for (n = ref->u.ar.dimen; n < ref->u.ar.dimen + ref->u.ar.codimen; n++)
    if (ref->u.ar.dimen_type[n] != DIMEN_THIS_IMAGE)
      return false;

  return true;
}


bool
gfc_is_coindexed (gfc_expr *e)
{
  gfc_ref *ref;

  for (ref = e->ref; ref; ref = ref->next)
    if (ref->type == REF_ARRAY && ref->u.ar.codimen > 0)
      return !gfc_ref_this_image (ref);

  return false;
}


/* Coarrays are variables with a corank but not being coindexed. However, also
   the following is a coarray: A subobject of a coarray is a coarray if it does
   not have any cosubscripts, vector subscripts, allocatable component
   selection, or pointer component selection. (F2008, 2.4.7)  */

bool
gfc_is_coarray (gfc_expr *e)
{
  gfc_ref *ref;
  gfc_symbol *sym;
  gfc_component *comp;
  bool coindexed;
  bool coarray;
  int i;

  if (e->expr_type != EXPR_VARIABLE)
    return false;

  coindexed = false;
  sym = e->symtree->n.sym;

  if (sym->ts.type == BT_CLASS && sym->attr.class_ok)
    coarray = CLASS_DATA (sym)->attr.codimension;
  else
    coarray = sym->attr.codimension;

  for (ref = e->ref; ref; ref = ref->next)
    switch (ref->type)
    {
      case REF_COMPONENT:
	comp = ref->u.c.component;
	if (comp->ts.type == BT_CLASS && comp->attr.class_ok
	    && (CLASS_DATA (comp)->attr.class_pointer
		|| CLASS_DATA (comp)->attr.allocatable))
	  {
	    coindexed = false;
	    coarray = CLASS_DATA (comp)->attr.codimension;
	  }
        else if (comp->attr.pointer || comp->attr.allocatable)
	  {
	    coindexed = false;
	    coarray = comp->attr.codimension;
	  }
        break;

     case REF_ARRAY:
	if (!coarray)
	  break;

	if (ref->u.ar.codimen > 0 && !gfc_ref_this_image (ref))
	  {
	    coindexed = true;
	    break;
	  }

	for (i = 0; i < ref->u.ar.dimen; i++)
	  if (ref->u.ar.dimen_type[i] == DIMEN_VECTOR)
	    {
	      coarray = false;
	      break;
	    }
	break;

     case REF_SUBSTRING:
	break;
    }

  return coarray && !coindexed;
}


int
gfc_get_corank (gfc_expr *e)
{
  int corank;
  gfc_ref *ref;

  if (!gfc_is_coarray (e))
    return 0;

  if (e->ts.type == BT_CLASS && e->ts.u.derived->components)
    corank = e->ts.u.derived->components->as
	     ? e->ts.u.derived->components->as->corank : 0;
  else
    corank = e->symtree->n.sym->as ? e->symtree->n.sym->as->corank : 0;

  for (ref = e->ref; ref; ref = ref->next)
    {
      if (ref->type == REF_ARRAY)
	corank = ref->u.ar.as->corank;
      gcc_assert (ref->type != REF_SUBSTRING);
    }

  return corank;
}


/* Check whether the expression has an ultimate allocatable component.
   Being itself allocatable does not count.  */
bool
gfc_has_ultimate_allocatable (gfc_expr *e)
{
  gfc_ref *ref, *last = NULL;

  if (e->expr_type != EXPR_VARIABLE)
    return false;

  for (ref = e->ref; ref; ref = ref->next)
    if (ref->type == REF_COMPONENT)
      last = ref;

  if (last && last->u.c.component->ts.type == BT_CLASS)
    return CLASS_DATA (last->u.c.component)->attr.alloc_comp;
  else if (last && last->u.c.component->ts.type == BT_DERIVED)
    return last->u.c.component->ts.u.derived->attr.alloc_comp;
  else if (last)
    return false;

  if (e->ts.type == BT_CLASS)
    return CLASS_DATA (e)->attr.alloc_comp;
  else if (e->ts.type == BT_DERIVED)
    return e->ts.u.derived->attr.alloc_comp;
  else
    return false;
}


/* Check whether the expression has an pointer component.
   Being itself a pointer does not count.  */
bool
gfc_has_ultimate_pointer (gfc_expr *e)
{
  gfc_ref *ref, *last = NULL;

  if (e->expr_type != EXPR_VARIABLE)
    return false;

  for (ref = e->ref; ref; ref = ref->next)
    if (ref->type == REF_COMPONENT)
      last = ref;

  if (last && last->u.c.component->ts.type == BT_CLASS)
    return CLASS_DATA (last->u.c.component)->attr.pointer_comp;
  else if (last && last->u.c.component->ts.type == BT_DERIVED)
    return last->u.c.component->ts.u.derived->attr.pointer_comp;
  else if (last)
    return false;

  if (e->ts.type == BT_CLASS)
    return CLASS_DATA (e)->attr.pointer_comp;
  else if (e->ts.type == BT_DERIVED)
    return e->ts.u.derived->attr.pointer_comp;
  else
    return false;
}


/* Check whether an expression is "simply contiguous", cf. F2008, 6.5.4.
   Note: A scalar is not regarded as "simply contiguous" by the standard.
   if bool is not strict, some further checks are done - for instance,
   a "(::1)" is accepted.  */

bool
gfc_is_simply_contiguous (gfc_expr *expr, bool strict)
{
  bool colon;
  int i;
  gfc_array_ref *ar = NULL;
  gfc_ref *ref, *part_ref = NULL;
  gfc_symbol *sym;

  if (expr->expr_type == EXPR_FUNCTION)
    return expr->value.function.esym
	   ? expr->value.function.esym->result->attr.contiguous : false;
  else if (expr->expr_type != EXPR_VARIABLE)
    return false;

  if (expr->rank == 0)
    return false;

  for (ref = expr->ref; ref; ref = ref->next)
    {
      if (ar)
	return false; /* Array shall be last part-ref. */

      if (ref->type == REF_COMPONENT)
	part_ref  = ref;
      else if (ref->type == REF_SUBSTRING)
	return false;
      else if (ref->u.ar.type != AR_ELEMENT)
	ar = &ref->u.ar;
    }

  sym = expr->symtree->n.sym;
  if (expr->ts.type != BT_CLASS
	&& ((part_ref
		&& !part_ref->u.c.component->attr.contiguous
		&& part_ref->u.c.component->attr.pointer)
	    || (!part_ref
		&& !sym->attr.contiguous
		&& (sym->attr.pointer
		    || sym->as->type == AS_ASSUMED_RANK
		    || sym->as->type == AS_ASSUMED_SHAPE))))
    return false;

  if (!ar || ar->type == AR_FULL)
    return true;

  gcc_assert (ar->type == AR_SECTION);

  /* Check for simply contiguous array */
  colon = true;
  for (i = 0; i < ar->dimen; i++)
    {
      if (ar->dimen_type[i] == DIMEN_VECTOR)
	return false;

      if (ar->dimen_type[i] == DIMEN_ELEMENT)
	{
	  colon = false;
	  continue;
	}

      gcc_assert (ar->dimen_type[i] == DIMEN_RANGE);


      /* If the previous section was not contiguous, that's an error,
	 unless we have effective only one element and checking is not
	 strict.  */
      if (!colon && (strict || !ar->start[i] || !ar->end[i]
		     || ar->start[i]->expr_type != EXPR_CONSTANT
		     || ar->end[i]->expr_type != EXPR_CONSTANT
		     || mpz_cmp (ar->start[i]->value.integer,
				 ar->end[i]->value.integer) != 0))
	return false;

      /* Following the standard, "(::1)" or - if known at compile time -
	 "(lbound:ubound)" are not simply contiguous; if strict
	 is false, they are regarded as simply contiguous.  */
      if (ar->stride[i] && (strict || ar->stride[i]->expr_type != EXPR_CONSTANT
			    || ar->stride[i]->ts.type != BT_INTEGER
			    || mpz_cmp_si (ar->stride[i]->value.integer, 1) != 0))
	return false;

      if (ar->start[i]
	  && (strict || ar->start[i]->expr_type != EXPR_CONSTANT
	      || !ar->as->lower[i]
	      || ar->as->lower[i]->expr_type != EXPR_CONSTANT
	      || mpz_cmp (ar->start[i]->value.integer,
			  ar->as->lower[i]->value.integer) != 0))
	colon = false;

      if (ar->end[i]
	  && (strict || ar->end[i]->expr_type != EXPR_CONSTANT
	      || !ar->as->upper[i]
	      || ar->as->upper[i]->expr_type != EXPR_CONSTANT
	      || mpz_cmp (ar->end[i]->value.integer,
			  ar->as->upper[i]->value.integer) != 0))
	colon = false;
    }

  return true;
}


/* Build call to an intrinsic procedure.  The number of arguments has to be
   passed (rather than ending the list with a NULL value) because we may
   want to add arguments but with a NULL-expression.  */

gfc_expr*
gfc_build_intrinsic_call (gfc_namespace *ns, gfc_isym_id id, const char* name,
			  locus where, unsigned numarg, ...)
{
  gfc_expr* result;
  gfc_actual_arglist* atail;
  gfc_intrinsic_sym* isym;
  va_list ap;
  unsigned i;
  const char *mangled_name = gfc_get_string (GFC_PREFIX ("%s"), name);

  isym = gfc_intrinsic_function_by_id (id);
  gcc_assert (isym);

  result = gfc_get_expr ();
  result->expr_type = EXPR_FUNCTION;
  result->ts = isym->ts;
  result->where = where;
  result->value.function.name = mangled_name;
  result->value.function.isym = isym;

  gfc_get_sym_tree (mangled_name, ns, &result->symtree, false);
  gfc_commit_symbol (result->symtree->n.sym);
  gcc_assert (result->symtree
	      && (result->symtree->n.sym->attr.flavor == FL_PROCEDURE
		  || result->symtree->n.sym->attr.flavor == FL_UNKNOWN));
  result->symtree->n.sym->intmod_sym_id = id;
  result->symtree->n.sym->attr.flavor = FL_PROCEDURE;
  result->symtree->n.sym->attr.intrinsic = 1;
  result->symtree->n.sym->attr.artificial = 1;

  va_start (ap, numarg);
  atail = NULL;
  for (i = 0; i < numarg; ++i)
    {
      if (atail)
	{
	  atail->next = gfc_get_actual_arglist ();
	  atail = atail->next;
	}
      else
	atail = result->value.function.actual = gfc_get_actual_arglist ();

      atail->expr = va_arg (ap, gfc_expr*);
    }
  va_end (ap);

  return result;
}


/* Check if an expression may appear in a variable definition context
   (F2008, 16.6.7) or pointer association context (F2008, 16.6.8).
   This is called from the various places when resolving
   the pieces that make up such a context.
   If own_scope is true (applies to, e.g., ac-implied-do/data-implied-do
   variables), some checks are not performed.

   Optionally, a possible error message can be suppressed if context is NULL
   and just the return status (true / false) be requested.  */

bool
gfc_check_vardef_context (gfc_expr* e, bool pointer, bool alloc_obj,
			  bool own_scope, const char* context)
{
  gfc_symbol* sym = NULL;
  bool is_pointer;
  bool check_intentin;
  bool ptr_component;
  symbol_attribute attr;
  gfc_ref* ref;
  int i;

  if (e->expr_type == EXPR_VARIABLE)
    {
      gcc_assert (e->symtree);
      sym = e->symtree->n.sym;
    }
  else if (e->expr_type == EXPR_FUNCTION)
    {
      gcc_assert (e->symtree);
      sym = e->value.function.esym ? e->value.function.esym : e->symtree->n.sym;
    }

  attr = gfc_expr_attr (e);
  if (!pointer && e->expr_type == EXPR_FUNCTION && attr.pointer)
    {
      if (!(gfc_option.allow_std & GFC_STD_F2008))
	{
	  if (context)
	    gfc_error ("Fortran 2008: Pointer functions in variable definition"
		       " context (%s) at %L", context, &e->where);
	  return false;
	}
    }
  else if (e->expr_type != EXPR_VARIABLE)
    {
      if (context)
	gfc_error ("Non-variable expression in variable definition context (%s)"
		   " at %L", context, &e->where);
      return false;
    }

  if (!pointer && sym->attr.flavor == FL_PARAMETER)
    {
      if (context)
	gfc_error ("Named constant '%s' in variable definition context (%s)"
		   " at %L", sym->name, context, &e->where);
      return false;
    }
  if (!pointer && sym->attr.flavor != FL_VARIABLE
      && !(sym->attr.flavor == FL_PROCEDURE && sym == sym->result)
      && !(sym->attr.flavor == FL_PROCEDURE && sym->attr.proc_pointer))
    {
      if (context)
	gfc_error ("'%s' in variable definition context (%s) at %L is not"
		   " a variable", sym->name, context, &e->where);
      return false;
    }

  /* Find out whether the expr is a pointer; this also means following
     component references to the last one.  */
  is_pointer = (attr.pointer || attr.proc_pointer);
  if (pointer && !is_pointer)
    {
      if (context)
	gfc_error ("Non-POINTER in pointer association context (%s)"
		   " at %L", context, &e->where);
      return false;
    }

  /* F2008, C1303.  */
  if (!alloc_obj
      && (attr.lock_comp
	  || (e->ts.type == BT_DERIVED
	      && e->ts.u.derived->from_intmod == INTMOD_ISO_FORTRAN_ENV
	      && e->ts.u.derived->intmod_sym_id == ISOFORTRAN_LOCK_TYPE)))
    {
      if (context)
	gfc_error ("LOCK_TYPE in variable definition context (%s) at %L",
		   context, &e->where);
      return false;
    }

  /* INTENT(IN) dummy argument.  Check this, unless the object itself is the
     component of sub-component of a pointer; we need to distinguish
     assignment to a pointer component from pointer-assignment to a pointer
     component.  Note that (normal) assignment to procedure pointers is not
     possible.  */
  check_intentin = !own_scope;
  ptr_component = (sym->ts.type == BT_CLASS && CLASS_DATA (sym))
		  ? CLASS_DATA (sym)->attr.class_pointer : sym->attr.pointer;
  for (ref = e->ref; ref && check_intentin; ref = ref->next)
    {
      if (ptr_component && ref->type == REF_COMPONENT)
	check_intentin = false;
      if (ref->type == REF_COMPONENT && ref->u.c.component->attr.pointer)
	{
	  ptr_component = true;
	  if (!pointer)
	    check_intentin = false;
	}
    }
  if (check_intentin && sym->attr.intent == INTENT_IN)
    {
      if (pointer && is_pointer)
	{
	  if (context)
	    gfc_error ("Dummy argument '%s' with INTENT(IN) in pointer"
		       " association context (%s) at %L",
		       sym->name, context, &e->where);
	  return false;
	}
      if (!pointer && !is_pointer && !sym->attr.pointer)
	{
	  if (context)
	    gfc_error ("Dummy argument '%s' with INTENT(IN) in variable"
		       " definition context (%s) at %L",
		       sym->name, context, &e->where);
	  return false;
	}
    }

  /* PROTECTED and use-associated.  */
  if (sym->attr.is_protected && sym->attr.use_assoc && check_intentin)
    {
      if (pointer && is_pointer)
	{
	  if (context)
	    gfc_error ("Variable '%s' is PROTECTED and can not appear in a"
		       " pointer association context (%s) at %L",
		       sym->name, context, &e->where);
	  return false;
	}
      if (!pointer && !is_pointer)
	{
	  if (context)
	    gfc_error ("Variable '%s' is PROTECTED and can not appear in a"
		       " variable definition context (%s) at %L",
		       sym->name, context, &e->where);
	  return false;
	}
    }

  /* Variable not assignable from a PURE procedure but appears in
     variable definition context.  */
  if (!pointer && !own_scope && gfc_pure (NULL) && gfc_impure_variable (sym))
    {
      if (context)
	gfc_error ("Variable '%s' can not appear in a variable definition"
		   " context (%s) at %L in PURE procedure",
		   sym->name, context, &e->where);
      return false;
    }

  if (!pointer && context && gfc_implicit_pure (NULL)
      && gfc_impure_variable (sym))
    {
      gfc_namespace *ns;
      gfc_symbol *sym;

      for (ns = gfc_current_ns; ns; ns = ns->parent)
	{
	  sym = ns->proc_name;
	  if (sym == NULL)
	    break;
	  if (sym->attr.flavor == FL_PROCEDURE)
	    {
	      sym->attr.implicit_pure = 0;
	      break;
	    }
	}
    }
  /* Check variable definition context for associate-names.  */
  if (!pointer && sym->assoc)
    {
      const char* name;
      gfc_association_list* assoc;

      gcc_assert (sym->assoc->target);

      /* If this is a SELECT TYPE temporary (the association is used internally
	 for SELECT TYPE), silently go over to the target.  */
      if (sym->attr.select_type_temporary)
	{
	  gfc_expr* t = sym->assoc->target;

	  gcc_assert (t->expr_type == EXPR_VARIABLE);
	  name = t->symtree->name;

	  if (t->symtree->n.sym->assoc)
	    assoc = t->symtree->n.sym->assoc;
	  else
	    assoc = sym->assoc;
	}
      else
	{
	  name = sym->name;
	  assoc = sym->assoc;
	}
      gcc_assert (name && assoc);

      /* Is association to a valid variable?  */
      if (!assoc->variable)
	{
	  if (context)
	    {
	      if (assoc->target->expr_type == EXPR_VARIABLE)
		gfc_error ("'%s' at %L associated to vector-indexed target can"
			   " not be used in a variable definition context (%s)",
			   name, &e->where, context);
	      else
		gfc_error ("'%s' at %L associated to expression can"
			   " not be used in a variable definition context (%s)",
			   name, &e->where, context);
	    }
	  return false;
	}

      /* Target must be allowed to appear in a variable definition context.  */
      if (!gfc_check_vardef_context (assoc->target, pointer, false, false, NULL))
	{
	  if (context)
	    gfc_error ("Associate-name '%s' can not appear in a variable"
		       " definition context (%s) at %L because its target"
		       " at %L can not, either",
		       name, context, &e->where,
		       &assoc->target->where);
	  return false;
	}
    }

  /* Check for same value in vector expression subscript.  */

  if (e->rank > 0)
    for (ref = e->ref; ref != NULL; ref = ref->next)
      if (ref->type == REF_ARRAY && ref->u.ar.type == AR_SECTION)
	for (i = 0; i < GFC_MAX_DIMENSIONS
	       && ref->u.ar.dimen_type[i] != 0; i++)
	  if (ref->u.ar.dimen_type[i] == DIMEN_VECTOR)
	    {
	      gfc_expr *arr = ref->u.ar.start[i];
	      if (arr->expr_type == EXPR_ARRAY)
		{
		  gfc_constructor *c, *n;
		  gfc_expr *ec, *en;
		  
		  for (c = gfc_constructor_first (arr->value.constructor);
		       c != NULL; c = gfc_constructor_next (c))
		    {
		      if (c == NULL || c->iterator != NULL)
			continue;
		      
		      ec = c->expr;

		      for (n = gfc_constructor_next (c); n != NULL;
			   n = gfc_constructor_next (n))
			{
			  if (n->iterator != NULL)
			    continue;
			  
			  en = n->expr;
			  if (gfc_dep_compare_expr (ec, en) == 0)
			    {
			      gfc_error_now ("Elements with the same value at %L"
					     " and %L in vector subscript"
					     " in a variable definition"
					     " context (%s)", &(ec->where),
					     &(en->where), context);
			      return false;
			    }
			}
		    }
		}
	    }
  
  return true;
}
