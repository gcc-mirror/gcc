/* Check functions
   Copyright (C) 2002, 2003, 2004, 2005 Free Software Foundation, Inc.
   Contributed by Andy Vaught & Katherine Holcomb

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


/* These functions check to see if an argument list is compatible with
   a particular intrinsic function or subroutine.  Presence of
   required arguments has already been established, the argument list
   has been sorted into the right order and has NULL arguments in the
   correct places for missing optional arguments.  */

#include "config.h"
#include "system.h"
#include "flags.h"
#include "gfortran.h"
#include "intrinsic.h"


/* Check the type of an expression.  */

static try
type_check (gfc_expr * e, int n, bt type)
{
  if (e->ts.type == type)
    return SUCCESS;

  gfc_error ("'%s' argument of '%s' intrinsic at %L must be %s",
	     gfc_current_intrinsic_arg[n], gfc_current_intrinsic, &e->where,
	     gfc_basic_typename (type));

  return FAILURE;
}


/* Check that the expression is a numeric type.  */

static try
numeric_check (gfc_expr * e, int n)
{
  if (gfc_numeric_ts (&e->ts))
    return SUCCESS;

  gfc_error ("'%s' argument of '%s' intrinsic at %L must be a numeric type",
	     gfc_current_intrinsic_arg[n], gfc_current_intrinsic, &e->where);

  return FAILURE;
}


/* Check that an expression is integer or real.  */

static try
int_or_real_check (gfc_expr * e, int n)
{
  if (e->ts.type != BT_INTEGER && e->ts.type != BT_REAL)
    {
      gfc_error (
	"'%s' argument of '%s' intrinsic at %L must be INTEGER or REAL",
	gfc_current_intrinsic_arg[n], gfc_current_intrinsic, &e->where);
      return FAILURE;
    }

  return SUCCESS;
}


/* Check that an expression is real or complex.  */

static try
real_or_complex_check (gfc_expr * e, int n)
{
  if (e->ts.type != BT_REAL && e->ts.type != BT_COMPLEX)
    {
      gfc_error (
	"'%s' argument of '%s' intrinsic at %L must be REAL or COMPLEX",
	gfc_current_intrinsic_arg[n], gfc_current_intrinsic, &e->where);
      return FAILURE;
    }

  return SUCCESS;
}


/* Check that the expression is an optional constant integer
   and that it specifies a valid kind for that type.  */

static try
kind_check (gfc_expr * k, int n, bt type)
{
  int kind;

  if (k == NULL)
    return SUCCESS;

  if (type_check (k, n, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (k->expr_type != EXPR_CONSTANT)
    {
      gfc_error (
	"'%s' argument of '%s' intrinsic at %L must be a constant",
	gfc_current_intrinsic_arg[n], gfc_current_intrinsic, &k->where);
      return FAILURE;
    }

  if (gfc_extract_int (k, &kind) != NULL
      || gfc_validate_kind (type, kind, true) < 0)
    {
      gfc_error ("Invalid kind for %s at %L", gfc_basic_typename (type),
		 &k->where);
      return FAILURE;
    }

  return SUCCESS;
}


/* Make sure the expression is a double precision real.  */

static try
double_check (gfc_expr * d, int n)
{
  if (type_check (d, n, BT_REAL) == FAILURE)
    return FAILURE;

  if (d->ts.kind != gfc_default_double_kind)
    {
      gfc_error (
	"'%s' argument of '%s' intrinsic at %L must be double precision",
	gfc_current_intrinsic_arg[n], gfc_current_intrinsic, &d->where);
      return FAILURE;
    }

  return SUCCESS;
}


/* Make sure the expression is a logical array.  */

static try
logical_array_check (gfc_expr * array, int n)
{
  if (array->ts.type != BT_LOGICAL || array->rank == 0)
    {
      gfc_error (
	"'%s' argument of '%s' intrinsic at %L must be a logical array",
	gfc_current_intrinsic_arg[n], gfc_current_intrinsic, &array->where);
      return FAILURE;
    }

  return SUCCESS;
}


/* Make sure an expression is an array.  */

static try
array_check (gfc_expr * e, int n)
{
  if (e->rank != 0)
    return SUCCESS;

  gfc_error ("'%s' argument of '%s' intrinsic at %L must be an array",
	     gfc_current_intrinsic_arg[n], gfc_current_intrinsic, &e->where);

  return FAILURE;
}


/* Make sure an expression is a scalar.  */

static try
scalar_check (gfc_expr * e, int n)
{
  if (e->rank == 0)
    return SUCCESS;

  gfc_error ("'%s' argument of '%s' intrinsic at %L must be a scalar",
	     gfc_current_intrinsic_arg[n], gfc_current_intrinsic, &e->where);

  return FAILURE;
}


/* Make sure two expression have the same type.  */

static try
same_type_check (gfc_expr * e, int n, gfc_expr * f, int m)
{
  if (gfc_compare_types (&e->ts, &f->ts))
    return SUCCESS;

  gfc_error ("'%s' argument of '%s' intrinsic at %L must be the same type "
	     "and kind as '%s'", gfc_current_intrinsic_arg[m],
	     gfc_current_intrinsic, &f->where, gfc_current_intrinsic_arg[n]);
  return FAILURE;
}


/* Make sure that an expression has a certain (nonzero) rank.  */

static try
rank_check (gfc_expr * e, int n, int rank)
{
  if (e->rank == rank)
    return SUCCESS;

  gfc_error ("'%s' argument of '%s' intrinsic at %L must be of rank %d",
	     gfc_current_intrinsic_arg[n], gfc_current_intrinsic,
	     &e->where, rank);
  return FAILURE;
}


/* Make sure a variable expression is not an optional dummy argument.  */

static try
nonoptional_check (gfc_expr * e, int n)
{
  if (e->expr_type == EXPR_VARIABLE && e->symtree->n.sym->attr.optional)
    {
      gfc_error ("'%s' argument of '%s' intrinsic at %L must not be OPTIONAL",
		 gfc_current_intrinsic_arg[n], gfc_current_intrinsic,
		 &e->where);

    }

  /* TODO: Recursive check on nonoptional variables?  */

  return SUCCESS;
}


/* Check that an expression has a particular kind.  */

static try
kind_value_check (gfc_expr * e, int n, int k)
{
  if (e->ts.kind == k)
    return SUCCESS;

  gfc_error ("'%s' argument of '%s' intrinsic at %L must be of kind %d",
	     gfc_current_intrinsic_arg[n], gfc_current_intrinsic,
	     &e->where, k);
  return FAILURE;
}


/* Make sure an expression is a variable.  */

static try
variable_check (gfc_expr * e, int n)
{
  if ((e->expr_type == EXPR_VARIABLE
       && e->symtree->n.sym->attr.flavor != FL_PARAMETER)
      || (e->expr_type == EXPR_FUNCTION
	  && e->symtree->n.sym->result == e->symtree->n.sym))
    return SUCCESS;

  if (e->expr_type == EXPR_VARIABLE
      && e->symtree->n.sym->attr.intent == INTENT_IN)
    {
      gfc_error ("'%s' argument of '%s' intrinsic at %L cannot be INTENT(IN)",
		 gfc_current_intrinsic_arg[n], gfc_current_intrinsic,
		 &e->where);
      return FAILURE;
    }

  gfc_error ("'%s' argument of '%s' intrinsic at %L must be a variable",
	     gfc_current_intrinsic_arg[n], gfc_current_intrinsic, &e->where);

  return FAILURE;
}


/* Check the common DIM parameter for correctness.  */

static try
dim_check (gfc_expr * dim, int n, int optional)
{
  if (optional)
    {
      if (dim == NULL)
	return SUCCESS;

      if (nonoptional_check (dim, n) == FAILURE)
	return FAILURE;

      return SUCCESS;
    }

  if (dim == NULL)
    {
      gfc_error ("Missing DIM parameter in intrinsic '%s' at %L",
		 gfc_current_intrinsic, gfc_current_intrinsic_where);
      return FAILURE;
    }

  if (type_check (dim, n, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (scalar_check (dim, n) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


/* If a DIM parameter is a constant, make sure that it is greater than
   zero and less than or equal to the rank of the given array.  If
   allow_assumed is zero then dim must be less than the rank of the array
   for assumed size arrays.  */

static try
dim_rank_check (gfc_expr * dim, gfc_expr * array, int allow_assumed)
{
  gfc_array_ref *ar;
  int rank;

  if (dim->expr_type != EXPR_CONSTANT || array->expr_type != EXPR_VARIABLE)
    return SUCCESS;

  ar = gfc_find_array_ref (array);
  rank = array->rank;
  if (ar->as->type == AS_ASSUMED_SIZE && !allow_assumed)
    rank--;

  if (mpz_cmp_ui (dim->value.integer, 1) < 0
      || mpz_cmp_ui (dim->value.integer, rank) > 0)
    {
      gfc_error ("'dim' argument of '%s' intrinsic at %L is not a valid "
		 "dimension index", gfc_current_intrinsic, &dim->where);

      return FAILURE;
    }

  return SUCCESS;
}


/***** Check functions *****/

/* Check subroutine suitable for intrinsics taking a real argument and
   a kind argument for the result.  */

static try
check_a_kind (gfc_expr * a, gfc_expr * kind, bt type)
{
  if (type_check (a, 0, BT_REAL) == FAILURE)
    return FAILURE;
  if (kind_check (kind, 1, type) == FAILURE)
    return FAILURE;

  return SUCCESS;
}

/* Check subroutine suitable for ceiling, floor and nint.  */

try
gfc_check_a_ikind (gfc_expr * a, gfc_expr * kind)
{
  return check_a_kind (a, kind, BT_INTEGER);
}

/* Check subroutine suitable for aint, anint.  */

try
gfc_check_a_xkind (gfc_expr * a, gfc_expr * kind)
{
  return check_a_kind (a, kind, BT_REAL);
}

try
gfc_check_abs (gfc_expr * a)
{
  if (numeric_check (a, 0) == FAILURE)
    return FAILURE;

  return SUCCESS;
}

try
gfc_check_achar (gfc_expr * a)
{

  if (type_check (a, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_all_any (gfc_expr * mask, gfc_expr * dim)
{
  if (logical_array_check (mask, 0) == FAILURE)
    return FAILURE;

  if (dim_check (dim, 1, 1) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_allocated (gfc_expr * array)
{
  if (variable_check (array, 0) == FAILURE)
    return FAILURE;

  if (array_check (array, 0) == FAILURE)
    return FAILURE;

  if (!array->symtree->n.sym->attr.allocatable)
    {
      gfc_error ("'%s' argument of '%s' intrinsic at %L must be ALLOCATABLE",
		 gfc_current_intrinsic_arg[0], gfc_current_intrinsic,
		 &array->where);
      return FAILURE;
    }

  return SUCCESS;
}


/* Common check function where the first argument must be real or
   integer and the second argument must be the same as the first.  */

try
gfc_check_a_p (gfc_expr * a, gfc_expr * p)
{
  if (int_or_real_check (a, 0) == FAILURE)
    return FAILURE;

  if (same_type_check (a, 0, p, 1) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_associated (gfc_expr * pointer, gfc_expr * target)
{
  symbol_attribute attr;
  int i;
  try t;

  if (variable_check (pointer, 0) == FAILURE)
    return FAILURE;

  attr = gfc_variable_attr (pointer, NULL);
  if (!attr.pointer)
    {
      gfc_error ("'%s' argument of '%s' intrinsic at %L must be a POINTER",
		 gfc_current_intrinsic_arg[0], gfc_current_intrinsic,
		 &pointer->where);
      return FAILURE;
    }

  if (target == NULL)
    return SUCCESS;

  /* Target argument is optional.  */
  if (target->expr_type == EXPR_NULL)
    {
      gfc_error ("NULL pointer at %L is not permitted as actual argument "
                 "of '%s' intrinsic function",
                 &target->where, gfc_current_intrinsic);
      return FAILURE;
    }

  attr = gfc_variable_attr (target, NULL);
  if (!attr.pointer && !attr.target)
    {
      gfc_error ("'%s' argument of '%s' intrinsic at %L must be a POINTER "
		 "or a TARGET", gfc_current_intrinsic_arg[1],
		 gfc_current_intrinsic, &target->where);
      return FAILURE;
    }

  t = SUCCESS;
  if (same_type_check (pointer, 0, target, 1) == FAILURE)
    t = FAILURE;
  if (rank_check (target, 0, pointer->rank) == FAILURE)
    t = FAILURE;
  if (target->rank > 0)
    {
      for (i = 0; i < target->rank; i++)
        if (target->ref->u.ar.dimen_type[i] == DIMEN_VECTOR)
          {
            gfc_error ("Array section with a vector subscript at %L shall not "
		       "be the target of a pointer",
                       &target->where);
            t = FAILURE;
            break;
          }
    }
  return t;
}


try
gfc_check_atan2 (gfc_expr * y, gfc_expr * x)
{
  if (type_check (y, 0, BT_REAL) == FAILURE)
    return FAILURE;
  if (same_type_check (y, 0, x, 1) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


/* BESJN and BESYN functions.  */

try
gfc_check_besn (gfc_expr * n, gfc_expr * x)
{
  if (scalar_check (n, 0) == FAILURE)
    return FAILURE;

  if (type_check (n, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (scalar_check (x, 1) == FAILURE)
    return FAILURE;

  if (type_check (x, 1, BT_REAL) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_btest (gfc_expr * i, gfc_expr * pos)
{
  if (type_check (i, 0, BT_INTEGER) == FAILURE)
    return FAILURE;
  if (type_check (pos, 1, BT_INTEGER) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_char (gfc_expr * i, gfc_expr * kind)
{
  if (type_check (i, 0, BT_INTEGER) == FAILURE)
    return FAILURE;
  if (kind_check (kind, 1, BT_CHARACTER) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_chdir (gfc_expr * dir)
{
  if (type_check (dir, 0, BT_CHARACTER) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_chdir_sub (gfc_expr * dir, gfc_expr * status)
{
  if (type_check (dir, 0, BT_CHARACTER) == FAILURE)
    return FAILURE;

  if (status == NULL)
    return SUCCESS;

  if (type_check (status, 1, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (scalar_check (status, 1) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_cmplx (gfc_expr * x, gfc_expr * y, gfc_expr * kind)
{
  if (numeric_check (x, 0) == FAILURE)
    return FAILURE;

  if (y != NULL)
    {
      if (numeric_check (y, 1) == FAILURE)
	return FAILURE;

      if (x->ts.type == BT_COMPLEX)
	{
	  gfc_error ("'%s' argument of '%s' intrinsic at %L must not be "
		     "present if 'x' is COMPLEX", gfc_current_intrinsic_arg[1],
		     gfc_current_intrinsic, &y->where);
	  return FAILURE;
	}
    }

  if (kind_check (kind, 2, BT_COMPLEX) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_count (gfc_expr * mask, gfc_expr * dim)
{
  if (logical_array_check (mask, 0) == FAILURE)
    return FAILURE;
  if (dim_check (dim, 1, 1) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_cshift (gfc_expr * array, gfc_expr * shift, gfc_expr * dim)
{
  if (array_check (array, 0) == FAILURE)
    return FAILURE;

  if (array->rank == 1)
    {
      if (scalar_check (shift, 1) == FAILURE)
	return FAILURE;
    }
  else
    {
      /* TODO: more requirements on shift parameter.  */
    }

  if (dim_check (dim, 2, 1) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_ctime (gfc_expr * time)
{
  if (scalar_check (time, 0) == FAILURE)
    return FAILURE;

  if (type_check (time, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_dcmplx (gfc_expr * x, gfc_expr * y)
{
  if (numeric_check (x, 0) == FAILURE)
    return FAILURE;

  if (y != NULL)
    {
      if (numeric_check (y, 1) == FAILURE)
	return FAILURE;

      if (x->ts.type == BT_COMPLEX)
	{
	  gfc_error ("'%s' argument of '%s' intrinsic at %L must not be "
		     "present if 'x' is COMPLEX", gfc_current_intrinsic_arg[1],
		     gfc_current_intrinsic, &y->where);
	  return FAILURE;
	}
    }

  return SUCCESS;
}


try
gfc_check_dble (gfc_expr * x)
{
  if (numeric_check (x, 0) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_digits (gfc_expr * x)
{
  if (int_or_real_check (x, 0) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_dot_product (gfc_expr * vector_a, gfc_expr * vector_b)
{
  switch (vector_a->ts.type)
    {
    case BT_LOGICAL:
      if (type_check (vector_b, 1, BT_LOGICAL) == FAILURE)
	return FAILURE;
      break;

    case BT_INTEGER:
    case BT_REAL:
    case BT_COMPLEX:
      if (numeric_check (vector_b, 1) == FAILURE)
	return FAILURE;
      break;

    default:
      gfc_error ("'%s' argument of '%s' intrinsic at %L must be numeric "
		 "or LOGICAL", gfc_current_intrinsic_arg[0],
		 gfc_current_intrinsic, &vector_a->where);
      return FAILURE;
    }

  if (rank_check (vector_a, 0, 1) == FAILURE)
    return FAILURE;

  if (rank_check (vector_b, 1, 1) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_eoshift (gfc_expr * array, gfc_expr * shift, gfc_expr * boundary,
		   gfc_expr * dim)
{
  if (array_check (array, 0) == FAILURE)
    return FAILURE;

  if (type_check (shift, 1, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (array->rank == 1)
    {
      if (scalar_check (shift, 2) == FAILURE)
	return FAILURE;
    }
  else
    {
      /* TODO: more weird restrictions on shift.  */
    }

  if (boundary != NULL)
    {
      if (same_type_check (array, 0, boundary, 2) == FAILURE)
	return FAILURE;

      /* TODO: more restrictions on boundary.  */
    }

  if (dim_check (dim, 1, 1) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


/* A single complex argument.  */

try
gfc_check_fn_c (gfc_expr * a)
{
  if (type_check (a, 0, BT_COMPLEX) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


/* A single real argument.  */

try
gfc_check_fn_r (gfc_expr * a)
{
  if (type_check (a, 0, BT_REAL) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


/* A single real or complex argument.  */

try
gfc_check_fn_rc (gfc_expr * a)
{
  if (real_or_complex_check (a, 0) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_fnum (gfc_expr * unit)
{
  if (type_check (unit, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (scalar_check (unit, 0) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


/* This is used for the g77 one-argument Bessel functions, and the
   error function.  */

try
gfc_check_g77_math1 (gfc_expr * x)
{
  if (scalar_check (x, 0) == FAILURE)
    return FAILURE;

  if (type_check (x, 0, BT_REAL) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_huge (gfc_expr * x)
{
  if (int_or_real_check (x, 0) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


/* Check that the single argument is an integer.  */

try
gfc_check_i (gfc_expr * i)
{
  if (type_check (i, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_iand (gfc_expr * i, gfc_expr * j)
{
  if (type_check (i, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (type_check (j, 1, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (i->ts.kind != j->ts.kind)
    {
      if (gfc_notify_std (GFC_STD_GNU, "Extension: Different type kinds at %L",
                          &i->where) == FAILURE)
	return FAILURE;
    }

  return SUCCESS;
}


try
gfc_check_ibclr (gfc_expr * i, gfc_expr * pos)
{
  if (type_check (i, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (type_check (pos, 1, BT_INTEGER) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_ibits (gfc_expr * i, gfc_expr * pos, gfc_expr * len)
{
  if (type_check (i, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (type_check (pos, 1, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (type_check (len, 2, BT_INTEGER) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_ibset (gfc_expr * i, gfc_expr * pos)
{
  if (type_check (i, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (type_check (pos, 1, BT_INTEGER) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_ichar_iachar (gfc_expr * c)
{
  int i;

  if (type_check (c, 0, BT_CHARACTER) == FAILURE)
    return FAILURE;

  if (c->expr_type == EXPR_VARIABLE || c->expr_type == EXPR_SUBSTRING)
    {
      gfc_expr *start;
      gfc_expr *end;
      gfc_ref *ref;

      /* Substring references don't have the charlength set.  */
      ref = c->ref;
      while (ref && ref->type != REF_SUBSTRING)
	ref = ref->next;

      gcc_assert (ref == NULL || ref->type == REF_SUBSTRING);

      if (!ref)
	{
          /* Check that the argument is length one.  Non-constant lengths
	     can't be checked here, so assume thay are ok.  */
	  if (c->ts.cl && c->ts.cl->length)
	    {
	      /* If we already have a length for this expression then use it.  */
	      if (c->ts.cl->length->expr_type != EXPR_CONSTANT)
		return SUCCESS;
	      i = mpz_get_si (c->ts.cl->length->value.integer);
	    }
	  else 
	    return SUCCESS;
	}
      else
	{
	  start = ref->u.ss.start;
	  end = ref->u.ss.end;

	  gcc_assert (start);
	  if (end == NULL || end->expr_type != EXPR_CONSTANT
	      || start->expr_type != EXPR_CONSTANT)
	    return SUCCESS;

	  i = mpz_get_si (end->value.integer) + 1
	      - mpz_get_si (start->value.integer);
	}
    }
  else
    return SUCCESS;

  if (i != 1)
    {
      gfc_error ("Argument of %s at %L must be of length one", 
		 gfc_current_intrinsic, &c->where);
      return FAILURE;
    }

  return SUCCESS;
}


try
gfc_check_idnint (gfc_expr * a)
{
  if (double_check (a, 0) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_ieor (gfc_expr * i, gfc_expr * j)
{
  if (type_check (i, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (type_check (j, 1, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (i->ts.kind != j->ts.kind)
    {
      if (gfc_notify_std (GFC_STD_GNU, "Extension: Different type kinds at %L",
                          &i->where) == FAILURE)
	return FAILURE;
    }

  return SUCCESS;
}


try
gfc_check_index (gfc_expr * string, gfc_expr * substring, gfc_expr * back)
{
  if (type_check (string, 0, BT_CHARACTER) == FAILURE
      || type_check (substring, 1, BT_CHARACTER) == FAILURE)
    return FAILURE;


  if (back != NULL && type_check (back, 2, BT_LOGICAL) == FAILURE)
    return FAILURE;

  if (string->ts.kind != substring->ts.kind)
    {
      gfc_error ("'%s' argument of '%s' intrinsic at %L must be the same "
		 "kind as '%s'", gfc_current_intrinsic_arg[1],
		 gfc_current_intrinsic, &substring->where,
		 gfc_current_intrinsic_arg[0]);
      return FAILURE;
    }

  return SUCCESS;
}


try
gfc_check_int (gfc_expr * x, gfc_expr * kind)
{
  if (numeric_check (x, 0) == FAILURE)
    return FAILURE;

  if (kind != NULL)
    {
      if (type_check (kind, 1, BT_INTEGER) == FAILURE)
    return FAILURE;

      if (scalar_check (kind, 1) == FAILURE)
	return FAILURE;
    }

  return SUCCESS;
}


try
gfc_check_ior (gfc_expr * i, gfc_expr * j)
{
  if (type_check (i, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (type_check (j, 1, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (i->ts.kind != j->ts.kind)
    {
      if (gfc_notify_std (GFC_STD_GNU, "Extension: Different type kinds at %L",
                          &i->where) == FAILURE)
    return FAILURE;
    }

  return SUCCESS;
}


try
gfc_check_ishft (gfc_expr * i, gfc_expr * shift)
{
  if (type_check (i, 0, BT_INTEGER) == FAILURE
      || type_check (shift, 1, BT_INTEGER) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_ishftc (gfc_expr * i, gfc_expr * shift, gfc_expr * size)
{
  if (type_check (i, 0, BT_INTEGER) == FAILURE
      || type_check (shift, 1, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (size != NULL && type_check (size, 2, BT_INTEGER) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_kill (gfc_expr * pid, gfc_expr * sig)
{
  if (type_check (pid, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (type_check (sig, 1, BT_INTEGER) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_kill_sub (gfc_expr * pid, gfc_expr * sig, gfc_expr * status)
{
  if (type_check (pid, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (type_check (sig, 1, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (status == NULL)
    return SUCCESS;

  if (type_check (status, 2, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (scalar_check (status, 2) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_kind (gfc_expr * x)
{
  if (x->ts.type == BT_DERIVED)
    {
      gfc_error ("'%s' argument of '%s' intrinsic at %L must be a "
		 "non-derived type", gfc_current_intrinsic_arg[0],
		 gfc_current_intrinsic, &x->where);
      return FAILURE;
    }

  return SUCCESS;
}


try
gfc_check_lbound (gfc_expr * array, gfc_expr * dim)
{
  if (array_check (array, 0) == FAILURE)
    return FAILURE;

  if (dim != NULL)
    {
      if (dim_check (dim, 1, 1) == FAILURE)
	return FAILURE;

      if (dim_rank_check (dim, array, 1) == FAILURE)
	return FAILURE;
    }
  return SUCCESS;
}


try
gfc_check_link (gfc_expr * path1, gfc_expr * path2)
{
  if (type_check (path1, 0, BT_CHARACTER) == FAILURE)
    return FAILURE;

  if (type_check (path2, 1, BT_CHARACTER) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_link_sub (gfc_expr * path1, gfc_expr * path2, gfc_expr * status)
{
  if (type_check (path1, 0, BT_CHARACTER) == FAILURE)
    return FAILURE;

  if (type_check (path2, 1, BT_CHARACTER) == FAILURE)
    return FAILURE;

  if (status == NULL)
    return SUCCESS;

  if (type_check (status, 2, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (scalar_check (status, 2) == FAILURE)
    return FAILURE;

  return SUCCESS;
}

try
gfc_check_loc (gfc_expr *expr)
{
  return variable_check (expr, 0);
}


try
gfc_check_symlnk (gfc_expr * path1, gfc_expr * path2)
{
  if (type_check (path1, 0, BT_CHARACTER) == FAILURE)
    return FAILURE;

  if (type_check (path2, 1, BT_CHARACTER) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_symlnk_sub (gfc_expr * path1, gfc_expr * path2, gfc_expr * status)
{
  if (type_check (path1, 0, BT_CHARACTER) == FAILURE)
    return FAILURE;

  if (type_check (path2, 1, BT_CHARACTER) == FAILURE)
    return FAILURE;

  if (status == NULL)
    return SUCCESS;

  if (type_check (status, 2, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (scalar_check (status, 2) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_logical (gfc_expr * a, gfc_expr * kind)
{
  if (type_check (a, 0, BT_LOGICAL) == FAILURE)
    return FAILURE;
  if (kind_check (kind, 1, BT_LOGICAL) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


/* Min/max family.  */

static try
min_max_args (gfc_actual_arglist * arg)
{
  if (arg == NULL || arg->next == NULL)
    {
      gfc_error ("Intrinsic '%s' at %L must have at least two arguments",
		 gfc_current_intrinsic, gfc_current_intrinsic_where);
      return FAILURE;
    }

  return SUCCESS;
}


static try
check_rest (bt type, int kind, gfc_actual_arglist * arg)
{
  gfc_expr *x;
  int n;

  if (min_max_args (arg) == FAILURE)
    return FAILURE;

  n = 1;

  for (; arg; arg = arg->next, n++)
    {
      x = arg->expr;
      if (x->ts.type != type || x->ts.kind != kind)
	{
          if (x->ts.type == type)
            {
	      if (gfc_notify_std (GFC_STD_GNU,
		    "Extension: Different type kinds at %L", &x->where)
		  == FAILURE)
		return FAILURE;
            }
          else
            {
              gfc_error ("'a%d' argument of '%s' intrinsic at %L must be %s(%d)",
                         n, gfc_current_intrinsic, &x->where,
                         gfc_basic_typename (type), kind);
              return FAILURE;
            }
	}
    }

  return SUCCESS;
}


try
gfc_check_min_max (gfc_actual_arglist * arg)
{
  gfc_expr *x;

  if (min_max_args (arg) == FAILURE)
    return FAILURE;

  x = arg->expr;

  if (x->ts.type != BT_INTEGER && x->ts.type != BT_REAL)
    {
      gfc_error
	("'a1' argument of '%s' intrinsic at %L must be INTEGER or REAL",
	 gfc_current_intrinsic, &x->where);
      return FAILURE;
    }

  return check_rest (x->ts.type, x->ts.kind, arg);
}


try
gfc_check_min_max_integer (gfc_actual_arglist * arg)
{
  return check_rest (BT_INTEGER, gfc_default_integer_kind, arg);
}


try
gfc_check_min_max_real (gfc_actual_arglist * arg)
{
  return check_rest (BT_REAL, gfc_default_real_kind, arg);
}


try
gfc_check_min_max_double (gfc_actual_arglist * arg)
{
  return check_rest (BT_REAL, gfc_default_double_kind, arg);
}

/* End of min/max family.  */

try
gfc_check_malloc (gfc_expr * size)
{
  if (type_check (size, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (scalar_check (size, 0) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_matmul (gfc_expr * matrix_a, gfc_expr * matrix_b)
{
  if ((matrix_a->ts.type != BT_LOGICAL) && !gfc_numeric_ts (&matrix_b->ts))
    {
      gfc_error ("'%s' argument of '%s' intrinsic at %L must be numeric "
		 "or LOGICAL", gfc_current_intrinsic_arg[0],
		 gfc_current_intrinsic, &matrix_a->where);
      return FAILURE;
    }

  if ((matrix_b->ts.type != BT_LOGICAL) && !gfc_numeric_ts (&matrix_a->ts))
    {
      gfc_error ("'%s' argument of '%s' intrinsic at %L must be numeric "
		 "or LOGICAL", gfc_current_intrinsic_arg[1],
		 gfc_current_intrinsic, &matrix_b->where);
      return FAILURE;
    }

  switch (matrix_a->rank)
    {
    case 1:
      if (rank_check (matrix_b, 1, 2) == FAILURE)
	return FAILURE;
      break;

    case 2:
      if (matrix_b->rank == 2)
	break;
      if (rank_check (matrix_b, 1, 1) == FAILURE)
	return FAILURE;
      break;

    default:
      gfc_error ("'%s' argument of '%s' intrinsic at %L must be of rank "
		 "1 or 2", gfc_current_intrinsic_arg[0],
		 gfc_current_intrinsic, &matrix_a->where);
      return FAILURE;
    }

  return SUCCESS;
}


/* Whoever came up with this interface was probably on something.
   The possibilities for the occupation of the second and third
   parameters are:

         Arg #2     Arg #3
         NULL       NULL
         DIM        NULL
         MASK       NULL
         NULL       MASK             minloc(array, mask=m)
         DIM        MASK

   I.e. in the case of minloc(array,mask), mask will be in the second
   position of the argument list and we'll have to fix that up.  */

try
gfc_check_minloc_maxloc (gfc_actual_arglist * ap)
{
  gfc_expr *a, *m, *d;

  a = ap->expr;
  if (int_or_real_check (a, 0) == FAILURE
      || array_check (a, 0) == FAILURE)
    return FAILURE;

  d = ap->next->expr;
  m = ap->next->next->expr;

  if (m == NULL && d != NULL && d->ts.type == BT_LOGICAL
      && ap->next->name == NULL)
    {
      m = d;
      d = NULL;

      ap->next->expr = NULL;
      ap->next->next->expr = m;
    }

  if (d != NULL
      && (scalar_check (d, 1) == FAILURE
      || type_check (d, 1, BT_INTEGER) == FAILURE))
    return FAILURE;

  if (m != NULL && type_check (m, 2, BT_LOGICAL) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


/* Similar to minloc/maxloc, the argument list might need to be
   reordered for the MINVAL, MAXVAL, PRODUCT, and SUM intrinsics.  The
   difference is that MINLOC/MAXLOC take an additional KIND argument.
   The possibilities are:

         Arg #2     Arg #3
         NULL       NULL
         DIM        NULL
         MASK       NULL
         NULL       MASK             minval(array, mask=m)
         DIM        MASK

   I.e. in the case of minval(array,mask), mask will be in the second
   position of the argument list and we'll have to fix that up.  */

static try
check_reduction (gfc_actual_arglist * ap)
{
  gfc_expr *m, *d;

  d = ap->next->expr;
  m = ap->next->next->expr;

  if (m == NULL && d != NULL && d->ts.type == BT_LOGICAL
      && ap->next->name == NULL)
    {
      m = d;
      d = NULL;

      ap->next->expr = NULL;
      ap->next->next->expr = m;
    }

  if (d != NULL
      && (scalar_check (d, 1) == FAILURE
      || type_check (d, 1, BT_INTEGER) == FAILURE))
    return FAILURE;

  if (m != NULL && type_check (m, 2, BT_LOGICAL) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_minval_maxval (gfc_actual_arglist * ap)
{
  if (int_or_real_check (ap->expr, 0) == FAILURE
      || array_check (ap->expr, 0) == FAILURE)
    return FAILURE;

  return check_reduction (ap);
}


try
gfc_check_product_sum (gfc_actual_arglist * ap)
{
  if (numeric_check (ap->expr, 0) == FAILURE
      || array_check (ap->expr, 0) == FAILURE)
    return FAILURE;

  return check_reduction (ap);
}


try
gfc_check_merge (gfc_expr * tsource, gfc_expr * fsource, gfc_expr * mask)
{
  if (same_type_check (tsource, 0, fsource, 1) == FAILURE)
    return FAILURE;

  if (type_check (mask, 2, BT_LOGICAL) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_nearest (gfc_expr * x, gfc_expr * s)
{
  if (type_check (x, 0, BT_REAL) == FAILURE)
    return FAILURE;

  if (type_check (s, 1, BT_REAL) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_null (gfc_expr * mold)
{
  symbol_attribute attr;

  if (mold == NULL)
    return SUCCESS;

  if (variable_check (mold, 0) == FAILURE)
    return FAILURE;

  attr = gfc_variable_attr (mold, NULL);

  if (!attr.pointer)
    {
      gfc_error ("'%s' argument of '%s' intrinsic at %L must be a POINTER",
		 gfc_current_intrinsic_arg[0],
		 gfc_current_intrinsic, &mold->where);
      return FAILURE;
    }

  return SUCCESS;
}


try
gfc_check_pack (gfc_expr * array, gfc_expr * mask, gfc_expr * vector)
{
  if (array_check (array, 0) == FAILURE)
    return FAILURE;

  if (type_check (mask, 1, BT_LOGICAL) == FAILURE)
    return FAILURE;

  if (mask->rank != 0 && mask->rank != array->rank)
    {
      gfc_error ("'%s' argument of '%s' intrinsic at %L must be conformable "
		 "with '%s' argument", gfc_current_intrinsic_arg[0],
		 gfc_current_intrinsic, &array->where,
		 gfc_current_intrinsic_arg[1]);
      return FAILURE;
    }

  if (vector != NULL)
    {
      if (same_type_check (array, 0, vector, 2) == FAILURE)
	return FAILURE;

      if (rank_check (vector, 2, 1) == FAILURE)
	return FAILURE;

      /* TODO: More constraints here.  */
    }

  return SUCCESS;
}


try
gfc_check_precision (gfc_expr * x)
{
  if (x->ts.type != BT_REAL && x->ts.type != BT_COMPLEX)
    {
      gfc_error ("'%s' argument of '%s' intrinsic at %L must be of type "
		 "REAL or COMPLEX", gfc_current_intrinsic_arg[0],
		 gfc_current_intrinsic, &x->where);
      return FAILURE;
    }

  return SUCCESS;
}


try
gfc_check_present (gfc_expr * a)
{
  gfc_symbol *sym;

  if (variable_check (a, 0) == FAILURE)
    return FAILURE;

  sym = a->symtree->n.sym;
  if (!sym->attr.dummy)
    {
      gfc_error ("'%s' argument of '%s' intrinsic at %L must be of a "
		 "dummy variable", gfc_current_intrinsic_arg[0],
		 gfc_current_intrinsic, &a->where);
      return FAILURE;
    }

  if (!sym->attr.optional)
    {
      gfc_error ("'%s' argument of '%s' intrinsic at %L must be of "
		 "an OPTIONAL dummy variable", gfc_current_intrinsic_arg[0],
		 gfc_current_intrinsic, &a->where);
      return FAILURE;
    }

  return SUCCESS;
}


try
gfc_check_radix (gfc_expr * x)
{
  if (int_or_real_check (x, 0) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_range (gfc_expr * x)
{
  if (numeric_check (x, 0) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


/* real, float, sngl.  */
try
gfc_check_real (gfc_expr * a, gfc_expr * kind)
{
  if (numeric_check (a, 0) == FAILURE)
    return FAILURE;

  if (kind_check (kind, 1, BT_REAL) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_rename (gfc_expr * path1, gfc_expr * path2)
{
  if (type_check (path1, 0, BT_CHARACTER) == FAILURE)
    return FAILURE;

  if (type_check (path2, 1, BT_CHARACTER) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_rename_sub (gfc_expr * path1, gfc_expr * path2, gfc_expr * status)
{
  if (type_check (path1, 0, BT_CHARACTER) == FAILURE)
    return FAILURE;

  if (type_check (path2, 1, BT_CHARACTER) == FAILURE)
    return FAILURE;

  if (status == NULL)
    return SUCCESS;

  if (type_check (status, 2, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (scalar_check (status, 2) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_repeat (gfc_expr * x, gfc_expr * y)
{
  if (type_check (x, 0, BT_CHARACTER) == FAILURE)
    return FAILURE;

  if (scalar_check (x, 0) == FAILURE)
    return FAILURE;

  if (type_check (y, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (scalar_check (y, 1) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_reshape (gfc_expr * source, gfc_expr * shape,
		   gfc_expr * pad, gfc_expr * order)
{
  mpz_t size;
  int m;

  if (array_check (source, 0) == FAILURE)
    return FAILURE;

  if (rank_check (shape, 1, 1) == FAILURE)
    return FAILURE;

  if (type_check (shape, 1, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (gfc_array_size (shape, &size) != SUCCESS)
    {
      gfc_error ("'shape' argument of 'reshape' intrinsic at %L must be an "
		 "array of constant size", &shape->where);
      return FAILURE;
    }

  m = mpz_cmp_ui (size, GFC_MAX_DIMENSIONS);
  mpz_clear (size);

  if (m > 0)
    {
      gfc_error ("'shape' argument of 'reshape' intrinsic at %L has more "
		 "than %d elements", &shape->where, GFC_MAX_DIMENSIONS);
      return FAILURE;
    }

  if (pad != NULL)
    {
      if (same_type_check (source, 0, pad, 2) == FAILURE)
	return FAILURE;
      if (array_check (pad, 2) == FAILURE)
	return FAILURE;
    }

  if (order != NULL && array_check (order, 3) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_scale (gfc_expr * x, gfc_expr * i)
{
  if (type_check (x, 0, BT_REAL) == FAILURE)
    return FAILURE;

  if (type_check (i, 1, BT_INTEGER) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_scan (gfc_expr * x, gfc_expr * y, gfc_expr * z)
{
  if (type_check (x, 0, BT_CHARACTER) == FAILURE)
    return FAILURE;

  if (type_check (y, 1, BT_CHARACTER) == FAILURE)
    return FAILURE;

  if (z != NULL && type_check (z, 2, BT_LOGICAL) == FAILURE)
    return FAILURE;

  if (same_type_check (x, 0, y, 1) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_secnds (gfc_expr * r)
{

  if (type_check (r, 0, BT_REAL) == FAILURE)
    return FAILURE;

  if (kind_value_check (r, 0, 4) == FAILURE)
    return FAILURE;

  if (scalar_check (r, 0) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_selected_int_kind (gfc_expr * r)
{

  if (type_check (r, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (scalar_check (r, 0) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_selected_real_kind (gfc_expr * p, gfc_expr * r)
{
  if (p == NULL && r == NULL)
    {
      gfc_error ("Missing arguments to %s intrinsic at %L",
		 gfc_current_intrinsic, gfc_current_intrinsic_where);

      return FAILURE;
    }

  if (p != NULL && type_check (p, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (r != NULL && type_check (r, 1, BT_INTEGER) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_set_exponent (gfc_expr * x, gfc_expr * i)
{
  if (type_check (x, 0, BT_REAL) == FAILURE)
    return FAILURE;

  if (type_check (i, 1, BT_INTEGER) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_shape (gfc_expr * source)
{
  gfc_array_ref *ar;

  if (source->rank == 0 || source->expr_type != EXPR_VARIABLE)
    return SUCCESS;

  ar = gfc_find_array_ref (source);

  if (ar->as && ar->as->type == AS_ASSUMED_SIZE)
    {
      gfc_error ("'source' argument of 'shape' intrinsic at %L must not be "
		 "an assumed size array", &source->where);
      return FAILURE;
    }

  return SUCCESS;
}


try
gfc_check_sign (gfc_expr * a, gfc_expr * b)
{
  if (int_or_real_check (a, 0) == FAILURE)
    return FAILURE;

  if (same_type_check (a, 0, b, 1) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_size (gfc_expr * array, gfc_expr * dim)
{
  if (array_check (array, 0) == FAILURE)
    return FAILURE;

  if (dim != NULL)
    {
      if (type_check (dim, 1, BT_INTEGER) == FAILURE)
	return FAILURE;

      if (kind_value_check (dim, 1, gfc_default_integer_kind) == FAILURE)
	return FAILURE;

      if (dim_rank_check (dim, array, 0) == FAILURE)
	return FAILURE;
    }

  return SUCCESS;
}


try
gfc_check_sleep_sub (gfc_expr * seconds)
{
  if (type_check (seconds, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (scalar_check (seconds, 0) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_spread (gfc_expr * source, gfc_expr * dim, gfc_expr * ncopies)
{
  if (source->rank >= GFC_MAX_DIMENSIONS)
    {
      gfc_error ("'%s' argument of '%s' intrinsic at %L must be less "
		 "than rank %d", gfc_current_intrinsic_arg[0],
		 gfc_current_intrinsic, &source->where, GFC_MAX_DIMENSIONS);

      return FAILURE;
    }

  if (dim_check (dim, 1, 0) == FAILURE)
    return FAILURE;

  if (type_check (ncopies, 2, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (scalar_check (ncopies, 2) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_fstat (gfc_expr * unit, gfc_expr * array)
{
  if (type_check (unit, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (scalar_check (unit, 0) == FAILURE)
    return FAILURE;

  if (type_check (array, 1, BT_INTEGER) == FAILURE
      || kind_value_check (unit, 0, gfc_default_integer_kind) == FAILURE)
    return FAILURE;

  if (array_check (array, 1) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_fstat_sub (gfc_expr * unit, gfc_expr * array, gfc_expr * status)
{
  if (type_check (unit, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (scalar_check (unit, 0) == FAILURE)
    return FAILURE;

  if (type_check (array, 1, BT_INTEGER) == FAILURE
      || kind_value_check (array, 1, gfc_default_integer_kind) == FAILURE)
    return FAILURE;

  if (array_check (array, 1) == FAILURE)
    return FAILURE;

  if (status == NULL)
    return SUCCESS;

  if (type_check (status, 2, BT_INTEGER) == FAILURE
      || kind_value_check (status, 2, gfc_default_integer_kind) == FAILURE)
    return FAILURE;

  if (scalar_check (status, 2) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_stat (gfc_expr * name, gfc_expr * array)
{
  if (type_check (name, 0, BT_CHARACTER) == FAILURE)
    return FAILURE;

  if (type_check (array, 1, BT_INTEGER) == FAILURE
      || kind_value_check (array, 1, gfc_default_integer_kind) == FAILURE)
    return FAILURE;

  if (array_check (array, 1) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_stat_sub (gfc_expr * name, gfc_expr * array, gfc_expr * status)
{
  if (type_check (name, 0, BT_CHARACTER) == FAILURE)
    return FAILURE;

  if (type_check (array, 1, BT_INTEGER) == FAILURE
      || kind_value_check (array, 1, gfc_default_integer_kind) == FAILURE)
    return FAILURE;

  if (array_check (array, 1) == FAILURE)
    return FAILURE;

  if (status == NULL)
    return SUCCESS;

  if (type_check (status, 2, BT_INTEGER) == FAILURE
      || kind_value_check (array, 1, gfc_default_integer_kind) == FAILURE)
    return FAILURE;

  if (scalar_check (status, 2) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_transfer (gfc_expr * source ATTRIBUTE_UNUSED,
		    gfc_expr * mold ATTRIBUTE_UNUSED,
		    gfc_expr * size)
{
  if (size != NULL)
    {
      if (type_check (size, 2, BT_INTEGER) == FAILURE)
	return FAILURE;

      if (scalar_check (size, 2) == FAILURE)
	return FAILURE;

      if (nonoptional_check (size, 2) == FAILURE)
	return FAILURE;
    }

  return SUCCESS;
}


try
gfc_check_transpose (gfc_expr * matrix)
{
  if (rank_check (matrix, 0, 2) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_ubound (gfc_expr * array, gfc_expr * dim)
{
  if (array_check (array, 0) == FAILURE)
    return FAILURE;

  if (dim != NULL)
    {
      if (dim_check (dim, 1, 1) == FAILURE)
	return FAILURE;

      if (dim_rank_check (dim, array, 0) == FAILURE)
	return FAILURE;
    }

  return SUCCESS;
}


try
gfc_check_unpack (gfc_expr * vector, gfc_expr * mask, gfc_expr * field)
{
  if (rank_check (vector, 0, 1) == FAILURE)
    return FAILURE;

  if (array_check (mask, 1) == FAILURE)
    return FAILURE;

  if (type_check (mask, 1, BT_LOGICAL) == FAILURE)
    return FAILURE;

  if (same_type_check (vector, 0, field, 2) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_verify (gfc_expr * x, gfc_expr * y, gfc_expr * z)
{
  if (type_check (x, 0, BT_CHARACTER) == FAILURE)
    return FAILURE;

  if (same_type_check (x, 0, y, 1) == FAILURE)
    return FAILURE;

  if (z != NULL && type_check (z, 2, BT_LOGICAL) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_trim (gfc_expr * x)
{
  if (type_check (x, 0, BT_CHARACTER) == FAILURE)
    return FAILURE;

  if (scalar_check (x, 0) == FAILURE)
    return FAILURE;

   return SUCCESS;
}


try
gfc_check_ttynam (gfc_expr * unit)
{
  if (scalar_check (unit, 0) == FAILURE)
    return FAILURE;

  if (type_check (unit, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


/* Common check function for the half a dozen intrinsics that have a
   single real argument.  */

try
gfc_check_x (gfc_expr * x)
{
  if (type_check (x, 0, BT_REAL) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


/************* Check functions for intrinsic subroutines *************/

try
gfc_check_cpu_time (gfc_expr * time)
{
  if (scalar_check (time, 0) == FAILURE)
    return FAILURE;

  if (type_check (time, 0, BT_REAL) == FAILURE)
    return FAILURE;

  if (variable_check (time, 0) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_date_and_time (gfc_expr * date, gfc_expr * time,
			 gfc_expr * zone, gfc_expr * values)
{
  if (date != NULL)
    {
      if (type_check (date, 0, BT_CHARACTER) == FAILURE)
	return FAILURE;
      if (scalar_check (date, 0) == FAILURE)
	return FAILURE;
      if (variable_check (date, 0) == FAILURE)
	return FAILURE;
    }

  if (time != NULL)
    {
      if (type_check (time, 1, BT_CHARACTER) == FAILURE)
	return FAILURE;
      if (scalar_check (time, 1) == FAILURE)
	return FAILURE;
      if (variable_check (time, 1) == FAILURE)
	return FAILURE;
    }

  if (zone != NULL)
    {
      if (type_check (zone, 2, BT_CHARACTER) == FAILURE)
	return FAILURE;
      if (scalar_check (zone, 2) == FAILURE)
	return FAILURE;
      if (variable_check (zone, 2) == FAILURE)
	return FAILURE;
    }

  if (values != NULL)
    {
      if (type_check (values, 3, BT_INTEGER) == FAILURE)
	return FAILURE;
      if (array_check (values, 3) == FAILURE)
	return FAILURE;
      if (rank_check (values, 3, 1) == FAILURE)
	return FAILURE;
      if (variable_check (values, 3) == FAILURE)
	return FAILURE;
    }

  return SUCCESS;
}


try
gfc_check_mvbits (gfc_expr * from, gfc_expr * frompos, gfc_expr * len,
		  gfc_expr * to, gfc_expr * topos)
{
  if (type_check (from, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (type_check (frompos, 1, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (type_check (len, 2, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (same_type_check (from, 0, to, 3) == FAILURE)
    return FAILURE;

  if (variable_check (to, 3) == FAILURE)
    return FAILURE;

  if (type_check (topos, 4, BT_INTEGER) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_random_number (gfc_expr * harvest)
{
  if (type_check (harvest, 0, BT_REAL) == FAILURE)
    return FAILURE;

  if (variable_check (harvest, 0) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_random_seed (gfc_expr * size, gfc_expr * put, gfc_expr * get)
{
  if (size != NULL)
    {
      if (scalar_check (size, 0) == FAILURE)
	return FAILURE;

      if (type_check (size, 0, BT_INTEGER) == FAILURE)
	return FAILURE;

      if (variable_check (size, 0) == FAILURE)
	return FAILURE;

      if (kind_value_check (size, 0, gfc_default_integer_kind) == FAILURE)
	return FAILURE;
    }

  if (put != NULL)
    {

      if (size != NULL)
        gfc_error ("Too many arguments to %s at %L", gfc_current_intrinsic,
                    &put->where);

      if (array_check (put, 1) == FAILURE)
	return FAILURE;

      if (rank_check (put, 1, 1) == FAILURE)
	return FAILURE;

      if (type_check (put, 1, BT_INTEGER) == FAILURE)
	return FAILURE;

      if (kind_value_check (put, 1, gfc_default_integer_kind) == FAILURE)
	return FAILURE;
    }

  if (get != NULL)
    {

      if (size != NULL || put != NULL)
        gfc_error ("Too many arguments to %s at %L", gfc_current_intrinsic,
                    &get->where);

      if (array_check (get, 2) == FAILURE)
	return FAILURE;

      if (rank_check (get, 2, 1) == FAILURE)
	return FAILURE;

      if (type_check (get, 2, BT_INTEGER) == FAILURE)
	return FAILURE;

      if (variable_check (get, 2) == FAILURE)
	return FAILURE;

      if (kind_value_check (get, 2, gfc_default_integer_kind) == FAILURE)
	return FAILURE;
    }

  return SUCCESS;
}

try
gfc_check_second_sub (gfc_expr * time)
{
  if (scalar_check (time, 0) == FAILURE)
    return FAILURE;

  if (type_check (time, 0, BT_REAL) == FAILURE)
    return FAILURE;

  if (kind_value_check(time, 0, 4) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


/* The arguments of SYSTEM_CLOCK are scalar, integer variables.  Note,
   count, count_rate, and count_max are all optional arguments */

try
gfc_check_system_clock (gfc_expr * count, gfc_expr * count_rate,
                        gfc_expr * count_max)
{
  if (count != NULL)
    {
      if (scalar_check (count, 0) == FAILURE)
        return FAILURE;

      if (type_check (count, 0, BT_INTEGER) == FAILURE)
        return FAILURE;

      if (variable_check (count, 0) == FAILURE)
        return FAILURE;
    }

  if (count_rate != NULL)
    {
      if (scalar_check (count_rate, 1) == FAILURE)
        return FAILURE;

      if (type_check (count_rate, 1, BT_INTEGER) == FAILURE)
        return FAILURE;

      if (variable_check (count_rate, 1) == FAILURE)
        return FAILURE;

      if (count != NULL
	  && same_type_check (count, 0, count_rate, 1) == FAILURE)
        return FAILURE;

    }

  if (count_max != NULL)
    {
      if (scalar_check (count_max, 2) == FAILURE)
        return FAILURE;

      if (type_check (count_max, 2, BT_INTEGER) == FAILURE)
        return FAILURE;

      if (variable_check (count_max, 2) == FAILURE)
        return FAILURE;

      if (count != NULL
	  && same_type_check (count, 0, count_max, 2) == FAILURE)
        return FAILURE;

      if (count_rate != NULL
          && same_type_check (count_rate, 1, count_max, 2) == FAILURE)
        return FAILURE;
    }

  return SUCCESS;
}

try
gfc_check_irand (gfc_expr * x)
{
  if (x == NULL)
    return SUCCESS;

  if (scalar_check (x, 0) == FAILURE)
    return FAILURE;

  if (type_check (x, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (kind_value_check(x, 0, 4) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_alarm_sub (gfc_expr * seconds, gfc_expr * handler, gfc_expr * status)
{
  if (scalar_check (seconds, 0) == FAILURE)
    return FAILURE;

  if (type_check (seconds, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (handler->ts.type != BT_INTEGER && handler->ts.type != BT_PROCEDURE)
    {
      gfc_error (
	"'%s' argument of '%s' intrinsic at %L must be INTEGER or PROCEDURE",
	gfc_current_intrinsic_arg[1], gfc_current_intrinsic, &handler->where);
      return FAILURE;
    }

  if (handler->ts.type == BT_INTEGER && scalar_check (handler, 1) == FAILURE)
    return FAILURE;

  if (status == NULL)
    return SUCCESS;

  if (scalar_check (status, 2) == FAILURE)
    return FAILURE;

  if (type_check (status, 2, BT_INTEGER) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_rand (gfc_expr * x)
{
  if (x == NULL)
    return SUCCESS;

  if (scalar_check (x, 0) == FAILURE)
    return FAILURE;

  if (type_check (x, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (kind_value_check(x, 0, 4) == FAILURE)
    return FAILURE;

  return SUCCESS;
}

try
gfc_check_srand (gfc_expr * x)
{
  if (scalar_check (x, 0) == FAILURE)
    return FAILURE;

  if (type_check (x, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (kind_value_check(x, 0, 4) == FAILURE)
    return FAILURE;

  return SUCCESS;
}

try
gfc_check_ctime_sub (gfc_expr * time, gfc_expr * result)
{
  if (scalar_check (time, 0) == FAILURE)
    return FAILURE;

  if (type_check (time, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (type_check (result, 1, BT_CHARACTER) == FAILURE)
    return FAILURE;

  return SUCCESS;
}

try
gfc_check_etime (gfc_expr * x)
{
  if (array_check (x, 0) == FAILURE)
    return FAILURE;

  if (rank_check (x, 0, 1) == FAILURE)
    return FAILURE;

  if (variable_check (x, 0) == FAILURE)
    return FAILURE;

  if (type_check (x, 0, BT_REAL) == FAILURE)
    return FAILURE;

  if (kind_value_check(x, 0, 4) == FAILURE)
    return FAILURE;

  return SUCCESS;
}

try
gfc_check_etime_sub (gfc_expr * values, gfc_expr * time)
{
  if (array_check (values, 0) == FAILURE)
    return FAILURE;

  if (rank_check (values, 0, 1) == FAILURE)
    return FAILURE;

  if (variable_check (values, 0) == FAILURE)
    return FAILURE;

  if (type_check (values, 0, BT_REAL) == FAILURE)
    return FAILURE;

  if (kind_value_check(values, 0, 4) == FAILURE)
    return FAILURE;

  if (scalar_check (time, 1) == FAILURE)
    return FAILURE;

  if (type_check (time, 1, BT_REAL) == FAILURE)
    return FAILURE;

  if (kind_value_check(time, 1, 4) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_fdate_sub (gfc_expr * date)
{
  if (type_check (date, 0, BT_CHARACTER) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_gerror (gfc_expr * msg)
{
  if (type_check (msg, 0, BT_CHARACTER) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_getcwd_sub (gfc_expr * cwd, gfc_expr * status)
{
  if (type_check (cwd, 0, BT_CHARACTER) == FAILURE)
    return FAILURE;

  if (status == NULL)
    return SUCCESS;

  if (scalar_check (status, 1) == FAILURE)
    return FAILURE;

  if (type_check (status, 1, BT_INTEGER) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_getlog (gfc_expr * msg)
{
  if (type_check (msg, 0, BT_CHARACTER) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_exit (gfc_expr * status)
{
  if (status == NULL)
    return SUCCESS;

  if (type_check (status, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (scalar_check (status, 0) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_flush (gfc_expr * unit)
{
  if (unit == NULL)
    return SUCCESS;

  if (type_check (unit, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (scalar_check (unit, 0) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_free (gfc_expr * i)
{
  if (type_check (i, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (scalar_check (i, 0) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_hostnm (gfc_expr * name)
{
  if (type_check (name, 0, BT_CHARACTER) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_hostnm_sub (gfc_expr * name, gfc_expr * status)
{
  if (type_check (name, 0, BT_CHARACTER) == FAILURE)
    return FAILURE;

  if (status == NULL)
    return SUCCESS;

  if (scalar_check (status, 1) == FAILURE)
    return FAILURE;

  if (type_check (status, 1, BT_INTEGER) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_ttynam_sub (gfc_expr * unit, gfc_expr * name)
{
  if (scalar_check (unit, 0) == FAILURE)
    return FAILURE;

  if (type_check (unit, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (type_check (name, 1, BT_CHARACTER) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_isatty (gfc_expr * unit)
{
  if (unit == NULL)
    return FAILURE;

  if (type_check (unit, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (scalar_check (unit, 0) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_perror (gfc_expr * string)
{
  if (type_check (string, 0, BT_CHARACTER) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_umask (gfc_expr * mask)
{
  if (type_check (mask, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (scalar_check (mask, 0) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_umask_sub (gfc_expr * mask, gfc_expr * old)
{
  if (type_check (mask, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (scalar_check (mask, 0) == FAILURE)
    return FAILURE;

  if (old == NULL)
    return SUCCESS;

  if (scalar_check (old, 1) == FAILURE)
    return FAILURE;

  if (type_check (old, 1, BT_INTEGER) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_unlink (gfc_expr * name)
{
  if (type_check (name, 0, BT_CHARACTER) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_unlink_sub (gfc_expr * name, gfc_expr * status)
{
  if (type_check (name, 0, BT_CHARACTER) == FAILURE)
    return FAILURE;

  if (status == NULL)
    return SUCCESS;

  if (scalar_check (status, 1) == FAILURE)
    return FAILURE;

  if (type_check (status, 1, BT_INTEGER) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_signal (gfc_expr * number, gfc_expr * handler)
{
  if (scalar_check (number, 0) == FAILURE)
    return FAILURE;

  if (type_check (number, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (handler->ts.type != BT_INTEGER && handler->ts.type != BT_PROCEDURE)
    {
      gfc_error (
	"'%s' argument of '%s' intrinsic at %L must be INTEGER or PROCEDURE",
	gfc_current_intrinsic_arg[1], gfc_current_intrinsic, &handler->where);
      return FAILURE;
    }

  if (handler->ts.type == BT_INTEGER && scalar_check (handler, 1) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_signal_sub (gfc_expr * number, gfc_expr * handler, gfc_expr * status)
{
  if (scalar_check (number, 0) == FAILURE)
    return FAILURE;

  if (type_check (number, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (handler->ts.type != BT_INTEGER && handler->ts.type != BT_PROCEDURE)
    {
      gfc_error (
	"'%s' argument of '%s' intrinsic at %L must be INTEGER or PROCEDURE",
	gfc_current_intrinsic_arg[1], gfc_current_intrinsic, &handler->where);
      return FAILURE;
    }

  if (handler->ts.type == BT_INTEGER && scalar_check (handler, 1) == FAILURE)
    return FAILURE;

  if (status == NULL)
    return SUCCESS;

  if (type_check (status, 2, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (scalar_check (status, 2) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try
gfc_check_system_sub (gfc_expr * cmd, gfc_expr * status)
{
  if (type_check (cmd, 0, BT_CHARACTER) == FAILURE)
    return FAILURE;

  if (scalar_check (status, 1) == FAILURE)
    return FAILURE;

  if (type_check (status, 1, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (kind_value_check (status, 1, gfc_default_integer_kind) == FAILURE)
    return FAILURE;

  return SUCCESS;
}
