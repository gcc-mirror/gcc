/* Simplify intrinsic functions at compile-time.
   Copyright (C) 2000-2016 Free Software Foundation, Inc.
   Contributed by Andy Vaught & Katherine Holcomb

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
#include "tm.h"		/* For BITS_PER_UNIT.  */
#include "gfortran.h"
#include "arith.h"
#include "intrinsic.h"
#include "target-memory.h"
#include "constructor.h"
#include "version.h"	/* For version_string.  */


gfc_expr gfc_bad_expr;

static gfc_expr *simplify_size (gfc_expr *, gfc_expr *, int);


/* Note that 'simplification' is not just transforming expressions.
   For functions that are not simplified at compile time, range
   checking is done if possible.

   The return convention is that each simplification function returns:

     A new expression node corresponding to the simplified arguments.
     The original arguments are destroyed by the caller, and must not
     be a part of the new expression.

     NULL pointer indicating that no simplification was possible and
     the original expression should remain intact.

     An expression pointer to gfc_bad_expr (a static placeholder)
     indicating that some error has prevented simplification.  The
     error is generated within the function and should be propagated
     upwards

   By the time a simplification function gets control, it has been
   decided that the function call is really supposed to be the
   intrinsic.  No type checking is strictly necessary, since only
   valid types will be passed on.  On the other hand, a simplification
   subroutine may have to look at the type of an argument as part of
   its processing.

   Array arguments are only passed to these subroutines that implement
   the simplification of transformational intrinsics.

   The functions in this file don't have much comment with them, but
   everything is reasonably straight-forward.  The Standard, chapter 13
   is the best comment you'll find for this file anyway.  */

/* Range checks an expression node.  If all goes well, returns the
   node, otherwise returns &gfc_bad_expr and frees the node.  */

static gfc_expr *
range_check (gfc_expr *result, const char *name)
{
  if (result == NULL)
    return &gfc_bad_expr;

  if (result->expr_type != EXPR_CONSTANT)
    return result;

  switch (gfc_range_check (result))
    {
      case ARITH_OK:
	return result;

      case ARITH_OVERFLOW:
	gfc_error ("Result of %s overflows its kind at %L", name,
		   &result->where);
	break;

      case ARITH_UNDERFLOW:
	gfc_error ("Result of %s underflows its kind at %L", name,
		   &result->where);
	break;

      case ARITH_NAN:
	gfc_error ("Result of %s is NaN at %L", name, &result->where);
	break;

      default:
	gfc_error ("Result of %s gives range error for its kind at %L", name,
		   &result->where);
	break;
    }

  gfc_free_expr (result);
  return &gfc_bad_expr;
}


/* A helper function that gets an optional and possibly missing
   kind parameter.  Returns the kind, -1 if something went wrong.  */

static int
get_kind (bt type, gfc_expr *k, const char *name, int default_kind)
{
  int kind;

  if (k == NULL)
    return default_kind;

  if (k->expr_type != EXPR_CONSTANT)
    {
      gfc_error ("KIND parameter of %s at %L must be an initialization "
		 "expression", name, &k->where);
      return -1;
    }

  if (gfc_extract_int (k, &kind) != NULL
      || gfc_validate_kind (type, kind, true) < 0)
    {
      gfc_error ("Invalid KIND parameter of %s at %L", name, &k->where);
      return -1;
    }

  return kind;
}


/* Converts an mpz_t signed variable into an unsigned one, assuming
   two's complement representations and a binary width of bitsize.
   The conversion is a no-op unless x is negative; otherwise, it can
   be accomplished by masking out the high bits.  */

static void
convert_mpz_to_unsigned (mpz_t x, int bitsize)
{
  mpz_t mask;

  if (mpz_sgn (x) < 0)
    {
      /* Confirm that no bits above the signed range are unset if we
	 are doing range checking.  */
      if (flag_range_check != 0)
	gcc_assert (mpz_scan0 (x, bitsize-1) == ULONG_MAX);

      mpz_init_set_ui (mask, 1);
      mpz_mul_2exp (mask, mask, bitsize);
      mpz_sub_ui (mask, mask, 1);

      mpz_and (x, x, mask);

      mpz_clear (mask);
    }
  else
    {
      /* Confirm that no bits above the signed range are set.  */
      gcc_assert (mpz_scan1 (x, bitsize-1) == ULONG_MAX);
    }
}


/* Converts an mpz_t unsigned variable into a signed one, assuming
   two's complement representations and a binary width of bitsize.
   If the bitsize-1 bit is set, this is taken as a sign bit and
   the number is converted to the corresponding negative number.  */

void
gfc_convert_mpz_to_signed (mpz_t x, int bitsize)
{
  mpz_t mask;

  /* Confirm that no bits above the unsigned range are set if we are
     doing range checking.  */
  if (flag_range_check != 0)
    gcc_assert (mpz_scan1 (x, bitsize) == ULONG_MAX);

  if (mpz_tstbit (x, bitsize - 1) == 1)
    {
      mpz_init_set_ui (mask, 1);
      mpz_mul_2exp (mask, mask, bitsize);
      mpz_sub_ui (mask, mask, 1);

      /* We negate the number by hand, zeroing the high bits, that is
	 make it the corresponding positive number, and then have it
	 negated by GMP, giving the correct representation of the
	 negative number.  */
      mpz_com (x, x);
      mpz_add_ui (x, x, 1);
      mpz_and (x, x, mask);

      mpz_neg (x, x);

      mpz_clear (mask);
    }
}


/* In-place convert BOZ to REAL of the specified kind.  */

static gfc_expr *
convert_boz (gfc_expr *x, int kind)
{
  if (x && x->ts.type == BT_INTEGER && x->is_boz)
    {
      gfc_typespec ts;
      gfc_clear_ts (&ts);
      ts.type = BT_REAL;
      ts.kind = kind;

      if (!gfc_convert_boz (x, &ts))
	return &gfc_bad_expr;
    }

  return x;
}


/* Test that the expression is an constant array.  */

static bool
is_constant_array_expr (gfc_expr *e)
{
  gfc_constructor *c;

  if (e == NULL)
    return true;

  if (e->expr_type != EXPR_ARRAY || !gfc_is_constant_expr (e))
    return false;

  for (c = gfc_constructor_first (e->value.constructor);
       c; c = gfc_constructor_next (c))
    if (c->expr->expr_type != EXPR_CONSTANT
	  && c->expr->expr_type != EXPR_STRUCTURE)
      return false;

  return true;
}


/* Initialize a transformational result expression with a given value.  */

static void
init_result_expr (gfc_expr *e, int init, gfc_expr *array)
{
  if (e && e->expr_type == EXPR_ARRAY)
    {
      gfc_constructor *ctor = gfc_constructor_first (e->value.constructor);
      while (ctor)
	{
	  init_result_expr (ctor->expr, init, array);
	  ctor = gfc_constructor_next (ctor);
	}
    }
  else if (e && e->expr_type == EXPR_CONSTANT)
    {
      int i = gfc_validate_kind (e->ts.type, e->ts.kind, false);
      int length;
      gfc_char_t *string;

      switch (e->ts.type)
	{
	  case BT_LOGICAL:
	    e->value.logical = (init ? 1 : 0);
	    break;

	  case BT_INTEGER:
	    if (init == INT_MIN)
	      mpz_set (e->value.integer, gfc_integer_kinds[i].min_int);
	    else if (init == INT_MAX)
	      mpz_set (e->value.integer, gfc_integer_kinds[i].huge);
	    else
	      mpz_set_si (e->value.integer, init);
	    break;

	  case BT_REAL:
	    if (init == INT_MIN)
	      {
		mpfr_set (e->value.real, gfc_real_kinds[i].huge, GFC_RND_MODE);
		mpfr_neg (e->value.real, e->value.real, GFC_RND_MODE);
	      }
	    else if (init == INT_MAX)
	      mpfr_set (e->value.real, gfc_real_kinds[i].huge, GFC_RND_MODE);
	    else
	      mpfr_set_si (e->value.real, init, GFC_RND_MODE);
	    break;

	  case BT_COMPLEX:
	    mpc_set_si (e->value.complex, init, GFC_MPC_RND_MODE);
	    break;

	  case BT_CHARACTER:
	    if (init == INT_MIN)
	      {
		gfc_expr *len = gfc_simplify_len (array, NULL);
		gfc_extract_int (len, &length);
		string = gfc_get_wide_string (length + 1);
		gfc_wide_memset (string, 0, length);
	      }
	    else if (init == INT_MAX)
	      {
		gfc_expr *len = gfc_simplify_len (array, NULL);
		gfc_extract_int (len, &length);
		string = gfc_get_wide_string (length + 1);
		gfc_wide_memset (string, 255, length);
	      }
	    else
	      {
		length = 0;
		string = gfc_get_wide_string (1);
	      }

	    string[length] = '\0';
	    e->value.character.length = length;
	    e->value.character.string = string;
	    break;

	  default:
	    gcc_unreachable();
	}
    }
  else
    gcc_unreachable();
}


/* Helper function for gfc_simplify_dot_product() and gfc_simplify_matmul;
   if conj_a is true, the matrix_a is complex conjugated.  */

static gfc_expr *
compute_dot_product (gfc_expr *matrix_a, int stride_a, int offset_a,
		     gfc_expr *matrix_b, int stride_b, int offset_b,
		     bool conj_a)
{
  gfc_expr *result, *a, *b, *c;

  result = gfc_get_constant_expr (matrix_a->ts.type, matrix_a->ts.kind,
				  &matrix_a->where);
  init_result_expr (result, 0, NULL);

  a = gfc_constructor_lookup_expr (matrix_a->value.constructor, offset_a);
  b = gfc_constructor_lookup_expr (matrix_b->value.constructor, offset_b);
  while (a && b)
    {
      /* Copying of expressions is required as operands are free'd
	 by the gfc_arith routines.  */
      switch (result->ts.type)
	{
	  case BT_LOGICAL:
	    result = gfc_or (result,
			     gfc_and (gfc_copy_expr (a),
				      gfc_copy_expr (b)));
	    break;

	  case BT_INTEGER:
	  case BT_REAL:
	  case BT_COMPLEX:
	    if (conj_a && a->ts.type == BT_COMPLEX)
	      c = gfc_simplify_conjg (a);
	    else
	      c = gfc_copy_expr (a);
	    result = gfc_add (result, gfc_multiply (c, gfc_copy_expr (b)));
	    break;

	  default:
	    gcc_unreachable();
	}

      offset_a += stride_a;
      a = gfc_constructor_lookup_expr (matrix_a->value.constructor, offset_a);

      offset_b += stride_b;
      b = gfc_constructor_lookup_expr (matrix_b->value.constructor, offset_b);
    }

  return result;
}


/* Build a result expression for transformational intrinsics,
   depending on DIM.  */

static gfc_expr *
transformational_result (gfc_expr *array, gfc_expr *dim, bt type,
			 int kind, locus* where)
{
  gfc_expr *result;
  int i, nelem;

  if (!dim || array->rank == 1)
    return gfc_get_constant_expr (type, kind, where);

  result = gfc_get_array_expr (type, kind, where);
  result->shape = gfc_copy_shape_excluding (array->shape, array->rank, dim);
  result->rank = array->rank - 1;

  /* gfc_array_size() would count the number of elements in the constructor,
     we have not built those yet.  */
  nelem = 1;
  for  (i = 0; i < result->rank; ++i)
    nelem *= mpz_get_ui (result->shape[i]);

  for (i = 0; i < nelem; ++i)
    {
      gfc_constructor_append_expr (&result->value.constructor,
				   gfc_get_constant_expr (type, kind, where),
				   NULL);
    }

  return result;
}


typedef gfc_expr* (*transformational_op)(gfc_expr*, gfc_expr*);

/* Wrapper function, implements 'op1 += 1'. Only called if MASK
   of COUNT intrinsic is .TRUE..

   Interface and implementation mimics arith functions as
   gfc_add, gfc_multiply, etc.  */

static gfc_expr* gfc_count (gfc_expr *op1, gfc_expr *op2)
{
  gfc_expr *result;

  gcc_assert (op1->ts.type == BT_INTEGER);
  gcc_assert (op2->ts.type == BT_LOGICAL);
  gcc_assert (op2->value.logical);

  result = gfc_copy_expr (op1);
  mpz_add_ui (result->value.integer, result->value.integer, 1);

  gfc_free_expr (op1);
  gfc_free_expr (op2);
  return result;
}


/* Transforms an ARRAY with operation OP, according to MASK, to a
   scalar RESULT. E.g. called if

     REAL, PARAMETER :: array(n, m) = ...
     REAL, PARAMETER :: s = SUM(array)

  where OP == gfc_add().  */

static gfc_expr *
simplify_transformation_to_scalar (gfc_expr *result, gfc_expr *array, gfc_expr *mask,
				   transformational_op op)
{
  gfc_expr *a, *m;
  gfc_constructor *array_ctor, *mask_ctor;

  /* Shortcut for constant .FALSE. MASK.  */
  if (mask
      && mask->expr_type == EXPR_CONSTANT
      && !mask->value.logical)
    return result;

  array_ctor = gfc_constructor_first (array->value.constructor);
  mask_ctor = NULL;
  if (mask && mask->expr_type == EXPR_ARRAY)
    mask_ctor = gfc_constructor_first (mask->value.constructor);

  while (array_ctor)
    {
      a = array_ctor->expr;
      array_ctor = gfc_constructor_next (array_ctor);

      /* A constant MASK equals .TRUE. here and can be ignored.  */
      if (mask_ctor)
	{
	  m = mask_ctor->expr;
	  mask_ctor = gfc_constructor_next (mask_ctor);
	  if (!m->value.logical)
	    continue;
	}

      result = op (result, gfc_copy_expr (a));
      if (!result)
	return result;
    }

  return result;
}

/* Transforms an ARRAY with operation OP, according to MASK, to an
   array RESULT. E.g. called if

     REAL, PARAMETER :: array(n, m) = ...
     REAL, PARAMETER :: s(n) = PROD(array, DIM=1)

   where OP == gfc_multiply().
   The result might be post processed using post_op.  */

static gfc_expr *
simplify_transformation_to_array (gfc_expr *result, gfc_expr *array, gfc_expr *dim,
				  gfc_expr *mask, transformational_op op,
				  transformational_op post_op)
{
  mpz_t size;
  int done, i, n, arraysize, resultsize, dim_index, dim_extent, dim_stride;
  gfc_expr **arrayvec, **resultvec, **base, **src, **dest;
  gfc_constructor *array_ctor, *mask_ctor, *result_ctor;

  int count[GFC_MAX_DIMENSIONS], extent[GFC_MAX_DIMENSIONS],
      sstride[GFC_MAX_DIMENSIONS], dstride[GFC_MAX_DIMENSIONS],
      tmpstride[GFC_MAX_DIMENSIONS];

  /* Shortcut for constant .FALSE. MASK.  */
  if (mask
      && mask->expr_type == EXPR_CONSTANT
      && !mask->value.logical)
    return result;

  /* Build an indexed table for array element expressions to minimize
     linked-list traversal. Masked elements are set to NULL.  */
  gfc_array_size (array, &size);
  arraysize = mpz_get_ui (size);
  mpz_clear (size);

  arrayvec = XCNEWVEC (gfc_expr*, arraysize);

  array_ctor = gfc_constructor_first (array->value.constructor);
  mask_ctor = NULL;
  if (mask && mask->expr_type == EXPR_ARRAY)
    mask_ctor = gfc_constructor_first (mask->value.constructor);

  for (i = 0; i < arraysize; ++i)
    {
      arrayvec[i] = array_ctor->expr;
      array_ctor = gfc_constructor_next (array_ctor);

      if (mask_ctor)
	{
	  if (!mask_ctor->expr->value.logical)
	    arrayvec[i] = NULL;

	  mask_ctor = gfc_constructor_next (mask_ctor);
	}
    }

  /* Same for the result expression.  */
  gfc_array_size (result, &size);
  resultsize = mpz_get_ui (size);
  mpz_clear (size);

  resultvec = XCNEWVEC (gfc_expr*, resultsize);
  result_ctor = gfc_constructor_first (result->value.constructor);
  for (i = 0; i < resultsize; ++i)
    {
      resultvec[i] = result_ctor->expr;
      result_ctor = gfc_constructor_next (result_ctor);
    }

  gfc_extract_int (dim, &dim_index);
  dim_index -= 1;               /* zero-base index */
  dim_extent = 0;
  dim_stride = 0;

  for (i = 0, n = 0; i < array->rank; ++i)
    {
      count[i] = 0;
      tmpstride[i] = (i == 0) ? 1 : tmpstride[i-1] * mpz_get_si (array->shape[i-1]);
      if (i == dim_index)
	{
	  dim_extent = mpz_get_si (array->shape[i]);
	  dim_stride = tmpstride[i];
	  continue;
	}

      extent[n] = mpz_get_si (array->shape[i]);
      sstride[n] = tmpstride[i];
      dstride[n] = (n == 0) ? 1 : dstride[n-1] * extent[n-1];
      n += 1;
    }

  done = false;
  base = arrayvec;
  dest = resultvec;
  while (!done)
    {
      for (src = base, n = 0; n < dim_extent; src += dim_stride, ++n)
	if (*src)
	  *dest = op (*dest, gfc_copy_expr (*src));

      count[0]++;
      base += sstride[0];
      dest += dstride[0];

      n = 0;
      while (!done && count[n] == extent[n])
	{
	  count[n] = 0;
	  base -= sstride[n] * extent[n];
	  dest -= dstride[n] * extent[n];

	  n++;
	  if (n < result->rank)
	    {
	      count [n]++;
	      base += sstride[n];
	      dest += dstride[n];
	    }
	  else
	    done = true;
       }
    }

  /* Place updated expression in result constructor.  */
  result_ctor = gfc_constructor_first (result->value.constructor);
  for (i = 0; i < resultsize; ++i)
    {
      if (post_op)
	result_ctor->expr = post_op (result_ctor->expr, resultvec[i]);
      else
	result_ctor->expr = resultvec[i];
      result_ctor = gfc_constructor_next (result_ctor);
    }

  free (arrayvec);
  free (resultvec);
  return result;
}


static gfc_expr *
simplify_transformation (gfc_expr *array, gfc_expr *dim, gfc_expr *mask,
			 int init_val, transformational_op op)
{
  gfc_expr *result;

  if (!is_constant_array_expr (array)
      || !gfc_is_constant_expr (dim))
    return NULL;

  if (mask
      && !is_constant_array_expr (mask)
      && mask->expr_type != EXPR_CONSTANT)
    return NULL;

  result = transformational_result (array, dim, array->ts.type,
				    array->ts.kind, &array->where);
  init_result_expr (result, init_val, NULL);

  return !dim || array->rank == 1 ?
    simplify_transformation_to_scalar (result, array, mask, op) :
    simplify_transformation_to_array (result, array, dim, mask, op, NULL);
}


/********************** Simplification functions *****************************/

gfc_expr *
gfc_simplify_abs (gfc_expr *e)
{
  gfc_expr *result;

  if (e->expr_type != EXPR_CONSTANT)
    return NULL;

  switch (e->ts.type)
    {
      case BT_INTEGER:
	result = gfc_get_constant_expr (BT_INTEGER, e->ts.kind, &e->where);
	mpz_abs (result->value.integer, e->value.integer);
	return range_check (result, "IABS");

      case BT_REAL:
	result = gfc_get_constant_expr (BT_REAL, e->ts.kind, &e->where);
	mpfr_abs (result->value.real, e->value.real, GFC_RND_MODE);
	return range_check (result, "ABS");

      case BT_COMPLEX:
	gfc_set_model_kind (e->ts.kind);
	result = gfc_get_constant_expr (BT_REAL, e->ts.kind, &e->where);
	mpc_abs (result->value.real, e->value.complex, GFC_RND_MODE);
	return range_check (result, "CABS");

      default:
	gfc_internal_error ("gfc_simplify_abs(): Bad type");
    }
}


static gfc_expr *
simplify_achar_char (gfc_expr *e, gfc_expr *k, const char *name, bool ascii)
{
  gfc_expr *result;
  int kind;
  bool too_large = false;

  if (e->expr_type != EXPR_CONSTANT)
    return NULL;

  kind = get_kind (BT_CHARACTER, k, name, gfc_default_character_kind);
  if (kind == -1)
    return &gfc_bad_expr;

  if (mpz_cmp_si (e->value.integer, 0) < 0)
    {
      gfc_error ("Argument of %s function at %L is negative", name,
		 &e->where);
      return &gfc_bad_expr;
    }

  if (ascii && warn_surprising && mpz_cmp_si (e->value.integer, 127) > 0)
    gfc_warning (OPT_Wsurprising,
		 "Argument of %s function at %L outside of range [0,127]",
		 name, &e->where);

  if (kind == 1 && mpz_cmp_si (e->value.integer, 255) > 0)
    too_large = true;
  else if (kind == 4)
    {
      mpz_t t;
      mpz_init_set_ui (t, 2);
      mpz_pow_ui (t, t, 32);
      mpz_sub_ui (t, t, 1);
      if (mpz_cmp (e->value.integer, t) > 0)
	too_large = true;
      mpz_clear (t);
    }

  if (too_large)
    {
      gfc_error ("Argument of %s function at %L is too large for the "
		 "collating sequence of kind %d", name, &e->where, kind);
      return &gfc_bad_expr;
    }

  result = gfc_get_character_expr (kind, &e->where, NULL, 1);
  result->value.character.string[0] = mpz_get_ui (e->value.integer);

  return result;
}



/* We use the processor's collating sequence, because all
   systems that gfortran currently works on are ASCII.  */

gfc_expr *
gfc_simplify_achar (gfc_expr *e, gfc_expr *k)
{
  return simplify_achar_char (e, k, "ACHAR", true);
}


gfc_expr *
gfc_simplify_acos (gfc_expr *x)
{
  gfc_expr *result;

  if (x->expr_type != EXPR_CONSTANT)
    return NULL;

  switch (x->ts.type)
    {
      case BT_REAL:
	if (mpfr_cmp_si (x->value.real, 1) > 0
	    || mpfr_cmp_si (x->value.real, -1) < 0)
	  {
	    gfc_error ("Argument of ACOS at %L must be between -1 and 1",
		       &x->where);
	    return &gfc_bad_expr;
	  }
	result = gfc_get_constant_expr (x->ts.type, x->ts.kind, &x->where);
	mpfr_acos (result->value.real, x->value.real, GFC_RND_MODE);
	break;

      case BT_COMPLEX:
	result = gfc_get_constant_expr (x->ts.type, x->ts.kind, &x->where);
	mpc_acos (result->value.complex, x->value.complex, GFC_MPC_RND_MODE);
	break;

      default:
	gfc_internal_error ("in gfc_simplify_acos(): Bad type");
    }

  return range_check (result, "ACOS");
}

gfc_expr *
gfc_simplify_acosh (gfc_expr *x)
{
  gfc_expr *result;

  if (x->expr_type != EXPR_CONSTANT)
    return NULL;

  switch (x->ts.type)
    {
      case BT_REAL:
	if (mpfr_cmp_si (x->value.real, 1) < 0)
	  {
	    gfc_error ("Argument of ACOSH at %L must not be less than 1",
		       &x->where);
	    return &gfc_bad_expr;
	  }

	result = gfc_get_constant_expr (x->ts.type, x->ts.kind, &x->where);
	mpfr_acosh (result->value.real, x->value.real, GFC_RND_MODE);
	break;

      case BT_COMPLEX:
	result = gfc_get_constant_expr (x->ts.type, x->ts.kind, &x->where);
	mpc_acosh (result->value.complex, x->value.complex, GFC_MPC_RND_MODE);
	break;

      default:
	gfc_internal_error ("in gfc_simplify_acosh(): Bad type");
    }

  return range_check (result, "ACOSH");
}

gfc_expr *
gfc_simplify_adjustl (gfc_expr *e)
{
  gfc_expr *result;
  int count, i, len;
  gfc_char_t ch;

  if (e->expr_type != EXPR_CONSTANT)
    return NULL;

  len = e->value.character.length;

  for (count = 0, i = 0; i < len; ++i)
    {
      ch = e->value.character.string[i];
      if (ch != ' ')
	break;
      ++count;
    }

  result = gfc_get_character_expr (e->ts.kind, &e->where, NULL, len);
  for (i = 0; i < len - count; ++i)
    result->value.character.string[i] = e->value.character.string[count + i];

  return result;
}


gfc_expr *
gfc_simplify_adjustr (gfc_expr *e)
{
  gfc_expr *result;
  int count, i, len;
  gfc_char_t ch;

  if (e->expr_type != EXPR_CONSTANT)
    return NULL;

  len = e->value.character.length;

  for (count = 0, i = len - 1; i >= 0; --i)
    {
      ch = e->value.character.string[i];
      if (ch != ' ')
	break;
      ++count;
    }

  result = gfc_get_character_expr (e->ts.kind, &e->where, NULL, len);
  for (i = 0; i < count; ++i)
    result->value.character.string[i] = ' ';

  for (i = count; i < len; ++i)
    result->value.character.string[i] = e->value.character.string[i - count];

  return result;
}


gfc_expr *
gfc_simplify_aimag (gfc_expr *e)
{
  gfc_expr *result;

  if (e->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_get_constant_expr (BT_REAL, e->ts.kind, &e->where);
  mpfr_set (result->value.real, mpc_imagref (e->value.complex), GFC_RND_MODE);

  return range_check (result, "AIMAG");
}


gfc_expr *
gfc_simplify_aint (gfc_expr *e, gfc_expr *k)
{
  gfc_expr *rtrunc, *result;
  int kind;

  kind = get_kind (BT_REAL, k, "AINT", e->ts.kind);
  if (kind == -1)
    return &gfc_bad_expr;

  if (e->expr_type != EXPR_CONSTANT)
    return NULL;

  rtrunc = gfc_copy_expr (e);
  mpfr_trunc (rtrunc->value.real, e->value.real);

  result = gfc_real2real (rtrunc, kind);

  gfc_free_expr (rtrunc);

  return range_check (result, "AINT");
}


gfc_expr *
gfc_simplify_all (gfc_expr *mask, gfc_expr *dim)
{
  return simplify_transformation (mask, dim, NULL, true, gfc_and);
}


gfc_expr *
gfc_simplify_dint (gfc_expr *e)
{
  gfc_expr *rtrunc, *result;

  if (e->expr_type != EXPR_CONSTANT)
    return NULL;

  rtrunc = gfc_copy_expr (e);
  mpfr_trunc (rtrunc->value.real, e->value.real);

  result = gfc_real2real (rtrunc, gfc_default_double_kind);

  gfc_free_expr (rtrunc);

  return range_check (result, "DINT");
}


gfc_expr *
gfc_simplify_dreal (gfc_expr *e)
{
  gfc_expr *result = NULL;

  if (e->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_get_constant_expr (BT_REAL, e->ts.kind, &e->where);
  mpc_real (result->value.real, e->value.complex, GFC_RND_MODE);

  return range_check (result, "DREAL");
}


gfc_expr *
gfc_simplify_anint (gfc_expr *e, gfc_expr *k)
{
  gfc_expr *result;
  int kind;

  kind = get_kind (BT_REAL, k, "ANINT", e->ts.kind);
  if (kind == -1)
    return &gfc_bad_expr;

  if (e->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_get_constant_expr (e->ts.type, kind, &e->where);
  mpfr_round (result->value.real, e->value.real);

  return range_check (result, "ANINT");
}


gfc_expr *
gfc_simplify_and (gfc_expr *x, gfc_expr *y)
{
  gfc_expr *result;
  int kind;

  if (x->expr_type != EXPR_CONSTANT || y->expr_type != EXPR_CONSTANT)
    return NULL;

  kind = x->ts.kind > y->ts.kind ? x->ts.kind : y->ts.kind;

  switch (x->ts.type)
    {
      case BT_INTEGER:
	result = gfc_get_constant_expr (BT_INTEGER, kind, &x->where);
	mpz_and (result->value.integer, x->value.integer, y->value.integer);
	return range_check (result, "AND");

      case BT_LOGICAL:
	return gfc_get_logical_expr (kind, &x->where,
				     x->value.logical && y->value.logical);

      default:
	gcc_unreachable ();
    }
}


gfc_expr *
gfc_simplify_any (gfc_expr *mask, gfc_expr *dim)
{
  return simplify_transformation (mask, dim, NULL, false, gfc_or);
}


gfc_expr *
gfc_simplify_dnint (gfc_expr *e)
{
  gfc_expr *result;

  if (e->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_get_constant_expr (BT_REAL, gfc_default_double_kind, &e->where);
  mpfr_round (result->value.real, e->value.real);

  return range_check (result, "DNINT");
}


gfc_expr *
gfc_simplify_asin (gfc_expr *x)
{
  gfc_expr *result;

  if (x->expr_type != EXPR_CONSTANT)
    return NULL;

  switch (x->ts.type)
    {
      case BT_REAL:
	if (mpfr_cmp_si (x->value.real, 1) > 0
	    || mpfr_cmp_si (x->value.real, -1) < 0)
	  {
	    gfc_error ("Argument of ASIN at %L must be between -1 and 1",
		       &x->where);
	    return &gfc_bad_expr;
	  }
	result = gfc_get_constant_expr (x->ts.type, x->ts.kind, &x->where);
	mpfr_asin (result->value.real, x->value.real, GFC_RND_MODE);
	break;

      case BT_COMPLEX:
	result = gfc_get_constant_expr (x->ts.type, x->ts.kind, &x->where);
	mpc_asin (result->value.complex, x->value.complex, GFC_MPC_RND_MODE);
	break;

      default:
	gfc_internal_error ("in gfc_simplify_asin(): Bad type");
    }

  return range_check (result, "ASIN");
}


gfc_expr *
gfc_simplify_asinh (gfc_expr *x)
{
  gfc_expr *result;

  if (x->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_get_constant_expr (x->ts.type, x->ts.kind, &x->where);

  switch (x->ts.type)
    {
      case BT_REAL:
	mpfr_asinh (result->value.real, x->value.real, GFC_RND_MODE);
	break;

      case BT_COMPLEX:
	mpc_asinh (result->value.complex, x->value.complex, GFC_MPC_RND_MODE);
	break;

      default:
	gfc_internal_error ("in gfc_simplify_asinh(): Bad type");
    }

  return range_check (result, "ASINH");
}


gfc_expr *
gfc_simplify_atan (gfc_expr *x)
{
  gfc_expr *result;

  if (x->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_get_constant_expr (x->ts.type, x->ts.kind, &x->where);

  switch (x->ts.type)
    {
      case BT_REAL:
	mpfr_atan (result->value.real, x->value.real, GFC_RND_MODE);
	break;

      case BT_COMPLEX:
	mpc_atan (result->value.complex, x->value.complex, GFC_MPC_RND_MODE);
	break;

      default:
	gfc_internal_error ("in gfc_simplify_atan(): Bad type");
    }

  return range_check (result, "ATAN");
}


gfc_expr *
gfc_simplify_atanh (gfc_expr *x)
{
  gfc_expr *result;

  if (x->expr_type != EXPR_CONSTANT)
    return NULL;

  switch (x->ts.type)
    {
      case BT_REAL:
	if (mpfr_cmp_si (x->value.real, 1) >= 0
	    || mpfr_cmp_si (x->value.real, -1) <= 0)
	  {
	    gfc_error ("Argument of ATANH at %L must be inside the range -1 "
		       "to 1", &x->where);
	    return &gfc_bad_expr;
	  }
	result = gfc_get_constant_expr (x->ts.type, x->ts.kind, &x->where);
	mpfr_atanh (result->value.real, x->value.real, GFC_RND_MODE);
	break;

      case BT_COMPLEX:
	result = gfc_get_constant_expr (x->ts.type, x->ts.kind, &x->where);
	mpc_atanh (result->value.complex, x->value.complex, GFC_MPC_RND_MODE);
	break;

      default:
	gfc_internal_error ("in gfc_simplify_atanh(): Bad type");
    }

  return range_check (result, "ATANH");
}


gfc_expr *
gfc_simplify_atan2 (gfc_expr *y, gfc_expr *x)
{
  gfc_expr *result;

  if (x->expr_type != EXPR_CONSTANT || y->expr_type != EXPR_CONSTANT)
    return NULL;

  if (mpfr_zero_p (y->value.real) && mpfr_zero_p (x->value.real))
    {
      gfc_error ("If first argument of ATAN2 %L is zero, then the "
		 "second argument must not be zero", &x->where);
      return &gfc_bad_expr;
    }

  result = gfc_get_constant_expr (x->ts.type, x->ts.kind, &x->where);
  mpfr_atan2 (result->value.real, y->value.real, x->value.real, GFC_RND_MODE);

  return range_check (result, "ATAN2");
}


gfc_expr *
gfc_simplify_bessel_j0 (gfc_expr *x)
{
  gfc_expr *result;

  if (x->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_get_constant_expr (x->ts.type, x->ts.kind, &x->where);
  mpfr_j0 (result->value.real, x->value.real, GFC_RND_MODE);

  return range_check (result, "BESSEL_J0");
}


gfc_expr *
gfc_simplify_bessel_j1 (gfc_expr *x)
{
  gfc_expr *result;

  if (x->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_get_constant_expr (x->ts.type, x->ts.kind, &x->where);
  mpfr_j1 (result->value.real, x->value.real, GFC_RND_MODE);

  return range_check (result, "BESSEL_J1");
}


gfc_expr *
gfc_simplify_bessel_jn (gfc_expr *order, gfc_expr *x)
{
  gfc_expr *result;
  long n;

  if (x->expr_type != EXPR_CONSTANT || order->expr_type != EXPR_CONSTANT)
    return NULL;

  n = mpz_get_si (order->value.integer);
  result = gfc_get_constant_expr (x->ts.type, x->ts.kind, &x->where);
  mpfr_jn (result->value.real, n, x->value.real, GFC_RND_MODE);

  return range_check (result, "BESSEL_JN");
}


/* Simplify transformational form of JN and YN.  */

static gfc_expr *
gfc_simplify_bessel_n2 (gfc_expr *order1, gfc_expr *order2, gfc_expr *x,
			bool jn)
{
  gfc_expr *result;
  gfc_expr *e;
  long n1, n2;
  int i;
  mpfr_t x2rev, last1, last2;

  if (x->expr_type != EXPR_CONSTANT || order1->expr_type != EXPR_CONSTANT
      || order2->expr_type != EXPR_CONSTANT)
    return NULL;

  n1 = mpz_get_si (order1->value.integer);
  n2 = mpz_get_si (order2->value.integer);
  result = gfc_get_array_expr (x->ts.type, x->ts.kind, &x->where);
  result->rank = 1;
  result->shape = gfc_get_shape (1);
  mpz_init_set_ui (result->shape[0], MAX (n2-n1+1, 0));

  if (n2 < n1)
    return result;

  /* Special case: x == 0; it is J0(0.0) == 1, JN(N > 0, 0.0) == 0; and
     YN(N, 0.0) = -Inf.  */

  if (mpfr_cmp_ui (x->value.real, 0.0) == 0)
    {
      if (!jn && flag_range_check)
	{
	  gfc_error ("Result of BESSEL_YN is -INF at %L", &result->where);
 	  gfc_free_expr (result);
	  return &gfc_bad_expr;
	}

      if (jn && n1 == 0)
	{
	  e = gfc_get_constant_expr (x->ts.type, x->ts.kind, &x->where);
	  mpfr_set_ui (e->value.real, 1, GFC_RND_MODE);
	  gfc_constructor_append_expr (&result->value.constructor, e,
				       &x->where);
	  n1++;
	}

      for (i = n1; i <= n2; i++)
	{
	  e = gfc_get_constant_expr (x->ts.type, x->ts.kind, &x->where);
	  if (jn)
	    mpfr_set_ui (e->value.real, 0, GFC_RND_MODE);
	  else
	    mpfr_set_inf (e->value.real, -1);
	  gfc_constructor_append_expr (&result->value.constructor, e,
				       &x->where);
	}

      return result;
    }

  /* Use the faster but more verbose recurrence algorithm. Bessel functions
     are stable for downward recursion and Neumann functions are stable
     for upward recursion. It is
       x2rev = 2.0/x,
       J(N-1, x) = x2rev * N * J(N, x) - J(N+1, x),
       Y(N+1, x) = x2rev * N * Y(N, x) - Y(N-1, x).
     Cf. http://dlmf.nist.gov/10.74#iv and http://dlmf.nist.gov/10.6#E1  */

  gfc_set_model_kind (x->ts.kind);

  /* Get first recursion anchor.  */

  mpfr_init (last1);
  if (jn)
    mpfr_jn (last1, n2, x->value.real, GFC_RND_MODE);
  else
    mpfr_yn (last1, n1, x->value.real, GFC_RND_MODE);

  e = gfc_get_constant_expr (x->ts.type, x->ts.kind, &x->where);
  mpfr_set (e->value.real, last1, GFC_RND_MODE);
  if (range_check (e, jn ? "BESSEL_JN" : "BESSEL_YN") == &gfc_bad_expr)
    {
      mpfr_clear (last1);
      gfc_free_expr (e);
      gfc_free_expr (result);
      return &gfc_bad_expr;
    }
  gfc_constructor_append_expr (&result->value.constructor, e, &x->where);

  if (n1 == n2)
    {
      mpfr_clear (last1);
      return result;
    }

  /* Get second recursion anchor.  */

  mpfr_init (last2);
  if (jn)
    mpfr_jn (last2, n2-1, x->value.real, GFC_RND_MODE);
  else
    mpfr_yn (last2, n1+1, x->value.real, GFC_RND_MODE);

  e = gfc_get_constant_expr (x->ts.type, x->ts.kind, &x->where);
  mpfr_set (e->value.real, last2, GFC_RND_MODE);
  if (range_check (e, jn ? "BESSEL_JN" : "BESSEL_YN") == &gfc_bad_expr)
    {
      mpfr_clear (last1);
      mpfr_clear (last2);
      gfc_free_expr (e);
      gfc_free_expr (result);
      return &gfc_bad_expr;
    }
  if (jn)
    gfc_constructor_insert_expr (&result->value.constructor, e, &x->where, -2);
  else
    gfc_constructor_append_expr (&result->value.constructor, e, &x->where);

  if (n1 + 1 == n2)
    {
      mpfr_clear (last1);
      mpfr_clear (last2);
      return result;
    }

  /* Start actual recursion.  */

  mpfr_init (x2rev);
  mpfr_ui_div (x2rev, 2, x->value.real, GFC_RND_MODE);

  for (i = 2; i <= n2-n1; i++)
    {
      e = gfc_get_constant_expr (x->ts.type, x->ts.kind, &x->where);

      /* Special case: For YN, if the previous N gave -INF, set
	 also N+1 to -INF.  */
      if (!jn && !flag_range_check && mpfr_inf_p (last2))
	{
	  mpfr_set_inf (e->value.real, -1);
	  gfc_constructor_append_expr (&result->value.constructor, e,
				       &x->where);
	  continue;
	}

      mpfr_mul_si (e->value.real, x2rev, jn ? (n2-i+1) : (n1+i-1),
		   GFC_RND_MODE);
      mpfr_mul (e->value.real, e->value.real, last2, GFC_RND_MODE);
      mpfr_sub (e->value.real, e->value.real, last1, GFC_RND_MODE);

      if (range_check (e, jn ? "BESSEL_JN" : "BESSEL_YN") == &gfc_bad_expr)
	{
	  /* Range_check frees "e" in that case.  */
	  e = NULL;
	  goto error;
	}

      if (jn)
	gfc_constructor_insert_expr (&result->value.constructor, e, &x->where,
				     -i-1);
      else
	gfc_constructor_append_expr (&result->value.constructor, e, &x->where);

      mpfr_set (last1, last2, GFC_RND_MODE);
      mpfr_set (last2, e->value.real, GFC_RND_MODE);
    }

  mpfr_clear (last1);
  mpfr_clear (last2);
  mpfr_clear (x2rev);
  return result;

error:
  mpfr_clear (last1);
  mpfr_clear (last2);
  mpfr_clear (x2rev);
  gfc_free_expr (e);
  gfc_free_expr (result);
  return &gfc_bad_expr;
}


gfc_expr *
gfc_simplify_bessel_jn2 (gfc_expr *order1, gfc_expr *order2, gfc_expr *x)
{
  return gfc_simplify_bessel_n2 (order1, order2, x, true);
}


gfc_expr *
gfc_simplify_bessel_y0 (gfc_expr *x)
{
  gfc_expr *result;

  if (x->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_get_constant_expr (x->ts.type, x->ts.kind, &x->where);
  mpfr_y0 (result->value.real, x->value.real, GFC_RND_MODE);

  return range_check (result, "BESSEL_Y0");
}


gfc_expr *
gfc_simplify_bessel_y1 (gfc_expr *x)
{
  gfc_expr *result;

  if (x->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_get_constant_expr (x->ts.type, x->ts.kind, &x->where);
  mpfr_y1 (result->value.real, x->value.real, GFC_RND_MODE);

  return range_check (result, "BESSEL_Y1");
}


gfc_expr *
gfc_simplify_bessel_yn (gfc_expr *order, gfc_expr *x)
{
  gfc_expr *result;
  long n;

  if (x->expr_type != EXPR_CONSTANT || order->expr_type != EXPR_CONSTANT)
    return NULL;

  n = mpz_get_si (order->value.integer);
  result = gfc_get_constant_expr (x->ts.type, x->ts.kind, &x->where);
  mpfr_yn (result->value.real, n, x->value.real, GFC_RND_MODE);

  return range_check (result, "BESSEL_YN");
}


gfc_expr *
gfc_simplify_bessel_yn2 (gfc_expr *order1, gfc_expr *order2, gfc_expr *x)
{
  return gfc_simplify_bessel_n2 (order1, order2, x, false);
}


gfc_expr *
gfc_simplify_bit_size (gfc_expr *e)
{
  int i = gfc_validate_kind (e->ts.type, e->ts.kind, false);
  return gfc_get_int_expr (e->ts.kind, &e->where,
			   gfc_integer_kinds[i].bit_size);
}


gfc_expr *
gfc_simplify_btest (gfc_expr *e, gfc_expr *bit)
{
  int b;

  if (e->expr_type != EXPR_CONSTANT || bit->expr_type != EXPR_CONSTANT)
    return NULL;

  if (gfc_extract_int (bit, &b) != NULL || b < 0)
    return gfc_get_logical_expr (gfc_default_logical_kind, &e->where, false);

  return gfc_get_logical_expr (gfc_default_logical_kind, &e->where,
			       mpz_tstbit (e->value.integer, b));
}


static int
compare_bitwise (gfc_expr *i, gfc_expr *j)
{
  mpz_t x, y;
  int k, res;

  gcc_assert (i->ts.type == BT_INTEGER);
  gcc_assert (j->ts.type == BT_INTEGER);

  mpz_init_set (x, i->value.integer);
  k = gfc_validate_kind (i->ts.type, i->ts.kind, false);
  convert_mpz_to_unsigned (x, gfc_integer_kinds[k].bit_size);

  mpz_init_set (y, j->value.integer);
  k = gfc_validate_kind (j->ts.type, j->ts.kind, false);
  convert_mpz_to_unsigned (y, gfc_integer_kinds[k].bit_size);

  res = mpz_cmp (x, y);
  mpz_clear (x);
  mpz_clear (y);
  return res;
}


gfc_expr *
gfc_simplify_bge (gfc_expr *i, gfc_expr *j)
{
  if (i->expr_type != EXPR_CONSTANT || j->expr_type != EXPR_CONSTANT)
    return NULL;

  return gfc_get_logical_expr (gfc_default_logical_kind, &i->where,
			       compare_bitwise (i, j) >= 0);
}


gfc_expr *
gfc_simplify_bgt (gfc_expr *i, gfc_expr *j)
{
  if (i->expr_type != EXPR_CONSTANT || j->expr_type != EXPR_CONSTANT)
    return NULL;

  return gfc_get_logical_expr (gfc_default_logical_kind, &i->where,
			       compare_bitwise (i, j) > 0);
}


gfc_expr *
gfc_simplify_ble (gfc_expr *i, gfc_expr *j)
{
  if (i->expr_type != EXPR_CONSTANT || j->expr_type != EXPR_CONSTANT)
    return NULL;

  return gfc_get_logical_expr (gfc_default_logical_kind, &i->where,
			       compare_bitwise (i, j) <= 0);
}


gfc_expr *
gfc_simplify_blt (gfc_expr *i, gfc_expr *j)
{
  if (i->expr_type != EXPR_CONSTANT || j->expr_type != EXPR_CONSTANT)
    return NULL;

  return gfc_get_logical_expr (gfc_default_logical_kind, &i->where,
			       compare_bitwise (i, j) < 0);
}


gfc_expr *
gfc_simplify_ceiling (gfc_expr *e, gfc_expr *k)
{
  gfc_expr *ceil, *result;
  int kind;

  kind = get_kind (BT_INTEGER, k, "CEILING", gfc_default_integer_kind);
  if (kind == -1)
    return &gfc_bad_expr;

  if (e->expr_type != EXPR_CONSTANT)
    return NULL;

  ceil = gfc_copy_expr (e);
  mpfr_ceil (ceil->value.real, e->value.real);

  result = gfc_get_constant_expr (BT_INTEGER, kind, &e->where);
  gfc_mpfr_to_mpz (result->value.integer, ceil->value.real, &e->where);

  gfc_free_expr (ceil);

  return range_check (result, "CEILING");
}


gfc_expr *
gfc_simplify_char (gfc_expr *e, gfc_expr *k)
{
  return simplify_achar_char (e, k, "CHAR", false);
}


/* Common subroutine for simplifying CMPLX, COMPLEX and DCMPLX.  */

static gfc_expr *
simplify_cmplx (const char *name, gfc_expr *x, gfc_expr *y, int kind)
{
  gfc_expr *result;

  if (convert_boz (x, kind) == &gfc_bad_expr)
    return &gfc_bad_expr;

  if (convert_boz (y, kind) == &gfc_bad_expr)
    return &gfc_bad_expr;

  if (x->expr_type != EXPR_CONSTANT
      || (y != NULL && y->expr_type != EXPR_CONSTANT))
    return NULL;

  result = gfc_get_constant_expr (BT_COMPLEX, kind, &x->where);

  switch (x->ts.type)
    {
      case BT_INTEGER:
	mpc_set_z (result->value.complex, x->value.integer, GFC_MPC_RND_MODE);
	break;

      case BT_REAL:
	mpc_set_fr (result->value.complex, x->value.real, GFC_RND_MODE);
	break;

      case BT_COMPLEX:
	mpc_set (result->value.complex, x->value.complex, GFC_MPC_RND_MODE);
	break;

      default:
	gfc_internal_error ("gfc_simplify_dcmplx(): Bad type (x)");
    }

  if (!y)
    return range_check (result, name);

  switch (y->ts.type)
    {
      case BT_INTEGER:
	mpfr_set_z (mpc_imagref (result->value.complex),
		    y->value.integer, GFC_RND_MODE);
	break;

      case BT_REAL:
	mpfr_set (mpc_imagref (result->value.complex),
		  y->value.real, GFC_RND_MODE);
	break;

      default:
	gfc_internal_error ("gfc_simplify_dcmplx(): Bad type (y)");
    }

  return range_check (result, name);
}


gfc_expr *
gfc_simplify_cmplx (gfc_expr *x, gfc_expr *y, gfc_expr *k)
{
  int kind;

  kind = get_kind (BT_REAL, k, "CMPLX", gfc_default_complex_kind);
  if (kind == -1)
    return &gfc_bad_expr;

  return simplify_cmplx ("CMPLX", x, y, kind);
}


gfc_expr *
gfc_simplify_complex (gfc_expr *x, gfc_expr *y)
{
  int kind;

  if (x->ts.type == BT_INTEGER && y->ts.type == BT_INTEGER)
    kind = gfc_default_complex_kind;
  else if (x->ts.type == BT_REAL || y->ts.type == BT_INTEGER)
    kind = x->ts.kind;
  else if (x->ts.type == BT_INTEGER || y->ts.type == BT_REAL)
    kind = y->ts.kind;
  else if (x->ts.type == BT_REAL && y->ts.type == BT_REAL)
    kind = (x->ts.kind > y->ts.kind) ? x->ts.kind : y->ts.kind;
  else
    gcc_unreachable ();

  return simplify_cmplx ("COMPLEX", x, y, kind);
}


gfc_expr *
gfc_simplify_conjg (gfc_expr *e)
{
  gfc_expr *result;

  if (e->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_copy_expr (e);
  mpc_conj (result->value.complex, result->value.complex, GFC_MPC_RND_MODE);

  return range_check (result, "CONJG");
}

/* Return the simplification of the constant expression in icall, or NULL
   if the expression is not constant.  */

static gfc_expr *
simplify_trig_call (gfc_expr *icall)
{
  gfc_isym_id func = icall->value.function.isym->id;
  gfc_expr *x = icall->value.function.actual->expr;

  /* The actual simplifiers will return NULL for non-constant x.  */
  switch (func)
  {
    case GFC_ISYM_ACOS:
	return gfc_simplify_acos (x);
    case GFC_ISYM_ASIN:
	return gfc_simplify_asin (x);
    case GFC_ISYM_ATAN:
	return gfc_simplify_atan (x);
    case GFC_ISYM_COS:
	return gfc_simplify_cos (x);
    case GFC_ISYM_COTAN:
	return gfc_simplify_cotan (x);
    case GFC_ISYM_SIN:
	return gfc_simplify_sin (x);
    case GFC_ISYM_TAN:
	return gfc_simplify_tan (x);
    default:
	 break;
  }

  gfc_internal_error ("in simplify_trig_call(): Bad intrinsic");
  return NULL;
}

/* Convert a floating-point number from radians to degrees.  */

static void
degrees_f (mpfr_t x, mp_rnd_t rnd_mode)
{
    mpfr_t tmp;
    mpfr_init (tmp);

    /* Set x = x % 2pi to avoid offsets with large angles.  */
    mpfr_const_pi (tmp, rnd_mode);
    mpfr_mul_ui (tmp, tmp, 2, rnd_mode);
    mpfr_fmod (tmp, x, tmp, rnd_mode);

    /* Set x = x * 180.  */
    mpfr_mul_ui (x, x, 180, rnd_mode);

    /* Set x = x / pi.  */
    mpfr_const_pi (tmp, rnd_mode);
    mpfr_div (x, x, tmp, rnd_mode);

    mpfr_clear (tmp);
}

/* Convert a floating-point number from degrees to radians.  */

static void
radians_f (mpfr_t x, mp_rnd_t rnd_mode)
{
    mpfr_t tmp;
    mpfr_init (tmp);

    /* Set x = x % 360 to avoid offsets with large angles.  */
    mpfr_set_ui (tmp, 360, rnd_mode);
    mpfr_fmod (tmp, x, tmp, rnd_mode);

    /* Set x = x * pi.  */
    mpfr_const_pi (tmp, rnd_mode);
    mpfr_mul (x, x, tmp, rnd_mode);

    /* Set x = x / 180.  */
    mpfr_div_ui (x, x, 180, rnd_mode);

    mpfr_clear (tmp);
}


/* Convert argument to radians before calling a trig function.  */

gfc_expr *
gfc_simplify_trigd (gfc_expr *icall)
{
  gfc_expr *arg;

  arg = icall->value.function.actual->expr;

  if (arg->ts.type != BT_REAL)
    gfc_internal_error ("in gfc_simplify_trigd(): Bad type");

  if (arg->expr_type == EXPR_CONSTANT)
    /* Convert constant to radians before passing off to simplifier.  */
    radians_f (arg->value.real, GFC_RND_MODE);

  /* Let the usual simplifier take over - we just simplified the arg.  */
  return simplify_trig_call (icall);
}

/* Convert result of an inverse trig function to degrees.  */

gfc_expr *
gfc_simplify_atrigd (gfc_expr *icall)
{
  gfc_expr *result;

  if (icall->value.function.actual->expr->ts.type != BT_REAL)
    gfc_internal_error ("in gfc_simplify_atrigd(): Bad type");

  /* See if another simplifier has work to do first.  */
  result = simplify_trig_call (icall);

  if (result && result->expr_type == EXPR_CONSTANT)
  {
      /* Convert constant to degrees after passing off to actual simplifier.  */
      degrees_f (result->value.real, GFC_RND_MODE);
      return result;
  }

  /* Let gfc_resolve_atrigd take care of the non-constant case.  */
  return NULL;
}

/* Convert the result of atan2 to degrees.  */

gfc_expr *
gfc_simplify_atan2d (gfc_expr *y, gfc_expr *x)
{
  gfc_expr *result;

  if (x->ts.type != BT_REAL || y->ts.type != BT_REAL)
    gfc_internal_error ("in gfc_simplify_atan2d(): Bad type");

  if (x->expr_type == EXPR_CONSTANT && y->expr_type == EXPR_CONSTANT)
    {
      result = gfc_simplify_atan2 (y, x);
      if (result != NULL)
	{
	  degrees_f (result->value.real, GFC_RND_MODE);
	  return result;
	}
    }

  /* Let gfc_resolve_atan2d take care of the non-constant case.  */
  return NULL;
}

gfc_expr *
gfc_simplify_cos (gfc_expr *x)
{
  gfc_expr *result;

  if (x->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_get_constant_expr (x->ts.type, x->ts.kind, &x->where);

  switch (x->ts.type)
    {
      case BT_REAL:
	mpfr_cos (result->value.real, x->value.real, GFC_RND_MODE);
	break;

      case BT_COMPLEX:
	gfc_set_model_kind (x->ts.kind);
	mpc_cos (result->value.complex, x->value.complex, GFC_MPC_RND_MODE);
	break;

      default:
	gfc_internal_error ("in gfc_simplify_cos(): Bad type");
    }

  return range_check (result, "COS");
}


gfc_expr *
gfc_simplify_cosh (gfc_expr *x)
{
  gfc_expr *result;

  if (x->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_get_constant_expr (x->ts.type, x->ts.kind, &x->where);

  switch (x->ts.type)
    {
      case BT_REAL:
	mpfr_cosh (result->value.real, x->value.real, GFC_RND_MODE);
	break;

      case BT_COMPLEX:
	mpc_cosh (result->value.complex, x->value.complex, GFC_MPC_RND_MODE);
	break;

      default:
	gcc_unreachable ();
    }

  return range_check (result, "COSH");
}


gfc_expr *
gfc_simplify_count (gfc_expr *mask, gfc_expr *dim, gfc_expr *kind)
{
  gfc_expr *result;

  if (!is_constant_array_expr (mask)
      || !gfc_is_constant_expr (dim)
      || !gfc_is_constant_expr (kind))
    return NULL;

  result = transformational_result (mask, dim,
				    BT_INTEGER,
				    get_kind (BT_INTEGER, kind, "COUNT",
					      gfc_default_integer_kind),
				    &mask->where);

  init_result_expr (result, 0, NULL);

  /* Passing MASK twice, once as data array, once as mask.
     Whenever gfc_count is called, '1' is added to the result.  */
  return !dim || mask->rank == 1 ?
    simplify_transformation_to_scalar (result, mask, mask, gfc_count) :
    simplify_transformation_to_array (result, mask, dim, mask, gfc_count, NULL);
}


gfc_expr *
gfc_simplify_cshift (gfc_expr *array, gfc_expr *shift, gfc_expr *dim)
{
  gfc_expr *a, *result;
  int dm;

  /* DIM is only useful for rank > 1, but deal with it here as one can
     set DIM = 1 for rank = 1.  */
  if (dim)
    {
      if (!gfc_is_constant_expr (dim))
	return NULL;
      dm = mpz_get_si (dim->value.integer);
    }
  else
    dm = 1;

  /* Copy array into 'a', simplify it, and then test for a constant array.  */
  a = gfc_copy_expr (array);
  gfc_simplify_expr (a, 0);
  if (!is_constant_array_expr (a))
    {
      gfc_free_expr (a);
      return NULL;
    }

  if (a->rank == 1)
    {
      gfc_constructor *ca, *cr;
      mpz_t size;
      int i, j, shft, sz;

      if (!gfc_is_constant_expr (shift))
	{
	  gfc_free_expr (a);
	  return NULL;
	}

      shft = mpz_get_si (shift->value.integer);

      /*  Case (i):  If ARRAY has rank one, element i of the result is
	  ARRAY (1 + MODULO (i + SHIFT - 1, SIZE (ARRAY))).  */

      mpz_init (size);
      gfc_array_size (a, &size);
      sz = mpz_get_si (size);
      mpz_clear (size);

      /* Adjust shft to deal with right or left shifts. */
      shft = shft < 0 ? 1 - shft : shft;

      /* Special case: Shift to the original order!  */
      if (sz == 0 || shft % sz == 0)
	return a;

      result = gfc_copy_expr (a);
      cr = gfc_constructor_first (result->value.constructor);
      for (i = 0; i < sz; i++, cr = gfc_constructor_next (cr))
	{
	  j = (i + shft) % sz;
	  ca = gfc_constructor_first (a->value.constructor);
	  while (j-- > 0)
	    ca = gfc_constructor_next (ca);
	  cr->expr = gfc_copy_expr (ca->expr);
	}

      gfc_free_expr (a);
      return result;
    }
  else
    {
      /* FIXME: Deal with rank > 1 arrays.  For now, don't leak memory.  */

      /* GCC bootstrap is too stupid to realize that the above code for dm
	 is correct.  First, dim can be specified for a rank 1 array.  It is
	 not needed in this nor used here.  Second, the code is simply waiting
	 for someone to implement rank > 1 simplification.   For now, add a
	 pessimization to the code that has a zero valid reason to be here.  */
      if (dm > array->rank)
	gcc_unreachable ();

      gfc_free_expr (a);
    }

  return NULL;
}


gfc_expr *
gfc_simplify_dcmplx (gfc_expr *x, gfc_expr *y)
{
  return simplify_cmplx ("DCMPLX", x, y, gfc_default_double_kind);
}


gfc_expr *
gfc_simplify_dble (gfc_expr *e)
{
  gfc_expr *result = NULL;

  if (e->expr_type != EXPR_CONSTANT)
    return NULL;

  if (convert_boz (e, gfc_default_double_kind) == &gfc_bad_expr)
    return &gfc_bad_expr;

  result = gfc_convert_constant (e, BT_REAL, gfc_default_double_kind);
  if (result == &gfc_bad_expr)
    return &gfc_bad_expr;

  return range_check (result, "DBLE");
}


gfc_expr *
gfc_simplify_digits (gfc_expr *x)
{
  int i, digits;

  i = gfc_validate_kind (x->ts.type, x->ts.kind, false);

  switch (x->ts.type)
    {
      case BT_INTEGER:
	digits = gfc_integer_kinds[i].digits;
	break;

      case BT_REAL:
      case BT_COMPLEX:
	digits = gfc_real_kinds[i].digits;
	break;

      default:
	gcc_unreachable ();
    }

  return gfc_get_int_expr (gfc_default_integer_kind, NULL, digits);
}


gfc_expr *
gfc_simplify_dim (gfc_expr *x, gfc_expr *y)
{
  gfc_expr *result;
  int kind;

  if (x->expr_type != EXPR_CONSTANT || y->expr_type != EXPR_CONSTANT)
    return NULL;

  kind = x->ts.kind > y->ts.kind ? x->ts.kind : y->ts.kind;
  result = gfc_get_constant_expr (x->ts.type, kind, &x->where);

  switch (x->ts.type)
    {
      case BT_INTEGER:
	if (mpz_cmp (x->value.integer, y->value.integer) > 0)
	  mpz_sub (result->value.integer, x->value.integer, y->value.integer);
	else
	  mpz_set_ui (result->value.integer, 0);

	break;

      case BT_REAL:
	if (mpfr_cmp (x->value.real, y->value.real) > 0)
	  mpfr_sub (result->value.real, x->value.real, y->value.real,
		    GFC_RND_MODE);
	else
	  mpfr_set_ui (result->value.real, 0, GFC_RND_MODE);

	break;

      default:
	gfc_internal_error ("gfc_simplify_dim(): Bad type");
    }

  return range_check (result, "DIM");
}


gfc_expr*
gfc_simplify_dot_product (gfc_expr *vector_a, gfc_expr *vector_b)
{

  gfc_expr temp;

  if (!is_constant_array_expr (vector_a)
      || !is_constant_array_expr (vector_b))
    return NULL;

  gcc_assert (vector_a->rank == 1);
  gcc_assert (vector_b->rank == 1);

  temp.expr_type = EXPR_OP;
  gfc_clear_ts (&temp.ts);
  temp.value.op.op = INTRINSIC_NONE;
  temp.value.op.op1 = vector_a;
  temp.value.op.op2 = vector_b;
  gfc_type_convert_binary (&temp, 1);

  return compute_dot_product (vector_a, 1, 0, vector_b, 1, 0, true);
}


gfc_expr *
gfc_simplify_dprod (gfc_expr *x, gfc_expr *y)
{
  gfc_expr *a1, *a2, *result;

  if (x->expr_type != EXPR_CONSTANT || y->expr_type != EXPR_CONSTANT)
    return NULL;

  a1 = gfc_real2real (x, gfc_default_double_kind);
  a2 = gfc_real2real (y, gfc_default_double_kind);

  result = gfc_get_constant_expr (BT_REAL, gfc_default_double_kind, &x->where);
  mpfr_mul (result->value.real, a1->value.real, a2->value.real, GFC_RND_MODE);

  gfc_free_expr (a2);
  gfc_free_expr (a1);

  return range_check (result, "DPROD");
}


static gfc_expr *
simplify_dshift (gfc_expr *arg1, gfc_expr *arg2, gfc_expr *shiftarg,
		      bool right)
{
  gfc_expr *result;
  int i, k, size, shift;

  if (arg1->expr_type != EXPR_CONSTANT || arg2->expr_type != EXPR_CONSTANT
      || shiftarg->expr_type != EXPR_CONSTANT)
    return NULL;

  k = gfc_validate_kind (BT_INTEGER, arg1->ts.kind, false);
  size = gfc_integer_kinds[k].bit_size;

  gfc_extract_int (shiftarg, &shift);

  /* DSHIFTR(I,J,SHIFT) = DSHIFTL(I,J,SIZE-SHIFT).  */
  if (right)
    shift = size - shift;

  result = gfc_get_constant_expr (BT_INTEGER, arg1->ts.kind, &arg1->where);
  mpz_set_ui (result->value.integer, 0);

  for (i = 0; i < shift; i++)
    if (mpz_tstbit (arg2->value.integer, size - shift + i))
      mpz_setbit (result->value.integer, i);

  for (i = 0; i < size - shift; i++)
    if (mpz_tstbit (arg1->value.integer, i))
      mpz_setbit (result->value.integer, shift + i);

  /* Convert to a signed value.  */
  gfc_convert_mpz_to_signed (result->value.integer, size);

  return result;
}


gfc_expr *
gfc_simplify_dshiftr (gfc_expr *arg1, gfc_expr *arg2, gfc_expr *shiftarg)
{
  return simplify_dshift (arg1, arg2, shiftarg, true);
}


gfc_expr *
gfc_simplify_dshiftl (gfc_expr *arg1, gfc_expr *arg2, gfc_expr *shiftarg)
{
  return simplify_dshift (arg1, arg2, shiftarg, false);
}


gfc_expr *
gfc_simplify_erf (gfc_expr *x)
{
  gfc_expr *result;

  if (x->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_get_constant_expr (x->ts.type, x->ts.kind, &x->where);
  mpfr_erf (result->value.real, x->value.real, GFC_RND_MODE);

  return range_check (result, "ERF");
}


gfc_expr *
gfc_simplify_erfc (gfc_expr *x)
{
  gfc_expr *result;

  if (x->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_get_constant_expr (x->ts.type, x->ts.kind, &x->where);
  mpfr_erfc (result->value.real, x->value.real, GFC_RND_MODE);

  return range_check (result, "ERFC");
}


/* Helper functions to simplify ERFC_SCALED(x) = ERFC(x) * EXP(X**2).  */

#define MAX_ITER 200
#define ARG_LIMIT 12

/* Calculate ERFC_SCALED directly by its definition:

     ERFC_SCALED(x) = ERFC(x) * EXP(X**2)

   using a large precision for intermediate results.  This is used for all
   but large values of the argument.  */
static void
fullprec_erfc_scaled (mpfr_t res, mpfr_t arg)
{
  mp_prec_t prec;
  mpfr_t a, b;

  prec = mpfr_get_default_prec ();
  mpfr_set_default_prec (10 * prec);

  mpfr_init (a);
  mpfr_init (b);

  mpfr_set (a, arg, GFC_RND_MODE);
  mpfr_sqr (b, a, GFC_RND_MODE);
  mpfr_exp (b, b, GFC_RND_MODE);
  mpfr_erfc (a, a, GFC_RND_MODE);
  mpfr_mul (a, a, b, GFC_RND_MODE);

  mpfr_set (res, a, GFC_RND_MODE);
  mpfr_set_default_prec (prec);

  mpfr_clear (a);
  mpfr_clear (b);
}

/* Calculate ERFC_SCALED using a power series expansion in 1/arg:

    ERFC_SCALED(x) = 1 / (x * sqrt(pi))
                     * (1 + Sum_n (-1)**n * (1 * 3 * 5 * ... * (2n-1))
                                          / (2 * x**2)**n)

  This is used for large values of the argument.  Intermediate calculations
  are performed with twice the precision.  We don't do a fixed number of
  iterations of the sum, but stop when it has converged to the required
  precision.  */
static void
asympt_erfc_scaled (mpfr_t res, mpfr_t arg)
{
  mpfr_t sum, x, u, v, w, oldsum, sumtrunc;
  mpz_t num;
  mp_prec_t prec;
  unsigned i;

  prec = mpfr_get_default_prec ();
  mpfr_set_default_prec (2 * prec);

  mpfr_init (sum);
  mpfr_init (x);
  mpfr_init (u);
  mpfr_init (v);
  mpfr_init (w);
  mpz_init (num);

  mpfr_init (oldsum);
  mpfr_init (sumtrunc);
  mpfr_set_prec (oldsum, prec);
  mpfr_set_prec (sumtrunc, prec);

  mpfr_set (x, arg, GFC_RND_MODE);
  mpfr_set_ui (sum, 1, GFC_RND_MODE);
  mpz_set_ui (num, 1);

  mpfr_set (u, x, GFC_RND_MODE);
  mpfr_sqr (u, u, GFC_RND_MODE);
  mpfr_mul_ui (u, u, 2, GFC_RND_MODE);
  mpfr_pow_si (u, u, -1, GFC_RND_MODE);

  for (i = 1; i < MAX_ITER; i++)
  {
    mpfr_set (oldsum, sum, GFC_RND_MODE);

    mpz_mul_ui (num, num, 2 * i - 1);
    mpz_neg (num, num);

    mpfr_set (w, u, GFC_RND_MODE);
    mpfr_pow_ui (w, w, i, GFC_RND_MODE);

    mpfr_set_z (v, num, GFC_RND_MODE);
    mpfr_mul (v, v, w, GFC_RND_MODE);

    mpfr_add (sum, sum, v, GFC_RND_MODE);

    mpfr_set (sumtrunc, sum, GFC_RND_MODE);
    if (mpfr_cmp (sumtrunc, oldsum) == 0)
      break;
  }

  /* We should have converged by now; otherwise, ARG_LIMIT is probably
     set too low.  */
  gcc_assert (i < MAX_ITER);

  /* Divide by x * sqrt(Pi).  */
  mpfr_const_pi (u, GFC_RND_MODE);
  mpfr_sqrt (u, u, GFC_RND_MODE);
  mpfr_mul (u, u, x, GFC_RND_MODE);
  mpfr_div (sum, sum, u, GFC_RND_MODE);

  mpfr_set (res, sum, GFC_RND_MODE);
  mpfr_set_default_prec (prec);

  mpfr_clears (sum, x, u, v, w, oldsum, sumtrunc, NULL);
  mpz_clear (num);
}


gfc_expr *
gfc_simplify_erfc_scaled (gfc_expr *x)
{
  gfc_expr *result;

  if (x->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_get_constant_expr (x->ts.type, x->ts.kind, &x->where);
  if (mpfr_cmp_d (x->value.real, ARG_LIMIT) >= 0)
    asympt_erfc_scaled (result->value.real, x->value.real);
  else
    fullprec_erfc_scaled (result->value.real, x->value.real);

  return range_check (result, "ERFC_SCALED");
}

#undef MAX_ITER
#undef ARG_LIMIT


gfc_expr *
gfc_simplify_epsilon (gfc_expr *e)
{
  gfc_expr *result;
  int i;

  i = gfc_validate_kind (e->ts.type, e->ts.kind, false);

  result = gfc_get_constant_expr (BT_REAL, e->ts.kind, &e->where);
  mpfr_set (result->value.real, gfc_real_kinds[i].epsilon, GFC_RND_MODE);

  return range_check (result, "EPSILON");
}


gfc_expr *
gfc_simplify_exp (gfc_expr *x)
{
  gfc_expr *result;

  if (x->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_get_constant_expr (x->ts.type, x->ts.kind, &x->where);

  switch (x->ts.type)
    {
      case BT_REAL:
	mpfr_exp (result->value.real, x->value.real, GFC_RND_MODE);
	break;

      case BT_COMPLEX:
	gfc_set_model_kind (x->ts.kind);
	mpc_exp (result->value.complex, x->value.complex, GFC_MPC_RND_MODE);
	break;

      default:
	gfc_internal_error ("in gfc_simplify_exp(): Bad type");
    }

  return range_check (result, "EXP");
}


gfc_expr *
gfc_simplify_exponent (gfc_expr *x)
{
  long int val;
  gfc_expr *result;

  if (x->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_get_constant_expr (BT_INTEGER, gfc_default_integer_kind,
				  &x->where);

  /* EXPONENT(inf) = EXPONENT(nan) = HUGE(0) */
  if (mpfr_inf_p (x->value.real) || mpfr_nan_p (x->value.real))
    {
      int i = gfc_validate_kind (BT_INTEGER, gfc_default_integer_kind, false);
      mpz_set (result->value.integer, gfc_integer_kinds[i].huge);
      return result;
    }

  /* EXPONENT(+/- 0.0) = 0  */
  if (mpfr_zero_p (x->value.real))
    {
      mpz_set_ui (result->value.integer, 0);
      return result;
    }

  gfc_set_model (x->value.real);

  val = (long int) mpfr_get_exp (x->value.real);
  mpz_set_si (result->value.integer, val);

  return range_check (result, "EXPONENT");
}


gfc_expr *
gfc_simplify_float (gfc_expr *a)
{
  gfc_expr *result;

  if (a->expr_type != EXPR_CONSTANT)
    return NULL;

  if (a->is_boz)
    {
      if (convert_boz (a, gfc_default_real_kind) == &gfc_bad_expr)
	return &gfc_bad_expr;

      result = gfc_copy_expr (a);
    }
  else
    result = gfc_int2real (a, gfc_default_real_kind);

  return range_check (result, "FLOAT");
}


static bool
is_last_ref_vtab (gfc_expr *e)
{
  gfc_ref *ref;
  gfc_component *comp = NULL;

  if (e->expr_type != EXPR_VARIABLE)
    return false;

  for (ref = e->ref; ref; ref = ref->next)
    if (ref->type == REF_COMPONENT)
      comp = ref->u.c.component;

  if (!e->ref || !comp)
    return e->symtree->n.sym->attr.vtab;

  if (comp->name[0] == '_' && strcmp (comp->name, "_vptr") == 0)
    return true;

  return false;
}


gfc_expr *
gfc_simplify_extends_type_of (gfc_expr *a, gfc_expr *mold)
{
  /* Avoid simplification of resolved symbols.  */
  if (is_last_ref_vtab (a) || is_last_ref_vtab (mold))
    return NULL;

  if (a->ts.type == BT_DERIVED && mold->ts.type == BT_DERIVED)
    return gfc_get_logical_expr (gfc_default_logical_kind, &a->where,
				 gfc_type_is_extension_of (mold->ts.u.derived,
							   a->ts.u.derived));

  if (UNLIMITED_POLY (a) || UNLIMITED_POLY (mold))
    return NULL;

  /* Return .false. if the dynamic type can never be the same.  */
  if ((a->ts.type == BT_CLASS && mold->ts.type == BT_CLASS
       && !gfc_type_is_extension_of
			(mold->ts.u.derived->components->ts.u.derived,
			 a->ts.u.derived->components->ts.u.derived)
       && !gfc_type_is_extension_of
			(a->ts.u.derived->components->ts.u.derived,
			 mold->ts.u.derived->components->ts.u.derived))
      || (a->ts.type == BT_DERIVED && mold->ts.type == BT_CLASS
	  && !gfc_type_is_extension_of
			(a->ts.u.derived,
			 mold->ts.u.derived->components->ts.u.derived)
	  && !gfc_type_is_extension_of
			(mold->ts.u.derived->components->ts.u.derived,
			 a->ts.u.derived))
      || (a->ts.type == BT_CLASS && mold->ts.type == BT_DERIVED
	  && !gfc_type_is_extension_of
			(mold->ts.u.derived,
			 a->ts.u.derived->components->ts.u.derived)))
    return gfc_get_logical_expr (gfc_default_logical_kind, &a->where, false);

  if (mold->ts.type == BT_DERIVED
      && gfc_type_is_extension_of (mold->ts.u.derived,
				   a->ts.u.derived->components->ts.u.derived))
    return gfc_get_logical_expr (gfc_default_logical_kind, &a->where, true);

  return NULL;
}


gfc_expr *
gfc_simplify_same_type_as (gfc_expr *a, gfc_expr *b)
{
  /* Avoid simplification of resolved symbols.  */
  if (is_last_ref_vtab (a) || is_last_ref_vtab (b))
    return NULL;

  /* Return .false. if the dynamic type can never be the
     same.  */
  if (((a->ts.type == BT_CLASS && gfc_expr_attr (a).class_ok)
       || (b->ts.type == BT_CLASS && gfc_expr_attr (b).class_ok))
      && !gfc_type_compatible (&a->ts, &b->ts)
      && !gfc_type_compatible (&b->ts, &a->ts))
    return gfc_get_logical_expr (gfc_default_logical_kind, &a->where, false);

  if (a->ts.type != BT_DERIVED || b->ts.type != BT_DERIVED)
     return NULL;

  return gfc_get_logical_expr (gfc_default_logical_kind, &a->where,
			       gfc_compare_derived_types (a->ts.u.derived,
							  b->ts.u.derived));
}


gfc_expr *
gfc_simplify_floor (gfc_expr *e, gfc_expr *k)
{
  gfc_expr *result;
  mpfr_t floor;
  int kind;

  kind = get_kind (BT_INTEGER, k, "FLOOR", gfc_default_integer_kind);
  if (kind == -1)
    gfc_internal_error ("gfc_simplify_floor(): Bad kind");

  if (e->expr_type != EXPR_CONSTANT)
    return NULL;

  mpfr_init2 (floor, mpfr_get_prec (e->value.real));
  mpfr_floor (floor, e->value.real);

  result = gfc_get_constant_expr (BT_INTEGER, kind, &e->where);
  gfc_mpfr_to_mpz (result->value.integer, floor, &e->where);

  mpfr_clear (floor);

  return range_check (result, "FLOOR");
}


gfc_expr *
gfc_simplify_fraction (gfc_expr *x)
{
  gfc_expr *result;

#if MPFR_VERSION < MPFR_VERSION_NUM(3,1,0)
  mpfr_t absv, exp, pow2;
#else
  mpfr_exp_t e;
#endif

  if (x->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_get_constant_expr (BT_REAL, x->ts.kind, &x->where);

  /* FRACTION(inf) = NaN.  */
  if (mpfr_inf_p (x->value.real))
    {
      mpfr_set_nan (result->value.real);
      return result;
    }

#if MPFR_VERSION < MPFR_VERSION_NUM(3,1,0)

  /* MPFR versions before 3.1.0 do not include mpfr_frexp.  
     TODO: remove the kludge when MPFR 3.1.0 or newer will be required */

  if (mpfr_sgn (x->value.real) == 0)
    {
      mpfr_set (result->value.real, x->value.real, GFC_RND_MODE);
      return result;
    }

  gfc_set_model_kind (x->ts.kind);
  mpfr_init (exp);
  mpfr_init (absv);
  mpfr_init (pow2);

  mpfr_abs (absv, x->value.real, GFC_RND_MODE);
  mpfr_log2 (exp, absv, GFC_RND_MODE);

  mpfr_trunc (exp, exp);
  mpfr_add_ui (exp, exp, 1, GFC_RND_MODE);

  mpfr_ui_pow (pow2, 2, exp, GFC_RND_MODE);

  mpfr_div (result->value.real, x->value.real, pow2, GFC_RND_MODE);

  mpfr_clears (exp, absv, pow2, NULL);

#else

  /* mpfr_frexp() correctly handles zeros and NaNs.  */
  mpfr_frexp (&e, result->value.real, x->value.real, GFC_RND_MODE);

#endif

  return range_check (result, "FRACTION");
}


gfc_expr *
gfc_simplify_gamma (gfc_expr *x)
{
  gfc_expr *result;

  if (x->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_get_constant_expr (x->ts.type, x->ts.kind, &x->where);
  mpfr_gamma (result->value.real, x->value.real, GFC_RND_MODE);

  return range_check (result, "GAMMA");
}


gfc_expr *
gfc_simplify_huge (gfc_expr *e)
{
  gfc_expr *result;
  int i;

  i = gfc_validate_kind (e->ts.type, e->ts.kind, false);
  result = gfc_get_constant_expr (e->ts.type, e->ts.kind, &e->where);

  switch (e->ts.type)
    {
      case BT_INTEGER:
	mpz_set (result->value.integer, gfc_integer_kinds[i].huge);
	break;

      case BT_REAL:
	mpfr_set (result->value.real, gfc_real_kinds[i].huge, GFC_RND_MODE);
	break;

      default:
	gcc_unreachable ();
    }

  return result;
}


gfc_expr *
gfc_simplify_hypot (gfc_expr *x, gfc_expr *y)
{
  gfc_expr *result;

  if (x->expr_type != EXPR_CONSTANT || y->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_get_constant_expr (x->ts.type, x->ts.kind, &x->where);
  mpfr_hypot (result->value.real, x->value.real, y->value.real, GFC_RND_MODE);
  return range_check (result, "HYPOT");
}


/* We use the processor's collating sequence, because all
   systems that gfortran currently works on are ASCII.  */

gfc_expr *
gfc_simplify_iachar (gfc_expr *e, gfc_expr *kind)
{
  gfc_expr *result;
  gfc_char_t index;
  int k;

  if (e->expr_type != EXPR_CONSTANT)
    return NULL;

  if (e->value.character.length != 1)
    {
      gfc_error ("Argument of IACHAR at %L must be of length one", &e->where);
      return &gfc_bad_expr;
    }

  index = e->value.character.string[0];

  if (warn_surprising && index > 127)
    gfc_warning (OPT_Wsurprising,
		 "Argument of IACHAR function at %L outside of range 0..127",
		 &e->where);

  k = get_kind (BT_INTEGER, kind, "IACHAR", gfc_default_integer_kind);
  if (k == -1)
    return &gfc_bad_expr;

  result = gfc_get_int_expr (k, &e->where, index);

  return range_check (result, "IACHAR");
}


static gfc_expr *
do_bit_and (gfc_expr *result, gfc_expr *e)
{
  gcc_assert (e->ts.type == BT_INTEGER && e->expr_type == EXPR_CONSTANT);
  gcc_assert (result->ts.type == BT_INTEGER
	      && result->expr_type == EXPR_CONSTANT);

  mpz_and (result->value.integer, result->value.integer, e->value.integer);
  return result;
}


gfc_expr *
gfc_simplify_iall (gfc_expr *array, gfc_expr *dim, gfc_expr *mask)
{
  return simplify_transformation (array, dim, mask, -1, do_bit_and);
}


static gfc_expr *
do_bit_ior (gfc_expr *result, gfc_expr *e)
{
  gcc_assert (e->ts.type == BT_INTEGER && e->expr_type == EXPR_CONSTANT);
  gcc_assert (result->ts.type == BT_INTEGER
	      && result->expr_type == EXPR_CONSTANT);

  mpz_ior (result->value.integer, result->value.integer, e->value.integer);
  return result;
}


gfc_expr *
gfc_simplify_iany (gfc_expr *array, gfc_expr *dim, gfc_expr *mask)
{
  return simplify_transformation (array, dim, mask, 0, do_bit_ior);
}


gfc_expr *
gfc_simplify_iand (gfc_expr *x, gfc_expr *y)
{
  gfc_expr *result;

  if (x->expr_type != EXPR_CONSTANT || y->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_get_constant_expr (BT_INTEGER, x->ts.kind, &x->where);
  mpz_and (result->value.integer, x->value.integer, y->value.integer);

  return range_check (result, "IAND");
}


gfc_expr *
gfc_simplify_ibclr (gfc_expr *x, gfc_expr *y)
{
  gfc_expr *result;
  int k, pos;

  if (x->expr_type != EXPR_CONSTANT || y->expr_type != EXPR_CONSTANT)
    return NULL;

  gfc_extract_int (y, &pos);

  k = gfc_validate_kind (x->ts.type, x->ts.kind, false);

  result = gfc_copy_expr (x);

  convert_mpz_to_unsigned (result->value.integer,
			   gfc_integer_kinds[k].bit_size);

  mpz_clrbit (result->value.integer, pos);

  gfc_convert_mpz_to_signed (result->value.integer,
			 gfc_integer_kinds[k].bit_size);

  return result;
}


gfc_expr *
gfc_simplify_ibits (gfc_expr *x, gfc_expr *y, gfc_expr *z)
{
  gfc_expr *result;
  int pos, len;
  int i, k, bitsize;
  int *bits;

  if (x->expr_type != EXPR_CONSTANT
      || y->expr_type != EXPR_CONSTANT
      || z->expr_type != EXPR_CONSTANT)
    return NULL;

  gfc_extract_int (y, &pos);
  gfc_extract_int (z, &len);

  k = gfc_validate_kind (BT_INTEGER, x->ts.kind, false);

  bitsize = gfc_integer_kinds[k].bit_size;

  if (pos + len > bitsize)
    {
      gfc_error ("Sum of second and third arguments of IBITS exceeds "
		 "bit size at %L", &y->where);
      return &gfc_bad_expr;
    }

  result = gfc_get_constant_expr (x->ts.type, x->ts.kind, &x->where);
  convert_mpz_to_unsigned (result->value.integer,
			   gfc_integer_kinds[k].bit_size);

  bits = XCNEWVEC (int, bitsize);

  for (i = 0; i < bitsize; i++)
    bits[i] = 0;

  for (i = 0; i < len; i++)
    bits[i] = mpz_tstbit (x->value.integer, i + pos);

  for (i = 0; i < bitsize; i++)
    {
      if (bits[i] == 0)
	mpz_clrbit (result->value.integer, i);
      else if (bits[i] == 1)
	mpz_setbit (result->value.integer, i);
      else
	gfc_internal_error ("IBITS: Bad bit");
    }

  free (bits);

  gfc_convert_mpz_to_signed (result->value.integer,
			 gfc_integer_kinds[k].bit_size);

  return result;
}


gfc_expr *
gfc_simplify_ibset (gfc_expr *x, gfc_expr *y)
{
  gfc_expr *result;
  int k, pos;

  if (x->expr_type != EXPR_CONSTANT || y->expr_type != EXPR_CONSTANT)
    return NULL;

  gfc_extract_int (y, &pos);

  k = gfc_validate_kind (x->ts.type, x->ts.kind, false);

  result = gfc_copy_expr (x);

  convert_mpz_to_unsigned (result->value.integer,
			   gfc_integer_kinds[k].bit_size);

  mpz_setbit (result->value.integer, pos);

  gfc_convert_mpz_to_signed (result->value.integer,
			 gfc_integer_kinds[k].bit_size);

  return result;
}


gfc_expr *
gfc_simplify_ichar (gfc_expr *e, gfc_expr *kind)
{
  gfc_expr *result;
  gfc_char_t index;
  int k;

  if (e->expr_type != EXPR_CONSTANT)
    return NULL;

  if (e->value.character.length != 1)
    {
      gfc_error ("Argument of ICHAR at %L must be of length one", &e->where);
      return &gfc_bad_expr;
    }

  index = e->value.character.string[0];

  k = get_kind (BT_INTEGER, kind, "ICHAR", gfc_default_integer_kind);
  if (k == -1)
    return &gfc_bad_expr;

  result = gfc_get_int_expr (k, &e->where, index);

  return range_check (result, "ICHAR");
}


gfc_expr *
gfc_simplify_ieor (gfc_expr *x, gfc_expr *y)
{
  gfc_expr *result;

  if (x->expr_type != EXPR_CONSTANT || y->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_get_constant_expr (BT_INTEGER, x->ts.kind, &x->where);
  mpz_xor (result->value.integer, x->value.integer, y->value.integer);

  return range_check (result, "IEOR");
}


gfc_expr *
gfc_simplify_index (gfc_expr *x, gfc_expr *y, gfc_expr *b, gfc_expr *kind)
{
  gfc_expr *result;
  int back, len, lensub;
  int i, j, k, count, index = 0, start;

  if (x->expr_type != EXPR_CONSTANT || y->expr_type != EXPR_CONSTANT
      || ( b != NULL && b->expr_type !=  EXPR_CONSTANT))
    return NULL;

  if (b != NULL && b->value.logical != 0)
    back = 1;
  else
    back = 0;

  k = get_kind (BT_INTEGER, kind, "INDEX", gfc_default_integer_kind);
  if (k == -1)
    return &gfc_bad_expr;

  result = gfc_get_constant_expr (BT_INTEGER, k, &x->where);

  len = x->value.character.length;
  lensub = y->value.character.length;

  if (len < lensub)
    {
      mpz_set_si (result->value.integer, 0);
      return result;
    }

  if (back == 0)
    {
      if (lensub == 0)
	{
	  mpz_set_si (result->value.integer, 1);
	  return result;
	}
      else if (lensub == 1)
	{
	  for (i = 0; i < len; i++)
	    {
	      for (j = 0; j < lensub; j++)
		{
		  if (y->value.character.string[j]
		      == x->value.character.string[i])
		    {
		      index = i + 1;
		      goto done;
		    }
		}
	    }
	}
      else
	{
	  for (i = 0; i < len; i++)
	    {
	      for (j = 0; j < lensub; j++)
		{
		  if (y->value.character.string[j]
		      == x->value.character.string[i])
		    {
		      start = i;
		      count = 0;

		      for (k = 0; k < lensub; k++)
			{
			  if (y->value.character.string[k]
			      == x->value.character.string[k + start])
			    count++;
			}

		      if (count == lensub)
			{
			  index = start + 1;
			  goto done;
			}
		    }
		}
	    }
	}

    }
  else
    {
      if (lensub == 0)
	{
	  mpz_set_si (result->value.integer, len + 1);
	  return result;
	}
      else if (lensub == 1)
	{
	  for (i = 0; i < len; i++)
	    {
	      for (j = 0; j < lensub; j++)
		{
		  if (y->value.character.string[j]
		      == x->value.character.string[len - i])
		    {
		      index = len - i + 1;
		      goto done;
		    }
		}
	    }
	}
      else
	{
	  for (i = 0; i < len; i++)
	    {
	      for (j = 0; j < lensub; j++)
		{
		  if (y->value.character.string[j]
		      == x->value.character.string[len - i])
		    {
		      start = len - i;
		      if (start <= len - lensub)
			{
			  count = 0;
			  for (k = 0; k < lensub; k++)
			    if (y->value.character.string[k]
			        == x->value.character.string[k + start])
			      count++;

			  if (count == lensub)
			    {
			      index = start + 1;
			      goto done;
			    }
			}
		      else
			{
			  continue;
			}
		    }
		}
	    }
	}
    }

done:
  mpz_set_si (result->value.integer, index);
  return range_check (result, "INDEX");
}


static gfc_expr *
simplify_intconv (gfc_expr *e, int kind, const char *name)
{
  gfc_expr *result = NULL;

  if (e->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_convert_constant (e, BT_INTEGER, kind);
  if (result == &gfc_bad_expr)
    return &gfc_bad_expr;

  return range_check (result, name);
}


gfc_expr *
gfc_simplify_int (gfc_expr *e, gfc_expr *k)
{
  int kind;

  kind = get_kind (BT_INTEGER, k, "INT", gfc_default_integer_kind);
  if (kind == -1)
    return &gfc_bad_expr;

  return simplify_intconv (e, kind, "INT");
}

gfc_expr *
gfc_simplify_int2 (gfc_expr *e)
{
  return simplify_intconv (e, 2, "INT2");
}


gfc_expr *
gfc_simplify_int8 (gfc_expr *e)
{
  return simplify_intconv (e, 8, "INT8");
}


gfc_expr *
gfc_simplify_long (gfc_expr *e)
{
  return simplify_intconv (e, 4, "LONG");
}


gfc_expr *
gfc_simplify_ifix (gfc_expr *e)
{
  gfc_expr *rtrunc, *result;

  if (e->expr_type != EXPR_CONSTANT)
    return NULL;

  rtrunc = gfc_copy_expr (e);
  mpfr_trunc (rtrunc->value.real, e->value.real);

  result = gfc_get_constant_expr (BT_INTEGER, gfc_default_integer_kind,
				  &e->where);
  gfc_mpfr_to_mpz (result->value.integer, rtrunc->value.real, &e->where);

  gfc_free_expr (rtrunc);

  return range_check (result, "IFIX");
}


gfc_expr *
gfc_simplify_idint (gfc_expr *e)
{
  gfc_expr *rtrunc, *result;

  if (e->expr_type != EXPR_CONSTANT)
    return NULL;

  rtrunc = gfc_copy_expr (e);
  mpfr_trunc (rtrunc->value.real, e->value.real);

  result = gfc_get_constant_expr (BT_INTEGER, gfc_default_integer_kind,
				  &e->where);
  gfc_mpfr_to_mpz (result->value.integer, rtrunc->value.real, &e->where);

  gfc_free_expr (rtrunc);

  return range_check (result, "IDINT");
}


gfc_expr *
gfc_simplify_ior (gfc_expr *x, gfc_expr *y)
{
  gfc_expr *result;

  if (x->expr_type != EXPR_CONSTANT || y->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_get_constant_expr (BT_INTEGER, x->ts.kind, &x->where);
  mpz_ior (result->value.integer, x->value.integer, y->value.integer);

  return range_check (result, "IOR");
}


static gfc_expr *
do_bit_xor (gfc_expr *result, gfc_expr *e)
{
  gcc_assert (e->ts.type == BT_INTEGER && e->expr_type == EXPR_CONSTANT);
  gcc_assert (result->ts.type == BT_INTEGER
	      && result->expr_type == EXPR_CONSTANT);

  mpz_xor (result->value.integer, result->value.integer, e->value.integer);
  return result;
}


gfc_expr *
gfc_simplify_iparity (gfc_expr *array, gfc_expr *dim, gfc_expr *mask)
{
  return simplify_transformation (array, dim, mask, 0, do_bit_xor);
}


gfc_expr *
gfc_simplify_is_iostat_end (gfc_expr *x)
{
  if (x->expr_type != EXPR_CONSTANT)
    return NULL;

  return gfc_get_logical_expr (gfc_default_logical_kind, &x->where,
			       mpz_cmp_si (x->value.integer,
					   LIBERROR_END) == 0);
}


gfc_expr *
gfc_simplify_is_iostat_eor (gfc_expr *x)
{
  if (x->expr_type != EXPR_CONSTANT)
    return NULL;

  return gfc_get_logical_expr (gfc_default_logical_kind, &x->where,
			       mpz_cmp_si (x->value.integer,
					   LIBERROR_EOR) == 0);
}


gfc_expr *
gfc_simplify_isnan (gfc_expr *x)
{
  if (x->expr_type != EXPR_CONSTANT)
    return NULL;

  return gfc_get_logical_expr (gfc_default_logical_kind, &x->where,
			       mpfr_nan_p (x->value.real));
}


/* Performs a shift on its first argument.  Depending on the last
   argument, the shift can be arithmetic, i.e. with filling from the
   left like in the SHIFTA intrinsic.  */
static gfc_expr *
simplify_shift (gfc_expr *e, gfc_expr *s, const char *name,
		bool arithmetic, int direction)
{
  gfc_expr *result;
  int ashift, *bits, i, k, bitsize, shift;

  if (e->expr_type != EXPR_CONSTANT || s->expr_type != EXPR_CONSTANT)
    return NULL;

  gfc_extract_int (s, &shift);

  k = gfc_validate_kind (BT_INTEGER, e->ts.kind, false);
  bitsize = gfc_integer_kinds[k].bit_size;

  result = gfc_get_constant_expr (e->ts.type, e->ts.kind, &e->where);

  if (shift == 0)
    {
      mpz_set (result->value.integer, e->value.integer);
      return result;
    }

  if (direction > 0 && shift < 0)
    {
      /* Left shift, as in SHIFTL.  */
      gfc_error ("Second argument of %s is negative at %L", name, &e->where);
      return &gfc_bad_expr;
    }
  else if (direction < 0)
    {
      /* Right shift, as in SHIFTR or SHIFTA.  */
      if (shift < 0)
	{
	  gfc_error ("Second argument of %s is negative at %L",
		     name, &e->where);
	  return &gfc_bad_expr;
	}

      shift = -shift;
    }

  ashift = (shift >= 0 ? shift : -shift);

  if (ashift > bitsize)
    {
      gfc_error ("Magnitude of second argument of %s exceeds bit size "
		 "at %L", name, &e->where);
      return &gfc_bad_expr;
    }

  bits = XCNEWVEC (int, bitsize);

  for (i = 0; i < bitsize; i++)
    bits[i] = mpz_tstbit (e->value.integer, i);

  if (shift > 0)
    {
      /* Left shift.  */
      for (i = 0; i < shift; i++)
	mpz_clrbit (result->value.integer, i);

      for (i = 0; i < bitsize - shift; i++)
	{
	  if (bits[i] == 0)
	    mpz_clrbit (result->value.integer, i + shift);
	  else
	    mpz_setbit (result->value.integer, i + shift);
	}
    }
  else
    {
      /* Right shift.  */
      if (arithmetic && bits[bitsize - 1])
	for (i = bitsize - 1; i >= bitsize - ashift; i--)
	  mpz_setbit (result->value.integer, i);
      else
	for (i = bitsize - 1; i >= bitsize - ashift; i--)
	  mpz_clrbit (result->value.integer, i);

      for (i = bitsize - 1; i >= ashift; i--)
	{
	  if (bits[i] == 0)
	    mpz_clrbit (result->value.integer, i - ashift);
	  else
	    mpz_setbit (result->value.integer, i - ashift);
	}
    }

  gfc_convert_mpz_to_signed (result->value.integer, bitsize);
  free (bits);

  return result;
}


gfc_expr *
gfc_simplify_ishft (gfc_expr *e, gfc_expr *s)
{
  return simplify_shift (e, s, "ISHFT", false, 0);
}


gfc_expr *
gfc_simplify_lshift (gfc_expr *e, gfc_expr *s)
{
  return simplify_shift (e, s, "LSHIFT", false, 1);
}


gfc_expr *
gfc_simplify_rshift (gfc_expr *e, gfc_expr *s)
{
  return simplify_shift (e, s, "RSHIFT", true, -1);
}


gfc_expr *
gfc_simplify_shifta (gfc_expr *e, gfc_expr *s)
{
  return simplify_shift (e, s, "SHIFTA", true, -1);
}


gfc_expr *
gfc_simplify_shiftl (gfc_expr *e, gfc_expr *s)
{
  return simplify_shift (e, s, "SHIFTL", false, 1);
}


gfc_expr *
gfc_simplify_shiftr (gfc_expr *e, gfc_expr *s)
{
  return simplify_shift (e, s, "SHIFTR", false, -1);
}


gfc_expr *
gfc_simplify_ishftc (gfc_expr *e, gfc_expr *s, gfc_expr *sz)
{
  gfc_expr *result;
  int shift, ashift, isize, ssize, delta, k;
  int i, *bits;

  if (e->expr_type != EXPR_CONSTANT || s->expr_type != EXPR_CONSTANT)
    return NULL;

  gfc_extract_int (s, &shift);

  k = gfc_validate_kind (e->ts.type, e->ts.kind, false);
  isize = gfc_integer_kinds[k].bit_size;

  if (sz != NULL)
    {
      if (sz->expr_type != EXPR_CONSTANT)
	return NULL;

      gfc_extract_int (sz, &ssize);
    }
  else
    ssize = isize;

  if (shift >= 0)
    ashift = shift;
  else
    ashift = -shift;

  if (ashift > ssize)
    {
      if (sz == NULL)
	gfc_error ("Magnitude of second argument of ISHFTC exceeds "
		   "BIT_SIZE of first argument at %C");
      else
	gfc_error ("Absolute value of SHIFT shall be less than or equal "
		   "to SIZE at %C");
      return &gfc_bad_expr;
    }

  result = gfc_get_constant_expr (e->ts.type, e->ts.kind, &e->where);

  mpz_set (result->value.integer, e->value.integer);

  if (shift == 0)
    return result;

  convert_mpz_to_unsigned (result->value.integer, isize);

  bits = XCNEWVEC (int, ssize);

  for (i = 0; i < ssize; i++)
    bits[i] = mpz_tstbit (e->value.integer, i);

  delta = ssize - ashift;

  if (shift > 0)
    {
      for (i = 0; i < delta; i++)
	{
	  if (bits[i] == 0)
	    mpz_clrbit (result->value.integer, i + shift);
	  else
	    mpz_setbit (result->value.integer, i + shift);
	}

      for (i = delta; i < ssize; i++)
	{
	  if (bits[i] == 0)
	    mpz_clrbit (result->value.integer, i - delta);
	  else
	    mpz_setbit (result->value.integer, i - delta);
	}
    }
  else
    {
      for (i = 0; i < ashift; i++)
	{
	  if (bits[i] == 0)
	    mpz_clrbit (result->value.integer, i + delta);
	  else
	    mpz_setbit (result->value.integer, i + delta);
	}

      for (i = ashift; i < ssize; i++)
	{
	  if (bits[i] == 0)
	    mpz_clrbit (result->value.integer, i + shift);
	  else
	    mpz_setbit (result->value.integer, i + shift);
	}
    }

  gfc_convert_mpz_to_signed (result->value.integer, isize);

  free (bits);
  return result;
}


gfc_expr *
gfc_simplify_kind (gfc_expr *e)
{
  return gfc_get_int_expr (gfc_default_integer_kind, NULL, e->ts.kind);
}


static gfc_expr *
simplify_bound_dim (gfc_expr *array, gfc_expr *kind, int d, int upper,
		    gfc_array_spec *as, gfc_ref *ref, bool coarray)
{
  gfc_expr *l, *u, *result;
  int k;

  k = get_kind (BT_INTEGER, kind, upper ? "UBOUND" : "LBOUND",
		gfc_default_integer_kind);
  if (k == -1)
    return &gfc_bad_expr;

  result = gfc_get_constant_expr (BT_INTEGER, k, &array->where);

  /* For non-variables, LBOUND(expr, DIM=n) = 1 and
     UBOUND(expr, DIM=n) = SIZE(expr, DIM=n).  */
  if (!coarray && array->expr_type != EXPR_VARIABLE)
    {
      if (upper)
	{
	  gfc_expr* dim = result;
	  mpz_set_si (dim->value.integer, d);

	  result = simplify_size (array, dim, k);
	  gfc_free_expr (dim);
	  if (!result)
	    goto returnNull;
	}
      else
	mpz_set_si (result->value.integer, 1);

      goto done;
    }

  /* Otherwise, we have a variable expression.  */
  gcc_assert (array->expr_type == EXPR_VARIABLE);
  gcc_assert (as);

  if (!gfc_resolve_array_spec (as, 0))
    return NULL;

  /* The last dimension of an assumed-size array is special.  */
  if ((!coarray && d == as->rank && as->type == AS_ASSUMED_SIZE && !upper)
      || (coarray && d == as->rank + as->corank
	  && (!upper || flag_coarray == GFC_FCOARRAY_SINGLE)))
    {
      if (as->lower[d-1]->expr_type == EXPR_CONSTANT)
	{
	  gfc_free_expr (result);
	  return gfc_copy_expr (as->lower[d-1]);
	}

      goto returnNull;
    }

  result = gfc_get_constant_expr (BT_INTEGER, k, &array->where);

  /* Then, we need to know the extent of the given dimension.  */
  if (coarray || (ref->u.ar.type == AR_FULL && !ref->next))
    {
      gfc_expr *declared_bound;
      int empty_bound;
      bool constant_lbound, constant_ubound;

      l = as->lower[d-1];
      u = as->upper[d-1];

      gcc_assert (l != NULL);

      constant_lbound = l->expr_type == EXPR_CONSTANT;
      constant_ubound = u && u->expr_type == EXPR_CONSTANT;

      empty_bound = upper ? 0 : 1;
      declared_bound = upper ? u : l;

      if ((!upper && !constant_lbound)
	  || (upper && !constant_ubound))
	goto returnNull;

      if (!coarray)
	{
	  /* For {L,U}BOUND, the value depends on whether the array
	     is empty.  We can nevertheless simplify if the declared bound
	     has the same value as that of an empty array, in which case
	     the result isn't dependent on the array emptyness.  */
	  if (mpz_cmp_si (declared_bound->value.integer, empty_bound) == 0)
	    mpz_set_si (result->value.integer, empty_bound);
	  else if (!constant_lbound || !constant_ubound)
	    /* Array emptyness can't be determined, we can't simplify.  */
	    goto returnNull;
	  else if (mpz_cmp (l->value.integer, u->value.integer) > 0)
	    mpz_set_si (result->value.integer, empty_bound);
	  else
	    mpz_set (result->value.integer, declared_bound->value.integer);
	}
      else
	mpz_set (result->value.integer, declared_bound->value.integer);
    }
  else
    {
      if (upper)
	{
	  if (!gfc_ref_dimen_size (&ref->u.ar, d - 1, &result->value.integer, NULL))
	    goto returnNull;
	}
      else
	mpz_set_si (result->value.integer, (long int) 1);
    }

done:
  return range_check (result, upper ? "UBOUND" : "LBOUND");

returnNull:
  gfc_free_expr (result);
  return NULL;
}


static gfc_expr *
simplify_bound (gfc_expr *array, gfc_expr *dim, gfc_expr *kind, int upper)
{
  gfc_ref *ref;
  gfc_array_spec *as;
  int d;

  if (array->ts.type == BT_CLASS)
    return NULL;

  if (array->expr_type != EXPR_VARIABLE)
    {
      as = NULL;
      ref = NULL;
      goto done;
    }

  /* Follow any component references.  */
  as = array->symtree->n.sym->as;
  for (ref = array->ref; ref; ref = ref->next)
    {
      switch (ref->type)
	{
	case REF_ARRAY:
	  switch (ref->u.ar.type)
	    {
	    case AR_ELEMENT:
	      as = NULL;
	      continue;

	    case AR_FULL:
	      /* We're done because 'as' has already been set in the
		 previous iteration.  */
	      goto done;

	    case AR_UNKNOWN:
	      return NULL;

	    case AR_SECTION:
	      as = ref->u.ar.as;
	      goto done;
	    }

	  gcc_unreachable ();

	case REF_COMPONENT:
	  as = ref->u.c.component->as;
	  continue;

	case REF_SUBSTRING:
	  continue;
	}
    }

  gcc_unreachable ();

 done:

  if (as && (as->type == AS_DEFERRED || as->type == AS_ASSUMED_RANK
	     || (as->type == AS_ASSUMED_SHAPE && upper)))
    return NULL;

  gcc_assert (!as
	      || (as->type != AS_DEFERRED
		  && array->expr_type == EXPR_VARIABLE
		  && !gfc_expr_attr (array).allocatable
		  && !gfc_expr_attr (array).pointer));

  if (dim == NULL)
    {
      /* Multi-dimensional bounds.  */
      gfc_expr *bounds[GFC_MAX_DIMENSIONS];
      gfc_expr *e;
      int k;

      /* UBOUND(ARRAY) is not valid for an assumed-size array.  */
      if (upper && as && as->type == AS_ASSUMED_SIZE)
	{
	  /* An error message will be emitted in
	     check_assumed_size_reference (resolve.c).  */
	  return &gfc_bad_expr;
	}

      /* Simplify the bounds for each dimension.  */
      for (d = 0; d < array->rank; d++)
	{
	  bounds[d] = simplify_bound_dim (array, kind, d + 1, upper, as, ref,
					  false);
	  if (bounds[d] == NULL || bounds[d] == &gfc_bad_expr)
	    {
	      int j;

	      for (j = 0; j < d; j++)
		gfc_free_expr (bounds[j]);
	      return bounds[d];
	    }
	}

      /* Allocate the result expression.  */
      k = get_kind (BT_INTEGER, kind, upper ? "UBOUND" : "LBOUND",
		    gfc_default_integer_kind);
      if (k == -1)
	return &gfc_bad_expr;

      e = gfc_get_array_expr (BT_INTEGER, k, &array->where);

      /* The result is a rank 1 array; its size is the rank of the first
	 argument to {L,U}BOUND.  */
      e->rank = 1;
      e->shape = gfc_get_shape (1);
      mpz_init_set_ui (e->shape[0], array->rank);

      /* Create the constructor for this array.  */
      for (d = 0; d < array->rank; d++)
	gfc_constructor_append_expr (&e->value.constructor,
				     bounds[d], &e->where);

      return e;
    }
  else
    {
      /* A DIM argument is specified.  */
      if (dim->expr_type != EXPR_CONSTANT)
	return NULL;

      d = mpz_get_si (dim->value.integer);

      if ((d < 1 || d > array->rank)
	  || (d == array->rank && as && as->type == AS_ASSUMED_SIZE && upper))
	{
	  gfc_error ("DIM argument at %L is out of bounds", &dim->where);
	  return &gfc_bad_expr;
	}

      if (as && as->type == AS_ASSUMED_RANK)
	return NULL;

      return simplify_bound_dim (array, kind, d, upper, as, ref, false);
    }
}


static gfc_expr *
simplify_cobound (gfc_expr *array, gfc_expr *dim, gfc_expr *kind, int upper)
{
  gfc_ref *ref;
  gfc_array_spec *as;
  int d;

  if (array->expr_type != EXPR_VARIABLE)
    return NULL;

  /* Follow any component references.  */
  as = (array->ts.type == BT_CLASS && array->ts.u.derived->components)
       ? array->ts.u.derived->components->as
       : array->symtree->n.sym->as;
  for (ref = array->ref; ref; ref = ref->next)
    {
      switch (ref->type)
	{
	case REF_ARRAY:
	  switch (ref->u.ar.type)
	    {
	    case AR_ELEMENT:
	      if (ref->u.ar.as->corank > 0)
		{
		  gcc_assert (as == ref->u.ar.as);
		  goto done;
		}
	      as = NULL;
	      continue;

	    case AR_FULL:
	      /* We're done because 'as' has already been set in the
		 previous iteration.  */
	      goto done;

	    case AR_UNKNOWN:
	      return NULL;

	    case AR_SECTION:
	      as = ref->u.ar.as;
	      goto done;
	    }

	  gcc_unreachable ();

	case REF_COMPONENT:
	  as = ref->u.c.component->as;
	  continue;

	case REF_SUBSTRING:
	  continue;
	}
    }

  if (!as)
    gcc_unreachable ();

 done:

  if (as->cotype == AS_DEFERRED || as->cotype == AS_ASSUMED_SHAPE)
    return NULL;

  if (dim == NULL)
    {
      /* Multi-dimensional cobounds.  */
      gfc_expr *bounds[GFC_MAX_DIMENSIONS];
      gfc_expr *e;
      int k;

      /* Simplify the cobounds for each dimension.  */
      for (d = 0; d < as->corank; d++)
	{
	  bounds[d] = simplify_bound_dim (array, kind, d + 1 + as->rank,
					  upper, as, ref, true);
	  if (bounds[d] == NULL || bounds[d] == &gfc_bad_expr)
	    {
	      int j;

	      for (j = 0; j < d; j++)
		gfc_free_expr (bounds[j]);
	      return bounds[d];
	    }
	}

      /* Allocate the result expression.  */
      e = gfc_get_expr ();
      e->where = array->where;
      e->expr_type = EXPR_ARRAY;
      e->ts.type = BT_INTEGER;
      k = get_kind (BT_INTEGER, kind, upper ? "UCOBOUND" : "LCOBOUND",
		    gfc_default_integer_kind);
      if (k == -1)
	{
	  gfc_free_expr (e);
	  return &gfc_bad_expr;
	}
      e->ts.kind = k;

      /* The result is a rank 1 array; its size is the rank of the first
	 argument to {L,U}COBOUND.  */
      e->rank = 1;
      e->shape = gfc_get_shape (1);
      mpz_init_set_ui (e->shape[0], as->corank);

      /* Create the constructor for this array.  */
      for (d = 0; d < as->corank; d++)
	gfc_constructor_append_expr (&e->value.constructor,
				     bounds[d], &e->where);
      return e;
    }
  else
    {
      /* A DIM argument is specified.  */
      if (dim->expr_type != EXPR_CONSTANT)
	return NULL;

      d = mpz_get_si (dim->value.integer);

      if (d < 1 || d > as->corank)
	{
	  gfc_error ("DIM argument at %L is out of bounds", &dim->where);
	  return &gfc_bad_expr;
	}

      return simplify_bound_dim (array, kind, d+as->rank, upper, as, ref, true);
    }
}


gfc_expr *
gfc_simplify_lbound (gfc_expr *array, gfc_expr *dim, gfc_expr *kind)
{
  return simplify_bound (array, dim, kind, 0);
}


gfc_expr *
gfc_simplify_lcobound (gfc_expr *array, gfc_expr *dim, gfc_expr *kind)
{
  return simplify_cobound (array, dim, kind, 0);
}

gfc_expr *
gfc_simplify_leadz (gfc_expr *e)
{
  unsigned long lz, bs;
  int i;

  if (e->expr_type != EXPR_CONSTANT)
    return NULL;

  i = gfc_validate_kind (e->ts.type, e->ts.kind, false);
  bs = gfc_integer_kinds[i].bit_size;
  if (mpz_cmp_si (e->value.integer, 0) == 0)
    lz = bs;
  else if (mpz_cmp_si (e->value.integer, 0) < 0)
    lz = 0;
  else
    lz = bs - mpz_sizeinbase (e->value.integer, 2);

  return gfc_get_int_expr (gfc_default_integer_kind, &e->where, lz);
}


gfc_expr *
gfc_simplify_len (gfc_expr *e, gfc_expr *kind)
{
  gfc_expr *result;
  int k = get_kind (BT_INTEGER, kind, "LEN", gfc_default_integer_kind);

  if (k == -1)
    return &gfc_bad_expr;

  if (e->expr_type == EXPR_CONSTANT)
    {
      result = gfc_get_constant_expr (BT_INTEGER, k, &e->where);
      mpz_set_si (result->value.integer, e->value.character.length);
      return range_check (result, "LEN");
    }
  else if (e->ts.u.cl != NULL && e->ts.u.cl->length != NULL
	   && e->ts.u.cl->length->expr_type == EXPR_CONSTANT
	   && e->ts.u.cl->length->ts.type == BT_INTEGER)
    {
      result = gfc_get_constant_expr (BT_INTEGER, k, &e->where);
      mpz_set (result->value.integer, e->ts.u.cl->length->value.integer);
      return range_check (result, "LEN");
    }
  else if (e->expr_type == EXPR_VARIABLE && e->ts.type == BT_CHARACTER
	   && e->symtree->n.sym
	   && e->symtree->n.sym->ts.type != BT_DERIVED
	   && e->symtree->n.sym->assoc && e->symtree->n.sym->assoc->target
	   && e->symtree->n.sym->assoc->target->ts.type == BT_DERIVED
	   && e->symtree->n.sym->assoc->target->symtree->n.sym
	   && UNLIMITED_POLY (e->symtree->n.sym->assoc->target->symtree->n.sym))

    /* The expression in assoc->target points to a ref to the _data component
       of the unlimited polymorphic entity.  To get the _len component the last
       _data ref needs to be stripped and a ref to the _len component added.  */
    return gfc_get_len_component (e->symtree->n.sym->assoc->target);
  else
    return NULL;
}


gfc_expr *
gfc_simplify_len_trim (gfc_expr *e, gfc_expr *kind)
{
  gfc_expr *result;
  int count, len, i;
  int k = get_kind (BT_INTEGER, kind, "LEN_TRIM", gfc_default_integer_kind);

  if (k == -1)
    return &gfc_bad_expr;

  if (e->expr_type != EXPR_CONSTANT)
    return NULL;

  len = e->value.character.length;
  for (count = 0, i = 1; i <= len; i++)
    if (e->value.character.string[len - i] == ' ')
      count++;
    else
      break;

  result = gfc_get_int_expr (k, &e->where, len - count);
  return range_check (result, "LEN_TRIM");
}

gfc_expr *
gfc_simplify_lgamma (gfc_expr *x)
{
  gfc_expr *result;
  int sg;

  if (x->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_get_constant_expr (x->ts.type, x->ts.kind, &x->where);
  mpfr_lgamma (result->value.real, &sg, x->value.real, GFC_RND_MODE);

  return range_check (result, "LGAMMA");
}


gfc_expr *
gfc_simplify_lge (gfc_expr *a, gfc_expr *b)
{
  if (a->expr_type != EXPR_CONSTANT || b->expr_type != EXPR_CONSTANT)
    return NULL;

  return gfc_get_logical_expr (gfc_default_logical_kind, &a->where,
			       gfc_compare_string (a, b) >= 0);
}


gfc_expr *
gfc_simplify_lgt (gfc_expr *a, gfc_expr *b)
{
  if (a->expr_type != EXPR_CONSTANT || b->expr_type != EXPR_CONSTANT)
    return NULL;

  return gfc_get_logical_expr (gfc_default_logical_kind, &a->where,
			       gfc_compare_string (a, b) > 0);
}


gfc_expr *
gfc_simplify_lle (gfc_expr *a, gfc_expr *b)
{
  if (a->expr_type != EXPR_CONSTANT || b->expr_type != EXPR_CONSTANT)
    return NULL;

  return gfc_get_logical_expr (gfc_default_logical_kind, &a->where,
			       gfc_compare_string (a, b) <= 0);
}


gfc_expr *
gfc_simplify_llt (gfc_expr *a, gfc_expr *b)
{
  if (a->expr_type != EXPR_CONSTANT || b->expr_type != EXPR_CONSTANT)
    return NULL;

  return gfc_get_logical_expr (gfc_default_logical_kind, &a->where,
			       gfc_compare_string (a, b) < 0);
}


gfc_expr *
gfc_simplify_log (gfc_expr *x)
{
  gfc_expr *result;

  if (x->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_get_constant_expr (x->ts.type, x->ts.kind, &x->where);

  switch (x->ts.type)
    {
    case BT_REAL:
      if (mpfr_sgn (x->value.real) <= 0)
	{
	  gfc_error ("Argument of LOG at %L cannot be less than or equal "
		     "to zero", &x->where);
	  gfc_free_expr (result);
	  return &gfc_bad_expr;
	}

      mpfr_log (result->value.real, x->value.real, GFC_RND_MODE);
      break;

    case BT_COMPLEX:
      if (mpfr_zero_p (mpc_realref (x->value.complex))
	  && mpfr_zero_p (mpc_imagref (x->value.complex)))
	{
	  gfc_error ("Complex argument of LOG at %L cannot be zero",
		     &x->where);
	  gfc_free_expr (result);
	  return &gfc_bad_expr;
	}

      gfc_set_model_kind (x->ts.kind);
      mpc_log (result->value.complex, x->value.complex, GFC_MPC_RND_MODE);
      break;

    default:
      gfc_internal_error ("gfc_simplify_log: bad type");
    }

  return range_check (result, "LOG");
}


gfc_expr *
gfc_simplify_log10 (gfc_expr *x)
{
  gfc_expr *result;

  if (x->expr_type != EXPR_CONSTANT)
    return NULL;

  if (mpfr_sgn (x->value.real) <= 0)
    {
      gfc_error ("Argument of LOG10 at %L cannot be less than or equal "
		 "to zero", &x->where);
      return &gfc_bad_expr;
    }

  result = gfc_get_constant_expr (x->ts.type, x->ts.kind, &x->where);
  mpfr_log10 (result->value.real, x->value.real, GFC_RND_MODE);

  return range_check (result, "LOG10");
}


gfc_expr *
gfc_simplify_logical (gfc_expr *e, gfc_expr *k)
{
  int kind;

  kind = get_kind (BT_LOGICAL, k, "LOGICAL", gfc_default_logical_kind);
  if (kind < 0)
    return &gfc_bad_expr;

  if (e->expr_type != EXPR_CONSTANT)
    return NULL;

  return gfc_get_logical_expr (kind, &e->where, e->value.logical);
}


gfc_expr*
gfc_simplify_matmul (gfc_expr *matrix_a, gfc_expr *matrix_b)
{
  gfc_expr *result;
  int row, result_rows, col, result_columns;
  int stride_a, offset_a, stride_b, offset_b;

  if (!is_constant_array_expr (matrix_a)
      || !is_constant_array_expr (matrix_b))
    return NULL;

  gcc_assert (gfc_compare_types (&matrix_a->ts, &matrix_b->ts));
  result = gfc_get_array_expr (matrix_a->ts.type,
			       matrix_a->ts.kind,
			       &matrix_a->where);

  if (matrix_a->rank == 1 && matrix_b->rank == 2)
    {
      result_rows = 1;
      result_columns = mpz_get_si (matrix_b->shape[1]);
      stride_a = 1;
      stride_b = mpz_get_si (matrix_b->shape[0]);

      result->rank = 1;
      result->shape = gfc_get_shape (result->rank);
      mpz_init_set_si (result->shape[0], result_columns);
    }
  else if (matrix_a->rank == 2 && matrix_b->rank == 1)
    {
      result_rows = mpz_get_si (matrix_a->shape[0]);
      result_columns = 1;
      stride_a = mpz_get_si (matrix_a->shape[0]);
      stride_b = 1;

      result->rank = 1;
      result->shape = gfc_get_shape (result->rank);
      mpz_init_set_si (result->shape[0], result_rows);
    }
  else if (matrix_a->rank == 2 && matrix_b->rank == 2)
    {
      result_rows = mpz_get_si (matrix_a->shape[0]);
      result_columns = mpz_get_si (matrix_b->shape[1]);
      stride_a = mpz_get_si (matrix_a->shape[0]);
      stride_b = mpz_get_si (matrix_b->shape[0]);

      result->rank = 2;
      result->shape = gfc_get_shape (result->rank);
      mpz_init_set_si (result->shape[0], result_rows);
      mpz_init_set_si (result->shape[1], result_columns);
    }
  else
    gcc_unreachable();

  offset_a = offset_b = 0;
  for (col = 0; col < result_columns; ++col)
    {
      offset_a = 0;

      for (row = 0; row < result_rows; ++row)
	{
	  gfc_expr *e = compute_dot_product (matrix_a, stride_a, offset_a,
					     matrix_b, 1, offset_b, false);
	  gfc_constructor_append_expr (&result->value.constructor,
				       e, NULL);

	  offset_a += 1;
        }

      offset_b += stride_b;
    }

  return result;
}


gfc_expr *
gfc_simplify_maskr (gfc_expr *i, gfc_expr *kind_arg)
{
  gfc_expr *result;
  int kind, arg, k;
  const char *s;

  if (i->expr_type != EXPR_CONSTANT)
    return NULL;

  kind = get_kind (BT_INTEGER, kind_arg, "MASKR", gfc_default_integer_kind);
  if (kind == -1)
    return &gfc_bad_expr;
  k = gfc_validate_kind (BT_INTEGER, kind, false);

  s = gfc_extract_int (i, &arg);
  gcc_assert (!s);

  result = gfc_get_constant_expr (BT_INTEGER, kind, &i->where);

  /* MASKR(n) = 2^n - 1 */
  mpz_set_ui (result->value.integer, 1);
  mpz_mul_2exp (result->value.integer, result->value.integer, arg);
  mpz_sub_ui (result->value.integer, result->value.integer, 1);

  gfc_convert_mpz_to_signed (result->value.integer, gfc_integer_kinds[k].bit_size);

  return result;
}


gfc_expr *
gfc_simplify_maskl (gfc_expr *i, gfc_expr *kind_arg)
{
  gfc_expr *result;
  int kind, arg, k;
  const char *s;
  mpz_t z;

  if (i->expr_type != EXPR_CONSTANT)
    return NULL;

  kind = get_kind (BT_INTEGER, kind_arg, "MASKL", gfc_default_integer_kind);
  if (kind == -1)
    return &gfc_bad_expr;
  k = gfc_validate_kind (BT_INTEGER, kind, false);

  s = gfc_extract_int (i, &arg);
  gcc_assert (!s);

  result = gfc_get_constant_expr (BT_INTEGER, kind, &i->where);

  /* MASKL(n) = 2^bit_size - 2^(bit_size - n) */
  mpz_init_set_ui (z, 1);
  mpz_mul_2exp (z, z, gfc_integer_kinds[k].bit_size);
  mpz_set_ui (result->value.integer, 1);
  mpz_mul_2exp (result->value.integer, result->value.integer,
		gfc_integer_kinds[k].bit_size - arg);
  mpz_sub (result->value.integer, z, result->value.integer);
  mpz_clear (z);

  gfc_convert_mpz_to_signed (result->value.integer, gfc_integer_kinds[k].bit_size);

  return result;
}


gfc_expr *
gfc_simplify_merge (gfc_expr *tsource, gfc_expr *fsource, gfc_expr *mask)
{
  gfc_expr * result;
  gfc_constructor *tsource_ctor, *fsource_ctor, *mask_ctor;

  if (mask->expr_type == EXPR_CONSTANT)
    return gfc_get_parentheses (gfc_copy_expr (mask->value.logical
					       ? tsource : fsource));

  if (!mask->rank || !is_constant_array_expr (mask)
      || !is_constant_array_expr (tsource) || !is_constant_array_expr (fsource))
    return NULL;

  result = gfc_get_array_expr (tsource->ts.type, tsource->ts.kind,
			       &tsource->where);
  if (tsource->ts.type == BT_DERIVED)
    result->ts.u.derived = tsource->ts.u.derived;
  else if (tsource->ts.type == BT_CHARACTER)
    result->ts.u.cl = tsource->ts.u.cl;

  tsource_ctor = gfc_constructor_first (tsource->value.constructor);
  fsource_ctor = gfc_constructor_first (fsource->value.constructor);
  mask_ctor = gfc_constructor_first (mask->value.constructor);

  while (mask_ctor)
    {
      if (mask_ctor->expr->value.logical)
	gfc_constructor_append_expr (&result->value.constructor,
				     gfc_copy_expr (tsource_ctor->expr),
				     NULL);
      else
	gfc_constructor_append_expr (&result->value.constructor,
				     gfc_copy_expr (fsource_ctor->expr),
				     NULL);
      tsource_ctor = gfc_constructor_next (tsource_ctor);
      fsource_ctor = gfc_constructor_next (fsource_ctor);
      mask_ctor = gfc_constructor_next (mask_ctor);
    }

  result->shape = gfc_get_shape (1);
  gfc_array_size (result, &result->shape[0]);

  return result;
}


gfc_expr *
gfc_simplify_merge_bits (gfc_expr *i, gfc_expr *j, gfc_expr *mask_expr)
{
  mpz_t arg1, arg2, mask;
  gfc_expr *result;

  if (i->expr_type != EXPR_CONSTANT || j->expr_type != EXPR_CONSTANT
      || mask_expr->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_get_constant_expr (BT_INTEGER, i->ts.kind, &i->where);

  /* Convert all argument to unsigned.  */
  mpz_init_set (arg1, i->value.integer);
  mpz_init_set (arg2, j->value.integer);
  mpz_init_set (mask, mask_expr->value.integer);

  /* MERGE_BITS(I,J,MASK) = IOR (IAND (I, MASK), IAND (J, NOT (MASK))).  */
  mpz_and (arg1, arg1, mask);
  mpz_com (mask, mask);
  mpz_and (arg2, arg2, mask);
  mpz_ior (result->value.integer, arg1, arg2);

  mpz_clear (arg1);
  mpz_clear (arg2);
  mpz_clear (mask);

  return result;
}


/* Selects between current value and extremum for simplify_min_max
   and simplify_minval_maxval.  */
static void
min_max_choose (gfc_expr *arg, gfc_expr *extremum, int sign)
{
  switch (arg->ts.type)
    {
      case BT_INTEGER:
	if (mpz_cmp (arg->value.integer,
			extremum->value.integer) * sign > 0)
	mpz_set (extremum->value.integer, arg->value.integer);
	break;

      case BT_REAL:
	/* We need to use mpfr_min and mpfr_max to treat NaN properly.  */
	if (sign > 0)
	  mpfr_max (extremum->value.real, extremum->value.real,
		      arg->value.real, GFC_RND_MODE);
	else
	  mpfr_min (extremum->value.real, extremum->value.real,
		      arg->value.real, GFC_RND_MODE);
	break;

      case BT_CHARACTER:
#define LENGTH(x) ((x)->value.character.length)
#define STRING(x) ((x)->value.character.string)
	if (LENGTH (extremum) < LENGTH(arg))
	  {
	    gfc_char_t *tmp = STRING(extremum);

	    STRING(extremum) = gfc_get_wide_string (LENGTH(arg) + 1);
	    memcpy (STRING(extremum), tmp,
		      LENGTH(extremum) * sizeof (gfc_char_t));
	    gfc_wide_memset (&STRING(extremum)[LENGTH(extremum)], ' ',
			       LENGTH(arg) - LENGTH(extremum));
	    STRING(extremum)[LENGTH(arg)] = '\0';  /* For debugger  */
	    LENGTH(extremum) = LENGTH(arg);
	    free (tmp);
	  }

	if (gfc_compare_string (arg, extremum) * sign > 0)
	  {
	    free (STRING(extremum));
	    STRING(extremum) = gfc_get_wide_string (LENGTH(extremum) + 1);
	    memcpy (STRING(extremum), STRING(arg),
		      LENGTH(arg) * sizeof (gfc_char_t));
	    gfc_wide_memset (&STRING(extremum)[LENGTH(arg)], ' ',
			       LENGTH(extremum) - LENGTH(arg));
	    STRING(extremum)[LENGTH(extremum)] = '\0';  /* For debugger  */
	  }
#undef LENGTH
#undef STRING
	break;

      default:
	gfc_internal_error ("simplify_min_max(): Bad type in arglist");
    }
}


/* This function is special since MAX() can take any number of
   arguments.  The simplified expression is a rewritten version of the
   argument list containing at most one constant element.  Other
   constant elements are deleted.  Because the argument list has
   already been checked, this function always succeeds.  sign is 1 for
   MAX(), -1 for MIN().  */

static gfc_expr *
simplify_min_max (gfc_expr *expr, int sign)
{
  gfc_actual_arglist *arg, *last, *extremum;
  gfc_intrinsic_sym * specific;

  last = NULL;
  extremum = NULL;
  specific = expr->value.function.isym;

  arg = expr->value.function.actual;

  for (; arg; last = arg, arg = arg->next)
    {
      if (arg->expr->expr_type != EXPR_CONSTANT)
	continue;

      if (extremum == NULL)
	{
	  extremum = arg;
	  continue;
	}

      min_max_choose (arg->expr, extremum->expr, sign);

      /* Delete the extra constant argument.  */
      last->next = arg->next;

      arg->next = NULL;
      gfc_free_actual_arglist (arg);
      arg = last;
    }

  /* If there is one value left, replace the function call with the
     expression.  */
  if (expr->value.function.actual->next != NULL)
    return NULL;

  /* Convert to the correct type and kind.  */
  if (expr->ts.type != BT_UNKNOWN)
    return gfc_convert_constant (expr->value.function.actual->expr,
	expr->ts.type, expr->ts.kind);

  if (specific->ts.type != BT_UNKNOWN)
    return gfc_convert_constant (expr->value.function.actual->expr,
	specific->ts.type, specific->ts.kind);

  return gfc_copy_expr (expr->value.function.actual->expr);
}


gfc_expr *
gfc_simplify_min (gfc_expr *e)
{
  return simplify_min_max (e, -1);
}


gfc_expr *
gfc_simplify_max (gfc_expr *e)
{
  return simplify_min_max (e, 1);
}


/* This is a simplified version of simplify_min_max to provide
   simplification of minval and maxval for a vector.  */

static gfc_expr *
simplify_minval_maxval (gfc_expr *expr, int sign)
{
  gfc_constructor *c, *extremum;
  gfc_intrinsic_sym * specific;

  extremum = NULL;
  specific = expr->value.function.isym;

  for (c = gfc_constructor_first (expr->value.constructor);
       c; c = gfc_constructor_next (c))
    {
      if (c->expr->expr_type != EXPR_CONSTANT)
	return NULL;

      if (extremum == NULL)
	{
	  extremum = c;
	  continue;
	}

      min_max_choose (c->expr, extremum->expr, sign);
     }

  if (extremum == NULL)
    return NULL;

  /* Convert to the correct type and kind.  */
  if (expr->ts.type != BT_UNKNOWN)
    return gfc_convert_constant (extremum->expr,
	expr->ts.type, expr->ts.kind);

  if (specific->ts.type != BT_UNKNOWN)
    return gfc_convert_constant (extremum->expr,
	specific->ts.type, specific->ts.kind);

  return gfc_copy_expr (extremum->expr);
}


gfc_expr *
gfc_simplify_minval (gfc_expr *array, gfc_expr* dim, gfc_expr *mask)
{
  if (array->expr_type != EXPR_ARRAY || array->rank != 1 || dim || mask)
    return NULL;

  return simplify_minval_maxval (array, -1);
}


gfc_expr *
gfc_simplify_maxval (gfc_expr *array, gfc_expr* dim, gfc_expr *mask)
{
  if (array->expr_type != EXPR_ARRAY || array->rank != 1 || dim || mask)
    return NULL;

  return simplify_minval_maxval (array, 1);
}


gfc_expr *
gfc_simplify_maxexponent (gfc_expr *x)
{
  int i = gfc_validate_kind (BT_REAL, x->ts.kind, false);
  return gfc_get_int_expr (gfc_default_integer_kind, &x->where,
			   gfc_real_kinds[i].max_exponent);
}


gfc_expr *
gfc_simplify_minexponent (gfc_expr *x)
{
  int i = gfc_validate_kind (BT_REAL, x->ts.kind, false);
  return gfc_get_int_expr (gfc_default_integer_kind, &x->where,
			   gfc_real_kinds[i].min_exponent);
}


gfc_expr *
gfc_simplify_mod (gfc_expr *a, gfc_expr *p)
{
  gfc_expr *result;
  int kind;

  if (a->expr_type != EXPR_CONSTANT || p->expr_type != EXPR_CONSTANT)
    return NULL;

  kind = a->ts.kind > p->ts.kind ? a->ts.kind : p->ts.kind;
  result = gfc_get_constant_expr (a->ts.type, kind, &a->where);

  switch (a->ts.type)
    {
      case BT_INTEGER:
	if (mpz_cmp_ui (p->value.integer, 0) == 0)
	  {
	    /* Result is processor-dependent.  */
	    gfc_error ("Second argument MOD at %L is zero", &a->where);
	    gfc_free_expr (result);
	    return &gfc_bad_expr;
	  }
	mpz_tdiv_r (result->value.integer, a->value.integer, p->value.integer);
	break;

      case BT_REAL:
	if (mpfr_cmp_ui (p->value.real, 0) == 0)
	  {
	    /* Result is processor-dependent.  */
	    gfc_error ("Second argument of MOD at %L is zero", &p->where);
	    gfc_free_expr (result);
	    return &gfc_bad_expr;
	  }

	gfc_set_model_kind (kind);
	mpfr_fmod (result->value.real, a->value.real, p->value.real,
		   GFC_RND_MODE);
	break;

      default:
	gfc_internal_error ("gfc_simplify_mod(): Bad arguments");
    }

  return range_check (result, "MOD");
}


gfc_expr *
gfc_simplify_modulo (gfc_expr *a, gfc_expr *p)
{
  gfc_expr *result;
  int kind;

  if (a->expr_type != EXPR_CONSTANT || p->expr_type != EXPR_CONSTANT)
    return NULL;

  kind = a->ts.kind > p->ts.kind ? a->ts.kind : p->ts.kind;
  result = gfc_get_constant_expr (a->ts.type, kind, &a->where);

  switch (a->ts.type)
    {
      case BT_INTEGER:
	if (mpz_cmp_ui (p->value.integer, 0) == 0)
	  {
	    /* Result is processor-dependent. This processor just opts
	      to not handle it at all.  */
	    gfc_error ("Second argument of MODULO at %L is zero", &a->where);
	    gfc_free_expr (result);
	    return &gfc_bad_expr;
	  }
	mpz_fdiv_r (result->value.integer, a->value.integer, p->value.integer);

	break;

      case BT_REAL:
	if (mpfr_cmp_ui (p->value.real, 0) == 0)
	  {
	    /* Result is processor-dependent.  */
	    gfc_error ("Second argument of MODULO at %L is zero", &p->where);
	    gfc_free_expr (result);
	    return &gfc_bad_expr;
	  }

	gfc_set_model_kind (kind);
	mpfr_fmod (result->value.real, a->value.real, p->value.real,
		   GFC_RND_MODE);
	if (mpfr_cmp_ui (result->value.real, 0) != 0)
	  {
	    if (mpfr_signbit (a->value.real) != mpfr_signbit (p->value.real))
	      mpfr_add (result->value.real, result->value.real, p->value.real,
			GFC_RND_MODE);
	  }
	else
	  mpfr_copysign (result->value.real, result->value.real,
			 p->value.real, GFC_RND_MODE);
	break;

      default:
	gfc_internal_error ("gfc_simplify_modulo(): Bad arguments");
    }

  return range_check (result, "MODULO");
}


gfc_expr *
gfc_simplify_nearest (gfc_expr *x, gfc_expr *s)
{
  gfc_expr *result;
  mp_exp_t emin, emax;
  int kind;

  if (x->expr_type != EXPR_CONSTANT || s->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_copy_expr (x);

  /* Save current values of emin and emax.  */
  emin = mpfr_get_emin ();
  emax = mpfr_get_emax ();

  /* Set emin and emax for the current model number.  */
  kind = gfc_validate_kind (BT_REAL, x->ts.kind, 0);
  mpfr_set_emin ((mp_exp_t) gfc_real_kinds[kind].min_exponent -
		mpfr_get_prec(result->value.real) + 1);
  mpfr_set_emax ((mp_exp_t) gfc_real_kinds[kind].max_exponent - 1);
  mpfr_check_range (result->value.real, 0, GMP_RNDU);

  if (mpfr_sgn (s->value.real) > 0)
    {
      mpfr_nextabove (result->value.real);
      mpfr_subnormalize (result->value.real, 0, GMP_RNDU);
    }
  else
    {
      mpfr_nextbelow (result->value.real);
      mpfr_subnormalize (result->value.real, 0, GMP_RNDD);
    }

  mpfr_set_emin (emin);
  mpfr_set_emax (emax);

  /* Only NaN can occur. Do not use range check as it gives an
     error for denormal numbers.  */
  if (mpfr_nan_p (result->value.real) && flag_range_check)
    {
      gfc_error ("Result of NEAREST is NaN at %L", &result->where);
      gfc_free_expr (result);
      return &gfc_bad_expr;
    }

  return result;
}


static gfc_expr *
simplify_nint (const char *name, gfc_expr *e, gfc_expr *k)
{
  gfc_expr *itrunc, *result;
  int kind;

  kind = get_kind (BT_INTEGER, k, name, gfc_default_integer_kind);
  if (kind == -1)
    return &gfc_bad_expr;

  if (e->expr_type != EXPR_CONSTANT)
    return NULL;

  itrunc = gfc_copy_expr (e);
  mpfr_round (itrunc->value.real, e->value.real);

  result = gfc_get_constant_expr (BT_INTEGER, kind, &e->where);
  gfc_mpfr_to_mpz (result->value.integer, itrunc->value.real, &e->where);

  gfc_free_expr (itrunc);

  return range_check (result, name);
}


gfc_expr *
gfc_simplify_new_line (gfc_expr *e)
{
  gfc_expr *result;

  result = gfc_get_character_expr (e->ts.kind, &e->where, NULL, 1);
  result->value.character.string[0] = '\n';

  return result;
}


gfc_expr *
gfc_simplify_nint (gfc_expr *e, gfc_expr *k)
{
  return simplify_nint ("NINT", e, k);
}


gfc_expr *
gfc_simplify_idnint (gfc_expr *e)
{
  return simplify_nint ("IDNINT", e, NULL);
}


static gfc_expr *
add_squared (gfc_expr *result, gfc_expr *e)
{
  mpfr_t tmp;

  gcc_assert (e->ts.type == BT_REAL && e->expr_type == EXPR_CONSTANT);
  gcc_assert (result->ts.type == BT_REAL
	      && result->expr_type == EXPR_CONSTANT);

  gfc_set_model_kind (result->ts.kind);
  mpfr_init (tmp);
  mpfr_pow_ui (tmp, e->value.real, 2, GFC_RND_MODE);
  mpfr_add (result->value.real, result->value.real, tmp,
	    GFC_RND_MODE);
  mpfr_clear (tmp);

  return result;
}


static gfc_expr *
do_sqrt (gfc_expr *result, gfc_expr *e)
{
  gcc_assert (e->ts.type == BT_REAL && e->expr_type == EXPR_CONSTANT);
  gcc_assert (result->ts.type == BT_REAL
	      && result->expr_type == EXPR_CONSTANT);

  mpfr_set (result->value.real, e->value.real, GFC_RND_MODE);
  mpfr_sqrt (result->value.real, result->value.real, GFC_RND_MODE);
  return result;
}


gfc_expr *
gfc_simplify_norm2 (gfc_expr *e, gfc_expr *dim)
{
  gfc_expr *result;

  if (!is_constant_array_expr (e)
      || (dim != NULL && !gfc_is_constant_expr (dim)))
    return NULL;

  result = transformational_result (e, dim, e->ts.type, e->ts.kind, &e->where);
  init_result_expr (result, 0, NULL);

  if (!dim || e->rank == 1)
    {
      result = simplify_transformation_to_scalar (result, e, NULL,
						  add_squared);
      mpfr_sqrt (result->value.real, result->value.real, GFC_RND_MODE);
    }
  else
    result = simplify_transformation_to_array (result, e, dim, NULL,
					       add_squared, &do_sqrt);

  return result;
}


gfc_expr *
gfc_simplify_not (gfc_expr *e)
{
  gfc_expr *result;

  if (e->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_get_constant_expr (e->ts.type, e->ts.kind, &e->where);
  mpz_com (result->value.integer, e->value.integer);

  return range_check (result, "NOT");
}


gfc_expr *
gfc_simplify_null (gfc_expr *mold)
{
  gfc_expr *result;

  if (mold)
    {
      result = gfc_copy_expr (mold);
      result->expr_type = EXPR_NULL;
    }
  else
    result = gfc_get_null_expr (NULL);

  return result;
}


gfc_expr *
gfc_simplify_num_images (gfc_expr *distance ATTRIBUTE_UNUSED, gfc_expr *failed)
{
  gfc_expr *result;

  if (flag_coarray == GFC_FCOARRAY_NONE)
    {
      gfc_fatal_error ("Coarrays disabled at %C, use %<-fcoarray=%> to enable");
      return &gfc_bad_expr;
    }

  if (flag_coarray != GFC_FCOARRAY_SINGLE)
    return NULL;

  if (failed && failed->expr_type != EXPR_CONSTANT)
    return NULL;

  /* FIXME: gfc_current_locus is wrong.  */
  result = gfc_get_constant_expr (BT_INTEGER, gfc_default_integer_kind,
				  &gfc_current_locus);

  if (failed && failed->value.logical != 0)
    mpz_set_si (result->value.integer, 0);
  else
    mpz_set_si (result->value.integer, 1);

  return result;
}


gfc_expr *
gfc_simplify_or (gfc_expr *x, gfc_expr *y)
{
  gfc_expr *result;
  int kind;

  if (x->expr_type != EXPR_CONSTANT || y->expr_type != EXPR_CONSTANT)
    return NULL;

  kind = x->ts.kind > y->ts.kind ? x->ts.kind : y->ts.kind;

  switch (x->ts.type)
    {
      case BT_INTEGER:
	result = gfc_get_constant_expr (BT_INTEGER, kind, &x->where);
	mpz_ior (result->value.integer, x->value.integer, y->value.integer);
	return range_check (result, "OR");

      case BT_LOGICAL:
	return gfc_get_logical_expr (kind, &x->where,
				     x->value.logical || y->value.logical);
      default:
	gcc_unreachable();
    }
}


gfc_expr *
gfc_simplify_pack (gfc_expr *array, gfc_expr *mask, gfc_expr *vector)
{
  gfc_expr *result;
  gfc_constructor *array_ctor, *mask_ctor, *vector_ctor;

  if (!is_constant_array_expr (array)
      || !is_constant_array_expr (vector)
      || (!gfc_is_constant_expr (mask)
          && !is_constant_array_expr (mask)))
    return NULL;

  result = gfc_get_array_expr (array->ts.type, array->ts.kind, &array->where);
  if (array->ts.type == BT_DERIVED)
    result->ts.u.derived = array->ts.u.derived;

  array_ctor = gfc_constructor_first (array->value.constructor);
  vector_ctor = vector
		  ? gfc_constructor_first (vector->value.constructor)
		  : NULL;

  if (mask->expr_type == EXPR_CONSTANT
      && mask->value.logical)
    {
      /* Copy all elements of ARRAY to RESULT.  */
      while (array_ctor)
	{
	  gfc_constructor_append_expr (&result->value.constructor,
				       gfc_copy_expr (array_ctor->expr),
				       NULL);

	  array_ctor = gfc_constructor_next (array_ctor);
	  vector_ctor = gfc_constructor_next (vector_ctor);
	}
    }
  else if (mask->expr_type == EXPR_ARRAY)
    {
      /* Copy only those elements of ARRAY to RESULT whose
	 MASK equals .TRUE..  */
      mask_ctor = gfc_constructor_first (mask->value.constructor);
      while (mask_ctor)
	{
	  if (mask_ctor->expr->value.logical)
	    {
	      gfc_constructor_append_expr (&result->value.constructor,
					   gfc_copy_expr (array_ctor->expr),
					   NULL);
	      vector_ctor = gfc_constructor_next (vector_ctor);
	    }

	  array_ctor = gfc_constructor_next (array_ctor);
	  mask_ctor = gfc_constructor_next (mask_ctor);
	}
    }

  /* Append any left-over elements from VECTOR to RESULT.  */
  while (vector_ctor)
    {
      gfc_constructor_append_expr (&result->value.constructor,
				   gfc_copy_expr (vector_ctor->expr),
				   NULL);
      vector_ctor = gfc_constructor_next (vector_ctor);
    }

  result->shape = gfc_get_shape (1);
  gfc_array_size (result, &result->shape[0]);

  if (array->ts.type == BT_CHARACTER)
    result->ts.u.cl = array->ts.u.cl;

  return result;
}


static gfc_expr *
do_xor (gfc_expr *result, gfc_expr *e)
{
  gcc_assert (e->ts.type == BT_LOGICAL && e->expr_type == EXPR_CONSTANT);
  gcc_assert (result->ts.type == BT_LOGICAL
	      && result->expr_type == EXPR_CONSTANT);

  result->value.logical = result->value.logical != e->value.logical;
  return result;
}



gfc_expr *
gfc_simplify_parity (gfc_expr *e, gfc_expr *dim)
{
  return simplify_transformation (e, dim, NULL, 0, do_xor);
}


gfc_expr *
gfc_simplify_popcnt (gfc_expr *e)
{
  int res, k;
  mpz_t x;

  if (e->expr_type != EXPR_CONSTANT)
    return NULL;

  k = gfc_validate_kind (e->ts.type, e->ts.kind, false);

  /* Convert argument to unsigned, then count the '1' bits.  */
  mpz_init_set (x, e->value.integer);
  convert_mpz_to_unsigned (x, gfc_integer_kinds[k].bit_size);
  res = mpz_popcount (x);
  mpz_clear (x);

  return gfc_get_int_expr (gfc_default_integer_kind, &e->where, res);
}


gfc_expr *
gfc_simplify_poppar (gfc_expr *e)
{
  gfc_expr *popcnt;
  const char *s;
  int i;

  if (e->expr_type != EXPR_CONSTANT)
    return NULL;

  popcnt = gfc_simplify_popcnt (e);
  gcc_assert (popcnt);

  s = gfc_extract_int (popcnt, &i);
  gcc_assert (!s);

  return gfc_get_int_expr (gfc_default_integer_kind, &e->where, i % 2);
}


gfc_expr *
gfc_simplify_precision (gfc_expr *e)
{
  int i = gfc_validate_kind (e->ts.type, e->ts.kind, false);
  return gfc_get_int_expr (gfc_default_integer_kind, &e->where,
			   gfc_real_kinds[i].precision);
}


gfc_expr *
gfc_simplify_product (gfc_expr *array, gfc_expr *dim, gfc_expr *mask)
{
  return simplify_transformation (array, dim, mask, 1, gfc_multiply);
}


gfc_expr *
gfc_simplify_radix (gfc_expr *e)
{
  int i;
  i = gfc_validate_kind (e->ts.type, e->ts.kind, false);

  switch (e->ts.type)
    {
      case BT_INTEGER:
	i = gfc_integer_kinds[i].radix;
	break;

      case BT_REAL:
	i = gfc_real_kinds[i].radix;
	break;

      default:
	gcc_unreachable ();
    }

  return gfc_get_int_expr (gfc_default_integer_kind, &e->where, i);
}


gfc_expr *
gfc_simplify_range (gfc_expr *e)
{
  int i;
  i = gfc_validate_kind (e->ts.type, e->ts.kind, false);

  switch (e->ts.type)
    {
      case BT_INTEGER:
	i = gfc_integer_kinds[i].range;
	break;

      case BT_REAL:
      case BT_COMPLEX:
	i = gfc_real_kinds[i].range;
	break;

      default:
	gcc_unreachable ();
    }

  return gfc_get_int_expr (gfc_default_integer_kind, &e->where, i);
}


gfc_expr *
gfc_simplify_rank (gfc_expr *e)
{
  /* Assumed rank.  */
  if (e->rank == -1)
    return NULL;

  return gfc_get_int_expr (gfc_default_integer_kind, &e->where, e->rank);
}


gfc_expr *
gfc_simplify_real (gfc_expr *e, gfc_expr *k)
{
  gfc_expr *result = NULL;
  int kind;

  if (e->ts.type == BT_COMPLEX)
    kind = get_kind (BT_REAL, k, "REAL", e->ts.kind);
  else
    kind = get_kind (BT_REAL, k, "REAL", gfc_default_real_kind);

  if (kind == -1)
    return &gfc_bad_expr;

  if (e->expr_type != EXPR_CONSTANT)
    return NULL;

  if (convert_boz (e, kind) == &gfc_bad_expr)
    return &gfc_bad_expr;

  result = gfc_convert_constant (e, BT_REAL, kind);
  if (result == &gfc_bad_expr)
    return &gfc_bad_expr;

  return range_check (result, "REAL");
}


gfc_expr *
gfc_simplify_realpart (gfc_expr *e)
{
  gfc_expr *result;

  if (e->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_get_constant_expr (BT_REAL, e->ts.kind, &e->where);
  mpc_real (result->value.real, e->value.complex, GFC_RND_MODE);

  return range_check (result, "REALPART");
}

gfc_expr *
gfc_simplify_repeat (gfc_expr *e, gfc_expr *n)
{
  gfc_expr *result;
  int i, j, len, ncop, nlen;
  mpz_t ncopies;
  bool have_length = false;

  /* If NCOPIES isn't a constant, there's nothing we can do.  */
  if (n->expr_type != EXPR_CONSTANT)
    return NULL;

  /* If NCOPIES is negative, it's an error.  */
  if (mpz_sgn (n->value.integer) < 0)
    {
      gfc_error ("Argument NCOPIES of REPEAT intrinsic is negative at %L",
		 &n->where);
      return &gfc_bad_expr;
    }

  /* If we don't know the character length, we can do no more.  */
  if (e->ts.u.cl && e->ts.u.cl->length
	&& e->ts.u.cl->length->expr_type == EXPR_CONSTANT)
    {
      len = mpz_get_si (e->ts.u.cl->length->value.integer);
      have_length = true;
    }
  else if (e->expr_type == EXPR_CONSTANT
	     && (e->ts.u.cl == NULL || e->ts.u.cl->length == NULL))
    {
      len = e->value.character.length;
    }
  else
    return NULL;

  /* If the source length is 0, any value of NCOPIES is valid
     and everything behaves as if NCOPIES == 0.  */
  mpz_init (ncopies);
  if (len == 0)
    mpz_set_ui (ncopies, 0);
  else
    mpz_set (ncopies, n->value.integer);

  /* Check that NCOPIES isn't too large.  */
  if (len)
    {
      mpz_t max, mlen;
      int i;

      /* Compute the maximum value allowed for NCOPIES: huge(cl) / len.  */
      mpz_init (max);
      i = gfc_validate_kind (BT_INTEGER, gfc_charlen_int_kind, false);

      if (have_length)
	{
	  mpz_tdiv_q (max, gfc_integer_kinds[i].huge,
		      e->ts.u.cl->length->value.integer);
	}
      else
	{
	  mpz_init_set_si (mlen, len);
	  mpz_tdiv_q (max, gfc_integer_kinds[i].huge, mlen);
	  mpz_clear (mlen);
	}

      /* The check itself.  */
      if (mpz_cmp (ncopies, max) > 0)
	{
	  mpz_clear (max);
	  mpz_clear (ncopies);
	  gfc_error ("Argument NCOPIES of REPEAT intrinsic is too large at %L",
		     &n->where);
	  return &gfc_bad_expr;
	}

      mpz_clear (max);
    }
  mpz_clear (ncopies);

  /* For further simplification, we need the character string to be
     constant.  */
  if (e->expr_type != EXPR_CONSTANT)
    return NULL;

  if (len ||
      (e->ts.u.cl->length &&
       mpz_sgn (e->ts.u.cl->length->value.integer) != 0))
    {
      const char *res = gfc_extract_int (n, &ncop);
      gcc_assert (res == NULL);
    }
  else
    ncop = 0;

  if (ncop == 0)
    return gfc_get_character_expr (e->ts.kind, &e->where, NULL, 0);

  len = e->value.character.length;
  nlen = ncop * len;

  result = gfc_get_character_expr (e->ts.kind, &e->where, NULL, nlen);
  for (i = 0; i < ncop; i++)
    for (j = 0; j < len; j++)
      result->value.character.string[j+i*len]= e->value.character.string[j];

  result->value.character.string[nlen] = '\0';	/* For debugger */
  return result;
}


/* This one is a bear, but mainly has to do with shuffling elements.  */

gfc_expr *
gfc_simplify_reshape (gfc_expr *source, gfc_expr *shape_exp,
		      gfc_expr *pad, gfc_expr *order_exp)
{
  int order[GFC_MAX_DIMENSIONS], shape[GFC_MAX_DIMENSIONS];
  int i, rank, npad, x[GFC_MAX_DIMENSIONS];
  mpz_t index, size;
  unsigned long j;
  size_t nsource;
  gfc_expr *e, *result;

  /* Check that argument expression types are OK.  */
  if (!is_constant_array_expr (source)
      || !is_constant_array_expr (shape_exp)
      || !is_constant_array_expr (pad)
      || !is_constant_array_expr (order_exp))
    return NULL;

  if (source->shape == NULL)
    return NULL;

  /* Proceed with simplification, unpacking the array.  */

  mpz_init (index);
  rank = 0;

  for (;;)
    {
      e = gfc_constructor_lookup_expr (shape_exp->value.constructor, rank);
      if (e == NULL)
	break;

      gfc_extract_int (e, &shape[rank]);

      gcc_assert (rank >= 0 && rank < GFC_MAX_DIMENSIONS);
      gcc_assert (shape[rank] >= 0);

      rank++;
    }

  gcc_assert (rank > 0);

  /* Now unpack the order array if present.  */
  if (order_exp == NULL)
    {
      for (i = 0; i < rank; i++)
	order[i] = i;
    }
  else
    {
      for (i = 0; i < rank; i++)
	x[i] = 0;

      for (i = 0; i < rank; i++)
	{
	  e = gfc_constructor_lookup_expr (order_exp->value.constructor, i);
	  gcc_assert (e);

	  gfc_extract_int (e, &order[i]);

	  gcc_assert (order[i] >= 1 && order[i] <= rank);
	  order[i]--;
	  gcc_assert (x[order[i]] == 0);
	  x[order[i]] = 1;
	}
    }

  /* Count the elements in the source and padding arrays.  */

  npad = 0;
  if (pad != NULL)
    {
      gfc_array_size (pad, &size);
      npad = mpz_get_ui (size);
      mpz_clear (size);
    }

  gfc_array_size (source, &size);
  nsource = mpz_get_ui (size);
  mpz_clear (size);

  /* If it weren't for that pesky permutation we could just loop
     through the source and round out any shortage with pad elements.
     But no, someone just had to have the compiler do something the
     user should be doing.  */

  for (i = 0; i < rank; i++)
    x[i] = 0;

  result = gfc_get_array_expr (source->ts.type, source->ts.kind,
			       &source->where);
  if (source->ts.type == BT_DERIVED)
    result->ts.u.derived = source->ts.u.derived;
  result->rank = rank;
  result->shape = gfc_get_shape (rank);
  for (i = 0; i < rank; i++)
    mpz_init_set_ui (result->shape[i], shape[i]);

  while (nsource > 0 || npad > 0)
    {
      /* Figure out which element to extract.  */
      mpz_set_ui (index, 0);

      for (i = rank - 1; i >= 0; i--)
	{
	  mpz_add_ui (index, index, x[order[i]]);
	  if (i != 0)
	    mpz_mul_ui (index, index, shape[order[i - 1]]);
	}

      if (mpz_cmp_ui (index, INT_MAX) > 0)
	gfc_internal_error ("Reshaped array too large at %C");

      j = mpz_get_ui (index);

      if (j < nsource)
	e = gfc_constructor_lookup_expr (source->value.constructor, j);
      else
	{
	  if (npad <= 0)
	    {
	      mpz_clear (index);
	      return NULL;
	    }
	  j = j - nsource;
	  j = j % npad;
	  e = gfc_constructor_lookup_expr (pad->value.constructor, j);
	}
      gcc_assert (e);

      gfc_constructor_append_expr (&result->value.constructor,
				   gfc_copy_expr (e), &e->where);

      /* Calculate the next element.  */
      i = 0;

inc:
      if (++x[i] < shape[i])
	continue;
      x[i++] = 0;
      if (i < rank)
	goto inc;

      break;
    }

  mpz_clear (index);

  return result;
}


gfc_expr *
gfc_simplify_rrspacing (gfc_expr *x)
{
  gfc_expr *result;
  int i;
  long int e, p;

  if (x->expr_type != EXPR_CONSTANT)
    return NULL;

  i = gfc_validate_kind (x->ts.type, x->ts.kind, false);

  result = gfc_get_constant_expr (BT_REAL, x->ts.kind, &x->where);

  /* RRSPACING(+/- 0.0) = 0.0  */
  if (mpfr_zero_p (x->value.real))
    {
      mpfr_set_ui (result->value.real, 0, GFC_RND_MODE);
      return result;
    }

  /* RRSPACING(inf) = NaN  */
  if (mpfr_inf_p (x->value.real))
    {
      mpfr_set_nan (result->value.real);
      return result;
    }

  /* RRSPACING(NaN) = same NaN  */
  if (mpfr_nan_p (x->value.real))
    {
      mpfr_set (result->value.real, x->value.real, GFC_RND_MODE);
      return result;
    }

  /* | x * 2**(-e) | * 2**p.  */
  mpfr_abs (result->value.real, x->value.real, GFC_RND_MODE);
  e = - (long int) mpfr_get_exp (x->value.real);
  mpfr_mul_2si (result->value.real, result->value.real, e, GFC_RND_MODE);

  p = (long int) gfc_real_kinds[i].digits;
  mpfr_mul_2si (result->value.real, result->value.real, p, GFC_RND_MODE);

  return range_check (result, "RRSPACING");
}


gfc_expr *
gfc_simplify_scale (gfc_expr *x, gfc_expr *i)
{
  int k, neg_flag, power, exp_range;
  mpfr_t scale, radix;
  gfc_expr *result;

  if (x->expr_type != EXPR_CONSTANT || i->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_get_constant_expr (BT_REAL, x->ts.kind, &x->where);

  if (mpfr_zero_p (x->value.real))
    {
      mpfr_set_ui (result->value.real, 0, GFC_RND_MODE);
      return result;
    }

  k = gfc_validate_kind (BT_REAL, x->ts.kind, false);

  exp_range = gfc_real_kinds[k].max_exponent - gfc_real_kinds[k].min_exponent;

  /* This check filters out values of i that would overflow an int.  */
  if (mpz_cmp_si (i->value.integer, exp_range + 2) > 0
      || mpz_cmp_si (i->value.integer, -exp_range - 2) < 0)
    {
      gfc_error ("Result of SCALE overflows its kind at %L", &result->where);
      gfc_free_expr (result);
      return &gfc_bad_expr;
    }

  /* Compute scale = radix ** power.  */
  power = mpz_get_si (i->value.integer);

  if (power >= 0)
    neg_flag = 0;
  else
    {
      neg_flag = 1;
      power = -power;
    }

  gfc_set_model_kind (x->ts.kind);
  mpfr_init (scale);
  mpfr_init (radix);
  mpfr_set_ui (radix, gfc_real_kinds[k].radix, GFC_RND_MODE);
  mpfr_pow_ui (scale, radix, power, GFC_RND_MODE);

  if (neg_flag)
    mpfr_div (result->value.real, x->value.real, scale, GFC_RND_MODE);
  else
    mpfr_mul (result->value.real, x->value.real, scale, GFC_RND_MODE);

  mpfr_clears (scale, radix, NULL);

  return range_check (result, "SCALE");
}


/* Variants of strspn and strcspn that operate on wide characters.  */

static size_t
wide_strspn (const gfc_char_t *s1, const gfc_char_t *s2)
{
  size_t i = 0;
  const gfc_char_t *c;

  while (s1[i])
    {
      for (c = s2; *c; c++)
	{
	  if (s1[i] == *c)
	    break;
	}
      if (*c == '\0')
	break;
      i++;
    }

  return i;
}

static size_t
wide_strcspn (const gfc_char_t *s1, const gfc_char_t *s2)
{
  size_t i = 0;
  const gfc_char_t *c;

  while (s1[i])
    {
      for (c = s2; *c; c++)
	{
	  if (s1[i] == *c)
	    break;
	}
      if (*c)
	break;
      i++;
    }

  return i;
}


gfc_expr *
gfc_simplify_scan (gfc_expr *e, gfc_expr *c, gfc_expr *b, gfc_expr *kind)
{
  gfc_expr *result;
  int back;
  size_t i;
  size_t indx, len, lenc;
  int k = get_kind (BT_INTEGER, kind, "SCAN", gfc_default_integer_kind);

  if (k == -1)
    return &gfc_bad_expr;

  if (e->expr_type != EXPR_CONSTANT || c->expr_type != EXPR_CONSTANT
      || ( b != NULL && b->expr_type !=  EXPR_CONSTANT))
    return NULL;

  if (b != NULL && b->value.logical != 0)
    back = 1;
  else
    back = 0;

  len = e->value.character.length;
  lenc = c->value.character.length;

  if (len == 0 || lenc == 0)
    {
      indx = 0;
    }
  else
    {
      if (back == 0)
	{
	  indx = wide_strcspn (e->value.character.string,
			       c->value.character.string) + 1;
	  if (indx > len)
	    indx = 0;
	}
      else
	{
	  i = 0;
	  for (indx = len; indx > 0; indx--)
	    {
	      for (i = 0; i < lenc; i++)
		{
		  if (c->value.character.string[i]
		      == e->value.character.string[indx - 1])
		    break;
		}
	      if (i < lenc)
		break;
	    }
	}
    }

  result = gfc_get_int_expr (k, &e->where, indx);
  return range_check (result, "SCAN");
}


gfc_expr *
gfc_simplify_selected_char_kind (gfc_expr *e)
{
  int kind;

  if (e->expr_type != EXPR_CONSTANT)
    return NULL;

  if (gfc_compare_with_Cstring (e, "ascii", false) == 0
      || gfc_compare_with_Cstring (e, "default", false) == 0)
    kind = 1;
  else if (gfc_compare_with_Cstring (e, "iso_10646", false) == 0)
    kind = 4;
  else
    kind = -1;

  return gfc_get_int_expr (gfc_default_integer_kind, &e->where, kind);
}


gfc_expr *
gfc_simplify_selected_int_kind (gfc_expr *e)
{
  int i, kind, range;

  if (e->expr_type != EXPR_CONSTANT || gfc_extract_int (e, &range) != NULL)
    return NULL;

  kind = INT_MAX;

  for (i = 0; gfc_integer_kinds[i].kind != 0; i++)
    if (gfc_integer_kinds[i].range >= range
	&& gfc_integer_kinds[i].kind < kind)
      kind = gfc_integer_kinds[i].kind;

  if (kind == INT_MAX)
    kind = -1;

  return gfc_get_int_expr (gfc_default_integer_kind, &e->where, kind);
}


gfc_expr *
gfc_simplify_selected_real_kind (gfc_expr *p, gfc_expr *q, gfc_expr *rdx)
{
  int range, precision, radix, i, kind, found_precision, found_range,
      found_radix;
  locus *loc = &gfc_current_locus;

  if (p == NULL)
    precision = 0;
  else
    {
      if (p->expr_type != EXPR_CONSTANT
	  || gfc_extract_int (p, &precision) != NULL)
	return NULL;
      loc = &p->where;
    }

  if (q == NULL)
    range = 0;
  else
    {
      if (q->expr_type != EXPR_CONSTANT
	  || gfc_extract_int (q, &range) != NULL)
	return NULL;

      if (!loc)
	loc = &q->where;
    }

  if (rdx == NULL)
    radix = 0;
  else
    {
      if (rdx->expr_type != EXPR_CONSTANT
	  || gfc_extract_int (rdx, &radix) != NULL)
	return NULL;

      if (!loc)
	loc = &rdx->where;
    }

  kind = INT_MAX;
  found_precision = 0;
  found_range = 0;
  found_radix = 0;

  for (i = 0; gfc_real_kinds[i].kind != 0; i++)
    {
      if (gfc_real_kinds[i].precision >= precision)
	found_precision = 1;

      if (gfc_real_kinds[i].range >= range)
	found_range = 1;

      if (radix == 0 || gfc_real_kinds[i].radix == radix)
	found_radix = 1;

      if (gfc_real_kinds[i].precision >= precision
	  && gfc_real_kinds[i].range >= range
	  && (radix == 0 || gfc_real_kinds[i].radix == radix)
	  && gfc_real_kinds[i].kind < kind)
	kind = gfc_real_kinds[i].kind;
    }

  if (kind == INT_MAX)
    {
      if (found_radix && found_range && !found_precision)
	kind = -1;
      else if (found_radix && found_precision && !found_range)
	kind = -2;
      else if (found_radix && !found_precision && !found_range)
	kind = -3;
      else if (found_radix)
	kind = -4;
      else
	kind = -5;
    }

  return gfc_get_int_expr (gfc_default_integer_kind, loc, kind);
}


gfc_expr *
gfc_simplify_set_exponent (gfc_expr *x, gfc_expr *i)
{
  gfc_expr *result;
  mpfr_t exp, absv, log2, pow2, frac;
  unsigned long exp2;

  if (x->expr_type != EXPR_CONSTANT || i->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_get_constant_expr (BT_REAL, x->ts.kind, &x->where);

  /* SET_EXPONENT (+/-0.0, I) = +/- 0.0
     SET_EXPONENT (NaN) = same NaN  */
  if (mpfr_zero_p (x->value.real) || mpfr_nan_p (x->value.real))
    {
      mpfr_set (result->value.real, x->value.real, GFC_RND_MODE);
      return result;
    }

  /* SET_EXPONENT (inf) = NaN  */
  if (mpfr_inf_p (x->value.real))
    {
      mpfr_set_nan (result->value.real);
      return result;
    }

  gfc_set_model_kind (x->ts.kind);
  mpfr_init (absv);
  mpfr_init (log2);
  mpfr_init (exp);
  mpfr_init (pow2);
  mpfr_init (frac);

  mpfr_abs (absv, x->value.real, GFC_RND_MODE);
  mpfr_log2 (log2, absv, GFC_RND_MODE);

  mpfr_trunc (log2, log2);
  mpfr_add_ui (exp, log2, 1, GFC_RND_MODE);

  /* Old exponent value, and fraction.  */
  mpfr_ui_pow (pow2, 2, exp, GFC_RND_MODE);

  mpfr_div (frac, absv, pow2, GFC_RND_MODE);

  /* New exponent.  */
  exp2 = (unsigned long) mpz_get_d (i->value.integer);
  mpfr_mul_2exp (result->value.real, frac, exp2, GFC_RND_MODE);

  mpfr_clears (absv, log2, pow2, frac, NULL);

  return range_check (result, "SET_EXPONENT");
}


gfc_expr *
gfc_simplify_shape (gfc_expr *source, gfc_expr *kind)
{
  mpz_t shape[GFC_MAX_DIMENSIONS];
  gfc_expr *result, *e, *f;
  gfc_array_ref *ar;
  int n;
  bool t;
  int k = get_kind (BT_INTEGER, kind, "SHAPE", gfc_default_integer_kind);

  if (source->rank == -1)
    return NULL;

  result = gfc_get_array_expr (BT_INTEGER, k, &source->where);

  if (source->rank == 0)
    return result;

  if (source->expr_type == EXPR_VARIABLE)
    {
      ar = gfc_find_array_ref (source);
      t = gfc_array_ref_shape (ar, shape);
    }
  else if (source->shape)
    {
      t = true;
      for (n = 0; n < source->rank; n++)
	{
	  mpz_init (shape[n]);
	  mpz_set (shape[n], source->shape[n]);
	}
    }
  else
    t = false;

  for (n = 0; n < source->rank; n++)
    {
      e = gfc_get_constant_expr (BT_INTEGER, k, &source->where);

      if (t)
	mpz_set (e->value.integer, shape[n]);
      else
	{
	  mpz_set_ui (e->value.integer, n + 1);

	  f = simplify_size (source, e, k);
	  gfc_free_expr (e);
	  if (f == NULL)
	    {
	      gfc_free_expr (result);
	      return NULL;
	    }
	  else
	    e = f;
	}

      if (e == &gfc_bad_expr || range_check (e, "SHAPE") == &gfc_bad_expr)
	{
	  gfc_free_expr (result);
	  if (t)
	    gfc_clear_shape (shape, source->rank);
	  return &gfc_bad_expr;
	}

      gfc_constructor_append_expr (&result->value.constructor, e, NULL);
    }

  if (t)
    gfc_clear_shape (shape, source->rank);

  return result;
}


static gfc_expr *
simplify_size (gfc_expr *array, gfc_expr *dim, int k)
{
  mpz_t size;
  gfc_expr *return_value;
  int d;

  /* For unary operations, the size of the result is given by the size
     of the operand.  For binary ones, it's the size of the first operand
     unless it is scalar, then it is the size of the second.  */
  if (array->expr_type == EXPR_OP && !array->value.op.uop)
    {
      gfc_expr* replacement;
      gfc_expr* simplified;

      switch (array->value.op.op)
	{
	  /* Unary operations.  */
	  case INTRINSIC_NOT:
	  case INTRINSIC_UPLUS:
	  case INTRINSIC_UMINUS:
	  case INTRINSIC_PARENTHESES:
	    replacement = array->value.op.op1;
	    break;

	  /* Binary operations.  If any one of the operands is scalar, take
	     the other one's size.  If both of them are arrays, it does not
	     matter -- try to find one with known shape, if possible.  */
	  default:
	    if (array->value.op.op1->rank == 0)
	      replacement = array->value.op.op2;
	    else if (array->value.op.op2->rank == 0)
	      replacement = array->value.op.op1;
	    else
	      {
		simplified = simplify_size (array->value.op.op1, dim, k);
		if (simplified)
		  return simplified;

		replacement = array->value.op.op2;
	      }
	    break;
	}

      /* Try to reduce it directly if possible.  */
      simplified = simplify_size (replacement, dim, k);

      /* Otherwise, we build a new SIZE call.  This is hopefully at least
	 simpler than the original one.  */
      if (!simplified)
	{
	  gfc_expr *kind = gfc_get_int_expr (gfc_default_integer_kind, NULL, k);
	  simplified = gfc_build_intrinsic_call (gfc_current_ns,
						 GFC_ISYM_SIZE, "size",
						 array->where, 3,
						 gfc_copy_expr (replacement),
						 gfc_copy_expr (dim),
						 kind);
	}
      return simplified;
    }

  if (dim == NULL)
    {
      if (!gfc_array_size (array, &size))
	return NULL;
    }
  else
    {
      if (dim->expr_type != EXPR_CONSTANT)
	return NULL;

      d = mpz_get_ui (dim->value.integer) - 1;
      if (!gfc_array_dimen_size (array, d, &size))
	return NULL;
    }

  return_value = gfc_get_constant_expr (BT_INTEGER, k, &array->where);
  mpz_set (return_value->value.integer, size);
  mpz_clear (size);

  return return_value;
}


gfc_expr *
gfc_simplify_size (gfc_expr *array, gfc_expr *dim, gfc_expr *kind)
{
  gfc_expr *result;
  int k = get_kind (BT_INTEGER, kind, "SIZE", gfc_default_integer_kind);

  if (k == -1)
    return &gfc_bad_expr;

  result = simplify_size (array, dim, k);
  if (result == NULL || result == &gfc_bad_expr)
    return result;

  return range_check (result, "SIZE");
}


/* SIZEOF and C_SIZEOF return the size in bytes of an array element
   multiplied by the array size.  */

gfc_expr *
gfc_simplify_sizeof (gfc_expr *x)
{
  gfc_expr *result = NULL;
  mpz_t array_size;

  if (x->ts.type == BT_CLASS || x->ts.deferred)
    return NULL;

  if (x->ts.type == BT_CHARACTER
      && (!x->ts.u.cl || !x->ts.u.cl->length
	  || x->ts.u.cl->length->expr_type != EXPR_CONSTANT))
    return NULL;

  if (x->rank && x->expr_type != EXPR_ARRAY
      && !gfc_array_size (x, &array_size))
    return NULL;

  result = gfc_get_constant_expr (BT_INTEGER, gfc_index_integer_kind,
				  &x->where);
  mpz_set_si (result->value.integer, gfc_target_expr_size (x));

  return result;
}


/* STORAGE_SIZE returns the size in bits of a single array element.  */

gfc_expr *
gfc_simplify_storage_size (gfc_expr *x,
			   gfc_expr *kind)
{
  gfc_expr *result = NULL;
  int k;

  if (x->ts.type == BT_CLASS || x->ts.deferred)
    return NULL;

  if (x->ts.type == BT_CHARACTER && x->expr_type != EXPR_CONSTANT
      && (!x->ts.u.cl || !x->ts.u.cl->length
	  || x->ts.u.cl->length->expr_type != EXPR_CONSTANT))
    return NULL;

  k = get_kind (BT_INTEGER, kind, "STORAGE_SIZE", gfc_default_integer_kind);
  if (k == -1)
    return &gfc_bad_expr;

  result = gfc_get_constant_expr (BT_INTEGER, k, &x->where);

  mpz_set_si (result->value.integer, gfc_element_size (x));
  mpz_mul_ui (result->value.integer, result->value.integer, BITS_PER_UNIT);

  return range_check (result, "STORAGE_SIZE");
}


gfc_expr *
gfc_simplify_sign (gfc_expr *x, gfc_expr *y)
{
  gfc_expr *result;

  if (x->expr_type != EXPR_CONSTANT || y->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_get_constant_expr (x->ts.type, x->ts.kind, &x->where);

  switch (x->ts.type)
    {
      case BT_INTEGER:
	mpz_abs (result->value.integer, x->value.integer);
	if (mpz_sgn (y->value.integer) < 0)
	  mpz_neg (result->value.integer, result->value.integer);
	break;

      case BT_REAL:
	if (flag_sign_zero)
	  mpfr_copysign (result->value.real, x->value.real, y->value.real,
			GFC_RND_MODE);
	else
	  mpfr_setsign (result->value.real, x->value.real,
			mpfr_sgn (y->value.real) < 0 ? 1 : 0, GFC_RND_MODE);
	break;

      default:
	gfc_internal_error ("Bad type in gfc_simplify_sign");
    }

  return result;
}


gfc_expr *
gfc_simplify_sin (gfc_expr *x)
{
  gfc_expr *result;

  if (x->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_get_constant_expr (x->ts.type, x->ts.kind, &x->where);

  switch (x->ts.type)
    {
      case BT_REAL:
	mpfr_sin (result->value.real, x->value.real, GFC_RND_MODE);
	break;

      case BT_COMPLEX:
	gfc_set_model (x->value.real);
	mpc_sin (result->value.complex, x->value.complex, GFC_MPC_RND_MODE);
	break;

      default:
	gfc_internal_error ("in gfc_simplify_sin(): Bad type");
    }

  return range_check (result, "SIN");
}


gfc_expr *
gfc_simplify_sinh (gfc_expr *x)
{
  gfc_expr *result;

  if (x->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_get_constant_expr (x->ts.type, x->ts.kind, &x->where);

  switch (x->ts.type)
    {
      case BT_REAL:
	mpfr_sinh (result->value.real, x->value.real, GFC_RND_MODE);
	break;

      case BT_COMPLEX:
	mpc_sinh (result->value.complex, x->value.complex, GFC_MPC_RND_MODE);
	break;

      default:
	gcc_unreachable ();
    }

  return range_check (result, "SINH");
}


/* The argument is always a double precision real that is converted to
   single precision.  TODO: Rounding!  */

gfc_expr *
gfc_simplify_sngl (gfc_expr *a)
{
  gfc_expr *result;

  if (a->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_real2real (a, gfc_default_real_kind);
  return range_check (result, "SNGL");
}


gfc_expr *
gfc_simplify_spacing (gfc_expr *x)
{
  gfc_expr *result;
  int i;
  long int en, ep;

  if (x->expr_type != EXPR_CONSTANT)
    return NULL;

  i = gfc_validate_kind (x->ts.type, x->ts.kind, false);
  result = gfc_get_constant_expr (BT_REAL, x->ts.kind, &x->where);

  /* SPACING(+/- 0.0) = SPACING(TINY(0.0)) = TINY(0.0)  */
  if (mpfr_zero_p (x->value.real))
    {
      mpfr_set (result->value.real, gfc_real_kinds[i].tiny, GFC_RND_MODE);
      return result;
    }

  /* SPACING(inf) = NaN  */
  if (mpfr_inf_p (x->value.real))
    {
      mpfr_set_nan (result->value.real);
      return result;
    }

  /* SPACING(NaN) = same NaN  */
  if (mpfr_nan_p (x->value.real))
    {
      mpfr_set (result->value.real, x->value.real, GFC_RND_MODE);
      return result;
    }

  /* In the Fortran 95 standard, the result is b**(e - p) where b, e, and p
     are the radix, exponent of x, and precision.  This excludes the
     possibility of subnormal numbers.  Fortran 2003 states the result is
     b**max(e - p, emin - 1).  */

  ep = (long int) mpfr_get_exp (x->value.real) - gfc_real_kinds[i].digits;
  en = (long int) gfc_real_kinds[i].min_exponent - 1;
  en = en > ep ? en : ep;

  mpfr_set_ui (result->value.real, 1, GFC_RND_MODE);
  mpfr_mul_2si (result->value.real, result->value.real, en, GFC_RND_MODE);

  return range_check (result, "SPACING");
}


gfc_expr *
gfc_simplify_spread (gfc_expr *source, gfc_expr *dim_expr, gfc_expr *ncopies_expr)
{
  gfc_expr *result = NULL;
  int nelem, i, j, dim, ncopies;
  mpz_t size;

  if ((!gfc_is_constant_expr (source)
       && !is_constant_array_expr (source))
      || !gfc_is_constant_expr (dim_expr)
      || !gfc_is_constant_expr (ncopies_expr))
    return NULL;

  gcc_assert (dim_expr->ts.type == BT_INTEGER);
  gfc_extract_int (dim_expr, &dim);
  dim -= 1;   /* zero-base DIM */

  gcc_assert (ncopies_expr->ts.type == BT_INTEGER);
  gfc_extract_int (ncopies_expr, &ncopies);
  ncopies = MAX (ncopies, 0);

  /* Do not allow the array size to exceed the limit for an array
     constructor.  */
  if (source->expr_type == EXPR_ARRAY)
    {
      if (!gfc_array_size (source, &size))
	gfc_internal_error ("Failure getting length of a constant array.");
    }
  else
    mpz_init_set_ui (size, 1);

  nelem = mpz_get_si (size) * ncopies;
  if (nelem > flag_max_array_constructor)
    {
      if (gfc_current_ns->sym_root->n.sym->attr.flavor == FL_PARAMETER)
	{
	  gfc_error ("The number of elements (%d) in the array constructor "
		     "at %L requires an increase of the allowed %d upper "
		     "limit.  See %<-fmax-array-constructor%> option.",
		     nelem, &source->where, flag_max_array_constructor);
	  return &gfc_bad_expr;
	}
      else
	return NULL;
    }

  if (source->expr_type == EXPR_CONSTANT)
    {
      gcc_assert (dim == 0);

      result = gfc_get_array_expr (source->ts.type, source->ts.kind,
				   &source->where);
      if (source->ts.type == BT_DERIVED)
	result->ts.u.derived = source->ts.u.derived;
      result->rank = 1;
      result->shape = gfc_get_shape (result->rank);
      mpz_init_set_si (result->shape[0], ncopies);

      for (i = 0; i < ncopies; ++i)
        gfc_constructor_append_expr (&result->value.constructor,
				     gfc_copy_expr (source), NULL);
    }
  else if (source->expr_type == EXPR_ARRAY)
    {
      int offset, rstride[GFC_MAX_DIMENSIONS], extent[GFC_MAX_DIMENSIONS];
      gfc_constructor *source_ctor;

      gcc_assert (source->rank < GFC_MAX_DIMENSIONS);
      gcc_assert (dim >= 0 && dim <= source->rank);

      result = gfc_get_array_expr (source->ts.type, source->ts.kind,
				   &source->where);
      if (source->ts.type == BT_DERIVED)
	result->ts.u.derived = source->ts.u.derived;
      result->rank = source->rank + 1;
      result->shape = gfc_get_shape (result->rank);

      for (i = 0, j = 0; i < result->rank; ++i)
	{
	  if (i != dim)
	    mpz_init_set (result->shape[i], source->shape[j++]);
	  else
	    mpz_init_set_si (result->shape[i], ncopies);

	  extent[i] = mpz_get_si (result->shape[i]);
	  rstride[i] = (i == 0) ? 1 : rstride[i-1] * extent[i-1];
	}

      offset = 0;
      for (source_ctor = gfc_constructor_first (source->value.constructor);
           source_ctor; source_ctor = gfc_constructor_next (source_ctor))
	{
	  for (i = 0; i < ncopies; ++i)
	    gfc_constructor_insert_expr (&result->value.constructor,
					 gfc_copy_expr (source_ctor->expr),
					 NULL, offset + i * rstride[dim]);

	  offset += (dim == 0 ? ncopies : 1);
	}
    }
  else
    {
      gfc_error ("Simplification of SPREAD at %C not yet implemented");
      return &gfc_bad_expr;
    }

  if (source->ts.type == BT_CHARACTER)
    result->ts.u.cl = source->ts.u.cl;

  return result;
}


gfc_expr *
gfc_simplify_sqrt (gfc_expr *e)
{
  gfc_expr *result = NULL;

  if (e->expr_type != EXPR_CONSTANT)
    return NULL;

  switch (e->ts.type)
    {
      case BT_REAL:
	if (mpfr_cmp_si (e->value.real, 0) < 0)
	  {
	    gfc_error ("Argument of SQRT at %L has a negative value",
		       &e->where);
	    return &gfc_bad_expr;
	  }
	result = gfc_get_constant_expr (e->ts.type, e->ts.kind, &e->where);
	mpfr_sqrt (result->value.real, e->value.real, GFC_RND_MODE);
	break;

      case BT_COMPLEX:
	gfc_set_model (e->value.real);

	result = gfc_get_constant_expr (e->ts.type, e->ts.kind, &e->where);
	mpc_sqrt (result->value.complex, e->value.complex, GFC_MPC_RND_MODE);
	break;

      default:
	gfc_internal_error ("invalid argument of SQRT at %L", &e->where);
    }

  return range_check (result, "SQRT");
}


gfc_expr *
gfc_simplify_sum (gfc_expr *array, gfc_expr *dim, gfc_expr *mask)
{
  return simplify_transformation (array, dim, mask, 0, gfc_add);
}


gfc_expr *
gfc_simplify_cotan (gfc_expr *x)
{
  gfc_expr *result;
  mpc_t swp, *val;

  if (x->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_get_constant_expr (x->ts.type, x->ts.kind, &x->where);

  switch (x->ts.type)
    {
      case BT_REAL:
	mpfr_cot (result->value.real, x->value.real, GFC_RND_MODE);
	break;

      case BT_COMPLEX:
	/* There is no builtin mpc_cot, so compute cot = cos / sin.  */
	val = &result->value.complex;
	mpc_init2 (swp, mpfr_get_default_prec ());
	mpc_cos (swp, x->value.complex, GFC_MPC_RND_MODE);
	mpc_sin (*val, x->value.complex, GFC_MPC_RND_MODE);
	mpc_div (*val, swp, *val, GFC_MPC_RND_MODE);
	mpc_clear (swp);
	break;

      default:
	gcc_unreachable ();
    }

  return range_check (result, "COTAN");
}


gfc_expr *
gfc_simplify_tan (gfc_expr *x)
{
  gfc_expr *result;

  if (x->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_get_constant_expr (x->ts.type, x->ts.kind, &x->where);

  switch (x->ts.type)
    {
      case BT_REAL:
	mpfr_tan (result->value.real, x->value.real, GFC_RND_MODE);
	break;

      case BT_COMPLEX:
	mpc_tan (result->value.complex, x->value.complex, GFC_MPC_RND_MODE);
	break;

      default:
	gcc_unreachable ();
    }

  return range_check (result, "TAN");
}


gfc_expr *
gfc_simplify_tanh (gfc_expr *x)
{
  gfc_expr *result;

  if (x->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_get_constant_expr (x->ts.type, x->ts.kind, &x->where);

  switch (x->ts.type)
    {
      case BT_REAL:
	mpfr_tanh (result->value.real, x->value.real, GFC_RND_MODE);
	break;

      case BT_COMPLEX:
	mpc_tanh (result->value.complex, x->value.complex, GFC_MPC_RND_MODE);
	break;

      default:
	gcc_unreachable ();
    }

  return range_check (result, "TANH");
}


gfc_expr *
gfc_simplify_tiny (gfc_expr *e)
{
  gfc_expr *result;
  int i;

  i = gfc_validate_kind (BT_REAL, e->ts.kind, false);

  result = gfc_get_constant_expr (BT_REAL, e->ts.kind, &e->where);
  mpfr_set (result->value.real, gfc_real_kinds[i].tiny, GFC_RND_MODE);

  return result;
}


gfc_expr *
gfc_simplify_trailz (gfc_expr *e)
{
  unsigned long tz, bs;
  int i;

  if (e->expr_type != EXPR_CONSTANT)
    return NULL;

  i = gfc_validate_kind (e->ts.type, e->ts.kind, false);
  bs = gfc_integer_kinds[i].bit_size;
  tz = mpz_scan1 (e->value.integer, 0);

  return gfc_get_int_expr (gfc_default_integer_kind,
			   &e->where, MIN (tz, bs));
}


gfc_expr *
gfc_simplify_transfer (gfc_expr *source, gfc_expr *mold, gfc_expr *size)
{
  gfc_expr *result;
  gfc_expr *mold_element;
  size_t source_size;
  size_t result_size;
  size_t buffer_size;
  mpz_t tmp;
  unsigned char *buffer;
  size_t result_length;


  if (!gfc_is_constant_expr (source)
	|| (gfc_init_expr_flag && !gfc_is_constant_expr (mold))
	|| !gfc_is_constant_expr (size))
    return NULL;

  if (!gfc_calculate_transfer_sizes (source, mold, size, &source_size, 
				     &result_size, &result_length))
    return NULL;

  /* Calculate the size of the source.  */
  if (source->expr_type == EXPR_ARRAY
      && !gfc_array_size (source, &tmp))
    gfc_internal_error ("Failure getting length of a constant array.");

  /* Create an empty new expression with the appropriate characteristics.  */
  result = gfc_get_constant_expr (mold->ts.type, mold->ts.kind,
				  &source->where);
  result->ts = mold->ts;

  mold_element = mold->expr_type == EXPR_ARRAY
		 ? gfc_constructor_first (mold->value.constructor)->expr
		 : mold;

  /* Set result character length, if needed.  Note that this needs to be
     set even for array expressions, in order to pass this information into
     gfc_target_interpret_expr.  */
  if (result->ts.type == BT_CHARACTER && gfc_is_constant_expr (mold_element))
    result->value.character.length = mold_element->value.character.length;

  /* Set the number of elements in the result, and determine its size.  */

  if (mold->expr_type == EXPR_ARRAY || mold->rank || size)
    {
      result->expr_type = EXPR_ARRAY;
      result->rank = 1;
      result->shape = gfc_get_shape (1);
      mpz_init_set_ui (result->shape[0], result_length);
    }
  else
    result->rank = 0;

  /* Allocate the buffer to store the binary version of the source.  */
  buffer_size = MAX (source_size, result_size);
  buffer = (unsigned char*)alloca (buffer_size);
  memset (buffer, 0, buffer_size);

  /* Now write source to the buffer.  */
  gfc_target_encode_expr (source, buffer, buffer_size);

  /* And read the buffer back into the new expression.  */
  gfc_target_interpret_expr (buffer, buffer_size, result, false);

  return result;
}


gfc_expr *
gfc_simplify_transpose (gfc_expr *matrix)
{
  int row, matrix_rows, col, matrix_cols;
  gfc_expr *result;

  if (!is_constant_array_expr (matrix))
    return NULL;

  gcc_assert (matrix->rank == 2);

  result = gfc_get_array_expr (matrix->ts.type, matrix->ts.kind,
			       &matrix->where);
  result->rank = 2;
  result->shape = gfc_get_shape (result->rank);
  mpz_set (result->shape[0], matrix->shape[1]);
  mpz_set (result->shape[1], matrix->shape[0]);

  if (matrix->ts.type == BT_CHARACTER)
    result->ts.u.cl = matrix->ts.u.cl;
  else if (matrix->ts.type == BT_DERIVED)
    result->ts.u.derived = matrix->ts.u.derived;

  matrix_rows = mpz_get_si (matrix->shape[0]);
  matrix_cols = mpz_get_si (matrix->shape[1]);
  for (row = 0; row < matrix_rows; ++row)
    for (col = 0; col < matrix_cols; ++col)
      {
	gfc_expr *e = gfc_constructor_lookup_expr (matrix->value.constructor,
						   col * matrix_rows + row);
	gfc_constructor_insert_expr (&result->value.constructor,
				     gfc_copy_expr (e), &matrix->where,
				     row * matrix_cols + col);
      }

  return result;
}


gfc_expr *
gfc_simplify_trim (gfc_expr *e)
{
  gfc_expr *result;
  int count, i, len, lentrim;

  if (e->expr_type != EXPR_CONSTANT)
    return NULL;

  len = e->value.character.length;
  for (count = 0, i = 1; i <= len; ++i)
    {
      if (e->value.character.string[len - i] == ' ')
	count++;
      else
	break;
    }

  lentrim = len - count;

  result = gfc_get_character_expr (e->ts.kind, &e->where, NULL, lentrim);
  for (i = 0; i < lentrim; i++)
    result->value.character.string[i] = e->value.character.string[i];

  return result;
}


gfc_expr *
gfc_simplify_image_index (gfc_expr *coarray, gfc_expr *sub)
{
  gfc_expr *result;
  gfc_ref *ref;
  gfc_array_spec *as;
  gfc_constructor *sub_cons;
  bool first_image;
  int d;

  if (!is_constant_array_expr (sub))
    return NULL;

  /* Follow any component references.  */
  as = coarray->symtree->n.sym->as;
  for (ref = coarray->ref; ref; ref = ref->next)
    if (ref->type == REF_COMPONENT)
      as = ref->u.ar.as;

  if (as->type == AS_DEFERRED)
    return NULL;

  /* "valid sequence of cosubscripts" are required; thus, return 0 unless
     the cosubscript addresses the first image.  */

  sub_cons = gfc_constructor_first (sub->value.constructor);
  first_image = true;

  for (d = 1; d <= as->corank; d++)
    {
      gfc_expr *ca_bound;
      int cmp;

      gcc_assert (sub_cons != NULL);

      ca_bound = simplify_bound_dim (coarray, NULL, d + as->rank, 0, as,
				     NULL, true);
      if (ca_bound == NULL)
	return NULL;

      if (ca_bound == &gfc_bad_expr)
	return ca_bound;

      cmp = mpz_cmp (ca_bound->value.integer, sub_cons->expr->value.integer);

      if (cmp == 0)
	{
          gfc_free_expr (ca_bound);
	  sub_cons = gfc_constructor_next (sub_cons);
	  continue;
	}

      first_image = false;

      if (cmp > 0)
	{
	  gfc_error ("Out of bounds in IMAGE_INDEX at %L for dimension %d, "
		     "SUB has %ld and COARRAY lower bound is %ld)",
		     &coarray->where, d,
		     mpz_get_si (sub_cons->expr->value.integer),
		     mpz_get_si (ca_bound->value.integer));
	  gfc_free_expr (ca_bound);
	  return &gfc_bad_expr;
	}

      gfc_free_expr (ca_bound);

      /* Check whether upperbound is valid for the multi-images case.  */
      if (d < as->corank)
	{
	  ca_bound = simplify_bound_dim (coarray, NULL, d + as->rank, 1, as,
					 NULL, true);
	  if (ca_bound == &gfc_bad_expr)
	    return ca_bound;

	  if (ca_bound && ca_bound->expr_type == EXPR_CONSTANT
	      && mpz_cmp (ca_bound->value.integer,
			  sub_cons->expr->value.integer) < 0)
	  {
	    gfc_error ("Out of bounds in IMAGE_INDEX at %L for dimension %d, "
		       "SUB has %ld and COARRAY upper bound is %ld)",
		       &coarray->where, d,
		       mpz_get_si (sub_cons->expr->value.integer),
		       mpz_get_si (ca_bound->value.integer));
	    gfc_free_expr (ca_bound);
	    return &gfc_bad_expr;
	  }

	  if (ca_bound)
	    gfc_free_expr (ca_bound);
	}

      sub_cons = gfc_constructor_next (sub_cons);
    }

  gcc_assert (sub_cons == NULL);

  if (flag_coarray != GFC_FCOARRAY_SINGLE && !first_image)
    return NULL;

  result = gfc_get_constant_expr (BT_INTEGER, gfc_default_integer_kind,
				  &gfc_current_locus);
  if (first_image)
    mpz_set_si (result->value.integer, 1);
  else
    mpz_set_si (result->value.integer, 0);

  return result;
}


gfc_expr *
gfc_simplify_this_image (gfc_expr *coarray, gfc_expr *dim,
			 gfc_expr *distance ATTRIBUTE_UNUSED)
{
  if (flag_coarray != GFC_FCOARRAY_SINGLE)
    return NULL;

  /* If no coarray argument has been passed or when the first argument
     is actually a distance argment.  */
  if (coarray == NULL || !gfc_is_coarray (coarray))
    {
      gfc_expr *result;
      /* FIXME: gfc_current_locus is wrong.  */
      result = gfc_get_constant_expr (BT_INTEGER, gfc_default_integer_kind,
				      &gfc_current_locus);
      mpz_set_si (result->value.integer, 1);
      return result;
    }

  /* For -fcoarray=single, this_image(A) is the same as lcobound(A).  */
  return simplify_cobound (coarray, dim, NULL, 0);
}


gfc_expr *
gfc_simplify_ubound (gfc_expr *array, gfc_expr *dim, gfc_expr *kind)
{
  return simplify_bound (array, dim, kind, 1);
}

gfc_expr *
gfc_simplify_ucobound (gfc_expr *array, gfc_expr *dim, gfc_expr *kind)
{
  return simplify_cobound (array, dim, kind, 1);
}


gfc_expr *
gfc_simplify_unpack (gfc_expr *vector, gfc_expr *mask, gfc_expr *field)
{
  gfc_expr *result, *e;
  gfc_constructor *vector_ctor, *mask_ctor, *field_ctor;

  if (!is_constant_array_expr (vector)
      || !is_constant_array_expr (mask)
      || (!gfc_is_constant_expr (field)
	  && !is_constant_array_expr (field)))
    return NULL;

  result = gfc_get_array_expr (vector->ts.type, vector->ts.kind,
			       &vector->where);
  if (vector->ts.type == BT_DERIVED)
    result->ts.u.derived = vector->ts.u.derived;
  result->rank = mask->rank;
  result->shape = gfc_copy_shape (mask->shape, mask->rank);

  if (vector->ts.type == BT_CHARACTER)
    result->ts.u.cl = vector->ts.u.cl;

  vector_ctor = gfc_constructor_first (vector->value.constructor);
  mask_ctor = gfc_constructor_first (mask->value.constructor);
  field_ctor
    = field->expr_type == EXPR_ARRAY
			    ? gfc_constructor_first (field->value.constructor)
			    : NULL;

  while (mask_ctor)
    {
      if (mask_ctor->expr->value.logical)
	{
	  gcc_assert (vector_ctor);
	  e = gfc_copy_expr (vector_ctor->expr);
	  vector_ctor = gfc_constructor_next (vector_ctor);
	}
      else if (field->expr_type == EXPR_ARRAY)
	e = gfc_copy_expr (field_ctor->expr);
      else
	e = gfc_copy_expr (field);

      gfc_constructor_append_expr (&result->value.constructor, e, NULL);

      mask_ctor = gfc_constructor_next (mask_ctor);
      field_ctor = gfc_constructor_next (field_ctor);
    }

  return result;
}


gfc_expr *
gfc_simplify_verify (gfc_expr *s, gfc_expr *set, gfc_expr *b, gfc_expr *kind)
{
  gfc_expr *result;
  int back;
  size_t index, len, lenset;
  size_t i;
  int k = get_kind (BT_INTEGER, kind, "VERIFY", gfc_default_integer_kind);

  if (k == -1)
    return &gfc_bad_expr;

  if (s->expr_type != EXPR_CONSTANT || set->expr_type != EXPR_CONSTANT
      || ( b != NULL && b->expr_type !=  EXPR_CONSTANT))
    return NULL;

  if (b != NULL && b->value.logical != 0)
    back = 1;
  else
    back = 0;

  result = gfc_get_constant_expr (BT_INTEGER, k, &s->where);

  len = s->value.character.length;
  lenset = set->value.character.length;

  if (len == 0)
    {
      mpz_set_ui (result->value.integer, 0);
      return result;
    }

  if (back == 0)
    {
      if (lenset == 0)
	{
	  mpz_set_ui (result->value.integer, 1);
	  return result;
	}

      index = wide_strspn (s->value.character.string,
			   set->value.character.string) + 1;
      if (index > len)
	index = 0;

    }
  else
    {
      if (lenset == 0)
	{
	  mpz_set_ui (result->value.integer, len);
	  return result;
	}
      for (index = len; index > 0; index --)
	{
	  for (i = 0; i < lenset; i++)
	    {
	      if (s->value.character.string[index - 1]
		  == set->value.character.string[i])
		break;
	    }
	  if (i == lenset)
	    break;
	}
    }

  mpz_set_ui (result->value.integer, index);
  return result;
}


gfc_expr *
gfc_simplify_xor (gfc_expr *x, gfc_expr *y)
{
  gfc_expr *result;
  int kind;

  if (x->expr_type != EXPR_CONSTANT || y->expr_type != EXPR_CONSTANT)
    return NULL;

  kind = x->ts.kind > y->ts.kind ? x->ts.kind : y->ts.kind;

  switch (x->ts.type)
    {
      case BT_INTEGER:
	result = gfc_get_constant_expr (BT_INTEGER, kind, &x->where);
	mpz_xor (result->value.integer, x->value.integer, y->value.integer);
	return range_check (result, "XOR");

      case BT_LOGICAL:
	return gfc_get_logical_expr (kind, &x->where,
				     (x->value.logical && !y->value.logical)
				     || (!x->value.logical && y->value.logical));

      default:
	gcc_unreachable ();
    }
}


/****************** Constant simplification *****************/

/* Master function to convert one constant to another.  While this is
   used as a simplification function, it requires the destination type
   and kind information which is supplied by a special case in
   do_simplify().  */

gfc_expr *
gfc_convert_constant (gfc_expr *e, bt type, int kind)
{
  gfc_expr *g, *result, *(*f) (gfc_expr *, int);
  gfc_constructor *c;

  switch (e->ts.type)
    {
    case BT_INTEGER:
      switch (type)
	{
	case BT_INTEGER:
	  f = gfc_int2int;
	  break;
	case BT_REAL:
	  f = gfc_int2real;
	  break;
	case BT_COMPLEX:
	  f = gfc_int2complex;
	  break;
	case BT_LOGICAL:
	  f = gfc_int2log;
	  break;
	default:
	  goto oops;
	}
      break;

    case BT_REAL:
      switch (type)
	{
	case BT_INTEGER:
	  f = gfc_real2int;
	  break;
	case BT_REAL:
	  f = gfc_real2real;
	  break;
	case BT_COMPLEX:
	  f = gfc_real2complex;
	  break;
	default:
	  goto oops;
	}
      break;

    case BT_COMPLEX:
      switch (type)
	{
	case BT_INTEGER:
	  f = gfc_complex2int;
	  break;
	case BT_REAL:
	  f = gfc_complex2real;
	  break;
	case BT_COMPLEX:
	  f = gfc_complex2complex;
	  break;

	default:
	  goto oops;
	}
      break;

    case BT_LOGICAL:
      switch (type)
	{
	case BT_INTEGER:
	  f = gfc_log2int;
	  break;
	case BT_LOGICAL:
	  f = gfc_log2log;
	  break;
	default:
	  goto oops;
	}
      break;

    case BT_HOLLERITH:
      switch (type)
	{
	case BT_INTEGER:
	  f = gfc_hollerith2int;
	  break;

	case BT_REAL:
	  f = gfc_hollerith2real;
	  break;

	case BT_COMPLEX:
	  f = gfc_hollerith2complex;
	  break;

	case BT_CHARACTER:
	  f = gfc_hollerith2character;
	  break;

	case BT_LOGICAL:
	  f = gfc_hollerith2logical;
	  break;

	default:
	  goto oops;
	}
      break;

    default:
    oops:
      gfc_internal_error ("gfc_convert_constant(): Unexpected type");
    }

  result = NULL;

  switch (e->expr_type)
    {
    case EXPR_CONSTANT:
      result = f (e, kind);
      if (result == NULL)
	return &gfc_bad_expr;
      break;

    case EXPR_ARRAY:
      if (!gfc_is_constant_expr (e))
	break;

      result = gfc_get_array_expr (type, kind, &e->where);
      result->shape = gfc_copy_shape (e->shape, e->rank);
      result->rank = e->rank;

      for (c = gfc_constructor_first (e->value.constructor);
	   c; c = gfc_constructor_next (c))
	{
	  gfc_expr *tmp;
	  if (c->iterator == NULL)
	    tmp = f (c->expr, kind);
	  else
	    {
	      g = gfc_convert_constant (c->expr, type, kind);
	      if (g == &gfc_bad_expr)
	        {
		  gfc_free_expr (result);
		  return g;
		}
	      tmp = g;
	    }

	  if (tmp == NULL)
	    {
	      gfc_free_expr (result);
	      return NULL;
	    }

	  gfc_constructor_append_expr (&result->value.constructor,
				       tmp, &c->where);
	}

      break;

    default:
      break;
    }

  return result;
}


/* Function for converting character constants.  */
gfc_expr *
gfc_convert_char_constant (gfc_expr *e, bt type ATTRIBUTE_UNUSED, int kind)
{
  gfc_expr *result;
  int i;

  if (!gfc_is_constant_expr (e))
    return NULL;

  if (e->expr_type == EXPR_CONSTANT)
    {
      /* Simple case of a scalar.  */
      result = gfc_get_constant_expr (BT_CHARACTER, kind, &e->where);
      if (result == NULL)
	return &gfc_bad_expr;

      result->value.character.length = e->value.character.length;
      result->value.character.string
	= gfc_get_wide_string (e->value.character.length + 1);
      memcpy (result->value.character.string, e->value.character.string,
	      (e->value.character.length + 1) * sizeof (gfc_char_t));

      /* Check we only have values representable in the destination kind.  */
      for (i = 0; i < result->value.character.length; i++)
	if (!gfc_check_character_range (result->value.character.string[i],
					kind))
	  {
	    gfc_error ("Character %qs in string at %L cannot be converted "
		       "into character kind %d",
		       gfc_print_wide_char (result->value.character.string[i]),
		       &e->where, kind);
	    return &gfc_bad_expr;
	  }

      return result;
    }
  else if (e->expr_type == EXPR_ARRAY)
    {
      /* For an array constructor, we convert each constructor element.  */
      gfc_constructor *c;

      result = gfc_get_array_expr (type, kind, &e->where);
      result->shape = gfc_copy_shape (e->shape, e->rank);
      result->rank = e->rank;
      result->ts.u.cl = e->ts.u.cl;

      for (c = gfc_constructor_first (e->value.constructor);
	   c; c = gfc_constructor_next (c))
	{
	  gfc_expr *tmp = gfc_convert_char_constant (c->expr, type, kind);
	  if (tmp == &gfc_bad_expr)
	    {
	      gfc_free_expr (result);
	      return &gfc_bad_expr;
	    }

	  if (tmp == NULL)
	    {
	      gfc_free_expr (result);
	      return NULL;
	    }

	  gfc_constructor_append_expr (&result->value.constructor,
				       tmp, &c->where);
	}

      return result;
    }
  else
    return NULL;
}


gfc_expr *
gfc_simplify_compiler_options (void)
{
  char *str;
  gfc_expr *result;

  str = gfc_get_option_string ();
  result = gfc_get_character_expr (gfc_default_character_kind,
				   &gfc_current_locus, str, strlen (str));
  free (str);
  return result;
}


gfc_expr *
gfc_simplify_compiler_version (void)
{
  char *buffer;
  size_t len;

  len = strlen ("GCC version ") + strlen (version_string);
  buffer = XALLOCAVEC (char, len + 1);
  snprintf (buffer, len + 1, "GCC version %s", version_string);
  return gfc_get_character_expr (gfc_default_character_kind,
                                &gfc_current_locus, buffer, len);
}

/* Simplification routines for intrinsics of IEEE modules.  */

gfc_expr *
simplify_ieee_selected_real_kind (gfc_expr *expr)
{
  gfc_actual_arglist *arg;
  gfc_expr *p = NULL, *q = NULL, *rdx = NULL;

  arg = expr->value.function.actual;
  p = arg->expr;
  if (arg->next)
    {
      q = arg->next->expr;
      if (arg->next->next)
	rdx = arg->next->next->expr;
    }

  /* Currently, if IEEE is supported and this module is built, it means
     all our floating-point types conform to IEEE. Hence, we simply handle
     IEEE_SELECTED_REAL_KIND like SELECTED_REAL_KIND.  */
  return gfc_simplify_selected_real_kind (p, q, rdx);
}

gfc_expr *
simplify_ieee_support (gfc_expr *expr)
{
  /* We consider that if the IEEE modules are loaded, we have full support
     for flags, halting and rounding, which are the three functions
     (IEEE_SUPPORT_{FLAG,HALTING,ROUNDING}) allowed in constant
     expressions. One day, we will need libgfortran to detect support and
     communicate it back to us, allowing for partial support.  */

  return gfc_get_logical_expr (gfc_default_logical_kind, &expr->where,
			       true);
}

bool
matches_ieee_function_name (gfc_symbol *sym, const char *name)
{
  int n = strlen(name);

  if (!strncmp(sym->name, name, n))
    return true;

  /* If a generic was used and renamed, we need more work to find out.
     Compare the specific name.  */
  if (sym->generic && !strncmp(sym->generic->sym->name, name, n))
    return true;

  return false;
}

gfc_expr *
gfc_simplify_ieee_functions (gfc_expr *expr)
{
  gfc_symbol* sym = expr->symtree->n.sym;

  if (matches_ieee_function_name(sym, "ieee_selected_real_kind"))
    return simplify_ieee_selected_real_kind (expr);
  else if (matches_ieee_function_name(sym, "ieee_support_flag")
	   || matches_ieee_function_name(sym, "ieee_support_halting")
	   || matches_ieee_function_name(sym, "ieee_support_rounding"))
    return simplify_ieee_support (expr);
  else
    return NULL;
}
