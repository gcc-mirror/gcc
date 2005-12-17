/* Simplify intrinsic functions at compile-time.
   Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005 Free Software Foundation,
   Inc.
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

#include "config.h"
#include "system.h"
#include "flags.h"
#include "gfortran.h"
#include "arith.h"
#include "intrinsic.h"

gfc_expr gfc_bad_expr;


/* Note that 'simplification' is not just transforming expressions.
   For functions that are not simplified at compile time, range
   checking is done if possible.

   The return convention is that each simplification function returns:

     A new expression node corresponding to the simplified arguments.
     The original arguments are destroyed by the caller, and must not
     be a part of the new expression.

     NULL pointer indicating that no simplification was possible and
     the original expression should remain intact.  If the
     simplification function sets the type and/or the function name
     via the pointer gfc_simple_expression, then this type is
     retained.

     An expression pointer to gfc_bad_expr (a static placeholder)
     indicating that some error has prevented simplification.  For
     example, sqrt(-1.0).  The error is generated within the function
     and should be propagated upwards

   By the time a simplification function gets control, it has been
   decided that the function call is really supposed to be the
   intrinsic.  No type checking is strictly necessary, since only
   valid types will be passed on.  On the other hand, a simplification
   subroutine may have to look at the type of an argument as part of
   its processing.

   Array arguments are never passed to these subroutines.

   The functions in this file don't have much comment with them, but
   everything is reasonably straight-forward.  The Standard, chapter 13
   is the best comment you'll find for this file anyway.  */

/* Static table for converting non-ascii character sets to ascii.
   The xascii_table[] is the inverse table.  */

static int ascii_table[256] = {
  '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0',
  '\b', '\t', '\n', '\v', '\0', '\r', '\0', '\0',
  '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0',
  '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0',
  ' ', '!', '\'', '#', '$', '%', '&', '\'',
  '(', ')', '*', '+', ',', '-', '.', '/',
  '0', '1', '2', '3', '4', '5', '6', '7',
  '8', '9', ':', ';', '<', '=', '>', '?',
  '@', 'A', 'B', 'C', 'D', 'E', 'F', 'G',
  'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O',
  'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W',
  'X', 'Y', 'Z', '[', '\\', ']', '^', '_',
  '`', 'a', 'b', 'c', 'd', 'e', 'f', 'g',
  'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o',
  'p', 'q', 'r', 's', 't', 'u', 'v', 'w',
  'x', 'y', 'z', '{', '|', '}', '~', '\?'
};

static int xascii_table[256];


/* Range checks an expression node.  If all goes well, returns the
   node, otherwise returns &gfc_bad_expr and frees the node.  */

static gfc_expr *
range_check (gfc_expr * result, const char *name)
{
  if (gfc_range_check (result) == ARITH_OK)
    return result;

  gfc_error ("Result of %s overflows its kind at %L", name, &result->where);
  gfc_free_expr (result);
  return &gfc_bad_expr;
}


/* A helper function that gets an optional and possibly missing
   kind parameter.  Returns the kind, -1 if something went wrong.  */

static int
get_kind (bt type, gfc_expr * k, const char *name, int default_kind)
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


/* Checks if X, which is assumed to represent a two's complement
   integer of binary width BITSIZE, has the signbit set.  If so, makes 
   X the corresponding negative number.  */

static void
twos_complement (mpz_t x, int bitsize)
{
  mpz_t mask;

  if (mpz_tstbit (x, bitsize - 1) == 1)
    {
      mpz_init_set_ui(mask, 1);
      mpz_mul_2exp(mask, mask, bitsize);
      mpz_sub_ui(mask, mask, 1);

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


/********************** Simplification functions *****************************/

gfc_expr *
gfc_simplify_abs (gfc_expr * e)
{
  gfc_expr *result;

  if (e->expr_type != EXPR_CONSTANT)
    return NULL;

  switch (e->ts.type)
    {
    case BT_INTEGER:
      result = gfc_constant_result (BT_INTEGER, e->ts.kind, &e->where);

      mpz_abs (result->value.integer, e->value.integer);

      result = range_check (result, "IABS");
      break;

    case BT_REAL:
      result = gfc_constant_result (BT_REAL, e->ts.kind, &e->where);

      mpfr_abs (result->value.real, e->value.real, GFC_RND_MODE);

      result = range_check (result, "ABS");
      break;

    case BT_COMPLEX:
      result = gfc_constant_result (BT_REAL, e->ts.kind, &e->where);

      gfc_set_model_kind (e->ts.kind);

      mpfr_hypot (result->value.real, e->value.complex.r, 
		  e->value.complex.i, GFC_RND_MODE);
      result = range_check (result, "CABS");
      break;

    default:
      gfc_internal_error ("gfc_simplify_abs(): Bad type");
    }

  return result;
}


gfc_expr *
gfc_simplify_achar (gfc_expr * e)
{
  gfc_expr *result;
  int index;

  if (e->expr_type != EXPR_CONSTANT)
    return NULL;

  /* We cannot assume that the native character set is ASCII in this
     function.  */
  if (gfc_extract_int (e, &index) != NULL || index < 0 || index > 127)
    {
      gfc_error ("Extended ASCII not implemented: argument of ACHAR at %L "
		 "must be between 0 and 127", &e->where);
      return &gfc_bad_expr;
    }

  result = gfc_constant_result (BT_CHARACTER, gfc_default_character_kind,
				&e->where);

  result->value.character.string = gfc_getmem (2);

  result->value.character.length = 1;
  result->value.character.string[0] = ascii_table[index];
  result->value.character.string[1] = '\0';	/* For debugger */
  return result;
}


gfc_expr *
gfc_simplify_acos (gfc_expr * x)
{
  gfc_expr *result;

  if (x->expr_type != EXPR_CONSTANT)
    return NULL;

  if (mpfr_cmp_si (x->value.real, 1) > 0 || mpfr_cmp_si (x->value.real, -1) < 0)
    {
      gfc_error ("Argument of ACOS at %L must be between -1 and 1",
		 &x->where);
      return &gfc_bad_expr;
    }

  result = gfc_constant_result (x->ts.type, x->ts.kind, &x->where);

  mpfr_acos (result->value.real, x->value.real, GFC_RND_MODE);

  return range_check (result, "ACOS");
}

gfc_expr *
gfc_simplify_acosh (gfc_expr * x)
{
  gfc_expr *result;

  if (x->expr_type != EXPR_CONSTANT)
    return NULL;

  if (mpfr_cmp_si (x->value.real, 1) < 0)
    {
      gfc_error ("Argument of ACOSH at %L must not be less than 1",
		 &x->where);
      return &gfc_bad_expr;
    }

  result = gfc_constant_result (x->ts.type, x->ts.kind, &x->where);

  mpfr_acosh (result->value.real, x->value.real, GFC_RND_MODE);

  return range_check (result, "ACOSH");
}

gfc_expr *
gfc_simplify_adjustl (gfc_expr * e)
{
  gfc_expr *result;
  int count, i, len;
  char ch;

  if (e->expr_type != EXPR_CONSTANT)
    return NULL;

  len = e->value.character.length;

  result = gfc_constant_result (BT_CHARACTER, e->ts.kind, &e->where);

  result->value.character.length = len;
  result->value.character.string = gfc_getmem (len + 1);

  for (count = 0, i = 0; i < len; ++i)
    {
      ch = e->value.character.string[i];
      if (ch != ' ')
	break;
      ++count;
    }

  for (i = 0; i < len - count; ++i)
    {
      result->value.character.string[i] =
	e->value.character.string[count + i];
    }

  for (i = len - count; i < len; ++i)
    {
      result->value.character.string[i] = ' ';
    }

  result->value.character.string[len] = '\0';	/* For debugger */

  return result;
}


gfc_expr *
gfc_simplify_adjustr (gfc_expr * e)
{
  gfc_expr *result;
  int count, i, len;
  char ch;

  if (e->expr_type != EXPR_CONSTANT)
    return NULL;

  len = e->value.character.length;

  result = gfc_constant_result (BT_CHARACTER, e->ts.kind, &e->where);

  result->value.character.length = len;
  result->value.character.string = gfc_getmem (len + 1);

  for (count = 0, i = len - 1; i >= 0; --i)
    {
      ch = e->value.character.string[i];
      if (ch != ' ')
	break;
      ++count;
    }

  for (i = 0; i < count; ++i)
    {
      result->value.character.string[i] = ' ';
    }

  for (i = count; i < len; ++i)
    {
      result->value.character.string[i] =
	e->value.character.string[i - count];
    }

  result->value.character.string[len] = '\0';	/* For debugger */

  return result;
}


gfc_expr *
gfc_simplify_aimag (gfc_expr * e)
{

  gfc_expr *result;

  if (e->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_constant_result (BT_REAL, e->ts.kind, &e->where);
  mpfr_set (result->value.real, e->value.complex.i, GFC_RND_MODE);

  return range_check (result, "AIMAG");
}


gfc_expr *
gfc_simplify_aint (gfc_expr * e, gfc_expr * k)
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
gfc_simplify_dint (gfc_expr * e)
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
gfc_simplify_anint (gfc_expr * e, gfc_expr * k)
{
  gfc_expr *result;
  int kind;

  kind = get_kind (BT_REAL, k, "ANINT", e->ts.kind);
  if (kind == -1)
    return &gfc_bad_expr;

  if (e->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_constant_result (e->ts.type, kind, &e->where);

  mpfr_round (result->value.real, e->value.real);

  return range_check (result, "ANINT");
}


gfc_expr *
gfc_simplify_and (gfc_expr * x, gfc_expr * y)
{
  gfc_expr *result;
  int kind;

  if (x->expr_type != EXPR_CONSTANT || y->expr_type != EXPR_CONSTANT)
    return NULL;

  kind = x->ts.kind > y->ts.kind ? x->ts.kind : y->ts.kind;
  if (x->ts.type == BT_INTEGER)
    {
      result = gfc_constant_result (BT_INTEGER, kind, &x->where);
      mpz_and (result->value.integer, x->value.integer, y->value.integer);
    }
  else /* BT_LOGICAL */
    {
      result = gfc_constant_result (BT_LOGICAL, kind, &x->where);
      result->value.logical = x->value.logical && y->value.logical;
    }

  return range_check (result, "AND");
}


gfc_expr *
gfc_simplify_dnint (gfc_expr * e)
{
  gfc_expr *result;

  if (e->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_constant_result (BT_REAL, gfc_default_double_kind, &e->where);

  mpfr_round (result->value.real, e->value.real);

  return range_check (result, "DNINT");
}


gfc_expr *
gfc_simplify_asin (gfc_expr * x)
{
  gfc_expr *result;

  if (x->expr_type != EXPR_CONSTANT)
    return NULL;

  if (mpfr_cmp_si (x->value.real, 1) > 0 || mpfr_cmp_si (x->value.real, -1) < 0)
    {
      gfc_error ("Argument of ASIN at %L must be between -1 and 1",
		 &x->where);
      return &gfc_bad_expr;
    }

  result = gfc_constant_result (x->ts.type, x->ts.kind, &x->where);

  mpfr_asin(result->value.real, x->value.real, GFC_RND_MODE);

  return range_check (result, "ASIN");
}


gfc_expr *
gfc_simplify_asinh (gfc_expr * x)
{
  gfc_expr *result;

  if (x->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_constant_result (x->ts.type, x->ts.kind, &x->where);

  mpfr_asinh(result->value.real, x->value.real, GFC_RND_MODE);

  return range_check (result, "ASINH");
}


gfc_expr *
gfc_simplify_atan (gfc_expr * x)
{
  gfc_expr *result;

  if (x->expr_type != EXPR_CONSTANT)
    return NULL;
    
  result = gfc_constant_result (x->ts.type, x->ts.kind, &x->where);

  mpfr_atan(result->value.real, x->value.real, GFC_RND_MODE);

  return range_check (result, "ATAN");
}


gfc_expr *
gfc_simplify_atanh (gfc_expr * x)
{
  gfc_expr *result;

  if (x->expr_type != EXPR_CONSTANT)
    return NULL;

  if (mpfr_cmp_si (x->value.real, 1) >= 0 ||
      mpfr_cmp_si (x->value.real, -1) <= 0)
    {
      gfc_error ("Argument of ATANH at %L must be inside the range -1 to 1",
		 &x->where);
      return &gfc_bad_expr;
    }

  result = gfc_constant_result (x->ts.type, x->ts.kind, &x->where);

  mpfr_atanh(result->value.real, x->value.real, GFC_RND_MODE);

  return range_check (result, "ATANH");
}


gfc_expr *
gfc_simplify_atan2 (gfc_expr * y, gfc_expr * x)
{
  gfc_expr *result;

  if (x->expr_type != EXPR_CONSTANT || y->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_constant_result (x->ts.type, x->ts.kind, &x->where);

  if (mpfr_sgn (y->value.real) == 0 && mpfr_sgn (x->value.real) == 0)
    {
      gfc_error
	("If first argument of ATAN2 %L is zero, then the second argument "
	  "must not be zero", &x->where);
      gfc_free_expr (result);
      return &gfc_bad_expr;
    }

  arctangent2 (y->value.real, x->value.real, result->value.real);

  return range_check (result, "ATAN2");
}


gfc_expr *
gfc_simplify_bit_size (gfc_expr * e)
{
  gfc_expr *result;
  int i;

  i = gfc_validate_kind (e->ts.type, e->ts.kind, false);
  result = gfc_constant_result (BT_INTEGER, e->ts.kind, &e->where);
  mpz_set_ui (result->value.integer, gfc_integer_kinds[i].bit_size);

  return result;
}


gfc_expr *
gfc_simplify_btest (gfc_expr * e, gfc_expr * bit)
{
  int b;

  if (e->expr_type != EXPR_CONSTANT || bit->expr_type != EXPR_CONSTANT)
    return NULL;

  if (gfc_extract_int (bit, &b) != NULL || b < 0)
    return gfc_logical_expr (0, &e->where);

  return gfc_logical_expr (mpz_tstbit (e->value.integer, b), &e->where);
}


gfc_expr *
gfc_simplify_ceiling (gfc_expr * e, gfc_expr * k)
{
  gfc_expr *ceil, *result;
  int kind;

  kind = get_kind (BT_INTEGER, k, "CEILING", gfc_default_integer_kind);
  if (kind == -1)
    return &gfc_bad_expr;

  if (e->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_constant_result (BT_INTEGER, kind, &e->where);

  ceil = gfc_copy_expr (e);

  mpfr_ceil (ceil->value.real, e->value.real);
  gfc_mpfr_to_mpz(result->value.integer, ceil->value.real);

  gfc_free_expr (ceil);

  return range_check (result, "CEILING");
}


gfc_expr *
gfc_simplify_char (gfc_expr * e, gfc_expr * k)
{
  gfc_expr *result;
  int c, kind;

  kind = get_kind (BT_CHARACTER, k, "CHAR", gfc_default_character_kind);
  if (kind == -1)
    return &gfc_bad_expr;

  if (e->expr_type != EXPR_CONSTANT)
    return NULL;

  if (gfc_extract_int (e, &c) != NULL || c < 0 || c > 255)
    {
      gfc_error ("Bad character in CHAR function at %L", &e->where);
      return &gfc_bad_expr;
    }

  result = gfc_constant_result (BT_CHARACTER, kind, &e->where);

  result->value.character.length = 1;
  result->value.character.string = gfc_getmem (2);

  result->value.character.string[0] = c;
  result->value.character.string[1] = '\0';	/* For debugger */

  return result;
}


/* Common subroutine for simplifying CMPLX and DCMPLX.  */

static gfc_expr *
simplify_cmplx (const char *name, gfc_expr * x, gfc_expr * y, int kind)
{
  gfc_expr *result;

  result = gfc_constant_result (BT_COMPLEX, kind, &x->where);

  mpfr_set_ui (result->value.complex.i, 0, GFC_RND_MODE);

  switch (x->ts.type)
    {
    case BT_INTEGER:
      mpfr_set_z (result->value.complex.r, x->value.integer, GFC_RND_MODE);
      break;

    case BT_REAL:
      mpfr_set (result->value.complex.r, x->value.real, GFC_RND_MODE);
      break;

    case BT_COMPLEX:
      mpfr_set (result->value.complex.r, x->value.complex.r, GFC_RND_MODE);
      mpfr_set (result->value.complex.i, x->value.complex.i, GFC_RND_MODE);
      break;

    default:
      gfc_internal_error ("gfc_simplify_dcmplx(): Bad type (x)");
    }

  if (y != NULL)
    {
      switch (y->ts.type)
	{
	case BT_INTEGER:
	  mpfr_set_z (result->value.complex.i, y->value.integer, GFC_RND_MODE);
	  break;

	case BT_REAL:
	  mpfr_set (result->value.complex.i, y->value.real, GFC_RND_MODE);
	  break;

	default:
	  gfc_internal_error ("gfc_simplify_dcmplx(): Bad type (y)");
	}
    }

  return range_check (result, name);
}


gfc_expr *
gfc_simplify_cmplx (gfc_expr * x, gfc_expr * y, gfc_expr * k)
{
  int kind;

  if (x->expr_type != EXPR_CONSTANT
      || (y != NULL && y->expr_type != EXPR_CONSTANT))
    return NULL;

  kind = get_kind (BT_REAL, k, "CMPLX", gfc_default_real_kind);
  if (kind == -1)
    return &gfc_bad_expr;

  return simplify_cmplx ("CMPLX", x, y, kind);
}


gfc_expr *
gfc_simplify_complex (gfc_expr * x, gfc_expr * y)
{
  int kind;

  if (x->expr_type != EXPR_CONSTANT
      || (y != NULL && y->expr_type != EXPR_CONSTANT))
    return NULL;

  if (x->ts.type == BT_INTEGER)
    {
      if (y->ts.type == BT_INTEGER)
	kind = gfc_default_real_kind;
      else
	kind = y->ts.kind;
    }
  else
    {
      if (y->ts.type == BT_REAL)
	kind = (x->ts.kind > y->ts.kind) ? x->ts.kind : y->ts.kind;
      else
	kind = x->ts.kind;
    }

  return simplify_cmplx ("COMPLEX", x, y, kind);
}


gfc_expr *
gfc_simplify_conjg (gfc_expr * e)
{
  gfc_expr *result;

  if (e->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_copy_expr (e);
  mpfr_neg (result->value.complex.i, result->value.complex.i, GFC_RND_MODE);

  return range_check (result, "CONJG");
}


gfc_expr *
gfc_simplify_cos (gfc_expr * x)
{
  gfc_expr *result;
  mpfr_t xp, xq;

  if (x->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_constant_result (x->ts.type, x->ts.kind, &x->where);

  switch (x->ts.type)
    {
    case BT_REAL:
      mpfr_cos (result->value.real, x->value.real, GFC_RND_MODE);
      break;
    case BT_COMPLEX:
      gfc_set_model_kind (x->ts.kind);
      mpfr_init (xp);
      mpfr_init (xq);

      mpfr_cos  (xp, x->value.complex.r, GFC_RND_MODE);
      mpfr_cosh (xq, x->value.complex.i, GFC_RND_MODE);
      mpfr_mul(result->value.complex.r, xp, xq, GFC_RND_MODE);

      mpfr_sin  (xp, x->value.complex.r, GFC_RND_MODE);
      mpfr_sinh (xq, x->value.complex.i, GFC_RND_MODE);
      mpfr_mul (xp, xp, xq, GFC_RND_MODE);
      mpfr_neg (result->value.complex.i, xp, GFC_RND_MODE );

      mpfr_clear (xp);
      mpfr_clear (xq);
      break;
    default:
      gfc_internal_error ("in gfc_simplify_cos(): Bad type");
    }

  return range_check (result, "COS");

}


gfc_expr *
gfc_simplify_cosh (gfc_expr * x)
{
  gfc_expr *result;

  if (x->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_constant_result (x->ts.type, x->ts.kind, &x->where);

  mpfr_cosh (result->value.real, x->value.real, GFC_RND_MODE);

  return range_check (result, "COSH");
}


gfc_expr *
gfc_simplify_dcmplx (gfc_expr * x, gfc_expr * y)
{

  if (x->expr_type != EXPR_CONSTANT
      || (y != NULL && y->expr_type != EXPR_CONSTANT))
    return NULL;

  return simplify_cmplx ("DCMPLX", x, y, gfc_default_double_kind);
}


gfc_expr *
gfc_simplify_dble (gfc_expr * e)
{
  gfc_expr *result;

  if (e->expr_type != EXPR_CONSTANT)
    return NULL;

  switch (e->ts.type)
    {
    case BT_INTEGER:
      result = gfc_int2real (e, gfc_default_double_kind);
      break;

    case BT_REAL:
      result = gfc_real2real (e, gfc_default_double_kind);
      break;

    case BT_COMPLEX:
      result = gfc_complex2real (e, gfc_default_double_kind);
      break;

    default:
      gfc_internal_error ("gfc_simplify_dble(): bad type at %L", &e->where);
    }

  return range_check (result, "DBLE");
}


gfc_expr *
gfc_simplify_digits (gfc_expr * x)
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

  return gfc_int_expr (digits);
}


gfc_expr *
gfc_simplify_dim (gfc_expr * x, gfc_expr * y)
{
  gfc_expr *result;
  int kind;

  if (x->expr_type != EXPR_CONSTANT || y->expr_type != EXPR_CONSTANT)
    return NULL;

  kind = x->ts.kind > y->ts.kind ? x->ts.kind : y->ts.kind;
  result = gfc_constant_result (x->ts.type, kind, &x->where);

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
	mpfr_sub (result->value.real, x->value.real, y->value.real, GFC_RND_MODE);
      else
	mpfr_set_ui (result->value.real, 0, GFC_RND_MODE);

      break;

    default:
      gfc_internal_error ("gfc_simplify_dim(): Bad type");
    }

  return range_check (result, "DIM");
}


gfc_expr *
gfc_simplify_dprod (gfc_expr * x, gfc_expr * y)
{
  gfc_expr *a1, *a2, *result;

  if (x->expr_type != EXPR_CONSTANT || y->expr_type != EXPR_CONSTANT)
    return NULL;

  result =
    gfc_constant_result (BT_REAL, gfc_default_double_kind, &x->where);

  a1 = gfc_real2real (x, gfc_default_double_kind);
  a2 = gfc_real2real (y, gfc_default_double_kind);

  mpfr_mul (result->value.real, a1->value.real, a2->value.real, GFC_RND_MODE);

  gfc_free_expr (a1);
  gfc_free_expr (a2);

  return range_check (result, "DPROD");
}


gfc_expr *
gfc_simplify_epsilon (gfc_expr * e)
{
  gfc_expr *result;
  int i;

  i = gfc_validate_kind (e->ts.type, e->ts.kind, false);

  result = gfc_constant_result (BT_REAL, e->ts.kind, &e->where);

  mpfr_set (result->value.real, gfc_real_kinds[i].epsilon, GFC_RND_MODE);

  return range_check (result, "EPSILON");
}


gfc_expr *
gfc_simplify_exp (gfc_expr * x)
{
  gfc_expr *result;
  mpfr_t xp, xq;

  if (x->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_constant_result (x->ts.type, x->ts.kind, &x->where);

  switch (x->ts.type)
    {
    case BT_REAL:
      mpfr_exp(result->value.real, x->value.real, GFC_RND_MODE);
      break;

    case BT_COMPLEX:
      gfc_set_model_kind (x->ts.kind);
      mpfr_init (xp);
      mpfr_init (xq);
      mpfr_exp (xq, x->value.complex.r, GFC_RND_MODE);
      mpfr_cos (xp, x->value.complex.i, GFC_RND_MODE);
      mpfr_mul (result->value.complex.r, xq, xp, GFC_RND_MODE);
      mpfr_sin (xp, x->value.complex.i, GFC_RND_MODE);
      mpfr_mul (result->value.complex.i, xq, xp, GFC_RND_MODE);
      mpfr_clear (xp);
      mpfr_clear (xq);
      break;

    default:
      gfc_internal_error ("in gfc_simplify_exp(): Bad type");
    }

  return range_check (result, "EXP");
}

/* FIXME:  MPFR should be able to do this better */
gfc_expr *
gfc_simplify_exponent (gfc_expr * x)
{
  int i;
  mpfr_t tmp;
  gfc_expr *result;

  if (x->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_constant_result (BT_INTEGER, gfc_default_integer_kind,
				&x->where);

  gfc_set_model (x->value.real);

  if (mpfr_sgn (x->value.real) == 0)
    {
      mpz_set_ui (result->value.integer, 0);
      return result;
    }

  mpfr_init (tmp);

  mpfr_abs (tmp, x->value.real, GFC_RND_MODE);
  mpfr_log2 (tmp, tmp, GFC_RND_MODE);

  gfc_mpfr_to_mpz (result->value.integer, tmp);

  /* The model number for tiny(x) is b**(emin - 1) where b is the base and emin
     is the smallest exponent value.  So, we need to add 1 if x is tiny(x).  */
  i = gfc_validate_kind (x->ts.type, x->ts.kind, false);
  if (mpfr_cmp (x->value.real, gfc_real_kinds[i].tiny) == 0)
    mpz_add_ui (result->value.integer,result->value.integer, 1);

  mpfr_clear (tmp);

  return range_check (result, "EXPONENT");
}


gfc_expr *
gfc_simplify_float (gfc_expr * a)
{
  gfc_expr *result;

  if (a->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_int2real (a, gfc_default_real_kind);
  return range_check (result, "FLOAT");
}


gfc_expr *
gfc_simplify_floor (gfc_expr * e, gfc_expr * k)
{
  gfc_expr *result;
  mpfr_t floor;
  int kind;

  kind = get_kind (BT_INTEGER, k, "FLOOR", gfc_default_integer_kind);
  if (kind == -1)
    gfc_internal_error ("gfc_simplify_floor(): Bad kind");

  if (e->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_constant_result (BT_INTEGER, kind, &e->where);

  gfc_set_model_kind (kind);
  mpfr_init (floor);
  mpfr_floor (floor, e->value.real);

  gfc_mpfr_to_mpz (result->value.integer, floor);

  mpfr_clear (floor);

  return range_check (result, "FLOOR");
}


gfc_expr *
gfc_simplify_fraction (gfc_expr * x)
{
  gfc_expr *result;
  mpfr_t absv, exp, pow2;

  if (x->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_constant_result (BT_REAL, x->ts.kind, &x->where);

  gfc_set_model_kind (x->ts.kind);

  if (mpfr_sgn (x->value.real) == 0)
    {
      mpfr_set_ui (result->value.real, 0, GFC_RND_MODE);
      return result;
    }

  mpfr_init (exp);
  mpfr_init (absv);
  mpfr_init (pow2);

  mpfr_abs (absv, x->value.real, GFC_RND_MODE);
  mpfr_log2 (exp, absv, GFC_RND_MODE);

  mpfr_trunc (exp, exp);
  mpfr_add_ui (exp, exp, 1, GFC_RND_MODE);

  mpfr_ui_pow (pow2, 2, exp, GFC_RND_MODE);

  mpfr_div (result->value.real, absv, pow2, GFC_RND_MODE);

  mpfr_clear (exp);
  mpfr_clear (absv);
  mpfr_clear (pow2);

  return range_check (result, "FRACTION");
}


gfc_expr *
gfc_simplify_huge (gfc_expr * e)
{
  gfc_expr *result;
  int i;

  i = gfc_validate_kind (e->ts.type, e->ts.kind, false);

  result = gfc_constant_result (e->ts.type, e->ts.kind, &e->where);

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
gfc_simplify_iachar (gfc_expr * e)
{
  gfc_expr *result;
  int index;

  if (e->expr_type != EXPR_CONSTANT)
    return NULL;

  if (e->value.character.length != 1)
    {
      gfc_error ("Argument of IACHAR at %L must be of length one", &e->where);
      return &gfc_bad_expr;
    }

  index = xascii_table[(int) e->value.character.string[0] & 0xFF];

  result = gfc_int_expr (index);
  result->where = e->where;

  return range_check (result, "IACHAR");
}


gfc_expr *
gfc_simplify_iand (gfc_expr * x, gfc_expr * y)
{
  gfc_expr *result;

  if (x->expr_type != EXPR_CONSTANT || y->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_constant_result (BT_INTEGER, x->ts.kind, &x->where);

  mpz_and (result->value.integer, x->value.integer, y->value.integer);

  return range_check (result, "IAND");
}


gfc_expr *
gfc_simplify_ibclr (gfc_expr * x, gfc_expr * y)
{
  gfc_expr *result;
  int k, pos;

  if (x->expr_type != EXPR_CONSTANT || y->expr_type != EXPR_CONSTANT)
    return NULL;

  if (gfc_extract_int (y, &pos) != NULL || pos < 0)
    {
      gfc_error ("Invalid second argument of IBCLR at %L", &y->where);
      return &gfc_bad_expr;
    }

  k = gfc_validate_kind (x->ts.type, x->ts.kind, false);

  if (pos > gfc_integer_kinds[k].bit_size)
    {
      gfc_error ("Second argument of IBCLR exceeds bit size at %L",
		 &y->where);
      return &gfc_bad_expr;
    }

  result = gfc_copy_expr (x);

  mpz_clrbit (result->value.integer, pos);
  return range_check (result, "IBCLR");
}


gfc_expr *
gfc_simplify_ibits (gfc_expr * x, gfc_expr * y, gfc_expr * z)
{
  gfc_expr *result;
  int pos, len;
  int i, k, bitsize;
  int *bits;

  if (x->expr_type != EXPR_CONSTANT
      || y->expr_type != EXPR_CONSTANT
      || z->expr_type != EXPR_CONSTANT)
    return NULL;

  if (gfc_extract_int (y, &pos) != NULL || pos < 0)
    {
      gfc_error ("Invalid second argument of IBITS at %L", &y->where);
      return &gfc_bad_expr;
    }

  if (gfc_extract_int (z, &len) != NULL || len < 0)
    {
      gfc_error ("Invalid third argument of IBITS at %L", &z->where);
      return &gfc_bad_expr;
    }

  k = gfc_validate_kind (BT_INTEGER, x->ts.kind, false);

  bitsize = gfc_integer_kinds[k].bit_size;

  if (pos + len > bitsize)
    {
      gfc_error
	("Sum of second and third arguments of IBITS exceeds bit size "
	 "at %L", &y->where);
      return &gfc_bad_expr;
    }

  result = gfc_constant_result (x->ts.type, x->ts.kind, &x->where);

  bits = gfc_getmem (bitsize * sizeof (int));

  for (i = 0; i < bitsize; i++)
    bits[i] = 0;

  for (i = 0; i < len; i++)
    bits[i] = mpz_tstbit (x->value.integer, i + pos);

  for (i = 0; i < bitsize; i++)
    {
      if (bits[i] == 0)
	{
	  mpz_clrbit (result->value.integer, i);
	}
      else if (bits[i] == 1)
	{
	  mpz_setbit (result->value.integer, i);
	}
      else
	{
	  gfc_internal_error ("IBITS: Bad bit");
	}
    }

  gfc_free (bits);

  return range_check (result, "IBITS");
}


gfc_expr *
gfc_simplify_ibset (gfc_expr * x, gfc_expr * y)
{
  gfc_expr *result;
  int k, pos;

  if (x->expr_type != EXPR_CONSTANT || y->expr_type != EXPR_CONSTANT)
    return NULL;

  if (gfc_extract_int (y, &pos) != NULL || pos < 0)
    {
      gfc_error ("Invalid second argument of IBSET at %L", &y->where);
      return &gfc_bad_expr;
    }

  k = gfc_validate_kind (x->ts.type, x->ts.kind, false);

  if (pos > gfc_integer_kinds[k].bit_size)
    {
      gfc_error ("Second argument of IBSET exceeds bit size at %L",
		 &y->where);
      return &gfc_bad_expr;
    }

  result = gfc_copy_expr (x);

  mpz_setbit (result->value.integer, pos);

  twos_complement (result->value.integer, gfc_integer_kinds[k].bit_size);

  return range_check (result, "IBSET");
}


gfc_expr *
gfc_simplify_ichar (gfc_expr * e)
{
  gfc_expr *result;
  int index;

  if (e->expr_type != EXPR_CONSTANT)
    return NULL;

  if (e->value.character.length != 1)
    {
      gfc_error ("Argument of ICHAR at %L must be of length one", &e->where);
      return &gfc_bad_expr;
    }

  index = (int) e->value.character.string[0];

  if (index < CHAR_MIN || index > CHAR_MAX)
    {
      gfc_error ("Argument of ICHAR at %L out of range of this processor",
		 &e->where);
      return &gfc_bad_expr;
    }

  result = gfc_int_expr (index);
  result->where = e->where;
  return range_check (result, "ICHAR");
}


gfc_expr *
gfc_simplify_ieor (gfc_expr * x, gfc_expr * y)
{
  gfc_expr *result;

  if (x->expr_type != EXPR_CONSTANT || y->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_constant_result (BT_INTEGER, x->ts.kind, &x->where);

  mpz_xor (result->value.integer, x->value.integer, y->value.integer);

  return range_check (result, "IEOR");
}


gfc_expr *
gfc_simplify_index (gfc_expr * x, gfc_expr * y, gfc_expr * b)
{
  gfc_expr *result;
  int back, len, lensub;
  int i, j, k, count, index = 0, start;

  if (x->expr_type != EXPR_CONSTANT || y->expr_type != EXPR_CONSTANT)
    return NULL;

  if (b != NULL && b->value.logical != 0)
    back = 1;
  else
    back = 0;

  result = gfc_constant_result (BT_INTEGER, gfc_default_integer_kind,
				&x->where);

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
		  if (y->value.character.string[j] ==
		      x->value.character.string[i])
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
		  if (y->value.character.string[j] ==
		      x->value.character.string[i])
		    {
		      start = i;
		      count = 0;

		      for (k = 0; k < lensub; k++)
			{
			  if (y->value.character.string[k] ==
			      x->value.character.string[k + start])
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
		  if (y->value.character.string[j] ==
		      x->value.character.string[len - i])
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
		  if (y->value.character.string[j] ==
		      x->value.character.string[len - i])
		    {
		      start = len - i;
		      if (start <= len - lensub)
			{
			  count = 0;
			  for (k = 0; k < lensub; k++)
			    if (y->value.character.string[k] ==
				x->value.character.string[k + start])
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


gfc_expr *
gfc_simplify_int (gfc_expr * e, gfc_expr * k)
{
  gfc_expr *rpart, *rtrunc, *result;
  int kind;

  kind = get_kind (BT_INTEGER, k, "INT", gfc_default_integer_kind);
  if (kind == -1)
    return &gfc_bad_expr;

  if (e->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_constant_result (BT_INTEGER, kind, &e->where);

  switch (e->ts.type)
    {
    case BT_INTEGER:
      mpz_set (result->value.integer, e->value.integer);
      break;

    case BT_REAL:
      rtrunc = gfc_copy_expr (e);
      mpfr_trunc (rtrunc->value.real, e->value.real);
      gfc_mpfr_to_mpz (result->value.integer, rtrunc->value.real);
      gfc_free_expr (rtrunc);
      break;

    case BT_COMPLEX:
      rpart = gfc_complex2real (e, kind);
      rtrunc = gfc_copy_expr (rpart);
      mpfr_trunc (rtrunc->value.real, rpart->value.real);
      gfc_mpfr_to_mpz (result->value.integer, rtrunc->value.real);
      gfc_free_expr (rpart);
      gfc_free_expr (rtrunc);
      break;

    default:
      gfc_error ("Argument of INT at %L is not a valid type", &e->where);
      gfc_free_expr (result);
      return &gfc_bad_expr;
    }

  return range_check (result, "INT");
}


gfc_expr *
gfc_simplify_ifix (gfc_expr * e)
{
  gfc_expr *rtrunc, *result;

  if (e->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_constant_result (BT_INTEGER, gfc_default_integer_kind,
				&e->where);

  rtrunc = gfc_copy_expr (e);

  mpfr_trunc (rtrunc->value.real, e->value.real);
  gfc_mpfr_to_mpz (result->value.integer, rtrunc->value.real);

  gfc_free_expr (rtrunc);
  return range_check (result, "IFIX");
}


gfc_expr *
gfc_simplify_idint (gfc_expr * e)
{
  gfc_expr *rtrunc, *result;

  if (e->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_constant_result (BT_INTEGER, gfc_default_integer_kind,
				&e->where);

  rtrunc = gfc_copy_expr (e);

  mpfr_trunc (rtrunc->value.real, e->value.real);
  gfc_mpfr_to_mpz (result->value.integer, rtrunc->value.real);

  gfc_free_expr (rtrunc);
  return range_check (result, "IDINT");
}


gfc_expr *
gfc_simplify_ior (gfc_expr * x, gfc_expr * y)
{
  gfc_expr *result;

  if (x->expr_type != EXPR_CONSTANT || y->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_constant_result (BT_INTEGER, x->ts.kind, &x->where);

  mpz_ior (result->value.integer, x->value.integer, y->value.integer);
  return range_check (result, "IOR");
}


gfc_expr *
gfc_simplify_ishft (gfc_expr * e, gfc_expr * s)
{
  gfc_expr *result;
  int shift, ashift, isize, k, *bits, i;

  if (e->expr_type != EXPR_CONSTANT || s->expr_type != EXPR_CONSTANT)
    return NULL;

  if (gfc_extract_int (s, &shift) != NULL)
    {
      gfc_error ("Invalid second argument of ISHFT at %L", &s->where);
      return &gfc_bad_expr;
    }

  k = gfc_validate_kind (BT_INTEGER, e->ts.kind, false);

  isize = gfc_integer_kinds[k].bit_size;

  if (shift >= 0)
    ashift = shift;
  else
    ashift = -shift;

  if (ashift > isize)
    {
      gfc_error
	("Magnitude of second argument of ISHFT exceeds bit size at %L",
	 &s->where);
      return &gfc_bad_expr;
    }

  result = gfc_constant_result (e->ts.type, e->ts.kind, &e->where);

  if (shift == 0)
    {
      mpz_set (result->value.integer, e->value.integer);
      return range_check (result, "ISHFT");
    }
  
  bits = gfc_getmem (isize * sizeof (int));

  for (i = 0; i < isize; i++)
    bits[i] = mpz_tstbit (e->value.integer, i);

  if (shift > 0)
    {
      for (i = 0; i < shift; i++)
	mpz_clrbit (result->value.integer, i);

      for (i = 0; i < isize - shift; i++)
	{
	  if (bits[i] == 0)
	    mpz_clrbit (result->value.integer, i + shift);
	  else
	    mpz_setbit (result->value.integer, i + shift);
	}
    }
  else
    {
      for (i = isize - 1; i >= isize - ashift; i--)
	mpz_clrbit (result->value.integer, i);

      for (i = isize - 1; i >= ashift; i--)
	{
	  if (bits[i] == 0)
	    mpz_clrbit (result->value.integer, i - ashift);
	  else
	    mpz_setbit (result->value.integer, i - ashift);
	}
    }

  twos_complement (result->value.integer, isize);

  gfc_free (bits);
  return result;
}


gfc_expr *
gfc_simplify_ishftc (gfc_expr * e, gfc_expr * s, gfc_expr * sz)
{
  gfc_expr *result;
  int shift, ashift, isize, delta, k;
  int i, *bits;

  if (e->expr_type != EXPR_CONSTANT || s->expr_type != EXPR_CONSTANT)
    return NULL;

  if (gfc_extract_int (s, &shift) != NULL)
    {
      gfc_error ("Invalid second argument of ISHFTC at %L", &s->where);
      return &gfc_bad_expr;
    }

  k = gfc_validate_kind (e->ts.type, e->ts.kind, false);

  if (sz != NULL)
    {
      if (gfc_extract_int (sz, &isize) != NULL || isize < 0)
	{
	  gfc_error ("Invalid third argument of ISHFTC at %L", &sz->where);
	  return &gfc_bad_expr;
	}
    }
  else
    isize = gfc_integer_kinds[k].bit_size;

  if (shift >= 0)
    ashift = shift;
  else
    ashift = -shift;

  if (ashift > isize)
    {
      gfc_error
	("Magnitude of second argument of ISHFTC exceeds third argument "
	 "at %L", &s->where);
      return &gfc_bad_expr;
    }

  result = gfc_constant_result (e->ts.type, e->ts.kind, &e->where);

  if (shift == 0)
    {
      mpz_set (result->value.integer, e->value.integer);
      return result;
    }

  bits = gfc_getmem (isize * sizeof (int));

  for (i = 0; i < isize; i++)
    bits[i] = mpz_tstbit (e->value.integer, i);

  delta = isize - ashift;

  if (shift > 0)
    {
      for (i = 0; i < delta; i++)
	{
	  if (bits[i] == 0)
	    mpz_clrbit (result->value.integer, i + shift);
	  else
	    mpz_setbit (result->value.integer, i + shift);
	}

      for (i = delta; i < isize; i++)
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

      for (i = ashift; i < isize; i++)
	{
	  if (bits[i] == 0)
	    mpz_clrbit (result->value.integer, i + shift);
	  else
	    mpz_setbit (result->value.integer, i + shift);
	}
    }

  twos_complement (result->value.integer, isize);

  gfc_free (bits);
  return result;
}


gfc_expr *
gfc_simplify_kind (gfc_expr * e)
{

  if (e->ts.type == BT_DERIVED)
    {
      gfc_error ("Argument of KIND at %L is a DERIVED type", &e->where);
      return &gfc_bad_expr;
    }

  return gfc_int_expr (e->ts.kind);
}


static gfc_expr *
simplify_bound (gfc_expr * array, gfc_expr * dim, int upper)
{
  gfc_ref *ref;
  gfc_array_spec *as;
  gfc_expr *e;
  int d;

  if (array->expr_type != EXPR_VARIABLE)
    return NULL;

  if (dim == NULL)
    /* TODO: Simplify constant multi-dimensional bounds.  */
    return NULL;

  if (dim->expr_type != EXPR_CONSTANT)
    return NULL;

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

	    case AR_SECTION:
	    case AR_UNKNOWN:
	      return NULL;
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
  if (as->type == AS_DEFERRED || as->type == AS_ASSUMED_SHAPE)
    return NULL;

  d = mpz_get_si (dim->value.integer);

  if (d < 1 || d > as->rank
      || (d == as->rank && as->type == AS_ASSUMED_SIZE && upper))
    {
      gfc_error ("DIM argument at %L is out of bounds", &dim->where);
      return &gfc_bad_expr;
    }

  e = upper ? as->upper[d-1] : as->lower[d-1];

  if (e->expr_type != EXPR_CONSTANT)
    return NULL;

  return gfc_copy_expr (e);
}


gfc_expr *
gfc_simplify_lbound (gfc_expr * array, gfc_expr * dim)
{
  return simplify_bound (array, dim, 0);
}


gfc_expr *
gfc_simplify_len (gfc_expr * e)
{
  gfc_expr *result;

  if (e->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_constant_result (BT_INTEGER, gfc_default_integer_kind,
				&e->where);

  mpz_set_si (result->value.integer, e->value.character.length);
  return range_check (result, "LEN");
}


gfc_expr *
gfc_simplify_len_trim (gfc_expr * e)
{
  gfc_expr *result;
  int count, len, lentrim, i;

  if (e->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_constant_result (BT_INTEGER, gfc_default_integer_kind,
				&e->where);

  len = e->value.character.length;

  for (count = 0, i = 1; i <= len; i++)
    if (e->value.character.string[len - i] == ' ')
      count++;
    else
      break;

  lentrim = len - count;

  mpz_set_si (result->value.integer, lentrim);
  return range_check (result, "LEN_TRIM");
}


gfc_expr *
gfc_simplify_lge (gfc_expr * a, gfc_expr * b)
{

  if (a->expr_type != EXPR_CONSTANT || b->expr_type != EXPR_CONSTANT)
    return NULL;

  return gfc_logical_expr (gfc_compare_string (a, b, xascii_table) >= 0,
			   &a->where);
}


gfc_expr *
gfc_simplify_lgt (gfc_expr * a, gfc_expr * b)
{

  if (a->expr_type != EXPR_CONSTANT || b->expr_type != EXPR_CONSTANT)
    return NULL;

  return gfc_logical_expr (gfc_compare_string (a, b, xascii_table) > 0,
			   &a->where);
}


gfc_expr *
gfc_simplify_lle (gfc_expr * a, gfc_expr * b)
{

  if (a->expr_type != EXPR_CONSTANT || b->expr_type != EXPR_CONSTANT)
    return NULL;

  return gfc_logical_expr (gfc_compare_string (a, b, xascii_table) <= 0,
			   &a->where);
}


gfc_expr *
gfc_simplify_llt (gfc_expr * a, gfc_expr * b)
{

  if (a->expr_type != EXPR_CONSTANT || b->expr_type != EXPR_CONSTANT)
    return NULL;

  return gfc_logical_expr (gfc_compare_string (a, b, xascii_table) < 0,
			   &a->where);
}


gfc_expr *
gfc_simplify_log (gfc_expr * x)
{
  gfc_expr *result;
  mpfr_t xr, xi;

  if (x->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_constant_result (x->ts.type, x->ts.kind, &x->where);

  gfc_set_model_kind (x->ts.kind);

  switch (x->ts.type)
    {
    case BT_REAL:
      if (mpfr_sgn (x->value.real) <= 0)
	{
	  gfc_error
	    ("Argument of LOG at %L cannot be less than or equal to zero",
	     &x->where);
	  gfc_free_expr (result);
	  return &gfc_bad_expr;
	}

      mpfr_log(result->value.real, x->value.real, GFC_RND_MODE);
      break;

    case BT_COMPLEX:
      if ((mpfr_sgn (x->value.complex.r) == 0)
	  && (mpfr_sgn (x->value.complex.i) == 0))
	{
	  gfc_error ("Complex argument of LOG at %L cannot be zero",
		     &x->where);
	  gfc_free_expr (result);
	  return &gfc_bad_expr;
	}

      mpfr_init (xr);
      mpfr_init (xi);

      arctangent2 (x->value.complex.i, x->value.complex.r,
	           result->value.complex.i);

      mpfr_mul (xr, x->value.complex.r, x->value.complex.r, GFC_RND_MODE);
      mpfr_mul (xi, x->value.complex.i, x->value.complex.i, GFC_RND_MODE);
      mpfr_add (xr, xr, xi, GFC_RND_MODE);
      mpfr_sqrt (xr, xr, GFC_RND_MODE);
      mpfr_log (result->value.complex.r, xr, GFC_RND_MODE);

      mpfr_clear (xr);
      mpfr_clear (xi);

      break;

    default:
      gfc_internal_error ("gfc_simplify_log: bad type");
    }

  return range_check (result, "LOG");
}


gfc_expr *
gfc_simplify_log10 (gfc_expr * x)
{
  gfc_expr *result;

  if (x->expr_type != EXPR_CONSTANT)
    return NULL;

  gfc_set_model_kind (x->ts.kind);

  if (mpfr_sgn (x->value.real) <= 0)
    {
      gfc_error
	("Argument of LOG10 at %L cannot be less than or equal to zero",
	 &x->where);
      return &gfc_bad_expr;
    }

  result = gfc_constant_result (x->ts.type, x->ts.kind, &x->where);

  mpfr_log10 (result->value.real, x->value.real, GFC_RND_MODE);

  return range_check (result, "LOG10");
}


gfc_expr *
gfc_simplify_logical (gfc_expr * e, gfc_expr * k)
{
  gfc_expr *result;
  int kind;

  kind = get_kind (BT_LOGICAL, k, "LOGICAL", gfc_default_logical_kind);
  if (kind < 0)
    return &gfc_bad_expr;

  if (e->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_constant_result (BT_LOGICAL, kind, &e->where);

  result->value.logical = e->value.logical;

  return result;
}


/* This function is special since MAX() can take any number of
   arguments.  The simplified expression is a rewritten version of the
   argument list containing at most one constant element.  Other
   constant elements are deleted.  Because the argument list has
   already been checked, this function always succeeds.  sign is 1 for
   MAX(), -1 for MIN().  */

static gfc_expr *
simplify_min_max (gfc_expr * expr, int sign)
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

      switch (arg->expr->ts.type)
	{
	case BT_INTEGER:
	  if (mpz_cmp (arg->expr->value.integer,
		       extremum->expr->value.integer) * sign > 0)
	    mpz_set (extremum->expr->value.integer, arg->expr->value.integer);

	  break;

	case BT_REAL:
	  if (mpfr_cmp (arg->expr->value.real, extremum->expr->value.real) *
	      sign > 0)
	    mpfr_set (extremum->expr->value.real, arg->expr->value.real,
                      GFC_RND_MODE);

	  break;

	default:
	  gfc_internal_error ("gfc_simplify_max(): Bad type in arglist");
	}

      /* Delete the extra constant argument.  */
      if (last == NULL)
	expr->value.function.actual = arg->next;
      else
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
gfc_simplify_min (gfc_expr * e)
{
  return simplify_min_max (e, -1);
}


gfc_expr *
gfc_simplify_max (gfc_expr * e)
{
  return simplify_min_max (e, 1);
}


gfc_expr *
gfc_simplify_maxexponent (gfc_expr * x)
{
  gfc_expr *result;
  int i;

  i = gfc_validate_kind (BT_REAL, x->ts.kind, false);

  result = gfc_int_expr (gfc_real_kinds[i].max_exponent);
  result->where = x->where;

  return result;
}


gfc_expr *
gfc_simplify_minexponent (gfc_expr * x)
{
  gfc_expr *result;
  int i;

  i = gfc_validate_kind (BT_REAL, x->ts.kind, false);

  result = gfc_int_expr (gfc_real_kinds[i].min_exponent);
  result->where = x->where;

  return result;
}


gfc_expr *
gfc_simplify_mod (gfc_expr * a, gfc_expr * p)
{
  gfc_expr *result;
  mpfr_t quot, iquot, term;
  int kind;

  if (a->expr_type != EXPR_CONSTANT || p->expr_type != EXPR_CONSTANT)
    return NULL;

  kind = a->ts.kind > p->ts.kind ? a->ts.kind : p->ts.kind;
  result = gfc_constant_result (a->ts.type, kind, &a->where);

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
      mpfr_init (quot);
      mpfr_init (iquot);
      mpfr_init (term);

      mpfr_div (quot, a->value.real, p->value.real, GFC_RND_MODE);
      mpfr_trunc (iquot, quot);
      mpfr_mul (term, iquot, p->value.real, GFC_RND_MODE);
      mpfr_sub (result->value.real, a->value.real, term, GFC_RND_MODE);

      mpfr_clear (quot);
      mpfr_clear (iquot);
      mpfr_clear (term);
      break;

    default:
      gfc_internal_error ("gfc_simplify_mod(): Bad arguments");
    }

  return range_check (result, "MOD");
}


gfc_expr *
gfc_simplify_modulo (gfc_expr * a, gfc_expr * p)
{
  gfc_expr *result;
  mpfr_t quot, iquot, term;
  int kind;

  if (a->expr_type != EXPR_CONSTANT || p->expr_type != EXPR_CONSTANT)
    return NULL;

  kind = a->ts.kind > p->ts.kind ? a->ts.kind : p->ts.kind;
  result = gfc_constant_result (a->ts.type, kind, &a->where);

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
      mpfr_init (quot);
      mpfr_init (iquot);
      mpfr_init (term);

      mpfr_div (quot, a->value.real, p->value.real, GFC_RND_MODE);
      mpfr_floor (iquot, quot);
      mpfr_mul (term, iquot, p->value.real, GFC_RND_MODE);
      mpfr_sub (result->value.real, a->value.real, term, GFC_RND_MODE);

      mpfr_clear (quot);
      mpfr_clear (iquot);
      mpfr_clear (term);
      break;

    default:
      gfc_internal_error ("gfc_simplify_modulo(): Bad arguments");
    }

  return range_check (result, "MODULO");
}


/* Exists for the sole purpose of consistency with other intrinsics.  */
gfc_expr *
gfc_simplify_mvbits (gfc_expr * f  ATTRIBUTE_UNUSED,
		     gfc_expr * fp ATTRIBUTE_UNUSED,
		     gfc_expr * l  ATTRIBUTE_UNUSED,
		     gfc_expr * to ATTRIBUTE_UNUSED,
		     gfc_expr * tp ATTRIBUTE_UNUSED)
{
  return NULL;
}


gfc_expr *
gfc_simplify_nearest (gfc_expr * x, gfc_expr * s)
{
  gfc_expr *result;
  mpfr_t tmp;
  int direction, sgn;

  if (x->expr_type != EXPR_CONSTANT || s->expr_type != EXPR_CONSTANT)
    return NULL;

  gfc_set_model_kind (x->ts.kind);
  result = gfc_copy_expr (x);

  direction = mpfr_sgn (s->value.real);

  if (direction == 0)
    {
      gfc_error ("Second argument of NEAREST at %L may not be zero",
		 &s->where);
      gfc_free (result);
      return &gfc_bad_expr;
    }

  /* TODO: Use mpfr_nextabove and mpfr_nextbelow once we move to a
     newer version of mpfr.  */

  sgn = mpfr_sgn (x->value.real);

  if (sgn == 0)
    {
      int k = gfc_validate_kind (BT_REAL, x->ts.kind, 0);

      if (direction > 0)
	mpfr_add (result->value.real,
		  x->value.real, gfc_real_kinds[k].subnormal, GFC_RND_MODE);
      else
	mpfr_sub (result->value.real,
		  x->value.real, gfc_real_kinds[k].subnormal, GFC_RND_MODE);
    }
  else
    {
      if (sgn < 0)
	{
	  direction = -direction;
	  mpfr_neg (result->value.real, result->value.real, GFC_RND_MODE);
	}

      if (direction > 0)
	mpfr_add_one_ulp (result->value.real, GFC_RND_MODE);
      else
	{
	  /* In this case the exponent can shrink, which makes us skip
	     over one number because we subtract one ulp with the
	     larger exponent.  Thus we need to compensate for this.  */
	  mpfr_init_set (tmp, result->value.real, GFC_RND_MODE);

	  mpfr_sub_one_ulp (result->value.real, GFC_RND_MODE);
	  mpfr_add_one_ulp (result->value.real, GFC_RND_MODE);

	  /* If we're back to where we started, the spacing is one
	     ulp, and we get the correct result by subtracting.  */
	  if (mpfr_cmp (tmp, result->value.real) == 0)
	    mpfr_sub_one_ulp (result->value.real, GFC_RND_MODE);

	  mpfr_clear (tmp);
	}

      if (sgn < 0)
	mpfr_neg (result->value.real, result->value.real, GFC_RND_MODE);
    }

  return range_check (result, "NEAREST");
}


static gfc_expr *
simplify_nint (const char *name, gfc_expr * e, gfc_expr * k)
{
  gfc_expr *itrunc, *result;
  int kind;

  kind = get_kind (BT_INTEGER, k, name, gfc_default_integer_kind);
  if (kind == -1)
    return &gfc_bad_expr;

  if (e->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_constant_result (BT_INTEGER, kind, &e->where);

  itrunc = gfc_copy_expr (e);

  mpfr_round(itrunc->value.real, e->value.real);

  gfc_mpfr_to_mpz (result->value.integer, itrunc->value.real);

  gfc_free_expr (itrunc);

  return range_check (result, name);
}


gfc_expr *
gfc_simplify_nint (gfc_expr * e, gfc_expr * k)
{
  return simplify_nint ("NINT", e, k);
}


gfc_expr *
gfc_simplify_idnint (gfc_expr * e)
{
  return simplify_nint ("IDNINT", e, NULL);
}


gfc_expr *
gfc_simplify_not (gfc_expr * e)
{
  gfc_expr *result;
  int i;

  if (e->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_constant_result (e->ts.type, e->ts.kind, &e->where);

  mpz_com (result->value.integer, e->value.integer);

  /* Because of how GMP handles numbers, the result must be ANDed with
     the max_int mask.  For radices <> 2, this will require change.  */

  i = gfc_validate_kind (BT_INTEGER, e->ts.kind, false);

  mpz_and (result->value.integer, result->value.integer,
	   gfc_integer_kinds[i].max_int);

  twos_complement (result->value.integer, gfc_integer_kinds[i].bit_size);

  return range_check (result, "NOT");
}


gfc_expr *
gfc_simplify_null (gfc_expr * mold)
{
  gfc_expr *result;

  result = gfc_get_expr ();
  result->expr_type = EXPR_NULL;

  if (mold == NULL)
    result->ts.type = BT_UNKNOWN;
  else
    {
      result->ts = mold->ts;
      result->where = mold->where;
    }

  return result;
}


gfc_expr *
gfc_simplify_or (gfc_expr * x, gfc_expr * y)
{
  gfc_expr *result;
  int kind;

  if (x->expr_type != EXPR_CONSTANT || y->expr_type != EXPR_CONSTANT)
    return NULL;

  kind = x->ts.kind > y->ts.kind ? x->ts.kind : y->ts.kind;
  if (x->ts.type == BT_INTEGER)
    {
      result = gfc_constant_result (BT_INTEGER, kind, &x->where);
      mpz_ior (result->value.integer, x->value.integer, y->value.integer);
    }
  else /* BT_LOGICAL */
    {
      result = gfc_constant_result (BT_LOGICAL, kind, &x->where);
      result->value.logical = x->value.logical || y->value.logical;
    }

  return range_check (result, "OR");
}


gfc_expr *
gfc_simplify_precision (gfc_expr * e)
{
  gfc_expr *result;
  int i;

  i = gfc_validate_kind (e->ts.type, e->ts.kind, false);

  result = gfc_int_expr (gfc_real_kinds[i].precision);
  result->where = e->where;

  return result;
}


gfc_expr *
gfc_simplify_radix (gfc_expr * e)
{
  gfc_expr *result;
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

  result = gfc_int_expr (i);
  result->where = e->where;

  return result;
}


gfc_expr *
gfc_simplify_range (gfc_expr * e)
{
  gfc_expr *result;
  int i;
  long j;

  i = gfc_validate_kind (e->ts.type, e->ts.kind, false);

  switch (e->ts.type)
    {
    case BT_INTEGER:
      j = gfc_integer_kinds[i].range;
      break;

    case BT_REAL:
    case BT_COMPLEX:
      j = gfc_real_kinds[i].range;
      break;

    default:
      gcc_unreachable ();
    }

  result = gfc_int_expr (j);
  result->where = e->where;

  return result;
}


gfc_expr *
gfc_simplify_real (gfc_expr * e, gfc_expr * k)
{
  gfc_expr *result;
  int kind;

  if (e->ts.type == BT_COMPLEX)
    kind = get_kind (BT_REAL, k, "REAL", e->ts.kind);
  else
    kind = get_kind (BT_REAL, k, "REAL", gfc_default_real_kind);

  if (kind == -1)
    return &gfc_bad_expr;

  if (e->expr_type != EXPR_CONSTANT)
    return NULL;

  switch (e->ts.type)
    {
    case BT_INTEGER:
      result = gfc_int2real (e, kind);
      break;

    case BT_REAL:
      result = gfc_real2real (e, kind);
      break;

    case BT_COMPLEX:
      result = gfc_complex2real (e, kind);
      break;

    default:
      gfc_internal_error ("bad type in REAL");
      /* Not reached */
    }

  return range_check (result, "REAL");
}


gfc_expr *
gfc_simplify_realpart (gfc_expr * e)
{
  gfc_expr *result;

  if (e->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_constant_result (BT_REAL, e->ts.kind, &e->where);
  mpfr_set (result->value.real, e->value.complex.r, GFC_RND_MODE);

  return range_check (result, "REALPART");
}

gfc_expr *
gfc_simplify_repeat (gfc_expr * e, gfc_expr * n)
{
  gfc_expr *result;
  int i, j, len, ncopies, nlen;

  if (e->expr_type != EXPR_CONSTANT || n->expr_type != EXPR_CONSTANT)
    return NULL;

  if (n != NULL && (gfc_extract_int (n, &ncopies) != NULL || ncopies < 0))
    {
      gfc_error ("Invalid second argument of REPEAT at %L", &n->where);
      return &gfc_bad_expr;
    }

  len = e->value.character.length;
  nlen = ncopies * len;

  result = gfc_constant_result (BT_CHARACTER, e->ts.kind, &e->where);

  if (ncopies == 0)
    {
      result->value.character.string = gfc_getmem (1);
      result->value.character.length = 0;
      result->value.character.string[0] = '\0';
      return result;
    }

  result->value.character.length = nlen;
  result->value.character.string = gfc_getmem (nlen + 1);

  for (i = 0; i < ncopies; i++)
    for (j = 0; j < len; j++)
      result->value.character.string[j + i * len] =
	e->value.character.string[j];

  result->value.character.string[nlen] = '\0';	/* For debugger */
  return result;
}


/* This one is a bear, but mainly has to do with shuffling elements.  */

gfc_expr *
gfc_simplify_reshape (gfc_expr * source, gfc_expr * shape_exp,
		      gfc_expr * pad, gfc_expr * order_exp)
{

  int order[GFC_MAX_DIMENSIONS], shape[GFC_MAX_DIMENSIONS];
  int i, rank, npad, x[GFC_MAX_DIMENSIONS];
  gfc_constructor *head, *tail;
  mpz_t index, size;
  unsigned long j;
  size_t nsource;
  gfc_expr *e;

  /* Unpack the shape array.  */
  if (source->expr_type != EXPR_ARRAY || !gfc_is_constant_expr (source))
    return NULL;

  if (shape_exp->expr_type != EXPR_ARRAY || !gfc_is_constant_expr (shape_exp))
    return NULL;

  if (pad != NULL
      && (pad->expr_type != EXPR_ARRAY
	  || !gfc_is_constant_expr (pad)))
    return NULL;

  if (order_exp != NULL
      && (order_exp->expr_type != EXPR_ARRAY
	  || !gfc_is_constant_expr (order_exp)))
    return NULL;

  mpz_init (index);
  rank = 0;
  head = tail = NULL;

  for (;;)
    {
      e = gfc_get_array_element (shape_exp, rank);
      if (e == NULL)
	break;

      if (gfc_extract_int (e, &shape[rank]) != NULL)
	{
	  gfc_error ("Integer too large in shape specification at %L",
		     &e->where);
	  gfc_free_expr (e);
	  goto bad_reshape;
	}

      gfc_free_expr (e);

      if (rank >= GFC_MAX_DIMENSIONS)
	{
	  gfc_error ("Too many dimensions in shape specification for RESHAPE "
		     "at %L", &e->where);

	  goto bad_reshape;
	}

      if (shape[rank] < 0)
	{
	  gfc_error ("Shape specification at %L cannot be negative",
		     &e->where);
	  goto bad_reshape;
	}

      rank++;
    }

  if (rank == 0)
    {
      gfc_error ("Shape specification at %L cannot be the null array",
		 &shape_exp->where);
      goto bad_reshape;
    }

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
	  e = gfc_get_array_element (order_exp, i);
	  if (e == NULL)
	    {
	      gfc_error
		("ORDER parameter of RESHAPE at %L is not the same size "
		 "as SHAPE parameter", &order_exp->where);
	      goto bad_reshape;
	    }

	  if (gfc_extract_int (e, &order[i]) != NULL)
	    {
	      gfc_error ("Error in ORDER parameter of RESHAPE at %L",
			 &e->where);
	      gfc_free_expr (e);
	      goto bad_reshape;
	    }

	  gfc_free_expr (e);

	  if (order[i] < 1 || order[i] > rank)
	    {
	      gfc_error ("ORDER parameter of RESHAPE at %L is out of range",
			 &e->where);
	      goto bad_reshape;
	    }

	  order[i]--;

	  if (x[order[i]])
	    {
	      gfc_error ("Invalid permutation in ORDER parameter at %L",
			 &e->where);
	      goto bad_reshape;
	    }

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

  for (;;)
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
	gfc_internal_error ("Reshaped array too large at %L", &e->where);

      j = mpz_get_ui (index);

      if (j < nsource)
	e = gfc_get_array_element (source, j);
      else
	{
	  j = j - nsource;

	  if (npad == 0)
	    {
	      gfc_error
		("PAD parameter required for short SOURCE parameter at %L",
		 &source->where);
	      goto bad_reshape;
	    }

	  j = j % npad;
	  e = gfc_get_array_element (pad, j);
	}

      if (head == NULL)
	head = tail = gfc_get_constructor ();
      else
	{
	  tail->next = gfc_get_constructor ();
	  tail = tail->next;
	}

      if (e == NULL)
	goto bad_reshape;

      tail->where = e->where;
      tail->expr = e;

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

  e = gfc_get_expr ();
  e->where = source->where;
  e->expr_type = EXPR_ARRAY;
  e->value.constructor = head;
  e->shape = gfc_get_shape (rank);

  for (i = 0; i < rank; i++)
    mpz_init_set_ui (e->shape[i], shape[i]);

  e->ts = source->ts;
  e->rank = rank;

  return e;

bad_reshape:
  gfc_free_constructor (head);
  mpz_clear (index);
  return &gfc_bad_expr;
}


gfc_expr *
gfc_simplify_rrspacing (gfc_expr * x)
{
  gfc_expr *result;
  mpfr_t absv, log2, exp, frac, pow2;
  int i, p;

  if (x->expr_type != EXPR_CONSTANT)
    return NULL;

  i = gfc_validate_kind (x->ts.type, x->ts.kind, false);

  result = gfc_constant_result (BT_REAL, x->ts.kind, &x->where);

  p = gfc_real_kinds[i].digits;

  gfc_set_model_kind (x->ts.kind);

  if (mpfr_sgn (x->value.real) == 0)
    {
      mpfr_ui_div (result->value.real, 1, gfc_real_kinds[i].tiny, GFC_RND_MODE);
      return result;
    }

  mpfr_init (log2);
  mpfr_init (absv);
  mpfr_init (frac);
  mpfr_init (pow2);

  mpfr_abs (absv, x->value.real, GFC_RND_MODE);
  mpfr_log2 (log2, absv, GFC_RND_MODE);

  mpfr_trunc (log2, log2);
  mpfr_add_ui (exp, log2, 1, GFC_RND_MODE);

  mpfr_ui_pow (pow2, 2, exp, GFC_RND_MODE);
  mpfr_div (frac, absv, pow2, GFC_RND_MODE);

  mpfr_mul_2exp (result->value.real, frac, (unsigned long)p, GFC_RND_MODE);

  mpfr_clear (log2);
  mpfr_clear (absv);
  mpfr_clear (frac);
  mpfr_clear (pow2);

  return range_check (result, "RRSPACING");
}


gfc_expr *
gfc_simplify_scale (gfc_expr * x, gfc_expr * i)
{
  int k, neg_flag, power, exp_range;
  mpfr_t scale, radix;
  gfc_expr *result;

  if (x->expr_type != EXPR_CONSTANT || i->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_constant_result (BT_REAL, x->ts.kind, &x->where);

  if (mpfr_sgn (x->value.real) == 0)
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

  mpfr_clear (scale);
  mpfr_clear (radix);

  return range_check (result, "SCALE");
}


gfc_expr *
gfc_simplify_scan (gfc_expr * e, gfc_expr * c, gfc_expr * b)
{
  gfc_expr *result;
  int back;
  size_t i;
  size_t indx, len, lenc;

  if (e->expr_type != EXPR_CONSTANT || c->expr_type != EXPR_CONSTANT)
    return NULL;

  if (b != NULL && b->value.logical != 0)
    back = 1;
  else
    back = 0;

  result = gfc_constant_result (BT_INTEGER, gfc_default_integer_kind,
				&e->where);

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
          indx =
            strcspn (e->value.character.string, c->value.character.string) + 1;
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
  mpz_set_ui (result->value.integer, indx);
  return range_check (result, "SCAN");
}


gfc_expr *
gfc_simplify_selected_int_kind (gfc_expr * e)
{
  int i, kind, range;
  gfc_expr *result;

  if (e->expr_type != EXPR_CONSTANT || gfc_extract_int (e, &range) != NULL)
    return NULL;

  kind = INT_MAX;

  for (i = 0; gfc_integer_kinds[i].kind != 0; i++)
    if (gfc_integer_kinds[i].range >= range
	&& gfc_integer_kinds[i].kind < kind)
      kind = gfc_integer_kinds[i].kind;

  if (kind == INT_MAX)
    kind = -1;

  result = gfc_int_expr (kind);
  result->where = e->where;

  return result;
}


gfc_expr *
gfc_simplify_selected_real_kind (gfc_expr * p, gfc_expr * q)
{
  int range, precision, i, kind, found_precision, found_range;
  gfc_expr *result;

  if (p == NULL)
    precision = 0;
  else
    {
      if (p->expr_type != EXPR_CONSTANT
	  || gfc_extract_int (p, &precision) != NULL)
	return NULL;
    }

  if (q == NULL)
    range = 0;
  else
    {
      if (q->expr_type != EXPR_CONSTANT
	  || gfc_extract_int (q, &range) != NULL)
	return NULL;
    }

  kind = INT_MAX;
  found_precision = 0;
  found_range = 0;

  for (i = 0; gfc_real_kinds[i].kind != 0; i++)
    {
      if (gfc_real_kinds[i].precision >= precision)
	found_precision = 1;

      if (gfc_real_kinds[i].range >= range)
	found_range = 1;

      if (gfc_real_kinds[i].precision >= precision
	  && gfc_real_kinds[i].range >= range && gfc_real_kinds[i].kind < kind)
	kind = gfc_real_kinds[i].kind;
    }

  if (kind == INT_MAX)
    {
      kind = 0;

      if (!found_precision)
	kind = -1;
      if (!found_range)
	kind -= 2;
    }

  result = gfc_int_expr (kind);
  result->where = (p != NULL) ? p->where : q->where;

  return result;
}


gfc_expr *
gfc_simplify_set_exponent (gfc_expr * x, gfc_expr * i)
{
  gfc_expr *result;
  mpfr_t exp, absv, log2, pow2, frac;
  unsigned long exp2;

  if (x->expr_type != EXPR_CONSTANT || i->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_constant_result (BT_REAL, x->ts.kind, &x->where);

  gfc_set_model_kind (x->ts.kind);

  if (mpfr_sgn (x->value.real) == 0)
    {
      mpfr_set_ui (result->value.real, 0, GFC_RND_MODE);
      return result;
    }

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

  mpfr_clear (absv);
  mpfr_clear (log2);
  mpfr_clear (pow2);
  mpfr_clear (frac);

  return range_check (result, "SET_EXPONENT");
}


gfc_expr *
gfc_simplify_shape (gfc_expr * source)
{
  mpz_t shape[GFC_MAX_DIMENSIONS];
  gfc_expr *result, *e, *f;
  gfc_array_ref *ar;
  int n;
  try t;

  if (source->rank == 0 || source->expr_type != EXPR_VARIABLE)
    return NULL;

  result = gfc_start_constructor (BT_INTEGER, gfc_default_integer_kind,
				  &source->where);

  ar = gfc_find_array_ref (source);

  t = gfc_array_ref_shape (ar, shape);

  for (n = 0; n < source->rank; n++)
    {
      e = gfc_constant_result (BT_INTEGER, gfc_default_integer_kind,
			       &source->where);

      if (t == SUCCESS)
	{
	  mpz_set (e->value.integer, shape[n]);
	  mpz_clear (shape[n]);
	}
      else
	{
	  mpz_set_ui (e->value.integer, n + 1);

	  f = gfc_simplify_size (source, e);
	  gfc_free_expr (e);
	  if (f == NULL)
	    {
	      gfc_free_expr (result);
	      return NULL;
	    }
	  else
	    {
	      e = f;
	    }
	}

      gfc_append_constructor (result, e);
    }

  return result;
}


gfc_expr *
gfc_simplify_size (gfc_expr * array, gfc_expr * dim)
{
  mpz_t size;
  gfc_expr *result;
  int d;

  if (dim == NULL)
    {
      if (gfc_array_size (array, &size) == FAILURE)
	return NULL;
    }
  else
    {
      if (dim->expr_type != EXPR_CONSTANT)
	return NULL;

      d = mpz_get_ui (dim->value.integer) - 1;
      if (gfc_array_dimen_size (array, d, &size) == FAILURE)
	return NULL;
    }

  result = gfc_constant_result (BT_INTEGER, gfc_default_integer_kind,
				&array->where);

  mpz_set (result->value.integer, size);

  return result;
}


gfc_expr *
gfc_simplify_sign (gfc_expr * x, gfc_expr * y)
{
  gfc_expr *result;

  if (x->expr_type != EXPR_CONSTANT || y->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_constant_result (x->ts.type, x->ts.kind, &x->where);

  switch (x->ts.type)
    {
    case BT_INTEGER:
      mpz_abs (result->value.integer, x->value.integer);
      if (mpz_sgn (y->value.integer) < 0)
	mpz_neg (result->value.integer, result->value.integer);

      break;

    case BT_REAL:
      /* TODO: Handle -0.0 and +0.0 correctly on machines that support
         it.  */
      mpfr_abs (result->value.real, x->value.real, GFC_RND_MODE);
      if (mpfr_sgn (y->value.real) < 0)
	mpfr_neg (result->value.real, result->value.real, GFC_RND_MODE);

      break;

    default:
      gfc_internal_error ("Bad type in gfc_simplify_sign");
    }

  return result;
}


gfc_expr *
gfc_simplify_sin (gfc_expr * x)
{
  gfc_expr *result;
  mpfr_t xp, xq;

  if (x->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_constant_result (x->ts.type, x->ts.kind, &x->where);

  switch (x->ts.type)
    {
    case BT_REAL:
      mpfr_sin (result->value.real, x->value.real, GFC_RND_MODE);
      break;

    case BT_COMPLEX:
      gfc_set_model (x->value.real);
      mpfr_init (xp);
      mpfr_init (xq);

      mpfr_sin  (xp, x->value.complex.r, GFC_RND_MODE);
      mpfr_cosh (xq, x->value.complex.i, GFC_RND_MODE);
      mpfr_mul (result->value.complex.r, xp, xq, GFC_RND_MODE);

      mpfr_cos  (xp, x->value.complex.r, GFC_RND_MODE);
      mpfr_sinh (xq, x->value.complex.i, GFC_RND_MODE);
      mpfr_mul (result->value.complex.i, xp, xq, GFC_RND_MODE);

      mpfr_clear (xp);
      mpfr_clear (xq);
      break;

    default:
      gfc_internal_error ("in gfc_simplify_sin(): Bad type");
    }

  return range_check (result, "SIN");
}


gfc_expr *
gfc_simplify_sinh (gfc_expr * x)
{
  gfc_expr *result;

  if (x->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_constant_result (x->ts.type, x->ts.kind, &x->where);

  mpfr_sinh(result->value.real, x->value.real, GFC_RND_MODE);

  return range_check (result, "SINH");
}


/* The argument is always a double precision real that is converted to
   single precision.  TODO: Rounding!  */

gfc_expr *
gfc_simplify_sngl (gfc_expr * a)
{
  gfc_expr *result;

  if (a->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_real2real (a, gfc_default_real_kind);
  return range_check (result, "SNGL");
}


gfc_expr *
gfc_simplify_spacing (gfc_expr * x)
{
  gfc_expr *result;
  mpfr_t absv, log2;
  long diff;
  int i, p;

  if (x->expr_type != EXPR_CONSTANT)
    return NULL;

  i = gfc_validate_kind (x->ts.type, x->ts.kind, false);

  p = gfc_real_kinds[i].digits;

  result = gfc_constant_result (BT_REAL, x->ts.kind, &x->where);

  gfc_set_model_kind (x->ts.kind);

  if (mpfr_sgn (x->value.real) == 0)
    {
      mpfr_set (result->value.real, gfc_real_kinds[i].tiny, GFC_RND_MODE);
      return result;
    }

  mpfr_init (log2);
  mpfr_init (absv);

  mpfr_abs (absv, x->value.real, GFC_RND_MODE);
  mpfr_log2 (log2, absv, GFC_RND_MODE);
  mpfr_trunc (log2, log2);

  mpfr_add_ui (log2, log2, 1, GFC_RND_MODE);

  /* FIXME: We should be using mpfr_get_si here, but this function is
     not available with the version of mpfr distributed with gmp (as of
     2004-09-17). Replace once mpfr has been imported into the gcc cvs
     tree.  */
  diff = (long)mpfr_get_d (log2, GFC_RND_MODE) - (long)p;
  mpfr_set_ui (result->value.real, 1, GFC_RND_MODE);
  mpfr_mul_2si (result->value.real, result->value.real, diff, GFC_RND_MODE);

  mpfr_clear (log2);
  mpfr_clear (absv);

  if (mpfr_cmp (result->value.real, gfc_real_kinds[i].tiny) < 0)
    mpfr_set (result->value.real, gfc_real_kinds[i].tiny, GFC_RND_MODE);

  return range_check (result, "SPACING");
}


gfc_expr *
gfc_simplify_sqrt (gfc_expr * e)
{
  gfc_expr *result;
  mpfr_t ac, ad, s, t, w;

  if (e->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_constant_result (e->ts.type, e->ts.kind, &e->where);

  switch (e->ts.type)
    {
    case BT_REAL:
      if (mpfr_cmp_si (e->value.real, 0) < 0)
	goto negative_arg;
      mpfr_sqrt (result->value.real, e->value.real, GFC_RND_MODE);

      break;

    case BT_COMPLEX:
      /* Formula taken from Numerical Recipes to avoid over- and
         underflow.  */

      gfc_set_model (e->value.real);
      mpfr_init (ac);
      mpfr_init (ad);
      mpfr_init (s);
      mpfr_init (t);
      mpfr_init (w);

      if (mpfr_cmp_ui (e->value.complex.r, 0) == 0
	  && mpfr_cmp_ui (e->value.complex.i, 0) == 0)
	{

	  mpfr_set_ui (result->value.complex.r, 0, GFC_RND_MODE);
	  mpfr_set_ui (result->value.complex.i, 0, GFC_RND_MODE);
	  break;
	}

      mpfr_abs (ac, e->value.complex.r, GFC_RND_MODE);
      mpfr_abs (ad, e->value.complex.i, GFC_RND_MODE);

      if (mpfr_cmp (ac, ad) >= 0)
	{
	  mpfr_div (t, e->value.complex.i, e->value.complex.r, GFC_RND_MODE);
	  mpfr_mul (t, t, t, GFC_RND_MODE);
	  mpfr_add_ui (t, t, 1, GFC_RND_MODE);
	  mpfr_sqrt (t, t, GFC_RND_MODE);
	  mpfr_add_ui (t, t, 1, GFC_RND_MODE);
	  mpfr_div_ui (t, t, 2, GFC_RND_MODE);
	  mpfr_sqrt (t, t, GFC_RND_MODE);
	  mpfr_sqrt (s, ac, GFC_RND_MODE);
	  mpfr_mul (w, s, t, GFC_RND_MODE);
	}
      else
	{
	  mpfr_div (s, e->value.complex.r, e->value.complex.i, GFC_RND_MODE);
	  mpfr_mul (t, s, s, GFC_RND_MODE);
	  mpfr_add_ui (t, t, 1, GFC_RND_MODE);
	  mpfr_sqrt (t, t, GFC_RND_MODE);
	  mpfr_abs (s, s, GFC_RND_MODE);
	  mpfr_add (t, t, s, GFC_RND_MODE);
	  mpfr_div_ui (t, t, 2, GFC_RND_MODE);
	  mpfr_sqrt (t, t, GFC_RND_MODE);
	  mpfr_sqrt (s, ad, GFC_RND_MODE);
	  mpfr_mul (w, s, t, GFC_RND_MODE);
	}

      if (mpfr_cmp_ui (w, 0) != 0 && mpfr_cmp_ui (e->value.complex.r, 0) >= 0)
	{
	  mpfr_mul_ui (t, w, 2, GFC_RND_MODE);
	  mpfr_div (result->value.complex.i, e->value.complex.i, t, GFC_RND_MODE);
	  mpfr_set (result->value.complex.r, w, GFC_RND_MODE);
	}
      else if (mpfr_cmp_ui (w, 0) != 0
	       && mpfr_cmp_ui (e->value.complex.r, 0) < 0
	       && mpfr_cmp_ui (e->value.complex.i, 0) >= 0)
	{
	  mpfr_mul_ui (t, w, 2, GFC_RND_MODE);
	  mpfr_div (result->value.complex.r, e->value.complex.i, t, GFC_RND_MODE);
	  mpfr_set (result->value.complex.i, w, GFC_RND_MODE);
	}
      else if (mpfr_cmp_ui (w, 0) != 0
	       && mpfr_cmp_ui (e->value.complex.r, 0) < 0
	       && mpfr_cmp_ui (e->value.complex.i, 0) < 0)
	{
	  mpfr_mul_ui (t, w, 2, GFC_RND_MODE);
	  mpfr_div (result->value.complex.r, ad, t, GFC_RND_MODE);
	  mpfr_neg (w, w, GFC_RND_MODE);
	  mpfr_set (result->value.complex.i, w, GFC_RND_MODE);
	}
      else
	gfc_internal_error ("invalid complex argument of SQRT at %L",
			    &e->where);

      mpfr_clear (s);
      mpfr_clear (t);
      mpfr_clear (ac);
      mpfr_clear (ad);
      mpfr_clear (w);

      break;

    default:
      gfc_internal_error ("invalid argument of SQRT at %L", &e->where);
    }

  return range_check (result, "SQRT");

negative_arg:
  gfc_free_expr (result);
  gfc_error ("Argument of SQRT at %L has a negative value", &e->where);
  return &gfc_bad_expr;
}


gfc_expr *
gfc_simplify_tan (gfc_expr * x)
{
  int i;
  gfc_expr *result;

  if (x->expr_type != EXPR_CONSTANT)
    return NULL;

  i = gfc_validate_kind (BT_REAL, x->ts.kind, false);

  result = gfc_constant_result (x->ts.type, x->ts.kind, &x->where);

  mpfr_tan (result->value.real, x->value.real, GFC_RND_MODE);

  return range_check (result, "TAN");
}


gfc_expr *
gfc_simplify_tanh (gfc_expr * x)
{
  gfc_expr *result;

  if (x->expr_type != EXPR_CONSTANT)
    return NULL;

  result = gfc_constant_result (x->ts.type, x->ts.kind, &x->where);

  mpfr_tanh (result->value.real, x->value.real, GFC_RND_MODE);

  return range_check (result, "TANH");

}


gfc_expr *
gfc_simplify_tiny (gfc_expr * e)
{
  gfc_expr *result;
  int i;

  i = gfc_validate_kind (BT_REAL, e->ts.kind, false);

  result = gfc_constant_result (BT_REAL, e->ts.kind, &e->where);
  mpfr_set (result->value.real, gfc_real_kinds[i].tiny, GFC_RND_MODE);

  return result;
}


gfc_expr *
gfc_simplify_trim (gfc_expr * e)
{
  gfc_expr *result;
  int count, i, len, lentrim;

  if (e->expr_type != EXPR_CONSTANT)
    return NULL;

  len = e->value.character.length;

  result = gfc_constant_result (BT_CHARACTER, e->ts.kind, &e->where);

  for (count = 0, i = 1; i <= len; ++i)
    {
      if (e->value.character.string[len - i] == ' ')
	count++;
      else
	break;
    }

  lentrim = len - count;

  result->value.character.length = lentrim;
  result->value.character.string = gfc_getmem (lentrim + 1);

  for (i = 0; i < lentrim; i++)
    result->value.character.string[i] = e->value.character.string[i];

  result->value.character.string[lentrim] = '\0';	/* For debugger */

  return result;
}


gfc_expr *
gfc_simplify_ubound (gfc_expr * array, gfc_expr * dim)
{
  return simplify_bound (array, dim, 1);
}


gfc_expr *
gfc_simplify_verify (gfc_expr * s, gfc_expr * set, gfc_expr * b)
{
  gfc_expr *result;
  int back;
  size_t index, len, lenset;
  size_t i;

  if (s->expr_type != EXPR_CONSTANT || set->expr_type != EXPR_CONSTANT)
    return NULL;

  if (b != NULL && b->value.logical != 0)
    back = 1;
  else
    back = 0;

  result = gfc_constant_result (BT_INTEGER, gfc_default_integer_kind,
				&s->where);

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
	  mpz_set_ui (result->value.integer, len);
	  return result;
	}

      index =
	strspn (s->value.character.string, set->value.character.string) + 1;
      if (index > len)
	index = 0;

    }
  else
    {
      if (lenset == 0)
	{
	  mpz_set_ui (result->value.integer, 1);
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
gfc_simplify_xor (gfc_expr * x, gfc_expr * y)
{
  gfc_expr *result;
  int kind;

  if (x->expr_type != EXPR_CONSTANT || y->expr_type != EXPR_CONSTANT)
    return NULL;

  kind = x->ts.kind > y->ts.kind ? x->ts.kind : y->ts.kind;
  if (x->ts.type == BT_INTEGER)
    {
      result = gfc_constant_result (BT_INTEGER, kind, &x->where);
      mpz_xor (result->value.integer, x->value.integer, y->value.integer);
    }
  else /* BT_LOGICAL */
    {
      result = gfc_constant_result (BT_LOGICAL, kind, &x->where);
      result->value.logical = (x->value.logical && ! y->value.logical)
			      || (! x->value.logical && y->value.logical);
    }

  return range_check (result, "XOR");
}



/****************** Constant simplification *****************/

/* Master function to convert one constant to another.  While this is
   used as a simplification function, it requires the destination type
   and kind information which is supplied by a special case in
   do_simplify().  */

gfc_expr *
gfc_convert_constant (gfc_expr * e, bt type, int kind)
{
  gfc_expr *g, *result, *(*f) (gfc_expr *, int);
  gfc_constructor *head, *c, *tail = NULL;

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

      head = NULL;

      for (c = e->value.constructor; c; c = c->next)
	{
	  if (head == NULL)
	    head = tail = gfc_get_constructor ();
	  else
	    {
	      tail->next = gfc_get_constructor ();
	      tail = tail->next;
	    }

	  tail->where = c->where;

	  if (c->iterator == NULL)
	    tail->expr = f (c->expr, kind);
	  else
	    {
	      g = gfc_convert_constant (c->expr, type, kind);
	      if (g == &gfc_bad_expr)
		return g;
	      tail->expr = g;
	    }

	  if (tail->expr == NULL)
	    {
	      gfc_free_constructor (head);
	      return NULL;
	    }
	}

      result = gfc_get_expr ();
      result->ts.type = type;
      result->ts.kind = kind;
      result->expr_type = EXPR_ARRAY;
      result->value.constructor = head;
      result->shape = gfc_copy_shape (e->shape, e->rank);
      result->where = e->where;
      result->rank = e->rank;
      break;

    default:
      break;
    }

  return result;
}


/****************** Helper functions ***********************/

/* Given a collating table, create the inverse table.  */

static void
invert_table (const int *table, int *xtable)
{
  int i;

  for (i = 0; i < 256; i++)
    xtable[i] = 0;

  for (i = 0; i < 256; i++)
    xtable[table[i]] = i;
}


void
gfc_simplify_init_1 (void)
{

  invert_table (ascii_table, xascii_table);
}
