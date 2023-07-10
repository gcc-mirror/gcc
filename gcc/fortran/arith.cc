/* Compiler arithmetic
   Copyright (C) 2000-2023 Free Software Foundation, Inc.
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

/* Since target arithmetic must be done on the host, there has to
   be some way of evaluating arithmetic expressions as the host
   would evaluate them.  We use the GNU MP library and the MPFR
   library to do arithmetic, and this file provides the interface.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "options.h"
#include "gfortran.h"
#include "arith.h"
#include "target-memory.h"
#include "constructor.h"

bool gfc_seen_div0;

/* MPFR does not have a direct replacement for mpz_set_f() from GMP.
   It's easily implemented with a few calls though.  */

void
gfc_mpfr_to_mpz (mpz_t z, mpfr_t x, locus *where)
{
  mpfr_exp_t e;

  if (mpfr_inf_p (x) || mpfr_nan_p (x))
    {
      gfc_error ("Conversion of an Infinity or Not-a-Number at %L "
		 "to INTEGER", where);
      mpz_set_ui (z, 0);
      return;
    }

  e = mpfr_get_z_exp (z, x);

  if (e > 0)
    mpz_mul_2exp (z, z, e);
  else
    mpz_tdiv_q_2exp (z, z, -e);
}


/* Set the model number precision by the requested KIND.  */

void
gfc_set_model_kind (int kind)
{
  int index = gfc_validate_kind (BT_REAL, kind, false);
  int base2prec;

  base2prec = gfc_real_kinds[index].digits;
  if (gfc_real_kinds[index].radix != 2)
    base2prec *= gfc_real_kinds[index].radix / 2;
  mpfr_set_default_prec (base2prec);
}


/* Set the model number precision from mpfr_t x.  */

void
gfc_set_model (mpfr_t x)
{
  mpfr_set_default_prec (mpfr_get_prec (x));
}


/* Given an arithmetic error code, return a pointer to a string that
   explains the error.  */

static const char *
gfc_arith_error (arith code)
{
  const char *p;

  switch (code)
    {
    case ARITH_OK:
      p = G_("Arithmetic OK at %L");
      break;
    case ARITH_OVERFLOW:
      p = G_("Arithmetic overflow at %L");
      break;
    case ARITH_UNDERFLOW:
      p = G_("Arithmetic underflow at %L");
      break;
    case ARITH_NAN:
      p = G_("Arithmetic NaN at %L");
      break;
    case ARITH_DIV0:
      p = G_("Division by zero at %L");
      break;
    case ARITH_INCOMMENSURATE:
      p = G_("Array operands are incommensurate at %L");
      break;
    case ARITH_ASYMMETRIC:
      p = G_("Integer outside symmetric range implied by Standard Fortran"
	     " at %L");
      break;
    case ARITH_WRONGCONCAT:
      p = G_("Illegal type in character concatenation at %L");
      break;
    case ARITH_INVALID_TYPE:
      p = G_("Invalid type in arithmetic operation at %L");
      break;

    default:
      gfc_internal_error ("gfc_arith_error(): Bad error code");
    }

  return p;
}


/* Get things ready to do math.  */

void
gfc_arith_init_1 (void)
{
  gfc_integer_info *int_info;
  gfc_real_info *real_info;
  mpfr_t a, b;
  int i;

  mpfr_set_default_prec (128);
  mpfr_init (a);

  /* Convert the minimum and maximum values for each kind into their
     GNU MP representation.  */
  for (int_info = gfc_integer_kinds; int_info->kind != 0; int_info++)
    {
      /* Huge  */
      mpz_init (int_info->huge);
      mpz_set_ui (int_info->huge, int_info->radix);
      mpz_pow_ui (int_info->huge, int_info->huge, int_info->digits);
      mpz_sub_ui (int_info->huge, int_info->huge, 1);

      /* These are the numbers that are actually representable by the
	 target.  For bases other than two, this needs to be changed.  */
      if (int_info->radix != 2)
	gfc_internal_error ("Fix min_int calculation");

      /* See PRs 13490 and 17912, related to integer ranges.
	 The pedantic_min_int exists for range checking when a program
	 is compiled with -pedantic, and reflects the belief that
	 Standard Fortran requires integers to be symmetrical, i.e.
	 every negative integer must have a representable positive
	 absolute value, and vice versa.  */

      mpz_init (int_info->pedantic_min_int);
      mpz_neg (int_info->pedantic_min_int, int_info->huge);

      mpz_init (int_info->min_int);
      mpz_sub_ui (int_info->min_int, int_info->pedantic_min_int, 1);

      /* Range  */
      mpfr_set_z (a, int_info->huge, GFC_RND_MODE);
      mpfr_log10 (a, a, GFC_RND_MODE);
      mpfr_trunc (a, a);
      int_info->range = (int) mpfr_get_si (a, GFC_RND_MODE);
    }

  mpfr_clear (a);

  for (real_info = gfc_real_kinds; real_info->kind != 0; real_info++)
    {
      gfc_set_model_kind (real_info->kind);

      mpfr_init (a);
      mpfr_init (b);

      /* huge(x) = (1 - b**(-p)) * b**(emax-1) * b  */
      /* 1 - b**(-p)  */
      mpfr_init (real_info->huge);
      mpfr_set_ui (real_info->huge, 1, GFC_RND_MODE);
      mpfr_set_ui (a, real_info->radix, GFC_RND_MODE);
      mpfr_pow_si (a, a, -real_info->digits, GFC_RND_MODE);
      mpfr_sub (real_info->huge, real_info->huge, a, GFC_RND_MODE);

      /* b**(emax-1)  */
      mpfr_set_ui (a, real_info->radix, GFC_RND_MODE);
      mpfr_pow_ui (a, a, real_info->max_exponent - 1, GFC_RND_MODE);

      /* (1 - b**(-p)) * b**(emax-1)  */
      mpfr_mul (real_info->huge, real_info->huge, a, GFC_RND_MODE);

      /* (1 - b**(-p)) * b**(emax-1) * b  */
      mpfr_mul_ui (real_info->huge, real_info->huge, real_info->radix,
		   GFC_RND_MODE);

      /* tiny(x) = b**(emin-1)  */
      mpfr_init (real_info->tiny);
      mpfr_set_ui (real_info->tiny, real_info->radix, GFC_RND_MODE);
      mpfr_pow_si (real_info->tiny, real_info->tiny,
		   real_info->min_exponent - 1, GFC_RND_MODE);

      /* subnormal (x) = b**(emin - digit)  */
      mpfr_init (real_info->subnormal);
      mpfr_set_ui (real_info->subnormal, real_info->radix, GFC_RND_MODE);
      mpfr_pow_si (real_info->subnormal, real_info->subnormal,
		   real_info->min_exponent - real_info->digits, GFC_RND_MODE);

      /* epsilon(x) = b**(1-p)  */
      mpfr_init (real_info->epsilon);
      mpfr_set_ui (real_info->epsilon, real_info->radix, GFC_RND_MODE);
      mpfr_pow_si (real_info->epsilon, real_info->epsilon,
		   1 - real_info->digits, GFC_RND_MODE);

      /* range(x) = int(min(log10(huge(x)), -log10(tiny))  */
      mpfr_log10 (a, real_info->huge, GFC_RND_MODE);
      mpfr_log10 (b, real_info->tiny, GFC_RND_MODE);
      mpfr_neg (b, b, GFC_RND_MODE);

      /* a = min(a, b)  */
      mpfr_min (a, a, b, GFC_RND_MODE);
      mpfr_trunc (a, a);
      real_info->range = (int) mpfr_get_si (a, GFC_RND_MODE);

      /* precision(x) = int((p - 1) * log10(b)) + k  */
      mpfr_set_ui (a, real_info->radix, GFC_RND_MODE);
      mpfr_log10 (a, a, GFC_RND_MODE);
      mpfr_mul_ui (a, a, real_info->digits - 1, GFC_RND_MODE);
      mpfr_trunc (a, a);
      real_info->precision = (int) mpfr_get_si (a, GFC_RND_MODE);

      /* If the radix is an integral power of 10, add one to the precision.  */
      for (i = 10; i <= real_info->radix; i *= 10)
	if (i == real_info->radix)
	  real_info->precision++;

      mpfr_clears (a, b, NULL);
    }
}


/* Clean up, get rid of numeric constants.  */

void
gfc_arith_done_1 (void)
{
  gfc_integer_info *ip;
  gfc_real_info *rp;

  for (ip = gfc_integer_kinds; ip->kind; ip++)
    {
      mpz_clear (ip->min_int);
      mpz_clear (ip->pedantic_min_int);
      mpz_clear (ip->huge);
    }

  for (rp = gfc_real_kinds; rp->kind; rp++)
    mpfr_clears (rp->epsilon, rp->huge, rp->tiny, rp->subnormal, NULL);

  mpfr_free_cache ();
}


/* Given a wide character value and a character kind, determine whether
   the character is representable for that kind.  */
bool
gfc_check_character_range (gfc_char_t c, int kind)
{
  /* As wide characters are stored as 32-bit values, they're all
     representable in UCS=4.  */
  if (kind == 4)
    return true;

  if (kind == 1)
    return c <= 255 ? true : false;

  gcc_unreachable ();
}


/* Given an integer and a kind, make sure that the integer lies within
   the range of the kind.  Returns ARITH_OK, ARITH_ASYMMETRIC or
   ARITH_OVERFLOW.  */

arith
gfc_check_integer_range (mpz_t p, int kind)
{
  arith result;
  int i;

  i = gfc_validate_kind (BT_INTEGER, kind, false);
  result = ARITH_OK;

  if (pedantic)
    {
      if (mpz_cmp (p, gfc_integer_kinds[i].pedantic_min_int) < 0)
	result = ARITH_ASYMMETRIC;
    }


  if (flag_range_check == 0)
    return result;

  if (mpz_cmp (p, gfc_integer_kinds[i].min_int) < 0
      || mpz_cmp (p, gfc_integer_kinds[i].huge) > 0)
    result = ARITH_OVERFLOW;

  return result;
}


/* Given a real and a kind, make sure that the real lies within the
   range of the kind.  Returns ARITH_OK, ARITH_OVERFLOW or
   ARITH_UNDERFLOW.  */

static arith
gfc_check_real_range (mpfr_t p, int kind)
{
  arith retval;
  mpfr_t q;
  int i;

  i = gfc_validate_kind (BT_REAL, kind, false);

  gfc_set_model (p);
  mpfr_init (q);
  mpfr_abs (q, p, GFC_RND_MODE);

  retval = ARITH_OK;

  if (mpfr_inf_p (p))
    {
      if (flag_range_check != 0)
	retval = ARITH_OVERFLOW;
    }
  else if (mpfr_nan_p (p))
    {
      if (flag_range_check != 0)
	retval = ARITH_NAN;
    }
  else if (mpfr_sgn (q) == 0)
    {
      mpfr_clear (q);
      return retval;
    }
  else if (mpfr_cmp (q, gfc_real_kinds[i].huge) > 0)
    {
      if (flag_range_check == 0)
	mpfr_set_inf (p, mpfr_sgn (p));
      else
	retval = ARITH_OVERFLOW;
    }
  else if (mpfr_cmp (q, gfc_real_kinds[i].subnormal) < 0)
    {
      if (flag_range_check == 0)
	{
	  if (mpfr_sgn (p) < 0)
	    {
	      mpfr_set_ui (p, 0, GFC_RND_MODE);
	      mpfr_set_si (q, -1, GFC_RND_MODE);
	      mpfr_copysign (p, p, q, GFC_RND_MODE);
	    }
	  else
	    mpfr_set_ui (p, 0, GFC_RND_MODE);
	}
      else
	retval = ARITH_UNDERFLOW;
    }
  else if (mpfr_cmp (q, gfc_real_kinds[i].tiny) < 0)
    {
      mpfr_exp_t emin, emax;
      int en;

      /* Save current values of emin and emax.  */
      emin = mpfr_get_emin ();
      emax = mpfr_get_emax ();

      /* Set emin and emax for the current model number.  */
      en = gfc_real_kinds[i].min_exponent - gfc_real_kinds[i].digits + 1;
      mpfr_set_emin ((mpfr_exp_t) en);
      mpfr_set_emax ((mpfr_exp_t) gfc_real_kinds[i].max_exponent);
      mpfr_check_range (q, 0, GFC_RND_MODE);
      mpfr_subnormalize (q, 0, GFC_RND_MODE);

      /* Reset emin and emax.  */
      mpfr_set_emin (emin);
      mpfr_set_emax (emax);

      /* Copy sign if needed.  */
      if (mpfr_sgn (p) < 0)
	mpfr_neg (p, q, MPFR_RNDN);
      else
	mpfr_set (p, q, MPFR_RNDN);
    }

  mpfr_clear (q);

  return retval;
}


/* Low-level arithmetic functions.  All of these subroutines assume
   that all operands are of the same type and return an operand of the
   same type.  The other thing about these subroutines is that they
   can fail in various ways -- overflow, underflow, division by zero,
   zero raised to the zero, etc.  */

static arith
gfc_arith_not (gfc_expr *op1, gfc_expr **resultp)
{
  gfc_expr *result;

  if (op1->ts.type != BT_LOGICAL)
    return ARITH_INVALID_TYPE;

  result = gfc_get_constant_expr (BT_LOGICAL, op1->ts.kind, &op1->where);
  result->value.logical = !op1->value.logical;
  *resultp = result;

  return ARITH_OK;
}


static arith
gfc_arith_and (gfc_expr *op1, gfc_expr *op2, gfc_expr **resultp)
{
  gfc_expr *result;

  if (op1->ts.type != BT_LOGICAL || op2->ts.type != BT_LOGICAL)
    return ARITH_INVALID_TYPE;

  result = gfc_get_constant_expr (BT_LOGICAL, gfc_kind_max (op1, op2),
				  &op1->where);
  result->value.logical = op1->value.logical && op2->value.logical;
  *resultp = result;

  return ARITH_OK;
}


static arith
gfc_arith_or (gfc_expr *op1, gfc_expr *op2, gfc_expr **resultp)
{
  gfc_expr *result;

  if (op1->ts.type != BT_LOGICAL || op2->ts.type != BT_LOGICAL)
    return ARITH_INVALID_TYPE;

  result = gfc_get_constant_expr (BT_LOGICAL, gfc_kind_max (op1, op2),
				  &op1->where);
  result->value.logical = op1->value.logical || op2->value.logical;
  *resultp = result;

  return ARITH_OK;
}


static arith
gfc_arith_eqv (gfc_expr *op1, gfc_expr *op2, gfc_expr **resultp)
{
  gfc_expr *result;

  if (op1->ts.type != BT_LOGICAL || op2->ts.type != BT_LOGICAL)
    return ARITH_INVALID_TYPE;

  result = gfc_get_constant_expr (BT_LOGICAL, gfc_kind_max (op1, op2),
				  &op1->where);
  result->value.logical = op1->value.logical == op2->value.logical;
  *resultp = result;

  return ARITH_OK;
}


static arith
gfc_arith_neqv (gfc_expr *op1, gfc_expr *op2, gfc_expr **resultp)
{
  gfc_expr *result;

  if (op1->ts.type != BT_LOGICAL || op2->ts.type != BT_LOGICAL)
    return ARITH_INVALID_TYPE;

  result = gfc_get_constant_expr (BT_LOGICAL, gfc_kind_max (op1, op2),
				  &op1->where);
  result->value.logical = op1->value.logical != op2->value.logical;
  *resultp = result;

  return ARITH_OK;
}


/* Make sure a constant numeric expression is within the range for
   its type and kind.  Note that there's also a gfc_check_range(),
   but that one deals with the intrinsic RANGE function.  */

arith
gfc_range_check (gfc_expr *e)
{
  arith rc;
  arith rc2;

  switch (e->ts.type)
    {
    case BT_INTEGER:
      rc = gfc_check_integer_range (e->value.integer, e->ts.kind);
      break;

    case BT_REAL:
      rc = gfc_check_real_range (e->value.real, e->ts.kind);
      if (rc == ARITH_UNDERFLOW)
	mpfr_set_ui (e->value.real, 0, GFC_RND_MODE);
      if (rc == ARITH_OVERFLOW)
	mpfr_set_inf (e->value.real, mpfr_sgn (e->value.real));
      if (rc == ARITH_NAN)
	mpfr_set_nan (e->value.real);
      break;

    case BT_COMPLEX:
      rc = gfc_check_real_range (mpc_realref (e->value.complex), e->ts.kind);
      if (rc == ARITH_UNDERFLOW)
	mpfr_set_ui (mpc_realref (e->value.complex), 0, GFC_RND_MODE);
      if (rc == ARITH_OVERFLOW)
	mpfr_set_inf (mpc_realref (e->value.complex),
		      mpfr_sgn (mpc_realref (e->value.complex)));
      if (rc == ARITH_NAN)
	mpfr_set_nan (mpc_realref (e->value.complex));

      rc2 = gfc_check_real_range (mpc_imagref (e->value.complex), e->ts.kind);
      if (rc == ARITH_UNDERFLOW)
	mpfr_set_ui (mpc_imagref (e->value.complex), 0, GFC_RND_MODE);
      if (rc == ARITH_OVERFLOW)
	mpfr_set_inf (mpc_imagref (e->value.complex),
		      mpfr_sgn (mpc_imagref (e->value.complex)));
      if (rc == ARITH_NAN)
	mpfr_set_nan (mpc_imagref (e->value.complex));

      if (rc == ARITH_OK)
	rc = rc2;
      break;

    default:
      gfc_internal_error ("gfc_range_check(): Bad type");
    }

  return rc;
}


/* Several of the following routines use the same set of statements to
   check the validity of the result.  Encapsulate the checking here.  */

static arith
check_result (arith rc, gfc_expr *x, gfc_expr *r, gfc_expr **rp)
{
  arith val = rc;

  if (val == ARITH_UNDERFLOW)
    {
      if (warn_underflow)
	gfc_warning (OPT_Wunderflow, gfc_arith_error (val), &x->where);
      val = ARITH_OK;
    }

  if (val == ARITH_ASYMMETRIC)
    {
      gfc_warning (0, gfc_arith_error (val), &x->where);
      val = ARITH_OK;
    }

  if (val == ARITH_OK || val == ARITH_OVERFLOW)
    *rp = r;
  else
    gfc_free_expr (r);

  return val;
}


/* It may seem silly to have a subroutine that actually computes the
   unary plus of a constant, but it prevents us from making exceptions
   in the code elsewhere.  Used for unary plus and parenthesized
   expressions.  */

static arith
gfc_arith_identity (gfc_expr *op1, gfc_expr **resultp)
{
  *resultp = gfc_copy_expr (op1);
  return ARITH_OK;
}


static arith
gfc_arith_uminus (gfc_expr *op1, gfc_expr **resultp)
{
  gfc_expr *result;
  arith rc;

  result = gfc_get_constant_expr (op1->ts.type, op1->ts.kind, &op1->where);

  switch (op1->ts.type)
    {
    case BT_INTEGER:
      mpz_neg (result->value.integer, op1->value.integer);
      break;

    case BT_REAL:
      mpfr_neg (result->value.real, op1->value.real, GFC_RND_MODE);
      break;

    case BT_COMPLEX:
      mpc_neg (result->value.complex, op1->value.complex, GFC_MPC_RND_MODE);
      break;

    default:
      gfc_internal_error ("gfc_arith_uminus(): Bad basic type");
    }

  rc = gfc_range_check (result);

  return check_result (rc, op1, result, resultp);
}


static arith
gfc_arith_plus (gfc_expr *op1, gfc_expr *op2, gfc_expr **resultp)
{
  gfc_expr *result;
  arith rc;

  if (op1->ts.type != op2->ts.type)
    return ARITH_INVALID_TYPE;

  result = gfc_get_constant_expr (op1->ts.type, op1->ts.kind, &op1->where);

  switch (op1->ts.type)
    {
    case BT_INTEGER:
      mpz_add (result->value.integer, op1->value.integer, op2->value.integer);
      break;

    case BT_REAL:
      mpfr_add (result->value.real, op1->value.real, op2->value.real,
	       GFC_RND_MODE);
      break;

    case BT_COMPLEX:
      mpc_add (result->value.complex, op1->value.complex, op2->value.complex,
	       GFC_MPC_RND_MODE);
      break;

    default:
      gfc_internal_error ("gfc_arith_plus(): Bad basic type");
    }

  rc = gfc_range_check (result);

  return check_result (rc, op1, result, resultp);
}


static arith
gfc_arith_minus (gfc_expr *op1, gfc_expr *op2, gfc_expr **resultp)
{
  gfc_expr *result;
  arith rc;

  if (op1->ts.type != op2->ts.type)
    return ARITH_INVALID_TYPE;

  result = gfc_get_constant_expr (op1->ts.type, op1->ts.kind, &op1->where);

  switch (op1->ts.type)
    {
    case BT_INTEGER:
      mpz_sub (result->value.integer, op1->value.integer, op2->value.integer);
      break;

    case BT_REAL:
      mpfr_sub (result->value.real, op1->value.real, op2->value.real,
		GFC_RND_MODE);
      break;

    case BT_COMPLEX:
      mpc_sub (result->value.complex, op1->value.complex,
	       op2->value.complex, GFC_MPC_RND_MODE);
      break;

    default:
      gfc_internal_error ("gfc_arith_minus(): Bad basic type");
    }

  rc = gfc_range_check (result);

  return check_result (rc, op1, result, resultp);
}


static arith
gfc_arith_times (gfc_expr *op1, gfc_expr *op2, gfc_expr **resultp)
{
  gfc_expr *result;
  arith rc;

  if (op1->ts.type != op2->ts.type)
    return ARITH_INVALID_TYPE;

  result = gfc_get_constant_expr (op1->ts.type, op1->ts.kind, &op1->where);

  switch (op1->ts.type)
    {
    case BT_INTEGER:
      mpz_mul (result->value.integer, op1->value.integer, op2->value.integer);
      break;

    case BT_REAL:
      mpfr_mul (result->value.real, op1->value.real, op2->value.real,
	       GFC_RND_MODE);
      break;

    case BT_COMPLEX:
      gfc_set_model (mpc_realref (op1->value.complex));
      mpc_mul (result->value.complex, op1->value.complex, op2->value.complex,
	       GFC_MPC_RND_MODE);
      break;

    default:
      gfc_internal_error ("gfc_arith_times(): Bad basic type");
    }

  rc = gfc_range_check (result);

  return check_result (rc, op1, result, resultp);
}


static arith
gfc_arith_divide (gfc_expr *op1, gfc_expr *op2, gfc_expr **resultp)
{
  gfc_expr *result;
  arith rc;

  if (op1->ts.type != op2->ts.type)
    return ARITH_INVALID_TYPE;

  rc = ARITH_OK;

  result = gfc_get_constant_expr (op1->ts.type, op1->ts.kind, &op1->where);

  switch (op1->ts.type)
    {
    case BT_INTEGER:
      if (mpz_sgn (op2->value.integer) == 0)
	{
	  rc = ARITH_DIV0;
	  break;
	}

      if (warn_integer_division)
	{
	  mpz_t r;
	  mpz_init (r);
	  mpz_tdiv_qr (result->value.integer, r, op1->value.integer,
		       op2->value.integer);

	  if (mpz_cmp_si (r, 0) != 0)
	    {
	      char *p;
	      p = mpz_get_str (NULL, 10, result->value.integer);
	      gfc_warning (OPT_Winteger_division, "Integer division "
			       "truncated to constant %qs at %L", p,
			       &op1->where);
	      free (p);
	    }
	  mpz_clear (r);
	}
      else
	mpz_tdiv_q (result->value.integer, op1->value.integer,
		    op2->value.integer);

      break;

    case BT_REAL:
      if (mpfr_sgn (op2->value.real) == 0 && flag_range_check == 1)
	{
	  rc = ARITH_DIV0;
	  break;
	}

      mpfr_div (result->value.real, op1->value.real, op2->value.real,
	       GFC_RND_MODE);
      break;

    case BT_COMPLEX:
      if (mpc_cmp_si_si (op2->value.complex, 0, 0) == 0
	  && flag_range_check == 1)
	{
	  rc = ARITH_DIV0;
	  break;
	}

      gfc_set_model (mpc_realref (op1->value.complex));
      if (mpc_cmp_si_si (op2->value.complex, 0, 0) == 0)
      {
	/* In Fortran, return (NaN + NaN I) for any zero divisor.  See
	   PR 40318.  */
	mpfr_set_nan (mpc_realref (result->value.complex));
	mpfr_set_nan (mpc_imagref (result->value.complex));
      }
      else
	mpc_div (result->value.complex, op1->value.complex, op2->value.complex,
		 GFC_MPC_RND_MODE);
      break;

    default:
      gfc_internal_error ("gfc_arith_divide(): Bad basic type");
    }

  if (rc == ARITH_OK)
    rc = gfc_range_check (result);

  return check_result (rc, op1, result, resultp);
}

/* Raise a number to a power.  */

static arith
arith_power (gfc_expr *op1, gfc_expr *op2, gfc_expr **resultp)
{
  int power_sign;
  gfc_expr *result;
  arith rc;

  if (!gfc_numeric_ts (&op1->ts) || !gfc_numeric_ts (&op2->ts))
    return ARITH_INVALID_TYPE;

  /* The result type is derived from op1 and must be compatible with the
     result of the simplification.  Otherwise postpone simplification until
     after operand conversions usually done by gfc_type_convert_binary.  */
  if ((op1->ts.type == BT_INTEGER && op2->ts.type != BT_INTEGER)
      || (op1->ts.type == BT_REAL && op2->ts.type == BT_COMPLEX))
    return ARITH_NOT_REDUCED;

  rc = ARITH_OK;
  result = gfc_get_constant_expr (op1->ts.type, op1->ts.kind, &op1->where);

  switch (op2->ts.type)
    {
    case BT_INTEGER:
      power_sign = mpz_sgn (op2->value.integer);

      if (power_sign == 0)
	{
	  /* Handle something to the zeroth power.  Since we're dealing
	     with integral exponents, there is no ambiguity in the
	     limiting procedure used to determine the value of 0**0.  */
	  switch (op1->ts.type)
	    {
	    case BT_INTEGER:
	      mpz_set_ui (result->value.integer, 1);
	      break;

	    case BT_REAL:
	      mpfr_set_ui (result->value.real, 1, GFC_RND_MODE);
	      break;

	    case BT_COMPLEX:
	      mpc_set_ui (result->value.complex, 1, GFC_MPC_RND_MODE);
	      break;

	    default:
	      gfc_internal_error ("arith_power(): Bad base");
	    }
	}
      else
	{
	  switch (op1->ts.type)
	    {
	    case BT_INTEGER:
	      {
		/* First, we simplify the cases of op1 == 1, 0 or -1.  */
		if (mpz_cmp_si (op1->value.integer, 1) == 0)
		  {
		    /* 1**op2 == 1 */
		    mpz_set_si (result->value.integer, 1);
		  }
		else if (mpz_cmp_si (op1->value.integer, 0) == 0)
		  {
		    /* 0**op2 == 0, if op2 > 0
	               0**op2 overflow, if op2 < 0 ; in that case, we
		       set the result to 0 and return ARITH_DIV0.  */
		    mpz_set_si (result->value.integer, 0);
		    if (mpz_cmp_si (op2->value.integer, 0) < 0)
		      rc = ARITH_DIV0;
		  }
		else if (mpz_cmp_si (op1->value.integer, -1) == 0)
		  {
		    /* (-1)**op2 == (-1)**(mod(op2,2)) */
		    unsigned int odd = mpz_fdiv_ui (op2->value.integer, 2);
		    if (odd)
		      mpz_set_si (result->value.integer, -1);
		    else
		      mpz_set_si (result->value.integer, 1);
		  }
		/* Then, we take care of op2 < 0.  */
		else if (mpz_cmp_si (op2->value.integer, 0) < 0)
		  {
		    /* if op2 < 0, op1**op2 == 0  because abs(op1) > 1.  */
		    mpz_set_si (result->value.integer, 0);
		    if (warn_integer_division)
		      gfc_warning_now (OPT_Winteger_division, "Negative "
				       "exponent of integer has zero "
				       "result at %L", &result->where);
		  }
		else
		  {
		    /* We have abs(op1) > 1 and op2 > 1.
		       If op2 > bit_size(op1), we'll have an out-of-range
		       result.  */
		    int k, power;

		    k = gfc_validate_kind (BT_INTEGER, op1->ts.kind, false);
		    power = gfc_integer_kinds[k].bit_size;
		    if (mpz_cmp_si (op2->value.integer, power) < 0)
		      {
			gfc_extract_int (op2, &power);
			mpz_pow_ui (result->value.integer, op1->value.integer,
				    power);
			rc = gfc_range_check (result);
			if (rc == ARITH_OVERFLOW)
			  gfc_error_now ("Result of exponentiation at %L "
					 "exceeds the range of %s", &op1->where,
					 gfc_typename (&(op1->ts)));
		      }
		    else
		      {
			/* Provide a nonsense value to propagate up. */
			mpz_set (result->value.integer,
				 gfc_integer_kinds[k].huge);
			mpz_add_ui (result->value.integer,
				    result->value.integer, 1);
			rc = ARITH_OVERFLOW;
		      }
		  }
	      }
	      break;

	    case BT_REAL:
	      mpfr_pow_z (result->value.real, op1->value.real,
			  op2->value.integer, GFC_RND_MODE);
	      break;

	    case BT_COMPLEX:
	      mpc_pow_z (result->value.complex, op1->value.complex,
			 op2->value.integer, GFC_MPC_RND_MODE);
	      break;

	    default:
	      break;
	    }
	}
      break;

    case BT_REAL:

      if (gfc_init_expr_flag)
	{
	  if (!gfc_notify_std (GFC_STD_F2003, "Noninteger "
			       "exponent in an initialization "
			       "expression at %L", &op2->where))
	    {
	      gfc_free_expr (result);
	      return ARITH_PROHIBIT;
	    }
	}

      if (mpfr_cmp_si (op1->value.real, 0) < 0)
	{
	  gfc_error ("Raising a negative REAL at %L to "
		     "a REAL power is prohibited", &op1->where);
	  gfc_free_expr (result);
	  return ARITH_PROHIBIT;
	}

	mpfr_pow (result->value.real, op1->value.real, op2->value.real,
		  GFC_RND_MODE);
      break;

    case BT_COMPLEX:
      {
	if (gfc_init_expr_flag)
	  {
	    if (!gfc_notify_std (GFC_STD_F2003, "Noninteger "
				 "exponent in an initialization "
				 "expression at %L", &op2->where))
	      {
		gfc_free_expr (result);
		return ARITH_PROHIBIT;
	      }
	  }

	mpc_pow (result->value.complex, op1->value.complex,
		 op2->value.complex, GFC_MPC_RND_MODE);
      }
      break;
    default:
      gfc_internal_error ("arith_power(): unknown type");
    }

  if (rc == ARITH_OK)
    rc = gfc_range_check (result);

  return check_result (rc, op1, result, resultp);
}


/* Concatenate two string constants.  */

static arith
gfc_arith_concat (gfc_expr *op1, gfc_expr *op2, gfc_expr **resultp)
{
  gfc_expr *result;
  size_t len;

  /* By cleverly playing around with constructors, it is possible
     to get mismaching types here.  */
  if (op1->ts.type != BT_CHARACTER || op2->ts.type != BT_CHARACTER
      || op1->ts.kind != op2->ts.kind)
    return ARITH_WRONGCONCAT;

  result = gfc_get_constant_expr (BT_CHARACTER, op1->ts.kind,
				  &op1->where);

  len = op1->value.character.length + op2->value.character.length;

  result->value.character.string = gfc_get_wide_string (len + 1);
  result->value.character.length = len;

  memcpy (result->value.character.string, op1->value.character.string,
	  op1->value.character.length * sizeof (gfc_char_t));

  memcpy (&result->value.character.string[op1->value.character.length],
	  op2->value.character.string,
	  op2->value.character.length * sizeof (gfc_char_t));

  result->value.character.string[len] = '\0';

  *resultp = result;

  return ARITH_OK;
}

/* Comparison between real values; returns 0 if (op1 .op. op2) is true.
   This function mimics mpfr_cmp but takes NaN into account.  */

static int
compare_real (gfc_expr *op1, gfc_expr *op2, gfc_intrinsic_op op)
{
  int rc;
  switch (op)
    {
      case INTRINSIC_EQ:
	rc = mpfr_equal_p (op1->value.real, op2->value.real) ? 0 : 1;
	break;
      case INTRINSIC_GT:
	rc = mpfr_greater_p (op1->value.real, op2->value.real) ? 1 : -1;
	break;
      case INTRINSIC_GE:
	rc = mpfr_greaterequal_p (op1->value.real, op2->value.real) ? 1 : -1;
	break;
      case INTRINSIC_LT:
	rc = mpfr_less_p (op1->value.real, op2->value.real) ? -1 : 1;
	break;
      case INTRINSIC_LE:
	rc = mpfr_lessequal_p (op1->value.real, op2->value.real) ? -1 : 1;
	break;
      default:
	gfc_internal_error ("compare_real(): Bad operator");
    }

  return rc;
}

/* Comparison operators.  Assumes that the two expression nodes
   contain two constants of the same type. The op argument is
   needed to handle NaN correctly.  */

int
gfc_compare_expr (gfc_expr *op1, gfc_expr *op2, gfc_intrinsic_op op)
{
  int rc;

  switch (op1->ts.type)
    {
    case BT_INTEGER:
      rc = mpz_cmp (op1->value.integer, op2->value.integer);
      break;

    case BT_REAL:
      rc = compare_real (op1, op2, op);
      break;

    case BT_CHARACTER:
      rc = gfc_compare_string (op1, op2);
      break;

    case BT_LOGICAL:
      rc = ((!op1->value.logical && op2->value.logical)
	    || (op1->value.logical && !op2->value.logical));
      break;

    case BT_COMPLEX:
      gcc_assert (op == INTRINSIC_EQ);
      rc = mpc_cmp (op1->value.complex, op2->value.complex);
      break;

    default:
      gfc_internal_error ("gfc_compare_expr(): Bad basic type");
    }

  return rc;
}


/* Compare a pair of complex numbers.  Naturally, this is only for
   equality and inequality.  */

static int
compare_complex (gfc_expr *op1, gfc_expr *op2)
{
  return mpc_cmp (op1->value.complex, op2->value.complex) == 0;
}


/* Given two constant strings and the inverse collating sequence, compare the
   strings.  We return -1 for a < b, 0 for a == b and 1 for a > b.
   We use the processor's default collating sequence.  */

int
gfc_compare_string (gfc_expr *a, gfc_expr *b)
{
  size_t len, alen, blen, i;
  gfc_char_t ac, bc;

  alen = a->value.character.length;
  blen = b->value.character.length;

  len = MAX(alen, blen);

  for (i = 0; i < len; i++)
    {
      ac = ((i < alen) ? a->value.character.string[i] : ' ');
      bc = ((i < blen) ? b->value.character.string[i] : ' ');

      if (ac < bc)
	return -1;
      if (ac > bc)
	return 1;
    }

  /* Strings are equal */
  return 0;
}


int
gfc_compare_with_Cstring (gfc_expr *a, const char *b, bool case_sensitive)
{
  size_t len, alen, blen, i;
  gfc_char_t ac, bc;

  alen = a->value.character.length;
  blen = strlen (b);

  len = MAX(alen, blen);

  for (i = 0; i < len; i++)
    {
      ac = ((i < alen) ? a->value.character.string[i] : ' ');
      bc = ((i < blen) ? b[i] : ' ');

      if (!case_sensitive)
	{
	  ac = TOLOWER (ac);
	  bc = TOLOWER (bc);
	}

      if (ac < bc)
	return -1;
      if (ac > bc)
	return 1;
    }

  /* Strings are equal */
  return 0;
}


/* Specific comparison subroutines.  */

static arith
gfc_arith_eq (gfc_expr *op1, gfc_expr *op2, gfc_expr **resultp)
{
  gfc_expr *result;

  if (op1->ts.type != op2->ts.type)
    return ARITH_INVALID_TYPE;

  result = gfc_get_constant_expr (BT_LOGICAL, gfc_default_logical_kind,
				  &op1->where);
  result->value.logical = (op1->ts.type == BT_COMPLEX)
			? compare_complex (op1, op2)
			: (gfc_compare_expr (op1, op2, INTRINSIC_EQ) == 0);

  *resultp = result;
  return ARITH_OK;
}


static arith
gfc_arith_ne (gfc_expr *op1, gfc_expr *op2, gfc_expr **resultp)
{
  gfc_expr *result;

  if (op1->ts.type != op2->ts.type)
    return ARITH_INVALID_TYPE;

  result = gfc_get_constant_expr (BT_LOGICAL, gfc_default_logical_kind,
				  &op1->where);
  result->value.logical = (op1->ts.type == BT_COMPLEX)
			? !compare_complex (op1, op2)
			: (gfc_compare_expr (op1, op2, INTRINSIC_EQ) != 0);

  *resultp = result;
  return ARITH_OK;
}


static arith
gfc_arith_gt (gfc_expr *op1, gfc_expr *op2, gfc_expr **resultp)
{
  gfc_expr *result;

  if (op1->ts.type != op2->ts.type)
    return ARITH_INVALID_TYPE;

  result = gfc_get_constant_expr (BT_LOGICAL, gfc_default_logical_kind,
				  &op1->where);
  result->value.logical = (gfc_compare_expr (op1, op2, INTRINSIC_GT) > 0);
  *resultp = result;

  return ARITH_OK;
}


static arith
gfc_arith_ge (gfc_expr *op1, gfc_expr *op2, gfc_expr **resultp)
{
  gfc_expr *result;

  if (op1->ts.type != op2->ts.type)
    return ARITH_INVALID_TYPE;

  result = gfc_get_constant_expr (BT_LOGICAL, gfc_default_logical_kind,
				  &op1->where);
  result->value.logical = (gfc_compare_expr (op1, op2, INTRINSIC_GE) >= 0);
  *resultp = result;

  return ARITH_OK;
}


static arith
gfc_arith_lt (gfc_expr *op1, gfc_expr *op2, gfc_expr **resultp)
{
  gfc_expr *result;

  if (op1->ts.type != op2->ts.type)
    return ARITH_INVALID_TYPE;

  result = gfc_get_constant_expr (BT_LOGICAL, gfc_default_logical_kind,
				  &op1->where);
  result->value.logical = (gfc_compare_expr (op1, op2, INTRINSIC_LT) < 0);
  *resultp = result;

  return ARITH_OK;
}


static arith
gfc_arith_le (gfc_expr *op1, gfc_expr *op2, gfc_expr **resultp)
{
  gfc_expr *result;

  if (op1->ts.type != op2->ts.type)
    return ARITH_INVALID_TYPE;

  result = gfc_get_constant_expr (BT_LOGICAL, gfc_default_logical_kind,
				  &op1->where);
  result->value.logical = (gfc_compare_expr (op1, op2, INTRINSIC_LE) <= 0);
  *resultp = result;

  return ARITH_OK;
}


static arith
reduce_unary (arith (*eval) (gfc_expr *, gfc_expr **), gfc_expr *op,
	      gfc_expr **result)
{
  gfc_constructor_base head;
  gfc_constructor *c;
  gfc_expr *r;
  arith rc;

  if (op->expr_type == EXPR_CONSTANT)
    return eval (op, result);

  if (op->expr_type != EXPR_ARRAY)
    return ARITH_NOT_REDUCED;

  rc = ARITH_OK;
  head = gfc_constructor_copy (op->value.constructor);
  for (c = gfc_constructor_first (head); c; c = gfc_constructor_next (c))
    {
      rc = reduce_unary (eval, c->expr, &r);

      if (rc != ARITH_OK)
	break;

      gfc_replace_expr (c->expr, r);
    }

  if (rc != ARITH_OK)
    gfc_constructor_free (head);
  else
    {
      gfc_constructor *c = gfc_constructor_first (head);
      if (c == NULL)
	{
	  /* Handle zero-sized arrays.  */
	  r = gfc_get_array_expr (op->ts.type, op->ts.kind, &op->where);
	}
      else
	{
	  r = gfc_get_array_expr (c->expr->ts.type, c->expr->ts.kind,
				  &op->where);
	}
      r->shape = gfc_copy_shape (op->shape, op->rank);
      r->rank = op->rank;
      r->value.constructor = head;
      *result = r;
    }

  return rc;
}


static arith
reduce_binary_ac (arith (*eval) (gfc_expr *, gfc_expr *, gfc_expr **),
		  gfc_expr *op1, gfc_expr *op2, gfc_expr **result)
{
  gfc_constructor_base head;
  gfc_constructor *c;
  gfc_expr *r;
  arith rc = ARITH_OK;

  head = gfc_constructor_copy (op1->value.constructor);
  for (c = gfc_constructor_first (head); c; c = gfc_constructor_next (c))
    {
      gfc_simplify_expr (c->expr, 0);

      if (c->expr->expr_type == EXPR_CONSTANT)
        rc = eval (c->expr, op2, &r);
      else if (c->expr->expr_type != EXPR_ARRAY)
	rc = ARITH_NOT_REDUCED;
      else
	rc = reduce_binary_ac (eval, c->expr, op2, &r);

      if (rc != ARITH_OK)
	break;

      gfc_replace_expr (c->expr, r);
    }

  if (rc != ARITH_OK)
    gfc_constructor_free (head);
  else
    {
      gfc_constructor *c = gfc_constructor_first (head);
      if (c)
	{
	  r = gfc_get_array_expr (c->expr->ts.type, c->expr->ts.kind,
				  &op1->where);
	  r->shape = gfc_copy_shape (op1->shape, op1->rank);
	}
      else
	{
	  gcc_assert (op1->ts.type != BT_UNKNOWN);
	  r = gfc_get_array_expr (op1->ts.type, op1->ts.kind,
				  &op1->where);
	  r->shape = gfc_get_shape (op1->rank);
	}
      r->rank = op1->rank;
      r->value.constructor = head;
      *result = r;
    }

  return rc;
}


static arith
reduce_binary_ca (arith (*eval) (gfc_expr *, gfc_expr *, gfc_expr **),
		  gfc_expr *op1, gfc_expr *op2, gfc_expr **result)
{
  gfc_constructor_base head;
  gfc_constructor *c;
  gfc_expr *r;
  arith rc = ARITH_OK;

  head = gfc_constructor_copy (op2->value.constructor);
  for (c = gfc_constructor_first (head); c; c = gfc_constructor_next (c))
    {
      gfc_simplify_expr (c->expr, 0);

      if (c->expr->expr_type == EXPR_CONSTANT)
	rc = eval (op1, c->expr, &r);
      else if (c->expr->expr_type != EXPR_ARRAY)
	rc = ARITH_NOT_REDUCED;
      else
	rc = reduce_binary_ca (eval, op1, c->expr, &r);

      if (rc != ARITH_OK)
	break;

      gfc_replace_expr (c->expr, r);
    }

  if (rc != ARITH_OK)
    gfc_constructor_free (head);
  else
    {
      gfc_constructor *c = gfc_constructor_first (head);
      if (c)
	{
	  r = gfc_get_array_expr (c->expr->ts.type, c->expr->ts.kind,
				  &op2->where);
	  r->shape = gfc_copy_shape (op2->shape, op2->rank);
	}
      else
	{
	  gcc_assert (op2->ts.type != BT_UNKNOWN);
	  r = gfc_get_array_expr (op2->ts.type, op2->ts.kind,
				  &op2->where);
	  r->shape = gfc_get_shape (op2->rank);
	}
      r->rank = op2->rank;
      r->value.constructor = head;
      *result = r;
    }

  return rc;
}


/* We need a forward declaration of reduce_binary.  */
static arith reduce_binary (arith (*eval) (gfc_expr *, gfc_expr *, gfc_expr **),
			    gfc_expr *op1, gfc_expr *op2, gfc_expr **result);


static arith
reduce_binary_aa (arith (*eval) (gfc_expr *, gfc_expr *, gfc_expr **),
		  gfc_expr *op1, gfc_expr *op2, gfc_expr **result)
{
  gfc_constructor_base head;
  gfc_constructor *c, *d;
  gfc_expr *r;
  arith rc = ARITH_OK;

  if (!gfc_check_conformance (op1, op2, _("elemental binary operation")))
    return ARITH_INCOMMENSURATE;

  head = gfc_constructor_copy (op1->value.constructor);
  for (c = gfc_constructor_first (head),
       d = gfc_constructor_first (op2->value.constructor);
       c && d;
       c = gfc_constructor_next (c), d = gfc_constructor_next (d))
    {
      rc = reduce_binary (eval, c->expr, d->expr, &r);

      if (rc != ARITH_OK)
	break;

      gfc_replace_expr (c->expr, r);
    }

  if (rc == ARITH_OK && (c || d))
    rc = ARITH_INCOMMENSURATE;

  if (rc != ARITH_OK)
    gfc_constructor_free (head);
  else
    {
      gfc_constructor *c = gfc_constructor_first (head);
      if (c == NULL)
	{
	  /* Handle zero-sized arrays.  */
	  r = gfc_get_array_expr (op1->ts.type, op1->ts.kind, &op1->where);
	}
      else
	{
	  r = gfc_get_array_expr (c->expr->ts.type, c->expr->ts.kind,
				  &op1->where);
	}
      r->shape = gfc_copy_shape (op1->shape, op1->rank);
      r->rank = op1->rank;
      r->value.constructor = head;
      *result = r;
    }

  return rc;
}


static arith
reduce_binary (arith (*eval) (gfc_expr *, gfc_expr *, gfc_expr **),
	       gfc_expr *op1, gfc_expr *op2, gfc_expr **result)
{
  if (op1->expr_type == EXPR_CONSTANT && op2->expr_type == EXPR_CONSTANT)
    return eval (op1, op2, result);

  if (op1->expr_type == EXPR_CONSTANT && op2->expr_type == EXPR_ARRAY)
    return reduce_binary_ca (eval, op1, op2, result);

  if (op1->expr_type == EXPR_ARRAY && op2->expr_type == EXPR_CONSTANT)
    return reduce_binary_ac (eval, op1, op2, result);

  if (op1->expr_type != EXPR_ARRAY || op2->expr_type != EXPR_ARRAY)
    return ARITH_NOT_REDUCED;

  return reduce_binary_aa (eval, op1, op2, result);
}


typedef union
{
  arith (*f2)(gfc_expr *, gfc_expr **);
  arith (*f3)(gfc_expr *, gfc_expr *, gfc_expr **);
}
eval_f;

/* High level arithmetic subroutines.  These subroutines go into
   eval_intrinsic(), which can do one of several things to its
   operands.  If the operands are incompatible with the intrinsic
   operation, we return a node pointing to the operands and hope that
   an operator interface is found during resolution.

   If the operands are compatible and are constants, then we try doing
   the arithmetic.  We also handle the cases where either or both
   operands are array constructors.  */

static gfc_expr *
eval_intrinsic (gfc_intrinsic_op op,
		eval_f eval, gfc_expr *op1, gfc_expr *op2)
{
  gfc_expr temp, *result;
  int unary;
  arith rc;

  if (!op1)
    return NULL;

  gfc_clear_ts (&temp.ts);

  switch (op)
    {
    /* Logical unary  */
    case INTRINSIC_NOT:
      if (op1->ts.type != BT_LOGICAL)
	goto runtime;

      temp.ts.type = BT_LOGICAL;
      temp.ts.kind = gfc_default_logical_kind;
      unary = 1;
      break;

    /* Logical binary operators  */
    case INTRINSIC_OR:
    case INTRINSIC_AND:
    case INTRINSIC_NEQV:
    case INTRINSIC_EQV:
      if (op1->ts.type != BT_LOGICAL || op2->ts.type != BT_LOGICAL)
	goto runtime;

      temp.ts.type = BT_LOGICAL;
      temp.ts.kind = gfc_default_logical_kind;
      unary = 0;
      break;

    /* Numeric unary  */
    case INTRINSIC_UPLUS:
    case INTRINSIC_UMINUS:
      if (!gfc_numeric_ts (&op1->ts))
	goto runtime;

      temp.ts = op1->ts;
      unary = 1;
      break;

    case INTRINSIC_PARENTHESES:
      temp.ts = op1->ts;
      unary = 1;
      break;

    /* Additional restrictions for ordering relations.  */
    case INTRINSIC_GE:
    case INTRINSIC_GE_OS:
    case INTRINSIC_LT:
    case INTRINSIC_LT_OS:
    case INTRINSIC_LE:
    case INTRINSIC_LE_OS:
    case INTRINSIC_GT:
    case INTRINSIC_GT_OS:
      if (op1->ts.type == BT_COMPLEX || op2->ts.type == BT_COMPLEX)
	{
	  temp.ts.type = BT_LOGICAL;
	  temp.ts.kind = gfc_default_logical_kind;
	  goto runtime;
	}

    /* Fall through  */
    case INTRINSIC_EQ:
    case INTRINSIC_EQ_OS:
    case INTRINSIC_NE:
    case INTRINSIC_NE_OS:
      if (op1->ts.type == BT_CHARACTER && op2->ts.type == BT_CHARACTER)
	{
	  unary = 0;
	  temp.ts.type = BT_LOGICAL;
	  temp.ts.kind = gfc_default_logical_kind;

	  /* If kind mismatch, exit and we'll error out later.  */
	  if (op1->ts.kind != op2->ts.kind)
	    goto runtime;

	  break;
	}

    gcc_fallthrough ();
    /* Numeric binary  */
    case INTRINSIC_PLUS:
    case INTRINSIC_MINUS:
    case INTRINSIC_TIMES:
    case INTRINSIC_DIVIDE:
    case INTRINSIC_POWER:
      if (!gfc_numeric_ts (&op1->ts) || !gfc_numeric_ts (&op2->ts))
	goto runtime;

      /* Do not perform conversions if operands are not conformable as
	 required for the binary intrinsic operators (F2018:10.1.5).
	 Defer to a possibly overloading user-defined operator.  */
      if (!gfc_op_rank_conformable (op1, op2))
	    goto runtime;

      /* Insert any necessary type conversions to make the operands
	 compatible.  */

      temp.expr_type = EXPR_OP;
      gfc_clear_ts (&temp.ts);
      temp.value.op.op = op;

      temp.value.op.op1 = op1;
      temp.value.op.op2 = op2;

      gfc_type_convert_binary (&temp, warn_conversion || warn_conversion_extra);

      if (op == INTRINSIC_EQ || op == INTRINSIC_NE
	  || op == INTRINSIC_GE || op == INTRINSIC_GT
	  || op == INTRINSIC_LE || op == INTRINSIC_LT
	  || op == INTRINSIC_EQ_OS || op == INTRINSIC_NE_OS
	  || op == INTRINSIC_GE_OS || op == INTRINSIC_GT_OS
	  || op == INTRINSIC_LE_OS || op == INTRINSIC_LT_OS)
	{
	  temp.ts.type = BT_LOGICAL;
	  temp.ts.kind = gfc_default_logical_kind;
	}

      unary = 0;
      break;

    /* Character binary  */
    case INTRINSIC_CONCAT:
      if (op1->ts.type != BT_CHARACTER || op2->ts.type != BT_CHARACTER
	  || op1->ts.kind != op2->ts.kind)
	goto runtime;

      temp.ts.type = BT_CHARACTER;
      temp.ts.kind = op1->ts.kind;
      unary = 0;
      break;

    case INTRINSIC_USER:
      goto runtime;

    default:
      gfc_internal_error ("eval_intrinsic(): Bad operator");
    }

  if (op1->expr_type != EXPR_CONSTANT
      && (op1->expr_type != EXPR_ARRAY
	  || !gfc_is_constant_expr (op1) || !gfc_expanded_ac (op1)))
    goto runtime;

  if (op2 != NULL
      && op2->expr_type != EXPR_CONSTANT
	 && (op2->expr_type != EXPR_ARRAY
	     || !gfc_is_constant_expr (op2) || !gfc_expanded_ac (op2)))
    goto runtime;

  if (unary)
    rc = reduce_unary (eval.f2, op1, &result);
  else
    rc = reduce_binary (eval.f3, op1, op2, &result);

  if (rc == ARITH_INVALID_TYPE || rc == ARITH_NOT_REDUCED)
    goto runtime;

  /* Something went wrong.  */
  if (op == INTRINSIC_POWER && rc == ARITH_PROHIBIT)
    return NULL;

  if (rc != ARITH_OK)
    {
      gfc_error (gfc_arith_error (rc), &op1->where);
      if (rc == ARITH_OVERFLOW)
	goto done;

      if (rc == ARITH_DIV0 && op2->ts.type == BT_INTEGER)
	gfc_seen_div0 = true;

      return NULL;
    }

done:

  gfc_free_expr (op1);
  gfc_free_expr (op2);
  return result;

runtime:
  /* Create a run-time expression.  */
  result = gfc_get_operator_expr (&op1->where, op, op1, op2);
  result->ts = temp.ts;

  return result;
}


/* Modify type of expression for zero size array.  */

static gfc_expr *
eval_type_intrinsic0 (gfc_intrinsic_op iop, gfc_expr *op)
{
  if (op == NULL)
    gfc_internal_error ("eval_type_intrinsic0(): op NULL");

  switch (iop)
    {
    case INTRINSIC_GE:
    case INTRINSIC_GE_OS:
    case INTRINSIC_LT:
    case INTRINSIC_LT_OS:
    case INTRINSIC_LE:
    case INTRINSIC_LE_OS:
    case INTRINSIC_GT:
    case INTRINSIC_GT_OS:
    case INTRINSIC_EQ:
    case INTRINSIC_EQ_OS:
    case INTRINSIC_NE:
    case INTRINSIC_NE_OS:
      op->ts.type = BT_LOGICAL;
      op->ts.kind = gfc_default_logical_kind;
      break;

    default:
      break;
    }

  return op;
}


/* Return nonzero if the expression is a zero size array.  */

static bool
gfc_zero_size_array (gfc_expr *e)
{
  if (e == NULL || e->expr_type != EXPR_ARRAY)
    return false;

  return e->value.constructor == NULL;
}


/* Reduce a binary expression where at least one of the operands
   involves a zero-length array.  Returns NULL if neither of the
   operands is a zero-length array.  */

static gfc_expr *
reduce_binary0 (gfc_expr *op1, gfc_expr *op2)
{
  if (gfc_zero_size_array (op1))
    {
      gfc_free_expr (op2);
      return op1;
    }

  if (gfc_zero_size_array (op2))
    {
      gfc_free_expr (op1);
      return op2;
    }

  return NULL;
}


static gfc_expr *
eval_intrinsic_f2 (gfc_intrinsic_op op,
		   arith (*eval) (gfc_expr *, gfc_expr **),
		   gfc_expr *op1, gfc_expr *op2)
{
  gfc_expr *result;
  eval_f f;

  if (op2 == NULL)
    {
      if (gfc_zero_size_array (op1))
	return eval_type_intrinsic0 (op, op1);
    }
  else
    {
      result = reduce_binary0 (op1, op2);
      if (result != NULL)
	return eval_type_intrinsic0 (op, result);
    }

  f.f2 = eval;
  return eval_intrinsic (op, f, op1, op2);
}


static gfc_expr *
eval_intrinsic_f3 (gfc_intrinsic_op op,
		   arith (*eval) (gfc_expr *, gfc_expr *, gfc_expr **),
		   gfc_expr *op1, gfc_expr *op2)
{
  gfc_expr *result;
  eval_f f;

  if (!op1 && !op2)
    return NULL;

  result = reduce_binary0 (op1, op2);
  if (result != NULL)
    return eval_type_intrinsic0(op, result);

  f.f3 = eval;
  return eval_intrinsic (op, f, op1, op2);
}


gfc_expr *
gfc_parentheses (gfc_expr *op)
{
  if (gfc_is_constant_expr (op))
    return op;

  return eval_intrinsic_f2 (INTRINSIC_PARENTHESES, gfc_arith_identity,
			    op, NULL);
}

gfc_expr *
gfc_uplus (gfc_expr *op)
{
  return eval_intrinsic_f2 (INTRINSIC_UPLUS, gfc_arith_identity, op, NULL);
}


gfc_expr *
gfc_uminus (gfc_expr *op)
{
  return eval_intrinsic_f2 (INTRINSIC_UMINUS, gfc_arith_uminus, op, NULL);
}


gfc_expr *
gfc_add (gfc_expr *op1, gfc_expr *op2)
{
  return eval_intrinsic_f3 (INTRINSIC_PLUS, gfc_arith_plus, op1, op2);
}


gfc_expr *
gfc_subtract (gfc_expr *op1, gfc_expr *op2)
{
  return eval_intrinsic_f3 (INTRINSIC_MINUS, gfc_arith_minus, op1, op2);
}


gfc_expr *
gfc_multiply (gfc_expr *op1, gfc_expr *op2)
{
  return eval_intrinsic_f3 (INTRINSIC_TIMES, gfc_arith_times, op1, op2);
}


gfc_expr *
gfc_divide (gfc_expr *op1, gfc_expr *op2)
{
  return eval_intrinsic_f3 (INTRINSIC_DIVIDE, gfc_arith_divide, op1, op2);
}


gfc_expr *
gfc_power (gfc_expr *op1, gfc_expr *op2)
{
  return eval_intrinsic_f3 (INTRINSIC_POWER, arith_power, op1, op2);
}


gfc_expr *
gfc_concat (gfc_expr *op1, gfc_expr *op2)
{
  return eval_intrinsic_f3 (INTRINSIC_CONCAT, gfc_arith_concat, op1, op2);
}


gfc_expr *
gfc_and (gfc_expr *op1, gfc_expr *op2)
{
  return eval_intrinsic_f3 (INTRINSIC_AND, gfc_arith_and, op1, op2);
}


gfc_expr *
gfc_or (gfc_expr *op1, gfc_expr *op2)
{
  return eval_intrinsic_f3 (INTRINSIC_OR, gfc_arith_or, op1, op2);
}


gfc_expr *
gfc_not (gfc_expr *op1)
{
  return eval_intrinsic_f2 (INTRINSIC_NOT, gfc_arith_not, op1, NULL);
}


gfc_expr *
gfc_eqv (gfc_expr *op1, gfc_expr *op2)
{
  return eval_intrinsic_f3 (INTRINSIC_EQV, gfc_arith_eqv, op1, op2);
}


gfc_expr *
gfc_neqv (gfc_expr *op1, gfc_expr *op2)
{
  return eval_intrinsic_f3 (INTRINSIC_NEQV, gfc_arith_neqv, op1, op2);
}


gfc_expr *
gfc_eq (gfc_expr *op1, gfc_expr *op2, gfc_intrinsic_op op)
{
  return eval_intrinsic_f3 (op, gfc_arith_eq, op1, op2);
}


gfc_expr *
gfc_ne (gfc_expr *op1, gfc_expr *op2, gfc_intrinsic_op op)
{
  return eval_intrinsic_f3 (op, gfc_arith_ne, op1, op2);
}


gfc_expr *
gfc_gt (gfc_expr *op1, gfc_expr *op2, gfc_intrinsic_op op)
{
  return eval_intrinsic_f3 (op, gfc_arith_gt, op1, op2);
}


gfc_expr *
gfc_ge (gfc_expr *op1, gfc_expr *op2, gfc_intrinsic_op op)
{
  return eval_intrinsic_f3 (op, gfc_arith_ge, op1, op2);
}


gfc_expr *
gfc_lt (gfc_expr *op1, gfc_expr *op2, gfc_intrinsic_op op)
{
  return eval_intrinsic_f3 (op, gfc_arith_lt, op1, op2);
}


gfc_expr *
gfc_le (gfc_expr *op1, gfc_expr *op2, gfc_intrinsic_op op)
{
  return eval_intrinsic_f3 (op, gfc_arith_le, op1, op2);
}


/******* Simplification of intrinsic functions with constant arguments *****/


/* Deal with an arithmetic error.  */

static void
arith_error (arith rc, gfc_typespec *from, gfc_typespec *to, locus *where)
{
  switch (rc)
    {
    case ARITH_OK:
      gfc_error ("Arithmetic OK converting %s to %s at %L",
		 gfc_typename (from), gfc_typename (to), where);
      break;
    case ARITH_OVERFLOW:
      gfc_error ("Arithmetic overflow converting %s to %s at %L. This check "
		 "can be disabled with the option %<-fno-range-check%>",
		 gfc_typename (from), gfc_typename (to), where);
      break;
    case ARITH_UNDERFLOW:
      gfc_error ("Arithmetic underflow converting %s to %s at %L. This check "
		 "can be disabled with the option %<-fno-range-check%>",
		 gfc_typename (from), gfc_typename (to), where);
      break;
    case ARITH_NAN:
      gfc_error ("Arithmetic NaN converting %s to %s at %L. This check "
		 "can be disabled with the option %<-fno-range-check%>",
		 gfc_typename (from), gfc_typename (to), where);
      break;
    case ARITH_DIV0:
      gfc_error ("Division by zero converting %s to %s at %L",
		 gfc_typename (from), gfc_typename (to), where);
      break;
    case ARITH_INCOMMENSURATE:
      gfc_error ("Array operands are incommensurate converting %s to %s at %L",
		 gfc_typename (from), gfc_typename (to), where);
      break;
    case ARITH_ASYMMETRIC:
      gfc_error ("Integer outside symmetric range implied by Standard Fortran"
	 	 " converting %s to %s at %L",
		 gfc_typename (from), gfc_typename (to), where);
      break;
    default:
      gfc_internal_error ("gfc_arith_error(): Bad error code");
    }

  /* TODO: Do something about the error, i.e., throw exception, return
     NaN, etc.  */
}

/* Returns true if significant bits were lost when converting real
   constant r from from_kind to to_kind.  */

static bool
wprecision_real_real (mpfr_t r, int from_kind, int to_kind)
{
  mpfr_t rv, diff;
  bool ret;

  gfc_set_model_kind (to_kind);
  mpfr_init (rv);
  gfc_set_model_kind (from_kind);
  mpfr_init (diff);

  mpfr_set (rv, r, GFC_RND_MODE);
  mpfr_sub (diff, rv, r, GFC_RND_MODE);

  ret = ! mpfr_zero_p (diff);
  mpfr_clear (rv);
  mpfr_clear (diff);
  return ret;
}

/* Return true if conversion from an integer to a real loses precision.  */

static bool
wprecision_int_real (mpz_t n, mpfr_t r)
{
  bool ret;
  mpz_t i;
  mpz_init (i);
  mpfr_get_z (i, r, GFC_RND_MODE);
  mpz_sub (i, i, n);
  ret = mpz_cmp_si (i, 0) != 0;
  mpz_clear (i);
  return ret;
}

/* Convert integers to integers.  */

gfc_expr *
gfc_int2int (gfc_expr *src, int kind)
{
  gfc_expr *result;
  arith rc;

  if (src->ts.type != BT_INTEGER)
    return NULL;

  result = gfc_get_constant_expr (BT_INTEGER, kind, &src->where);

  mpz_set (result->value.integer, src->value.integer);

  if ((rc = gfc_check_integer_range (result->value.integer, kind)) != ARITH_OK)
    {
      if (rc == ARITH_ASYMMETRIC)
	{
	  gfc_warning (0, gfc_arith_error (rc), &src->where);
	}
      else
	{
	  arith_error (rc, &src->ts, &result->ts, &src->where);
	  gfc_free_expr (result);
	  return NULL;
	}
    }

  /*  If we do not trap numeric overflow, we need to convert the number to
      signed, throwing away high-order bits if necessary.  */
  if (flag_range_check == 0)
    {
      int k;

      k = gfc_validate_kind (BT_INTEGER, kind, false);
      gfc_convert_mpz_to_signed (result->value.integer,
				 gfc_integer_kinds[k].bit_size);

      if (warn_conversion && !src->do_not_warn && kind < src->ts.kind)
	gfc_warning_now (OPT_Wconversion, "Conversion from %qs to %qs at %L",
			 gfc_typename (&src->ts), gfc_typename (&result->ts),
			 &src->where);
    }
  return result;
}


/* Convert integers to reals.  */

gfc_expr *
gfc_int2real (gfc_expr *src, int kind)
{
  gfc_expr *result;
  arith rc;

  if (src->ts.type != BT_INTEGER)
    return NULL;

  result = gfc_get_constant_expr (BT_REAL, kind, &src->where);

  mpfr_set_z (result->value.real, src->value.integer, GFC_RND_MODE);

  if ((rc = gfc_check_real_range (result->value.real, kind)) != ARITH_OK)
    {
      arith_error (rc, &src->ts, &result->ts, &src->where);
      gfc_free_expr (result);
      return NULL;
    }

  if (warn_conversion
      && wprecision_int_real (src->value.integer, result->value.real))
    gfc_warning (OPT_Wconversion, "Change of value in conversion "
		 "from %qs to %qs at %L",
		 gfc_typename (&src->ts),
		 gfc_typename (&result->ts),
		 &src->where);

  return result;
}


/* Convert default integer to default complex.  */

gfc_expr *
gfc_int2complex (gfc_expr *src, int kind)
{
  gfc_expr *result;
  arith rc;

  if (src->ts.type != BT_INTEGER)
    return NULL;

  result = gfc_get_constant_expr (BT_COMPLEX, kind, &src->where);

  mpc_set_z (result->value.complex, src->value.integer, GFC_MPC_RND_MODE);

  if ((rc = gfc_check_real_range (mpc_realref (result->value.complex), kind))
      != ARITH_OK)
    {
      arith_error (rc, &src->ts, &result->ts, &src->where);
      gfc_free_expr (result);
      return NULL;
    }

  if (warn_conversion
      && wprecision_int_real (src->value.integer,
			      mpc_realref (result->value.complex)))
      gfc_warning_now (OPT_Wconversion, "Change of value in conversion "
		       "from %qs to %qs at %L",
		       gfc_typename (&src->ts),
		       gfc_typename (&result->ts),
		       &src->where);

  return result;
}


/* Convert default real to default integer.  */

gfc_expr *
gfc_real2int (gfc_expr *src, int kind)
{
  gfc_expr *result;
  arith rc;
  bool did_warn = false;

  if (src->ts.type != BT_REAL)
    return NULL;

  result = gfc_get_constant_expr (BT_INTEGER, kind, &src->where);

  gfc_mpfr_to_mpz (result->value.integer, src->value.real, &src->where);

  if ((rc = gfc_check_integer_range (result->value.integer, kind)) != ARITH_OK)
    {
      arith_error (rc, &src->ts, &result->ts, &src->where);
      gfc_free_expr (result);
      return NULL;
    }

  /* If there was a fractional part, warn about this.  */

  if (warn_conversion)
    {
      mpfr_t f;
      mpfr_init (f);
      mpfr_frac (f, src->value.real, GFC_RND_MODE);
      if (mpfr_cmp_si (f, 0) != 0)
	{
	  gfc_warning_now (OPT_Wconversion, "Change of value in conversion "
			   "from %qs to %qs at %L", gfc_typename (&src->ts),
			   gfc_typename (&result->ts), &src->where);
	  did_warn = true;
	}
      mpfr_clear (f);
    }
  if (!did_warn && warn_conversion_extra)
    {
      gfc_warning_now (OPT_Wconversion_extra, "Conversion from %qs to %qs "
		       "at %L", gfc_typename (&src->ts),
		       gfc_typename (&result->ts), &src->where);
    }

  return result;
}


/* Convert real to real.  */

gfc_expr *
gfc_real2real (gfc_expr *src, int kind)
{
  gfc_expr *result;
  arith rc;
  bool did_warn = false;

  if (src->ts.type != BT_REAL)
    return NULL;

  result = gfc_get_constant_expr (BT_REAL, kind, &src->where);

  mpfr_set (result->value.real, src->value.real, GFC_RND_MODE);

  rc = gfc_check_real_range (result->value.real, kind);

  if (rc == ARITH_UNDERFLOW)
    {
      if (warn_underflow)
	gfc_warning (OPT_Woverflow, gfc_arith_error (rc), &src->where);
      mpfr_set_ui (result->value.real, 0, GFC_RND_MODE);
    }
  else if (rc != ARITH_OK)
    {
      arith_error (rc, &src->ts, &result->ts, &src->where);
      gfc_free_expr (result);
      return NULL;
    }

  /* As a special bonus, don't warn about REAL values which are not changed by
     the conversion if -Wconversion is specified and -Wconversion-extra is
     not.  */

  if ((warn_conversion || warn_conversion_extra) && src->ts.kind > kind)
    {
      int w = warn_conversion ? OPT_Wconversion : OPT_Wconversion_extra;

      /* Calculate the difference between the constant and the rounded
	 value and check it against zero.  */

      if (wprecision_real_real (src->value.real, src->ts.kind, kind))
	{
	  gfc_warning_now (w, "Change of value in conversion from "
			   "%qs to %qs at %L",
			   gfc_typename (&src->ts), gfc_typename (&result->ts),
			   &src->where);
	  /* Make sure the conversion warning is not emitted again.  */
	  did_warn = true;
	}
    }

    if (!did_warn && warn_conversion_extra)
      gfc_warning_now (OPT_Wconversion_extra, "Conversion from %qs to %qs "
		       "at %L", gfc_typename(&src->ts),
		       gfc_typename(&result->ts), &src->where);

  return result;
}


/* Convert real to complex.  */

gfc_expr *
gfc_real2complex (gfc_expr *src, int kind)
{
  gfc_expr *result;
  arith rc;
  bool did_warn = false;

  if (src->ts.type != BT_REAL)
    return NULL;

  result = gfc_get_constant_expr (BT_COMPLEX, kind, &src->where);

  mpc_set_fr (result->value.complex, src->value.real, GFC_MPC_RND_MODE);

  rc = gfc_check_real_range (mpc_realref (result->value.complex), kind);

  if (rc == ARITH_UNDERFLOW)
    {
      if (warn_underflow)
	gfc_warning (OPT_Woverflow, gfc_arith_error (rc), &src->where);
      mpfr_set_ui (mpc_realref (result->value.complex), 0, GFC_RND_MODE);
    }
  else if (rc != ARITH_OK)
    {
      arith_error (rc, &src->ts, &result->ts, &src->where);
      gfc_free_expr (result);
      return NULL;
    }

  if ((warn_conversion || warn_conversion_extra) && src->ts.kind > kind)
    {
      int w = warn_conversion ? OPT_Wconversion : OPT_Wconversion_extra;

      if (wprecision_real_real (src->value.real, src->ts.kind, kind))
	{
	  gfc_warning_now (w, "Change of value in conversion from "
			   "%qs to %qs at %L",
			   gfc_typename (&src->ts), gfc_typename (&result->ts),
			   &src->where);
	  /* Make sure the conversion warning is not emitted again.  */
	  did_warn = true;
	}
    }

  if (!did_warn && warn_conversion_extra)
    gfc_warning_now (OPT_Wconversion_extra, "Conversion from %qs to %qs "
		     "at %L", gfc_typename(&src->ts),
		     gfc_typename(&result->ts), &src->where);

  return result;
}


/* Convert complex to integer.  */

gfc_expr *
gfc_complex2int (gfc_expr *src, int kind)
{
  gfc_expr *result;
  arith rc;
  bool did_warn = false;

  if (src->ts.type != BT_COMPLEX)
    return NULL;

  result = gfc_get_constant_expr (BT_INTEGER, kind, &src->where);

  gfc_mpfr_to_mpz (result->value.integer, mpc_realref (src->value.complex),
		   &src->where);

  if ((rc = gfc_check_integer_range (result->value.integer, kind)) != ARITH_OK)
    {
      arith_error (rc, &src->ts, &result->ts, &src->where);
      gfc_free_expr (result);
      return NULL;
    }

  if (warn_conversion || warn_conversion_extra)
    {
      int w = warn_conversion ? OPT_Wconversion : OPT_Wconversion_extra;

      /* See if we discarded an imaginary part.  */
      if (mpfr_cmp_si (mpc_imagref (src->value.complex), 0) != 0)
	{
	  gfc_warning_now (w, "Non-zero imaginary part discarded "
			   "in conversion from %qs to %qs at %L",
			   gfc_typename(&src->ts), gfc_typename (&result->ts),
			   &src->where);
	  did_warn = true;
	}

      else {
	mpfr_t f;

	mpfr_init (f);
	mpfr_frac (f, src->value.real, GFC_RND_MODE);
	if (mpfr_cmp_si (f, 0) != 0)
	  {
	    gfc_warning_now (w, "Change of value in conversion from "
			     "%qs to %qs at %L", gfc_typename (&src->ts),
			     gfc_typename (&result->ts), &src->where);
	    did_warn = true;
	  }
	mpfr_clear (f);
      }

      if (!did_warn && warn_conversion_extra)
	{
	  gfc_warning_now (OPT_Wconversion_extra, "Conversion from %qs to %qs "
			   "at %L", gfc_typename (&src->ts),
			   gfc_typename (&result->ts), &src->where);
	}
    }

  return result;
}


/* Convert complex to real.  */

gfc_expr *
gfc_complex2real (gfc_expr *src, int kind)
{
  gfc_expr *result;
  arith rc;
  bool did_warn = false;

  if (src->ts.type != BT_COMPLEX)
    return NULL;

  result = gfc_get_constant_expr (BT_REAL, kind, &src->where);

  mpc_real (result->value.real, src->value.complex, GFC_RND_MODE);

  rc = gfc_check_real_range (result->value.real, kind);

  if (rc == ARITH_UNDERFLOW)
    {
      if (warn_underflow)
	gfc_warning (OPT_Woverflow, gfc_arith_error (rc), &src->where);
      mpfr_set_ui (result->value.real, 0, GFC_RND_MODE);
    }
  if (rc != ARITH_OK)
    {
      arith_error (rc, &src->ts, &result->ts, &src->where);
      gfc_free_expr (result);
      return NULL;
    }

  if (warn_conversion || warn_conversion_extra)
    {
      int w = warn_conversion ? OPT_Wconversion : OPT_Wconversion_extra;

      /* See if we discarded an imaginary part.  */
      if (mpfr_cmp_si (mpc_imagref (src->value.complex), 0) != 0)
	{
	  gfc_warning (w, "Non-zero imaginary part discarded "
		       "in conversion from %qs to %qs at %L",
		       gfc_typename(&src->ts), gfc_typename (&result->ts),
		       &src->where);
	  did_warn = true;
	}

      /* Calculate the difference between the real constant and the rounded
	 value and check it against zero.  */

      if (kind > src->ts.kind
	  && wprecision_real_real (mpc_realref (src->value.complex),
				   src->ts.kind, kind))
	{
	  gfc_warning_now (w, "Change of value in conversion from "
			   "%qs to %qs at %L",
			   gfc_typename (&src->ts), gfc_typename (&result->ts),
			   &src->where);
	  /* Make sure the conversion warning is not emitted again.  */
	  did_warn = true;
	}
    }

  if (!did_warn && warn_conversion_extra)
    gfc_warning_now (OPT_Wconversion, "Conversion from %qs to %qs at %L",
		     gfc_typename(&src->ts), gfc_typename (&result->ts),
		     &src->where);

  return result;
}


/* Convert complex to complex.  */

gfc_expr *
gfc_complex2complex (gfc_expr *src, int kind)
{
  gfc_expr *result;
  arith rc;
  bool did_warn = false;

  if (src->ts.type != BT_COMPLEX)
    return NULL;

  result = gfc_get_constant_expr (BT_COMPLEX, kind, &src->where);

  mpc_set (result->value.complex, src->value.complex, GFC_MPC_RND_MODE);

  rc = gfc_check_real_range (mpc_realref (result->value.complex), kind);

  if (rc == ARITH_UNDERFLOW)
    {
      if (warn_underflow)
	gfc_warning (OPT_Woverflow, gfc_arith_error (rc), &src->where);
      mpfr_set_ui (mpc_realref (result->value.complex), 0, GFC_RND_MODE);
    }
  else if (rc != ARITH_OK)
    {
      arith_error (rc, &src->ts, &result->ts, &src->where);
      gfc_free_expr (result);
      return NULL;
    }

  rc = gfc_check_real_range (mpc_imagref (result->value.complex), kind);

  if (rc == ARITH_UNDERFLOW)
    {
      if (warn_underflow)
	gfc_warning (OPT_Woverflow, gfc_arith_error (rc), &src->where);
      mpfr_set_ui (mpc_imagref (result->value.complex), 0, GFC_RND_MODE);
    }
  else if (rc != ARITH_OK)
    {
      arith_error (rc, &src->ts, &result->ts, &src->where);
      gfc_free_expr (result);
      return NULL;
    }

  if ((warn_conversion || warn_conversion_extra) && src->ts.kind > kind
      && (wprecision_real_real (mpc_realref (src->value.complex),
				src->ts.kind, kind)
	  || wprecision_real_real (mpc_imagref (src->value.complex),
				   src->ts.kind, kind)))
    {
      int w = warn_conversion ? OPT_Wconversion : OPT_Wconversion_extra;

      gfc_warning_now (w, "Change of value in conversion from "
		       "%qs to %qs at %L",
		       gfc_typename (&src->ts), gfc_typename (&result->ts),
		       &src->where);
      did_warn = true;
    }

  if (!did_warn && warn_conversion_extra && src->ts.kind != kind)
    gfc_warning_now (OPT_Wconversion_extra, "Conversion from %qs to %qs "
		     "at %L", gfc_typename(&src->ts),
		     gfc_typename (&result->ts), &src->where);

  return result;
}


/* Logical kind conversion.  */

gfc_expr *
gfc_log2log (gfc_expr *src, int kind)
{
  gfc_expr *result;

  if (src->ts.type != BT_LOGICAL)
    return NULL;

  result = gfc_get_constant_expr (BT_LOGICAL, kind, &src->where);
  result->value.logical = src->value.logical;

  return result;
}


/* Convert logical to integer.  */

gfc_expr *
gfc_log2int (gfc_expr *src, int kind)
{
  gfc_expr *result;

  if (src->ts.type != BT_LOGICAL)
    return NULL;

  result = gfc_get_constant_expr (BT_INTEGER, kind, &src->where);
  mpz_set_si (result->value.integer, src->value.logical);

  return result;
}


/* Convert integer to logical.  */

gfc_expr *
gfc_int2log (gfc_expr *src, int kind)
{
  gfc_expr *result;

  if (src->ts.type != BT_INTEGER)
    return NULL;

  result = gfc_get_constant_expr (BT_LOGICAL, kind, &src->where);
  result->value.logical = (mpz_cmp_si (src->value.integer, 0) != 0);

  return result;
}

/* Convert character to character. We only use wide strings internally,
   so we only set the kind.  */

gfc_expr *
gfc_character2character (gfc_expr *src, int kind)
{
  gfc_expr *result;
  result = gfc_copy_expr (src);
  result->ts.kind = kind;

  return result;
}

/* Helper function to set the representation in a Hollerith conversion.
   This assumes that the ts.type and ts.kind of the result have already
   been set.  */

static void
hollerith2representation (gfc_expr *result, gfc_expr *src)
{
  size_t src_len, result_len;

  src_len = src->representation.length - src->ts.u.pad;
  gfc_target_expr_size (result, &result_len);

  if (src_len > result_len)
    {
      gfc_warning (OPT_Wcharacter_truncation, "The Hollerith constant at %L "
		   "is truncated in conversion to %qs", &src->where,
		   gfc_typename(&result->ts));
    }

  result->representation.string = XCNEWVEC (char, result_len + 1);
  memcpy (result->representation.string, src->representation.string,
	  MIN (result_len, src_len));

  if (src_len < result_len)
    memset (&result->representation.string[src_len], ' ', result_len - src_len);

  result->representation.string[result_len] = '\0'; /* For debugger  */
  result->representation.length = result_len;
}


/* Helper function to set the representation in a character conversion.
   This assumes that the ts.type and ts.kind of the result have already
   been set.  */

static void
character2representation (gfc_expr *result, gfc_expr *src)
{
  size_t src_len, result_len, i;
  src_len = src->value.character.length;
  gfc_target_expr_size (result, &result_len);

  if (src_len > result_len)
    gfc_warning (OPT_Wcharacter_truncation, "The character constant at %L is "
		 "truncated in conversion to %s", &src->where,
		 gfc_typename(&result->ts));

  result->representation.string = XCNEWVEC (char, result_len + 1);

  for (i = 0; i < MIN (result_len, src_len); i++)
    result->representation.string[i] = (char) src->value.character.string[i];

  if (src_len < result_len)
    memset (&result->representation.string[src_len], ' ',
	    result_len - src_len);

  result->representation.string[result_len] = '\0'; /* For debugger.  */
  result->representation.length = result_len;
}

/* Convert Hollerith to integer. The constant will be padded or truncated.  */

gfc_expr *
gfc_hollerith2int (gfc_expr *src, int kind)
{
  gfc_expr *result;
  result = gfc_get_constant_expr (BT_INTEGER, kind, &src->where);

  hollerith2representation (result, src);
  gfc_interpret_integer (kind, (unsigned char *) result->representation.string,
			 result->representation.length, result->value.integer);

  return result;
}

/* Convert character to integer.  The constant will be padded or truncated.  */

gfc_expr *
gfc_character2int (gfc_expr *src, int kind)
{
  gfc_expr *result;
  result = gfc_get_constant_expr (BT_INTEGER, kind, &src->where);

  character2representation (result, src);
  gfc_interpret_integer (kind, (unsigned char *) result->representation.string,
			 result->representation.length, result->value.integer);
  return result;
}

/* Convert Hollerith to real.  The constant will be padded or truncated.  */

gfc_expr *
gfc_hollerith2real (gfc_expr *src, int kind)
{
  gfc_expr *result;
  result = gfc_get_constant_expr (BT_REAL, kind, &src->where);

  hollerith2representation (result, src);
  if (gfc_interpret_float (kind,
			   (unsigned char *) result->representation.string,
			   result->representation.length, result->value.real))
    return result;
  else
    return NULL;
}

/* Convert character to real.  The constant will be padded or truncated.  */

gfc_expr *
gfc_character2real (gfc_expr *src, int kind)
{
  gfc_expr *result;
  result = gfc_get_constant_expr (BT_REAL, kind, &src->where);

  character2representation (result, src);
  gfc_interpret_float (kind, (unsigned char *) result->representation.string,
		       result->representation.length, result->value.real);

  return result;
}


/* Convert Hollerith to complex. The constant will be padded or truncated.  */

gfc_expr *
gfc_hollerith2complex (gfc_expr *src, int kind)
{
  gfc_expr *result;
  result = gfc_get_constant_expr (BT_COMPLEX, kind, &src->where);

  hollerith2representation (result, src);
  gfc_interpret_complex (kind, (unsigned char *) result->representation.string,
			 result->representation.length, result->value.complex);

  return result;
}

/* Convert character to complex. The constant will be padded or truncated.  */

gfc_expr *
gfc_character2complex (gfc_expr *src, int kind)
{
  gfc_expr *result;
  result = gfc_get_constant_expr (BT_COMPLEX, kind, &src->where);

  character2representation (result, src);
  gfc_interpret_complex (kind, (unsigned char *) result->representation.string,
			 result->representation.length, result->value.complex);

  return result;
}


/* Convert Hollerith to character.  */

gfc_expr *
gfc_hollerith2character (gfc_expr *src, int kind)
{
  gfc_expr *result;

  result = gfc_copy_expr (src);
  result->ts.type = BT_CHARACTER;
  result->ts.kind = kind;
  result->ts.u.pad = 0;

  result->value.character.length = result->representation.length;
  result->value.character.string
    = gfc_char_to_widechar (result->representation.string);

  return result;
}


/* Convert Hollerith to logical. The constant will be padded or truncated.  */

gfc_expr *
gfc_hollerith2logical (gfc_expr *src, int kind)
{
  gfc_expr *result;
  result = gfc_get_constant_expr (BT_LOGICAL, kind, &src->where);

  hollerith2representation (result, src);
  gfc_interpret_logical (kind, (unsigned char *) result->representation.string,
			 result->representation.length, &result->value.logical);

  return result;
}

/* Convert character to logical. The constant will be padded or truncated.  */

gfc_expr *
gfc_character2logical (gfc_expr *src, int kind)
{
  gfc_expr *result;
  result = gfc_get_constant_expr (BT_LOGICAL, kind, &src->where);

  character2representation (result, src);
  gfc_interpret_logical (kind, (unsigned char *) result->representation.string,
			 result->representation.length, &result->value.logical);

  return result;
}
