/* Compiler arithmetic
   Copyright (C) 2000, 2001, 2002, 2003, 2004 Free Software Foundation,
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
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

/* Since target arithmetic must be done on the host, there has to
   be some way of evaluating arithmetic expressions as the host
   would evaluate them.  We use the GNU MP library to do arithmetic,
   and this file provides the interface.  */

#include "config.h"

#include <string.h>

#include "gfortran.h"
#include "arith.h"

mpf_t pi, half_pi, two_pi, e;

/* The gfc_(integer|real)_kinds[] structures have everything the front
   end needs to know about integers and real numbers on the target.
   Other entries of the structure are calculated from these values.
   The first entry is the default kind, the second entry of the real
   structure is the default double kind.  */

#define MPZ_NULL {{0,0,0}}
#define MPF_NULL {{0,0,0,0}}

#define DEF_GFC_INTEGER_KIND(KIND,RADIX,DIGITS,BIT_SIZE)		\
	{KIND, RADIX, DIGITS, BIT_SIZE, 0, MPZ_NULL, MPZ_NULL, MPZ_NULL}

#define DEF_GFC_LOGICAL_KIND(KIND,BIT_SIZE)				\
	{KIND, BIT_SIZE}

#define DEF_GFC_REAL_KIND(KIND,RADIX,DIGITS,MIN_EXP, MAX_EXP)		\
	{KIND, RADIX, DIGITS, MIN_EXP, MAX_EXP,				\
	 0, 0, MPF_NULL, MPF_NULL, MPF_NULL}

gfc_integer_info gfc_integer_kinds[] = {
  DEF_GFC_INTEGER_KIND (4, 2, 31, 32),
  DEF_GFC_INTEGER_KIND (8, 2, 63, 64),
  DEF_GFC_INTEGER_KIND (2, 2, 15, 16),
  DEF_GFC_INTEGER_KIND (1, 2,  7,  8),
  DEF_GFC_INTEGER_KIND (0, 0,  0,  0)
};

gfc_logical_info gfc_logical_kinds[] = {
  DEF_GFC_LOGICAL_KIND (4, 32),
  DEF_GFC_LOGICAL_KIND (8, 64),
  DEF_GFC_LOGICAL_KIND (2, 16),
  DEF_GFC_LOGICAL_KIND (1,  8),
  DEF_GFC_LOGICAL_KIND (0,  0)
};

gfc_real_info gfc_real_kinds[] = {
  DEF_GFC_REAL_KIND (4, 2, 24,  -125,  128),
  DEF_GFC_REAL_KIND (8, 2, 53, -1021, 1024),
  DEF_GFC_REAL_KIND (0, 0,  0,     0,    0)
};


/* The integer kind to use for array indices.  This will be set to the
   proper value based on target information from the backend.  */

int gfc_index_integer_kind;


/* Compute the natural log of arg.

   We first get the argument into the range 0.5 to 1.5 by successive
   multiplications or divisions by e.  Then we use the series:

     ln(x) = (x-1) - (x-1)^2/2 + (x-1)^3/3 - (x-1)^4/4 + ...

   Because we are expanding in powers of (x-1), and 0.5 < x < 1.5, we
   have -0.5 < (x-1) < 0.5.  Ignoring the harmonic term, this means
   that each term is at most 1/(2^i), meaning one bit is gained per
   iteration.

   Not very efficient, but it doesn't have to be.  */

void
natural_logarithm (mpf_t * arg, mpf_t * result)
{
  mpf_t x, xp, t, log;
  int i, p;

  mpf_init_set (x, *arg);
  mpf_init (t);

  p = 0;

  mpf_set_str (t, "0.5", 10);
  while (mpf_cmp (x, t) < 0)
    {
      mpf_mul (x, x, e);
      p--;
    }

  mpf_set_str (t, "1.5", 10);
  while (mpf_cmp (x, t) > 0)
    {
      mpf_div (x, x, e);
      p++;
    }

  mpf_sub_ui (x, x, 1);
  mpf_init_set_ui (log, 0);
  mpf_init_set_ui (xp, 1);

  for (i = 1; i < GFC_REAL_BITS; i++)
    {
      mpf_mul (xp, xp, x);
      mpf_div_ui (t, xp, i);

      if (i % 2 == 0)
	mpf_sub (log, log, t);
      else
	mpf_add (log, log, t);
    }

  /* Add in the log (e^p) = p */

  if (p < 0)
    mpf_sub_ui (log, log, -p);
  else
    mpf_add_ui (log, log, p);

  mpf_clear (x);
  mpf_clear (xp);
  mpf_clear (t);

  mpf_set (*result, log);
  mpf_clear (log);
}


/* Calculate the common logarithm of arg.  We use the natural
   logaritm of arg and of 10:

   log10(arg) = log(arg)/log(10)  */

void
common_logarithm (mpf_t * arg, mpf_t * result)
{
  mpf_t i10, log10;

  natural_logarithm (arg, result);

  mpf_init_set_ui (i10, 10);
  mpf_init (log10);
  natural_logarithm (&i10, &log10);

  mpf_div (*result, *result, log10);
  mpf_clear (i10);
  mpf_clear (log10);
}

/* Calculate exp(arg).

   We use a reduction of the form

     x = Nln2 + r

   Then we obtain exp(r) from the Maclaurin series.
   exp(x) is then recovered from the identity

     exp(x) = 2^N*exp(r).  */

void
exponential (mpf_t * arg, mpf_t * result)
{
  mpf_t two, ln2, power, q, r, num, denom, term, x, xp;
  int i;
  long n;
  unsigned long p, mp;


  mpf_init_set (x, *arg);

  if (mpf_cmp_ui (x, 0) == 0)
    {
      mpf_set_ui (*result, 1);
    }
  else if (mpf_cmp_ui (x, 1) == 0)
    {
      mpf_set (*result, e);
    }
  else
    {
      mpf_init_set_ui (two, 2);
      mpf_init (ln2);
      mpf_init (q);
      mpf_init (r);
      mpf_init (power);
      mpf_init (term);

      natural_logarithm (&two, &ln2);

      mpf_div (q, x, ln2);
      mpf_floor (power, q);
      mpf_mul (q, power, ln2);
      mpf_sub (r, x, q);

      mpf_init_set_ui (xp, 1);
      mpf_init_set_ui (num, 1);
      mpf_init_set_ui (denom, 1);

      for (i = 1; i <= GFC_REAL_BITS + 10; i++)
	{
	  mpf_mul (num, num, r);
	  mpf_mul_ui (denom, denom, i);
	  mpf_div (term, num, denom);
	  mpf_add (xp, xp, term);
	}

      /* Reconstruction step */
      n = (long) mpf_get_d (power);

      if (n > 0)
	{
	  p = (unsigned int) n;
	  mpf_mul_2exp (*result, xp, p);
	}
      else
	{
	  mp = (unsigned int) (-n);
	  mpf_div_2exp (*result, xp, mp);
	}

      mpf_clear (two);
      mpf_clear (ln2);
      mpf_clear (q);
      mpf_clear (r);
      mpf_clear (power);
      mpf_clear (num);
      mpf_clear (denom);
      mpf_clear (term);
      mpf_clear (xp);
    }

  mpf_clear (x);
}


/* Calculate sin(arg).

   We use a reduction of the form

     x= N*2pi + r

   Then we obtain sin(r) from the Maclaurin series.  */

void
sine (mpf_t * arg, mpf_t * result)
{
  mpf_t factor, q, r, num, denom, term, x, xp;
  int i, sign;

  mpf_init_set (x, *arg);

  /* Special case (we do not treat multiples of pi due to roundoff issues) */
  if (mpf_cmp_ui (x, 0) == 0)
    {
      mpf_set_ui (*result, 0);
    }
  else
    {
      mpf_init (q);
      mpf_init (r);
      mpf_init (factor);
      mpf_init (term);

      mpf_div (q, x, two_pi);
      mpf_floor (factor, q);
      mpf_mul (q, factor, two_pi);
      mpf_sub (r, x, q);

      mpf_init_set_ui (xp, 0);
      mpf_init_set_ui (num, 1);
      mpf_init_set_ui (denom, 1);

      sign = -1;
      for (i = 1; i < GFC_REAL_BITS + 10; i++)
	{
	  mpf_mul (num, num, r);
	  mpf_mul_ui (denom, denom, i);
	  if (i % 2 == 0)
	    continue;

	  sign = -sign;
	  mpf_div (term, num, denom);
	  if (sign > 0)
	    mpf_add (xp, xp, term);
	  else
	    mpf_sub (xp, xp, term);
	}

      mpf_set (*result, xp);

      mpf_clear (q);
      mpf_clear (r);
      mpf_clear (factor);
      mpf_clear (num);
      mpf_clear (denom);
      mpf_clear (term);
      mpf_clear (xp);
    }

  mpf_clear (x);
}


/* Calculate cos(arg).

   Similar to sine.  */

void
cosine (mpf_t * arg, mpf_t * result)
{
  mpf_t factor, q, r, num, denom, term, x, xp;
  int i, sign;

  mpf_init_set (x, *arg);

  /* Special case (we do not treat multiples of pi due to roundoff issues) */
  if (mpf_cmp_ui (x, 0) == 0)
    {
      mpf_set_ui (*result, 1);
    }
  else
    {
      mpf_init (q);
      mpf_init (r);
      mpf_init (factor);
      mpf_init (term);

      mpf_div (q, x, two_pi);
      mpf_floor (factor, q);
      mpf_mul (q, factor, two_pi);
      mpf_sub (r, x, q);

      mpf_init_set_ui (xp, 1);
      mpf_init_set_ui (num, 1);
      mpf_init_set_ui (denom, 1);

      sign = 1;
      for (i = 1; i < GFC_REAL_BITS + 10; i++)
	{
	  mpf_mul (num, num, r);
	  mpf_mul_ui (denom, denom, i);
	  if (i % 2 != 0)
	    continue;

	  sign = -sign;
	  mpf_div (term, num, denom);
	  if (sign > 0)
	    mpf_add (xp, xp, term);
	  else
	    mpf_sub (xp, xp, term);
	}
      mpf_set (*result, xp);

      mpf_clear (q);
      mpf_clear (r);
      mpf_clear (factor);
      mpf_clear (num);
      mpf_clear (denom);
      mpf_clear (term);
      mpf_clear (xp);
    }

  mpf_clear (x);
}


/* Calculate atan(arg).

   Similar to sine but requires special handling for x near 1.  */

void
arctangent (mpf_t * arg, mpf_t * result)
{
  mpf_t absval, convgu, convgl, num, term, x, xp;
  int i, sign;

  mpf_init_set (x, *arg);

  /* Special cases */
  if (mpf_cmp_ui (x, 0) == 0)
    {
      mpf_set_ui (*result, 0);
    }
  else if (mpf_cmp_ui (x, 1) == 0)
    {
      mpf_init (num);
      mpf_div_ui (num, half_pi, 2);
      mpf_set (*result, num);
      mpf_clear (num);
    }
  else if (mpf_cmp_si (x, -1) == 0)
    {
      mpf_init (num);
      mpf_div_ui (num, half_pi, 2);
      mpf_neg (*result, num);
      mpf_clear (num);
    }
  else
    {				/* General cases */

      mpf_init (absval);
      mpf_abs (absval, x);

      mpf_init_set_d (convgu, 1.5);
      mpf_init_set_d (convgl, 0.5);
      mpf_init_set_ui (num, 1);
      mpf_init (term);

      if (mpf_cmp (absval, convgl) < 0)
	{
	  mpf_init_set_ui (xp, 0);
	  sign = -1;
	  for (i = 1; i < GFC_REAL_BITS + 10; i++)
	    {
	      mpf_mul (num, num, absval);
	      if (i % 2 == 0)
		continue;

	      sign = -sign;
	      mpf_div_ui (term, num, i);
	      if (sign > 0)
		mpf_add (xp, xp, term);
	      else
		mpf_sub (xp, xp, term);
	    }
	}
      else if (mpf_cmp (absval, convgu) >= 0)
	{
	  mpf_init_set (xp, half_pi);
	  sign = 1;
	  for (i = 1; i < GFC_REAL_BITS + 10; i++)
	    {
	      mpf_div (num, num, absval);
	      if (i % 2 == 0)
		continue;

	      sign = -sign;
	      mpf_div_ui (term, num, i);
	      if (sign > 0)
		mpf_add (xp, xp, term);
	      else
		mpf_sub (xp, xp, term);
	    }
	}
      else
	{
	  mpf_init_set_ui (xp, 0);

	  mpf_sub_ui (num, absval, 1);
	  mpf_add_ui (term, absval, 1);
	  mpf_div (absval, num, term);

	  mpf_set_ui (num, 1);

	  sign = -1;
	  for (i = 1; i < GFC_REAL_BITS + 10; i++)
	    {
	      mpf_mul (num, num, absval);
	      if (i % 2 == 0)
		continue;
	      sign = -sign;
	      mpf_div_ui (term, num, i);
	      if (sign > 0)
		mpf_add (xp, xp, term);
	      else
		mpf_sub (xp, xp, term);
	    }

	  mpf_div_ui (term, half_pi, 2);
	  mpf_add (xp, term, xp);
	}

      /* This makes sure to preserve the identity arctan(-x) = -arctan(x)
         and improves accuracy to boot.  */

      if (mpf_cmp_ui (x, 0) > 0)
	mpf_set (*result, xp);
      else
	mpf_neg (*result, xp);

      mpf_clear (absval);
      mpf_clear (convgl);
      mpf_clear (convgu);
      mpf_clear (num);
      mpf_clear (term);
      mpf_clear (xp);
    }
  mpf_clear (x);
}


/* Calculate atan2 (y, x)

atan2(y, x) = atan(y/x)				if x > 0,
	      sign(y)*(pi - atan(|y/x|))	if x < 0,
	      0					if x = 0 && y == 0,
	      sign(y)*pi/2			if x = 0 && y != 0.
*/

void
arctangent2 (mpf_t * y, mpf_t * x, mpf_t * result)
{
  mpf_t t;

  mpf_init (t);

  switch (mpf_sgn (*x))
    {
    case 1:
      mpf_div (t, *y, *x);
      arctangent (&t, result);
      break;
    case -1:
      mpf_div (t, *y, *x);
      mpf_abs (t, t);
      arctangent (&t, &t);
      mpf_sub (*result, pi, t);
      if (mpf_sgn (*y) == -1)
	mpf_neg (*result, *result);
      break;
    case 0:
      if (mpf_sgn (*y) == 0)
	mpf_set_ui (*result, 0);
      else
	{
	  mpf_set (*result, half_pi);
	  if (mpf_sgn (*y) == -1)
	    mpf_neg (*result, *result);
	}
       break;
    }
  mpf_clear (t);
}

/* Calculate cosh(arg). */

void
hypercos (mpf_t * arg, mpf_t * result)
{
  mpf_t neg, term1, term2, x, xp;

  mpf_init_set (x, *arg);

  mpf_init (neg);
  mpf_init (term1);
  mpf_init (term2);
  mpf_init (xp);

  mpf_neg (neg, x);

  exponential (&x, &term1);
  exponential (&neg, &term2);

  mpf_add (xp, term1, term2);
  mpf_div_ui (*result, xp, 2);

  mpf_clear (neg);
  mpf_clear (term1);
  mpf_clear (term2);
  mpf_clear (x);
  mpf_clear (xp);
}


/* Calculate sinh(arg). */

void
hypersine (mpf_t * arg, mpf_t * result)
{
  mpf_t neg, term1, term2, x, xp;

  mpf_init_set (x, *arg);

  mpf_init (neg);
  mpf_init (term1);
  mpf_init (term2);
  mpf_init (xp);

  mpf_neg (neg, x);

  exponential (&x, &term1);
  exponential (&neg, &term2);

  mpf_sub (xp, term1, term2);
  mpf_div_ui (*result, xp, 2);

  mpf_clear (neg);
  mpf_clear (term1);
  mpf_clear (term2);
  mpf_clear (x);
  mpf_clear (xp);
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
      p = "Arithmetic OK";
      break;
    case ARITH_OVERFLOW:
      p = "Arithmetic overflow";
      break;
    case ARITH_UNDERFLOW:
      p = "Arithmetic underflow";
      break;
    case ARITH_DIV0:
      p = "Division by zero";
      break;
    case ARITH_0TO0:
      p = "Indeterminate form 0 ** 0";
      break;
    case ARITH_INCOMMENSURATE:
      p = "Array operands are incommensurate";
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
  mpf_t a, b;
  mpz_t r;
  int i, n, limit;

  /* Set the default precision for GMP computations.  */
  mpf_set_default_prec (GFC_REAL_BITS + 30);

  /* Calculate e, needed by the natural_logarithm() subroutine.  */
  mpf_init (b);
  mpf_init_set_ui (e, 0);
  mpf_init_set_ui (a, 1);

  for (i = 1; i < 100; i++)
    {
      mpf_add (e, e, a);
      mpf_div_ui (a, a, i);	/* 1/(i!) */
    }

  /* Calculate pi, 2pi, pi/2, and -pi/2, needed for trigonometric
     functions.

     We use the Bailey, Borwein and Plouffe formula:

       pi = \sum{n=0}^\infty (1/16)^n [4/(8n+1) - 2/(8n+4) - 1/(8n+5) - 1/(8n+6)]

     which gives about four bits per iteration.  */

  mpf_init_set_ui (pi, 0);

  mpf_init (two_pi);
  mpf_init (half_pi);

  limit = (GFC_REAL_BITS / 4) + 10;	/* (1/16)^n gives 4 bits per iteration */

  for (n = 0; n < limit; n++)
    {
      mpf_set_ui (b, 4);
      mpf_div_ui (b, b, 8 * n + 1);	/* 4/(8n+1) */

      mpf_set_ui (a, 2);
      mpf_div_ui (a, a, 8 * n + 4);	/* 2/(8n+4) */
      mpf_sub (b, b, a);

      mpf_set_ui (a, 1);
      mpf_div_ui (a, a, 8 * n + 5);	/* 1/(8n+5) */
      mpf_sub (b, b, a);

      mpf_set_ui (a, 1);
      mpf_div_ui (a, a, 8 * n + 6);	/* 1/(8n+6) */
      mpf_sub (b, b, a);

      mpf_set_ui (a, 16);
      mpf_pow_ui (a, a, n);	/* 16^n */

      mpf_div (b, b, a);

      mpf_add (pi, pi, b);
    }

  mpf_mul_ui (two_pi, pi, 2);
  mpf_div_ui (half_pi, pi, 2);

  /* Convert the minimum/maximum values for each kind into their
     GNU MP representation.  */
  mpz_init (r);

  for (int_info = gfc_integer_kinds; int_info->kind != 0; int_info++)
    {
      /* Huge */
      mpz_set_ui (r, int_info->radix);
      mpz_pow_ui (r, r, int_info->digits);

      mpz_init (int_info->huge);
      mpz_sub_ui (int_info->huge, r, 1);

      /* These are the numbers that are actually representable by the
         target.  For bases other than two, this needs to be changed.  */
      if (int_info->radix != 2)
	gfc_internal_error ("Fix min_int, max_int calculation");

      mpz_init (int_info->min_int);
      mpz_neg (int_info->min_int, int_info->huge);
      /* No -1 here, because the representation is symmetric.  */

      mpz_init (int_info->max_int);
      mpz_add (int_info->max_int, int_info->huge, int_info->huge);
      mpz_add_ui (int_info->max_int, int_info->max_int, 1);

      /* Range */
      mpf_set_z (a, int_info->huge);
      common_logarithm (&a, &a);
      mpf_trunc (a, a);
      mpz_set_f (r, a);
      int_info->range = mpz_get_si (r);
    }

  /*  mpf_set_default_prec(GFC_REAL_BITS); */
  for (real_info = gfc_real_kinds; real_info->kind != 0; real_info++)
    {
      /* Huge */
      mpf_set_ui (a, real_info->radix);
      mpf_set_ui (b, real_info->radix);

      mpf_pow_ui (a, a, real_info->max_exponent);
      mpf_pow_ui (b, b, real_info->max_exponent - real_info->digits);

      mpf_init (real_info->huge);
      mpf_sub (real_info->huge, a, b);

      /* Tiny */
      mpf_set_ui (b, real_info->radix);
      mpf_pow_ui (b, b, 1 - real_info->min_exponent);

      mpf_init (real_info->tiny);
      mpf_ui_div (real_info->tiny, 1, b);

      /* Epsilon */
      mpf_set_ui (b, real_info->radix);
      mpf_pow_ui (b, b, real_info->digits - 1);

      mpf_init (real_info->epsilon);
      mpf_ui_div (real_info->epsilon, 1, b);

      /* Range */
      common_logarithm (&real_info->huge, &a);
      common_logarithm (&real_info->tiny, &b);
      mpf_neg (b, b);

      if (mpf_cmp (a, b) > 0)
	mpf_set (a, b);		/* a = min(a, b) */

      mpf_trunc (a, a);
      mpz_set_f (r, a);
      real_info->range = mpz_get_si (r);

      /* Precision */
      mpf_set_ui (a, real_info->radix);
      common_logarithm (&a, &a);

      mpf_mul_ui (a, a, real_info->digits - 1);
      mpf_trunc (a, a);
      mpz_set_f (r, a);
      real_info->precision = mpz_get_si (r);

      /* If the radix is an integral power of 10, add one to the
         precision.  */
      for (i = 10; i <= real_info->radix; i *= 10)
	if (i == real_info->radix)
	  real_info->precision++;
    }

  mpz_clear (r);
  mpf_clear (a);
  mpf_clear (b);
}


/* Clean up, get rid of numeric constants.  */

void
gfc_arith_done_1 (void)
{
  gfc_integer_info *ip;
  gfc_real_info *rp;

  mpf_clear (e);

  mpf_clear (pi);
  mpf_clear (half_pi);
  mpf_clear (two_pi);

  for (ip = gfc_integer_kinds; ip->kind; ip++)
    {
      mpz_clear (ip->min_int);
      mpz_clear (ip->max_int);
      mpz_clear (ip->huge);
    }

  for (rp = gfc_real_kinds; rp->kind; rp++)
    {
      mpf_clear (rp->epsilon);
      mpf_clear (rp->huge);
      mpf_clear (rp->tiny);
    }
}


/* Return default kinds.  */

int
gfc_default_integer_kind (void)
{
  return gfc_integer_kinds[gfc_option.i8 ? 1 : 0].kind;
}

int
gfc_default_real_kind (void)
{
  return gfc_real_kinds[gfc_option.r8 ? 1 : 0].kind;
}

int
gfc_default_double_kind (void)
{
  return gfc_real_kinds[1].kind;
}

int
gfc_default_character_kind (void)
{
  return 1;
}

int
gfc_default_logical_kind (void)
{
  return gfc_logical_kinds[gfc_option.i8 ? 1 : 0].kind;
}

int
gfc_default_complex_kind (void)
{
  return gfc_default_real_kind ();
}


/* Make sure that a valid kind is present.  Returns an index into the
   gfc_integer_kinds array, -1 if the kind is not present.  */

static int
validate_integer (int kind)
{
  int i;

  for (i = 0;; i++)
    {
      if (gfc_integer_kinds[i].kind == 0)
	{
	  i = -1;
	  break;
	}
      if (gfc_integer_kinds[i].kind == kind)
	break;
    }

  return i;
}


static int
validate_real (int kind)
{
  int i;

  for (i = 0;; i++)
    {
      if (gfc_real_kinds[i].kind == 0)
	{
	  i = -1;
	  break;
	}
      if (gfc_real_kinds[i].kind == kind)
	break;
    }

  return i;
}


static int
validate_logical (int kind)
{
  int i;

  for (i = 0;; i++)
    {
      if (gfc_logical_kinds[i].kind == 0)
	{
	  i = -1;
	  break;
	}
      if (gfc_logical_kinds[i].kind == kind)
	break;
    }

  return i;
}


static int
validate_character (int kind)
{

  if (kind == gfc_default_character_kind ())
    return 0;
  return -1;
}


/* Validate a kind given a basic type.  The return value is the same
   for the child functions, with -1 indicating nonexistence of the
   type.  */

int
gfc_validate_kind (bt type, int kind)
{
  int rc;

  switch (type)
    {
    case BT_REAL:		/* Fall through */
    case BT_COMPLEX:
      rc = validate_real (kind);
      break;
    case BT_INTEGER:
      rc = validate_integer (kind);
      break;
    case BT_LOGICAL:
      rc = validate_logical (kind);
      break;
    case BT_CHARACTER:
      rc = validate_character (kind);
      break;

    default:
      gfc_internal_error ("gfc_validate_kind(): Got bad type");
    }

  return rc;
}


/* Given an integer and a kind, make sure that the integer lies within
   the range of the kind.  Returns ARITH_OK or ARITH_OVERFLOW.  */

static arith
gfc_check_integer_range (mpz_t p, int kind)
{
  arith result;
  int i;

  i = validate_integer (kind);
  if (i == -1)
    gfc_internal_error ("gfc_check_integer_range(): Bad kind");

  result = ARITH_OK;

  if (mpz_cmp (p, gfc_integer_kinds[i].min_int) < 0
      || mpz_cmp (p, gfc_integer_kinds[i].max_int) > 0)
    result = ARITH_OVERFLOW;

  return result;
}


/* Given a real and a kind, make sure that the real lies within the
   range of the kind.  Returns ARITH_OK, ARITH_OVERFLOW or
   ARITH_UNDERFLOW.  */

static arith
gfc_check_real_range (mpf_t p, int kind)
{
  arith retval;
  mpf_t q;
  int i;

  mpf_init (q);
  mpf_abs (q, p);

  i = validate_real (kind);
  if (i == -1)
    gfc_internal_error ("gfc_check_real_range(): Bad kind");

  retval = ARITH_OK;
  if (mpf_sgn (q) == 0)
    goto done;

  if (mpf_cmp (q, gfc_real_kinds[i].huge) == 1)
    {
      retval = ARITH_OVERFLOW;
      goto done;
    }

  if (mpf_cmp (q, gfc_real_kinds[i].tiny) == -1)
    retval = ARITH_UNDERFLOW;

done:
  mpf_clear (q);

  return retval;
}


/* Function to return a constant expression node of a given type and
   kind.  */

gfc_expr *
gfc_constant_result (bt type, int kind, locus * where)
{
  gfc_expr *result;

  if (!where)
    gfc_internal_error
      ("gfc_constant_result(): locus 'where' cannot be NULL");

  result = gfc_get_expr ();

  result->expr_type = EXPR_CONSTANT;
  result->ts.type = type;
  result->ts.kind = kind;
  result->where = *where;

  switch (type)
    {
    case BT_INTEGER:
      mpz_init (result->value.integer);
      break;

    case BT_REAL:
      mpf_init (result->value.real);
      break;

    case BT_COMPLEX:
      mpf_init (result->value.complex.r);
      mpf_init (result->value.complex.i);
      break;

    default:
      break;
    }

  return result;
}


/* Low-level arithmetic functions.  All of these subroutines assume
   that all operands are of the same type and return an operand of the
   same type.  The other thing about these subroutines is that they
   can fail in various ways -- overflow, underflow, division by zero,
   zero raised to the zero, etc.  */

static arith
gfc_arith_not (gfc_expr * op1, gfc_expr ** resultp)
{
  gfc_expr *result;

  result = gfc_constant_result (BT_LOGICAL, op1->ts.kind, &op1->where);
  result->value.logical = !op1->value.logical;
  *resultp = result;

  return ARITH_OK;
}


static arith
gfc_arith_and (gfc_expr * op1, gfc_expr * op2, gfc_expr ** resultp)
{
  gfc_expr *result;

  result = gfc_constant_result (BT_LOGICAL, gfc_kind_max (op1, op2),
				&op1->where);
  result->value.logical = op1->value.logical && op2->value.logical;
  *resultp = result;

  return ARITH_OK;
}


static arith
gfc_arith_or (gfc_expr * op1, gfc_expr * op2, gfc_expr ** resultp)
{
  gfc_expr *result;

  result = gfc_constant_result (BT_LOGICAL, gfc_kind_max (op1, op2),
				&op1->where);
  result->value.logical = op1->value.logical || op2->value.logical;
  *resultp = result;

  return ARITH_OK;
}


static arith
gfc_arith_eqv (gfc_expr * op1, gfc_expr * op2, gfc_expr ** resultp)
{
  gfc_expr *result;

  result = gfc_constant_result (BT_LOGICAL, gfc_kind_max (op1, op2),
				&op1->where);
  result->value.logical = op1->value.logical == op2->value.logical;
  *resultp = result;

  return ARITH_OK;
}


static arith
gfc_arith_neqv (gfc_expr * op1, gfc_expr * op2, gfc_expr ** resultp)
{
  gfc_expr *result;

  result = gfc_constant_result (BT_LOGICAL, gfc_kind_max (op1, op2),
				&op1->where);
  result->value.logical = op1->value.logical != op2->value.logical;
  *resultp = result;

  return ARITH_OK;
}


/* Make sure a constant numeric expression is within the range for
   its type and kind.  Note that there's also a gfc_check_range(),
   but that one deals with the intrinsic RANGE function.  */

arith
gfc_range_check (gfc_expr * e)
{
  arith rc;

  switch (e->ts.type)
    {
    case BT_INTEGER:
      rc = gfc_check_integer_range (e->value.integer, e->ts.kind);
      break;

    case BT_REAL:
      rc = gfc_check_real_range (e->value.real, e->ts.kind);
      break;

    case BT_COMPLEX:
      rc = gfc_check_real_range (e->value.complex.r, e->ts.kind);
      if (rc == ARITH_OK)
	rc = gfc_check_real_range (e->value.complex.i, e->ts.kind);

      break;

    default:
      gfc_internal_error ("gfc_range_check(): Bad type");
    }

  return rc;
}


/* It may seem silly to have a subroutine that actually computes the
   unary plus of a constant, but it prevents us from making exceptions
   in the code elsewhere.  */

static arith
gfc_arith_uplus (gfc_expr * op1, gfc_expr ** resultp)
{

  *resultp = gfc_copy_expr (op1);
  return ARITH_OK;
}


static arith
gfc_arith_uminus (gfc_expr * op1, gfc_expr ** resultp)
{
  gfc_expr *result;
  arith rc;

  result = gfc_constant_result (op1->ts.type, op1->ts.kind, &op1->where);

  switch (op1->ts.type)
    {
    case BT_INTEGER:
      mpz_neg (result->value.integer, op1->value.integer);
      break;

    case BT_REAL:
      mpf_neg (result->value.real, op1->value.real);
      break;

    case BT_COMPLEX:
      mpf_neg (result->value.complex.r, op1->value.complex.r);
      mpf_neg (result->value.complex.i, op1->value.complex.i);
      break;

    default:
      gfc_internal_error ("gfc_arith_uminus(): Bad basic type");
    }

  rc = gfc_range_check (result);

  if (rc != ARITH_OK)
    gfc_free_expr (result);
  else
    *resultp = result;

  return rc;
}


static arith
gfc_arith_plus (gfc_expr * op1, gfc_expr * op2, gfc_expr ** resultp)
{
  gfc_expr *result;
  arith rc;

  result = gfc_constant_result (op1->ts.type, op1->ts.kind, &op1->where);

  switch (op1->ts.type)
    {
    case BT_INTEGER:
      mpz_add (result->value.integer, op1->value.integer, op2->value.integer);
      break;

    case BT_REAL:
      mpf_add (result->value.real, op1->value.real, op2->value.real);
      break;

    case BT_COMPLEX:
      mpf_add (result->value.complex.r, op1->value.complex.r,
	       op2->value.complex.r);

      mpf_add (result->value.complex.i, op1->value.complex.i,
	       op2->value.complex.i);
      break;

    default:
      gfc_internal_error ("gfc_arith_plus(): Bad basic type");
    }

  rc = gfc_range_check (result);

  if (rc != ARITH_OK)
    gfc_free_expr (result);
  else
    *resultp = result;

  return rc;
}


static arith
gfc_arith_minus (gfc_expr * op1, gfc_expr * op2, gfc_expr ** resultp)
{
  gfc_expr *result;
  arith rc;

  result = gfc_constant_result (op1->ts.type, op1->ts.kind, &op1->where);

  switch (op1->ts.type)
    {
    case BT_INTEGER:
      mpz_sub (result->value.integer, op1->value.integer, op2->value.integer);
      break;

    case BT_REAL:
      mpf_sub (result->value.real, op1->value.real, op2->value.real);
      break;

    case BT_COMPLEX:
      mpf_sub (result->value.complex.r, op1->value.complex.r,
	       op2->value.complex.r);

      mpf_sub (result->value.complex.i, op1->value.complex.i,
	       op2->value.complex.i);

      break;

    default:
      gfc_internal_error ("gfc_arith_minus(): Bad basic type");
    }

  rc = gfc_range_check (result);

  if (rc != ARITH_OK)
    gfc_free_expr (result);
  else
    *resultp = result;

  return rc;
}


static arith
gfc_arith_times (gfc_expr * op1, gfc_expr * op2, gfc_expr ** resultp)
{
  gfc_expr *result;
  mpf_t x, y;
  arith rc;

  result = gfc_constant_result (op1->ts.type, op1->ts.kind, &op1->where);

  switch (op1->ts.type)
    {
    case BT_INTEGER:
      mpz_mul (result->value.integer, op1->value.integer, op2->value.integer);
      break;

    case BT_REAL:
      mpf_mul (result->value.real, op1->value.real, op2->value.real);
      break;

    case BT_COMPLEX:
      mpf_init (x);
      mpf_init (y);

      mpf_mul (x, op1->value.complex.r, op2->value.complex.r);
      mpf_mul (y, op1->value.complex.i, op2->value.complex.i);
      mpf_sub (result->value.complex.r, x, y);

      mpf_mul (x, op1->value.complex.r, op2->value.complex.i);
      mpf_mul (y, op1->value.complex.i, op2->value.complex.r);
      mpf_add (result->value.complex.i, x, y);

      mpf_clear (x);
      mpf_clear (y);

      break;

    default:
      gfc_internal_error ("gfc_arith_times(): Bad basic type");
    }

  rc = gfc_range_check (result);

  if (rc != ARITH_OK)
    gfc_free_expr (result);
  else
    *resultp = result;

  return rc;
}


static arith
gfc_arith_divide (gfc_expr * op1, gfc_expr * op2, gfc_expr ** resultp)
{
  gfc_expr *result;
  mpf_t x, y, div;
  arith rc;

  rc = ARITH_OK;

  result = gfc_constant_result (op1->ts.type, op1->ts.kind, &op1->where);

  switch (op1->ts.type)
    {
    case BT_INTEGER:
      if (mpz_sgn (op2->value.integer) == 0)
	{
	  rc = ARITH_DIV0;
	  break;
	}

      mpz_tdiv_q (result->value.integer, op1->value.integer,
		  op2->value.integer);
      break;

    case BT_REAL:
      if (mpf_sgn (op2->value.real) == 0)
	{
	  rc = ARITH_DIV0;
	  break;
	}

      mpf_div (result->value.real, op1->value.real, op2->value.real);
      break;

    case BT_COMPLEX:
      if (mpf_sgn (op2->value.complex.r) == 0
	  && mpf_sgn (op2->value.complex.i) == 0)
	{
	  rc = ARITH_DIV0;
	  break;
	}

      mpf_init (x);
      mpf_init (y);
      mpf_init (div);

      mpf_mul (x, op2->value.complex.r, op2->value.complex.r);
      mpf_mul (y, op2->value.complex.i, op2->value.complex.i);
      mpf_add (div, x, y);

      mpf_mul (x, op1->value.complex.r, op2->value.complex.r);
      mpf_mul (y, op1->value.complex.i, op2->value.complex.i);
      mpf_add (result->value.complex.r, x, y);
      mpf_div (result->value.complex.r, result->value.complex.r, div);

      mpf_mul (x, op1->value.complex.i, op2->value.complex.r);
      mpf_mul (y, op1->value.complex.r, op2->value.complex.i);
      mpf_sub (result->value.complex.i, x, y);
      mpf_div (result->value.complex.i, result->value.complex.i, div);

      mpf_clear (x);
      mpf_clear (y);
      mpf_clear (div);

      break;

    default:
      gfc_internal_error ("gfc_arith_divide(): Bad basic type");
    }

  if (rc == ARITH_OK)
    rc = gfc_range_check (result);

  if (rc != ARITH_OK)
    gfc_free_expr (result);
  else
    *resultp = result;

  return rc;
}


/* Compute the reciprocal of a complex number (guaranteed nonzero).  */

static void
complex_reciprocal (gfc_expr * op)
{
  mpf_t mod, a, result_r, result_i;

  mpf_init (mod);
  mpf_init (a);

  mpf_mul (mod, op->value.complex.r, op->value.complex.r);
  mpf_mul (a, op->value.complex.i, op->value.complex.i);
  mpf_add (mod, mod, a);

  mpf_init (result_r);
  mpf_div (result_r, op->value.complex.r, mod);

  mpf_init (result_i);
  mpf_neg (result_i, op->value.complex.i);
  mpf_div (result_i, result_i, mod);

  mpf_set (op->value.complex.r, result_r);
  mpf_set (op->value.complex.i, result_i);

  mpf_clear (result_r);
  mpf_clear (result_i);

  mpf_clear (mod);
  mpf_clear (a);
}


/* Raise a complex number to positive power.  */

static void
complex_pow_ui (gfc_expr * base, int power, gfc_expr * result)
{
  mpf_t temp_r, temp_i, a;

  mpf_set_ui (result->value.complex.r, 1);
  mpf_set_ui (result->value.complex.i, 0);

  mpf_init (temp_r);
  mpf_init (temp_i);
  mpf_init (a);

  for (; power > 0; power--)
    {
      mpf_mul (temp_r, base->value.complex.r, result->value.complex.r);
      mpf_mul (a, base->value.complex.i, result->value.complex.i);
      mpf_sub (temp_r, temp_r, a);

      mpf_mul (temp_i, base->value.complex.r, result->value.complex.i);
      mpf_mul (a, base->value.complex.i, result->value.complex.r);
      mpf_add (temp_i, temp_i, a);

      mpf_set (result->value.complex.r, temp_r);
      mpf_set (result->value.complex.i, temp_i);
    }

  mpf_clear (temp_r);
  mpf_clear (temp_i);
  mpf_clear (a);
}


/* Raise a number to an integer power.  */

static arith
gfc_arith_power (gfc_expr * op1, gfc_expr * op2, gfc_expr ** resultp)
{
  int power, apower;
  gfc_expr *result;
  mpz_t unity_z;
  mpf_t unity_f;
  arith rc;

  rc = ARITH_OK;

  if (gfc_extract_int (op2, &power) != NULL)
    gfc_internal_error ("gfc_arith_power(): Bad exponent");

  result = gfc_constant_result (op1->ts.type, op1->ts.kind, &op1->where);

  if (power == 0)
    {				/* Handle something to the zeroth power */
      switch (op1->ts.type)
	{
	case BT_INTEGER:
	  if (mpz_sgn (op1->value.integer) == 0)
	    rc = ARITH_0TO0;
	  else
	    mpz_set_ui (result->value.integer, 1);

	  break;

	case BT_REAL:
	  if (mpf_sgn (op1->value.real) == 0)
	    rc = ARITH_0TO0;
	  else
	    mpf_set_ui (result->value.real, 1);

	  break;

	case BT_COMPLEX:
	  if (mpf_sgn (op1->value.complex.r) == 0
	      && mpf_sgn (op1->value.complex.i) == 0)
	    rc = ARITH_0TO0;
	  else
	    {
	      mpf_set_ui (result->value.complex.r, 1);
	      mpf_set_ui (result->value.complex.r, 0);
	    }

	  break;

	default:
	  gfc_internal_error ("gfc_arith_power(): Bad base");
	}
    }

  if (power != 0)
    {
      apower = power;
      if (power < 0)
	apower = -power;

      switch (op1->ts.type)
	{
	case BT_INTEGER:
	  mpz_pow_ui (result->value.integer, op1->value.integer, apower);

	  if (power < 0)
	    {
	      mpz_init_set_ui (unity_z, 1);
	      mpz_tdiv_q (result->value.integer, unity_z,
			  result->value.integer);
	      mpz_clear (unity_z);
	    }

	  break;

	case BT_REAL:
	  mpf_pow_ui (result->value.real, op1->value.real, apower);

	  if (power < 0)
	    {
	      mpf_init_set_ui (unity_f, 1);
	      mpf_div (result->value.real, unity_f, result->value.real);
	      mpf_clear (unity_f);
	    }

	  break;

	case BT_COMPLEX:
	  complex_pow_ui (op1, apower, result);
	  if (power < 0)
	    complex_reciprocal (result);

	  break;

	default:
	  break;
	}
    }

  if (rc == ARITH_OK)
    rc = gfc_range_check (result);

  if (rc != ARITH_OK)
    gfc_free_expr (result);
  else
    *resultp = result;

  return rc;
}


/* Concatenate two string constants.  */

static arith
gfc_arith_concat (gfc_expr * op1, gfc_expr * op2, gfc_expr ** resultp)
{
  gfc_expr *result;
  int len;

  result = gfc_constant_result (BT_CHARACTER, gfc_default_character_kind (),
				&op1->where);

  len = op1->value.character.length + op2->value.character.length;

  result->value.character.string = gfc_getmem (len + 1);
  result->value.character.length = len;

  memcpy (result->value.character.string, op1->value.character.string,
	  op1->value.character.length);

  memcpy (result->value.character.string + op1->value.character.length,
	  op2->value.character.string, op2->value.character.length);

  result->value.character.string[len] = '\0';

  *resultp = result;

  return ARITH_OK;
}


/* Comparison operators.  Assumes that the two expression nodes
   contain two constants of the same type.  */

int
gfc_compare_expr (gfc_expr * op1, gfc_expr * op2)
{
  int rc;

  switch (op1->ts.type)
    {
    case BT_INTEGER:
      rc = mpz_cmp (op1->value.integer, op2->value.integer);
      break;

    case BT_REAL:
      rc = mpf_cmp (op1->value.real, op2->value.real);
      break;

    case BT_CHARACTER:
      rc = gfc_compare_string (op1, op2, NULL);
      break;

    case BT_LOGICAL:
      rc = ((!op1->value.logical && op2->value.logical)
	    || (op1->value.logical && !op2->value.logical));
      break;

    default:
      gfc_internal_error ("gfc_compare_expr(): Bad basic type");
    }

  return rc;
}


/* Compare a pair of complex numbers.  Naturally, this is only for
   equality/nonequality.  */

static int
compare_complex (gfc_expr * op1, gfc_expr * op2)
{

  return (mpf_cmp (op1->value.complex.r, op2->value.complex.r) == 0
	  && mpf_cmp (op1->value.complex.i, op2->value.complex.i) == 0);
}


/* Given two constant strings and the inverse collating sequence,
   compare the strings.  We return -1 for a<b, 0 for a==b and 1 for
   a>b.  If the xcoll_table is NULL, we use the processor's default
   collating sequence.  */

int
gfc_compare_string (gfc_expr * a, gfc_expr * b, const int *xcoll_table)
{
  int len, alen, blen, i, ac, bc;

  alen = a->value.character.length;
  blen = b->value.character.length;

  len = (alen > blen) ? alen : blen;

  for (i = 0; i < len; i++)
    {
      ac = (i < alen) ? a->value.character.string[i] : ' ';
      bc = (i < blen) ? b->value.character.string[i] : ' ';

      if (xcoll_table != NULL)
	{
	  ac = xcoll_table[ac];
	  bc = xcoll_table[bc];
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
gfc_arith_eq (gfc_expr * op1, gfc_expr * op2, gfc_expr ** resultp)
{
  gfc_expr *result;

  result = gfc_constant_result (BT_LOGICAL, gfc_default_logical_kind (),
				&op1->where);
  result->value.logical = (op1->ts.type == BT_COMPLEX) ?
    compare_complex (op1, op2) : (gfc_compare_expr (op1, op2) == 0);

  *resultp = result;
  return ARITH_OK;
}


static arith
gfc_arith_ne (gfc_expr * op1, gfc_expr * op2, gfc_expr ** resultp)
{
  gfc_expr *result;

  result = gfc_constant_result (BT_LOGICAL, gfc_default_logical_kind (),
				&op1->where);
  result->value.logical = (op1->ts.type == BT_COMPLEX) ?
    !compare_complex (op1, op2) : (gfc_compare_expr (op1, op2) != 0);

  *resultp = result;
  return ARITH_OK;
}


static arith
gfc_arith_gt (gfc_expr * op1, gfc_expr * op2, gfc_expr ** resultp)
{
  gfc_expr *result;

  result = gfc_constant_result (BT_LOGICAL, gfc_default_logical_kind (),
				&op1->where);
  result->value.logical = (gfc_compare_expr (op1, op2) > 0);
  *resultp = result;

  return ARITH_OK;
}


static arith
gfc_arith_ge (gfc_expr * op1, gfc_expr * op2, gfc_expr ** resultp)
{
  gfc_expr *result;

  result = gfc_constant_result (BT_LOGICAL, gfc_default_logical_kind (),
				&op1->where);
  result->value.logical = (gfc_compare_expr (op1, op2) >= 0);
  *resultp = result;

  return ARITH_OK;
}


static arith
gfc_arith_lt (gfc_expr * op1, gfc_expr * op2, gfc_expr ** resultp)
{
  gfc_expr *result;

  result = gfc_constant_result (BT_LOGICAL, gfc_default_logical_kind (),
				&op1->where);
  result->value.logical = (gfc_compare_expr (op1, op2) < 0);
  *resultp = result;

  return ARITH_OK;
}


static arith
gfc_arith_le (gfc_expr * op1, gfc_expr * op2, gfc_expr ** resultp)
{
  gfc_expr *result;

  result = gfc_constant_result (BT_LOGICAL, gfc_default_logical_kind (),
				&op1->where);
  result->value.logical = (gfc_compare_expr (op1, op2) <= 0);
  *resultp = result;

  return ARITH_OK;
}


static arith
reduce_unary (arith (*eval) (gfc_expr *, gfc_expr **), gfc_expr * op,
	      gfc_expr ** result)
{
  gfc_constructor *c, *head;
  gfc_expr *r;
  arith rc;

  if (op->expr_type == EXPR_CONSTANT)
    return eval (op, result);

  rc = ARITH_OK;
  head = gfc_copy_constructor (op->value.constructor);

  for (c = head; c; c = c->next)
    {
      rc = eval (c->expr, &r);
      if (rc != ARITH_OK)
	break;

      gfc_replace_expr (c->expr, r);
    }

  if (rc != ARITH_OK)
    gfc_free_constructor (head);
  else
    {
      r = gfc_get_expr ();
      r->expr_type = EXPR_ARRAY;
      r->value.constructor = head;
      r->shape = gfc_copy_shape (op->shape, op->rank);

      r->ts = head->expr->ts;
      r->where = op->where;
      r->rank = op->rank;

      *result = r;
    }

  return rc;
}


static arith
reduce_binary_ac (arith (*eval) (gfc_expr *, gfc_expr *, gfc_expr **),
		  gfc_expr * op1, gfc_expr * op2,
		  gfc_expr ** result)
{
  gfc_constructor *c, *head;
  gfc_expr *r;
  arith rc;

  head = gfc_copy_constructor (op1->value.constructor);
  rc = ARITH_OK;

  for (c = head; c; c = c->next)
    {
      rc = eval (c->expr, op2, &r);
      if (rc != ARITH_OK)
	break;

      gfc_replace_expr (c->expr, r);
    }

  if (rc != ARITH_OK)
    gfc_free_constructor (head);
  else
    {
      r = gfc_get_expr ();
      r->expr_type = EXPR_ARRAY;
      r->value.constructor = head;
      r->shape = gfc_copy_shape (op1->shape, op1->rank);

      r->ts = head->expr->ts;
      r->where = op1->where;
      r->rank = op1->rank;

      *result = r;
    }

  return rc;
}


static arith
reduce_binary_ca (arith (*eval) (gfc_expr *, gfc_expr *, gfc_expr **),
		  gfc_expr * op1, gfc_expr * op2,
		  gfc_expr ** result)
{
  gfc_constructor *c, *head;
  gfc_expr *r;
  arith rc;

  head = gfc_copy_constructor (op2->value.constructor);
  rc = ARITH_OK;

  for (c = head; c; c = c->next)
    {
      rc = eval (op1, c->expr, &r);
      if (rc != ARITH_OK)
	break;

      gfc_replace_expr (c->expr, r);
    }

  if (rc != ARITH_OK)
    gfc_free_constructor (head);
  else
    {
      r = gfc_get_expr ();
      r->expr_type = EXPR_ARRAY;
      r->value.constructor = head;
      r->shape = gfc_copy_shape (op2->shape, op2->rank);

      r->ts = head->expr->ts;
      r->where = op2->where;
      r->rank = op2->rank;

      *result = r;
    }

  return rc;
}


static arith
reduce_binary_aa (arith (*eval) (gfc_expr *, gfc_expr *, gfc_expr **),
		  gfc_expr * op1, gfc_expr * op2,
		  gfc_expr ** result)
{
  gfc_constructor *c, *d, *head;
  gfc_expr *r;
  arith rc;

  head = gfc_copy_constructor (op1->value.constructor);

  rc = ARITH_OK;
  d = op2->value.constructor;

  if (gfc_check_conformance ("Elemental binary operation", op1, op2)
      != SUCCESS)
    rc = ARITH_INCOMMENSURATE;
  else
    {

      for (c = head; c; c = c->next, d = d->next)
	{
	  if (d == NULL)
	    {
	      rc = ARITH_INCOMMENSURATE;
	      break;
	    }

	  rc = eval (c->expr, d->expr, &r);
	  if (rc != ARITH_OK)
	    break;

	  gfc_replace_expr (c->expr, r);
	}

      if (d != NULL)
	rc = ARITH_INCOMMENSURATE;
    }

  if (rc != ARITH_OK)
    gfc_free_constructor (head);
  else
    {
      r = gfc_get_expr ();
      r->expr_type = EXPR_ARRAY;
      r->value.constructor = head;
      r->shape = gfc_copy_shape (op1->shape, op1->rank);

      r->ts = head->expr->ts;
      r->where = op1->where;
      r->rank = op1->rank;

      *result = r;
    }

  return rc;
}


static arith
reduce_binary (arith (*eval) (gfc_expr *, gfc_expr *, gfc_expr **),
	       gfc_expr * op1, gfc_expr * op2,
	       gfc_expr ** result)
{

  if (op1->expr_type == EXPR_CONSTANT && op2->expr_type == EXPR_CONSTANT)
    return eval (op1, op2, result);

  if (op1->expr_type == EXPR_CONSTANT && op2->expr_type == EXPR_ARRAY)
    return reduce_binary_ca (eval, op1, op2, result);

  if (op1->expr_type == EXPR_ARRAY && op2->expr_type == EXPR_CONSTANT)
    return reduce_binary_ac (eval, op1, op2, result);

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
eval_intrinsic (gfc_intrinsic_op operator,
		eval_f eval, gfc_expr * op1, gfc_expr * op2)
{
  gfc_expr temp, *result;
  int unary;
  arith rc;

  gfc_clear_ts (&temp.ts);

  switch (operator)
    {
    case INTRINSIC_NOT:	/* Logical unary */
      if (op1->ts.type != BT_LOGICAL)
	goto runtime;

      temp.ts.type = BT_LOGICAL;
      temp.ts.kind = gfc_default_logical_kind ();

      unary = 1;
      break;

      /* Logical binary operators */
    case INTRINSIC_OR:
    case INTRINSIC_AND:
    case INTRINSIC_NEQV:
    case INTRINSIC_EQV:
      if (op1->ts.type != BT_LOGICAL || op2->ts.type != BT_LOGICAL)
	goto runtime;

      temp.ts.type = BT_LOGICAL;
      temp.ts.kind = gfc_default_logical_kind ();

      unary = 0;
      break;

    case INTRINSIC_UPLUS:
    case INTRINSIC_UMINUS:	/* Numeric unary */
      if (!gfc_numeric_ts (&op1->ts))
	goto runtime;

      temp.ts = op1->ts;

      unary = 1;
      break;

    case INTRINSIC_GE:
    case INTRINSIC_LT:		/* Additional restrictions  */
    case INTRINSIC_LE:          /* for ordering relations.  */
    case INTRINSIC_GT:
      if (op1->ts.type == BT_COMPLEX || op2->ts.type == BT_COMPLEX)
	{
	  temp.ts.type = BT_LOGICAL;
	  temp.ts.kind = gfc_default_logical_kind();
	  goto runtime;
	}

      /* else fall through */

    case INTRINSIC_EQ:
    case INTRINSIC_NE:
      if (op1->ts.type == BT_CHARACTER && op2->ts.type == BT_CHARACTER)
	{
	  unary = 0;
	  temp.ts.type = BT_LOGICAL;
	  temp.ts.kind = gfc_default_logical_kind();
	  break;
	}

      /* else fall through */

    case INTRINSIC_PLUS:
    case INTRINSIC_MINUS:
    case INTRINSIC_TIMES:
    case INTRINSIC_DIVIDE:
    case INTRINSIC_POWER:	/* Numeric binary */
      if (!gfc_numeric_ts (&op1->ts) || !gfc_numeric_ts (&op2->ts))
	goto runtime;

      /* Insert any necessary type conversions to make the operands compatible.  */

      temp.expr_type = EXPR_OP;
      gfc_clear_ts (&temp.ts);
      temp.operator = operator;

      temp.op1 = op1;
      temp.op2 = op2;

      gfc_type_convert_binary (&temp);

      if (operator == INTRINSIC_EQ || operator == INTRINSIC_NE
	  || operator == INTRINSIC_GE || operator == INTRINSIC_GT
	  || operator == INTRINSIC_LE || operator == INTRINSIC_LT)
	{
	  temp.ts.type = BT_LOGICAL;
	  temp.ts.kind = gfc_default_logical_kind ();
	}

      unary = 0;
      break;

    case INTRINSIC_CONCAT:	/* Character binary */
      if (op1->ts.type != BT_CHARACTER || op2->ts.type != BT_CHARACTER)
	goto runtime;

      temp.ts.type = BT_CHARACTER;
      temp.ts.kind = gfc_default_character_kind ();

      unary = 0;
      break;

    case INTRINSIC_USER:
      goto runtime;

    default:
      gfc_internal_error ("eval_intrinsic(): Bad operator");
    }

  /* Try to combine the operators.  */
  if (operator == INTRINSIC_POWER && op2->ts.type != BT_INTEGER)
    goto runtime;

  if (op1->expr_type != EXPR_CONSTANT
      && (op1->expr_type != EXPR_ARRAY
	  || !gfc_is_constant_expr (op1)
	  || !gfc_expanded_ac (op1)))
    goto runtime;

  if (op2 != NULL
      && op2->expr_type != EXPR_CONSTANT
      && (op2->expr_type != EXPR_ARRAY
	  || !gfc_is_constant_expr (op2)
	  || !gfc_expanded_ac (op2)))
    goto runtime;

  if (unary)
    rc = reduce_unary (eval.f2, op1, &result);
  else
    rc = reduce_binary (eval.f3, op1, op2, &result);

  if (rc != ARITH_OK)
    {				/* Something went wrong */
      gfc_error ("%s at %L", gfc_arith_error (rc), &op1->where);
      return NULL;
    }

  gfc_free_expr (op1);
  gfc_free_expr (op2);
  return result;

runtime:
  /* Create a run-time expression */
  result = gfc_get_expr ();
  result->ts = temp.ts;

  result->expr_type = EXPR_OP;
  result->operator = operator;

  result->op1 = op1;
  result->op2 = op2;

  result->where = op1->where;

  return result;
}


/* Modify type of expression for zero size array.  */
static gfc_expr *
eval_type_intrinsic0 (gfc_intrinsic_op operator, gfc_expr *op)
{
  if (op == NULL)
    gfc_internal_error("eval_type_intrinsic0(): op NULL");

  switch(operator)
    {
    case INTRINSIC_GE:
    case INTRINSIC_LT:
    case INTRINSIC_LE:
    case INTRINSIC_GT:
    case INTRINSIC_EQ:
    case INTRINSIC_NE:
      op->ts.type = BT_LOGICAL;
      op->ts.kind = gfc_default_logical_kind();
      break;

    default:
      break;
    }

  return op;
}


/* Return nonzero if the expression is a zero size array.  */

static int
gfc_zero_size_array (gfc_expr * e)
{

  if (e->expr_type != EXPR_ARRAY)
    return 0;

  return e->value.constructor == NULL;
}


/* Reduce a binary expression where at least one of the operands
   involves a zero-length array.  Returns NULL if neither of the
   operands is a zero-length array.  */

static gfc_expr *
reduce_binary0 (gfc_expr * op1, gfc_expr * op2)
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
eval_intrinsic_f2 (gfc_intrinsic_op operator,
		   arith (*eval) (gfc_expr *, gfc_expr **),
		   gfc_expr * op1, gfc_expr * op2)
{
  gfc_expr *result;
  eval_f f;

  if (op2 == NULL)
    {
      if (gfc_zero_size_array (op1))
	return eval_type_intrinsic0(operator, op1);
    }
  else
    {
      result = reduce_binary0 (op1, op2);
      if (result != NULL)
	return eval_type_intrinsic0(operator, result);
    }

  f.f2 = eval;
  return eval_intrinsic (operator, f, op1, op2);
}


static gfc_expr *
eval_intrinsic_f3 (gfc_intrinsic_op operator,
		   arith (*eval) (gfc_expr *, gfc_expr *, gfc_expr **),
		   gfc_expr * op1, gfc_expr * op2)
{
  gfc_expr *result;
  eval_f f;

  result = reduce_binary0 (op1, op2);
  if (result != NULL)
    return eval_type_intrinsic0(operator, result);

  f.f3 = eval;
  return eval_intrinsic (operator, f, op1, op2);
}



gfc_expr *
gfc_uplus (gfc_expr * op)
{
  return eval_intrinsic_f2 (INTRINSIC_UPLUS, gfc_arith_uplus, op, NULL);
}

gfc_expr *
gfc_uminus (gfc_expr * op)
{
  return eval_intrinsic_f2 (INTRINSIC_UMINUS, gfc_arith_uminus, op, NULL);
}

gfc_expr *
gfc_add (gfc_expr * op1, gfc_expr * op2)
{
  return eval_intrinsic_f3 (INTRINSIC_PLUS, gfc_arith_plus, op1, op2);
}

gfc_expr *
gfc_subtract (gfc_expr * op1, gfc_expr * op2)
{
  return eval_intrinsic_f3 (INTRINSIC_MINUS, gfc_arith_minus, op1, op2);
}

gfc_expr *
gfc_multiply (gfc_expr * op1, gfc_expr * op2)
{
  return eval_intrinsic_f3 (INTRINSIC_TIMES, gfc_arith_times, op1, op2);
}

gfc_expr *
gfc_divide (gfc_expr * op1, gfc_expr * op2)
{
  return eval_intrinsic_f3 (INTRINSIC_DIVIDE, gfc_arith_divide, op1, op2);
}

gfc_expr *
gfc_power (gfc_expr * op1, gfc_expr * op2)
{
  return eval_intrinsic_f3 (INTRINSIC_POWER, gfc_arith_power, op1, op2);
}

gfc_expr *
gfc_concat (gfc_expr * op1, gfc_expr * op2)
{
  return eval_intrinsic_f3 (INTRINSIC_CONCAT, gfc_arith_concat, op1, op2);
}

gfc_expr *
gfc_and (gfc_expr * op1, gfc_expr * op2)
{
  return eval_intrinsic_f3 (INTRINSIC_AND, gfc_arith_and, op1, op2);
}

gfc_expr *
gfc_or (gfc_expr * op1, gfc_expr * op2)
{
  return eval_intrinsic_f3 (INTRINSIC_OR, gfc_arith_or, op1, op2);
}

gfc_expr *
gfc_not (gfc_expr * op1)
{
  return eval_intrinsic_f2 (INTRINSIC_NOT, gfc_arith_not, op1, NULL);
}

gfc_expr *
gfc_eqv (gfc_expr * op1, gfc_expr * op2)
{
  return eval_intrinsic_f3 (INTRINSIC_EQV, gfc_arith_eqv, op1, op2);
}

gfc_expr *
gfc_neqv (gfc_expr * op1, gfc_expr * op2)
{
  return eval_intrinsic_f3 (INTRINSIC_NEQV, gfc_arith_neqv, op1, op2);
}

gfc_expr *
gfc_eq (gfc_expr * op1, gfc_expr * op2)
{
  return eval_intrinsic_f3 (INTRINSIC_EQ, gfc_arith_eq, op1, op2);
}

gfc_expr *
gfc_ne (gfc_expr * op1, gfc_expr * op2)
{
  return eval_intrinsic_f3 (INTRINSIC_NE, gfc_arith_ne, op1, op2);
}

gfc_expr *
gfc_gt (gfc_expr * op1, gfc_expr * op2)
{
  return eval_intrinsic_f3 (INTRINSIC_GT, gfc_arith_gt, op1, op2);
}

gfc_expr *
gfc_ge (gfc_expr * op1, gfc_expr * op2)
{
  return eval_intrinsic_f3 (INTRINSIC_GE, gfc_arith_ge, op1, op2);
}

gfc_expr *
gfc_lt (gfc_expr * op1, gfc_expr * op2)
{
  return eval_intrinsic_f3 (INTRINSIC_LT, gfc_arith_lt, op1, op2);
}

gfc_expr *
gfc_le (gfc_expr * op1, gfc_expr * op2)
{
  return eval_intrinsic_f3 (INTRINSIC_LE, gfc_arith_le, op1, op2);
}


/* Convert an integer string to an expression node.  */

gfc_expr *
gfc_convert_integer (const char *buffer, int kind, int radix, locus * where)
{
  gfc_expr *e;
  const char *t;

  e = gfc_constant_result (BT_INTEGER, kind, where);
  /* a leading plus is allowed, but not by mpz_set_str */
  if (buffer[0] == '+')
    t = buffer + 1;
  else
    t = buffer;
  mpz_set_str (e->value.integer, t, radix);

  return e;
}


/* Convert a real string to an expression node.  */

gfc_expr *
gfc_convert_real (const char *buffer, int kind, locus * where)
{
  gfc_expr *e;
  const char *t;

  e = gfc_constant_result (BT_REAL, kind, where);
  /* a leading plus is allowed, but not by mpf_set_str */
  if (buffer[0] == '+')
    t = buffer + 1;
  else
    t = buffer;
  mpf_set_str (e->value.real, t, 10);

  return e;
}


/* Convert a pair of real, constant expression nodes to a single
   complex expression node.  */

gfc_expr *
gfc_convert_complex (gfc_expr * real, gfc_expr * imag, int kind)
{
  gfc_expr *e;

  e = gfc_constant_result (BT_COMPLEX, kind, &real->where);
  mpf_set (e->value.complex.r, real->value.real);
  mpf_set (e->value.complex.i, imag->value.real);

  return e;
}


/******* Simplification of intrinsic functions with constant arguments *****/


/* Deal with an arithmetic error.  */

static void
arith_error (arith rc, gfc_typespec * from, gfc_typespec * to, locus * where)
{

  gfc_error ("%s converting %s to %s at %L", gfc_arith_error (rc),
	     gfc_typename (from), gfc_typename (to), where);

  /* TODO: Do something about the error, ie underflow rounds to 0,
     throw exception, return NaN, etc.  */
}

/* Convert integers to integers.  */

gfc_expr *
gfc_int2int (gfc_expr * src, int kind)
{
  gfc_expr *result;
  arith rc;

  result = gfc_constant_result (BT_INTEGER, kind, &src->where);

  mpz_set (result->value.integer, src->value.integer);

  if ((rc = gfc_check_integer_range (result->value.integer, kind))
      != ARITH_OK)
    {
      arith_error (rc, &src->ts, &result->ts, &src->where);
      gfc_free_expr (result);
      return NULL;
    }

  return result;
}


/* Convert integers to reals.  */

gfc_expr *
gfc_int2real (gfc_expr * src, int kind)
{
  gfc_expr *result;
  arith rc;

  result = gfc_constant_result (BT_REAL, kind, &src->where);

  mpf_set_z (result->value.real, src->value.integer);

  if ((rc = gfc_check_real_range (result->value.real, kind)) != ARITH_OK)
    {
      arith_error (rc, &src->ts, &result->ts, &src->where);
      gfc_free_expr (result);
      return NULL;
    }

  return result;
}


/* Convert default integer to default complex.  */

gfc_expr *
gfc_int2complex (gfc_expr * src, int kind)
{
  gfc_expr *result;
  arith rc;

  result = gfc_constant_result (BT_COMPLEX, kind, &src->where);

  mpf_set_z (result->value.complex.r, src->value.integer);
  mpf_set_ui (result->value.complex.i, 0);

  if ((rc = gfc_check_real_range (result->value.complex.i, kind)) != ARITH_OK)
    {
      arith_error (rc, &src->ts, &result->ts, &src->where);
      gfc_free_expr (result);
      return NULL;
    }

  return result;
}


/* Convert default real to default integer.  */

gfc_expr *
gfc_real2int (gfc_expr * src, int kind)
{
  gfc_expr *result;
  arith rc;

  result = gfc_constant_result (BT_INTEGER, kind, &src->where);

  mpz_set_f (result->value.integer, src->value.real);

  if ((rc = gfc_check_integer_range (result->value.integer, kind))
      != ARITH_OK)
    {
      arith_error (rc, &src->ts, &result->ts, &src->where);
      gfc_free_expr (result);
      return NULL;
    }

  return result;
}


/* Convert real to real.  */

gfc_expr *
gfc_real2real (gfc_expr * src, int kind)
{
  gfc_expr *result;
  arith rc;

  result = gfc_constant_result (BT_REAL, kind, &src->where);

  mpf_set (result->value.real, src->value.real);

  if ((rc = gfc_check_real_range (result->value.real, kind)) != ARITH_OK)
    {
      arith_error (rc, &src->ts, &result->ts, &src->where);
      gfc_free_expr (result);
      return NULL;
    }

  return result;
}


/* Convert real to complex.  */

gfc_expr *
gfc_real2complex (gfc_expr * src, int kind)
{
  gfc_expr *result;
  arith rc;

  result = gfc_constant_result (BT_COMPLEX, kind, &src->where);

  mpf_set (result->value.complex.r, src->value.real);
  mpf_set_ui (result->value.complex.i, 0);

  if ((rc = gfc_check_real_range (result->value.complex.r, kind)) != ARITH_OK)
    {
      arith_error (rc, &src->ts, &result->ts, &src->where);
      gfc_free_expr (result);
      return NULL;
    }

  return result;
}


/* Convert complex to integer.  */

gfc_expr *
gfc_complex2int (gfc_expr * src, int kind)
{
  gfc_expr *result;
  arith rc;

  result = gfc_constant_result (BT_INTEGER, kind, &src->where);

  mpz_set_f (result->value.integer, src->value.complex.r);

  if ((rc = gfc_check_integer_range (result->value.integer, kind))
      != ARITH_OK)
    {
      arith_error (rc, &src->ts, &result->ts, &src->where);
      gfc_free_expr (result);
      return NULL;
    }

  return result;
}


/* Convert complex to real.  */

gfc_expr *
gfc_complex2real (gfc_expr * src, int kind)
{
  gfc_expr *result;
  arith rc;

  result = gfc_constant_result (BT_REAL, kind, &src->where);

  mpf_set (result->value.real, src->value.complex.r);

  if ((rc = gfc_check_real_range (result->value.real, kind)) != ARITH_OK)
    {
      arith_error (rc, &src->ts, &result->ts, &src->where);
      gfc_free_expr (result);
      return NULL;
    }

  return result;
}


/* Convert complex to complex.  */

gfc_expr *
gfc_complex2complex (gfc_expr * src, int kind)
{
  gfc_expr *result;
  arith rc;

  result = gfc_constant_result (BT_COMPLEX, kind, &src->where);

  mpf_set (result->value.complex.r, src->value.complex.r);
  mpf_set (result->value.complex.i, src->value.complex.i);

  if ((rc = gfc_check_real_range (result->value.complex.r, kind)) != ARITH_OK
      || (rc =
	  gfc_check_real_range (result->value.complex.i, kind)) != ARITH_OK)
    {
      arith_error (rc, &src->ts, &result->ts, &src->where);
      gfc_free_expr (result);
      return NULL;
    }

  return result;
}


/* Logical kind conversion.  */

gfc_expr *
gfc_log2log (gfc_expr * src, int kind)
{
  gfc_expr *result;

  result = gfc_constant_result (BT_LOGICAL, kind, &src->where);
  result->value.logical = src->value.logical;

  return result;
}
