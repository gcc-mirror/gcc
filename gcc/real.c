/* real.c - software floating point emulation.
   Copyright (C) 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000, 2002, 2003 Free Software Foundation, Inc.
   Contributed by Stephen L. Moshier (moshier@world.std.com).
   Re-written by Richard Henderson  <rth@redhat.com>

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

#include "config.h"
#include "system.h"
#include "tree.h"
#include "toplev.h"
#include "real.h"
#include "tm_p.h"

/* The floating point model used internally is not exactly IEEE 754
   compliant, and close to the description in the ISO C standard,
   section 5.2.4.2.2 Characteristics of floating types.

   Specifically

	x = s * b^e * \sum_{k=1}^p f_k * b^{-k}

	where
		s = sign (+- 1)
		b = base or radix, here always 2
		e = exponent
		p = precision (the number of base-b digits in the significand)
		f_k = the digits of the significand.

   We differ from typical IEEE 754 encodings in that the entire
   significand is fractional.  Normalized significands are in the
   range [0.5, 1.0).

   A requirement of the model is that P be larger than than the 
   largest supported target floating-point type by at least 2 bits.
   This gives us proper rounding when we truncate to the target type.
   In addition, E must be large enough to hold the smallest supported
   denormal number in a normalized form.

   Both of these requirements are easily satisfied.  The largest target
   significand is 113 bits; we store at least 160.  The smallest
   denormal number fits in 17 exponent bits; we store 29.

   Note that the decimal string conversion routines are sensitive to 
   rounding error.  Since the raw arithmetic routines do not themselves
   have guard digits or rounding, the computation of 10**exp can
   accumulate more than a few digits of error.  The previous incarnation
   of real.c successfully used a 144 bit fraction; given the current
   layout of REAL_VALUE_TYPE we're forced to expand to at least 160 bits.

   Target floating point models that use base 16 instead of base 2
   (i.e. IBM 370), are handled during round_for_format, in which we
   canonicalize the exponent to be a multiple of 4 (log2(16)), and
   adjust the significand to match.  */


/* Used to classify two numbers simultaneously.  */
#define CLASS2(A, B)  ((A) << 2 | (B))

#if HOST_BITS_PER_LONG != 64 && HOST_BITS_PER_LONG != 32
 #error "Some constant folding done by hand to avoid shift count warnings"
#endif

static void get_zero PARAMS ((REAL_VALUE_TYPE *, int));
static void get_canonical_qnan PARAMS ((REAL_VALUE_TYPE *, int));
static void get_canonical_snan PARAMS ((REAL_VALUE_TYPE *, int));
static void get_inf PARAMS ((REAL_VALUE_TYPE *, int));
static bool sticky_rshift_significand PARAMS ((REAL_VALUE_TYPE *,
					       const REAL_VALUE_TYPE *,
					       unsigned int));
static void rshift_significand PARAMS ((REAL_VALUE_TYPE *,
					const REAL_VALUE_TYPE *,
					unsigned int));
static void lshift_significand PARAMS ((REAL_VALUE_TYPE *,
					const REAL_VALUE_TYPE *,
					unsigned int));
static void lshift_significand_1 PARAMS ((REAL_VALUE_TYPE *,
					  const REAL_VALUE_TYPE *));
static bool add_significands PARAMS ((REAL_VALUE_TYPE *r,
				      const REAL_VALUE_TYPE *,
				      const REAL_VALUE_TYPE *));
static bool sub_significands PARAMS ((REAL_VALUE_TYPE *,
				      const REAL_VALUE_TYPE *,
				      const REAL_VALUE_TYPE *, int));
static void neg_significand PARAMS ((REAL_VALUE_TYPE *,
				     const REAL_VALUE_TYPE *));
static int cmp_significands PARAMS ((const REAL_VALUE_TYPE *,
				     const REAL_VALUE_TYPE *));
static int cmp_significand_0 PARAMS ((const REAL_VALUE_TYPE *));
static void set_significand_bit PARAMS ((REAL_VALUE_TYPE *, unsigned int));
static void clear_significand_bit PARAMS ((REAL_VALUE_TYPE *, unsigned int));
static bool test_significand_bit PARAMS ((REAL_VALUE_TYPE *, unsigned int));
static void clear_significand_below PARAMS ((REAL_VALUE_TYPE *,
					     unsigned int));
static bool div_significands PARAMS ((REAL_VALUE_TYPE *,
				      const REAL_VALUE_TYPE *,
				      const REAL_VALUE_TYPE *));
static void normalize PARAMS ((REAL_VALUE_TYPE *));

static void do_add PARAMS ((REAL_VALUE_TYPE *, const REAL_VALUE_TYPE *,
			    const REAL_VALUE_TYPE *, int));
static void do_multiply PARAMS ((REAL_VALUE_TYPE *,
				 const REAL_VALUE_TYPE *,
				 const REAL_VALUE_TYPE *));
static void do_divide PARAMS ((REAL_VALUE_TYPE *, const REAL_VALUE_TYPE *,
			       const REAL_VALUE_TYPE *));
static int do_compare PARAMS ((const REAL_VALUE_TYPE *,
			       const REAL_VALUE_TYPE *, int));
static void do_fix_trunc PARAMS ((REAL_VALUE_TYPE *, const REAL_VALUE_TYPE *));

static unsigned long rtd_divmod PARAMS ((REAL_VALUE_TYPE *,
					 REAL_VALUE_TYPE *));

static const REAL_VALUE_TYPE * ten_to_ptwo PARAMS ((int));
static const REAL_VALUE_TYPE * ten_to_mptwo PARAMS ((int));
static const REAL_VALUE_TYPE * real_digit PARAMS ((int));
static void times_pten PARAMS ((REAL_VALUE_TYPE *, int));

static void round_for_format PARAMS ((const struct real_format *,
				      REAL_VALUE_TYPE *));

/* Initialize R with a positive zero.  */

static inline void
get_zero (r, sign)
     REAL_VALUE_TYPE *r;
     int sign;
{
  memset (r, 0, sizeof (*r));
  r->sign = sign;
}

/* Initialize R with the canonical quiet NaN.  */

static inline void
get_canonical_qnan (r, sign)
     REAL_VALUE_TYPE *r;
     int sign;
{
  memset (r, 0, sizeof (*r));
  r->class = rvc_nan;
  r->sign = sign;
  r->sig[SIGSZ-1] = SIG_MSB >> 1;
}

static inline void
get_canonical_snan (r, sign)
     REAL_VALUE_TYPE *r;
     int sign;
{
  memset (r, 0, sizeof (*r));
  r->class = rvc_nan;
  r->sign = sign;
  r->sig[SIGSZ-1] = SIG_MSB >> 2;
}

static inline void
get_inf (r, sign)
     REAL_VALUE_TYPE *r;
     int sign;
{
  memset (r, 0, sizeof (*r));
  r->class = rvc_inf;
  r->sign = sign;
}


/* Right-shift the significand of A by N bits; put the result in the
   significand of R.  If any one bits are shifted out, return true.  */

static bool
sticky_rshift_significand (r, a, n)
     REAL_VALUE_TYPE *r;
     const REAL_VALUE_TYPE *a;
     unsigned int n;
{
  unsigned long sticky = 0;
  unsigned int i, ofs = 0;

  if (n >= HOST_BITS_PER_LONG)
    {
      for (i = 0, ofs = n / HOST_BITS_PER_LONG; i < ofs; ++i)
	sticky |= a->sig[i];
      n &= HOST_BITS_PER_LONG - 1;
    }

  if (n != 0)
    {
      sticky |= a->sig[ofs] & (((unsigned long)1 << n) - 1);
      for (i = 0; i < SIGSZ; ++i)
	{
	  r->sig[i]
	    = (((ofs + i >= SIGSZ ? 0 : a->sig[ofs + i]) >> n)
	       | ((ofs + i + 1 >= SIGSZ ? 0 : a->sig[ofs + i + 1])
		  << (HOST_BITS_PER_LONG - n)));
	}
    }
  else
    {
      for (i = 0; ofs + i < SIGSZ; ++i)
	r->sig[i] = a->sig[ofs + i];
      for (; i < SIGSZ; ++i)
	r->sig[i] = 0;
    }

  return sticky != 0;
}

/* Right-shift the significand of A by N bits; put the result in the
   significand of R.  */

static void
rshift_significand (r, a, n)
     REAL_VALUE_TYPE *r;
     const REAL_VALUE_TYPE *a;
     unsigned int n;
{
  unsigned int i, ofs = n / HOST_BITS_PER_LONG;

  n &= HOST_BITS_PER_LONG - 1;
  if (n != 0)
    {
      for (i = 0; i < SIGSZ; ++i)
	{
	  r->sig[i]
	    = (((ofs + i >= SIGSZ ? 0 : a->sig[ofs + i]) >> n)
	       | ((ofs + i + 1 >= SIGSZ ? 0 : a->sig[ofs + i + 1])
		  << (HOST_BITS_PER_LONG - n)));
	}
    }
  else
    {
      for (i = 0; ofs + i < SIGSZ; ++i)
	r->sig[i] = a->sig[ofs + i];
      for (; i < SIGSZ; ++i)
	r->sig[i] = 0;
    }
}

/* Left-shift the significand of A by N bits; put the result in the
   significand of R.  */

static void
lshift_significand (r, a, n)
     REAL_VALUE_TYPE *r;
     const REAL_VALUE_TYPE *a;
     unsigned int n;
{
  unsigned int i, ofs = n / HOST_BITS_PER_LONG;

  n &= HOST_BITS_PER_LONG - 1;
  if (n == 0)
    {
      for (i = 0; ofs + i < SIGSZ; ++i)
	r->sig[SIGSZ-1-i] = a->sig[SIGSZ-1-i-ofs];
      for (; i < SIGSZ; ++i)
	r->sig[SIGSZ-1-i] = 0;
    }
  else
    for (i = 0; i < SIGSZ; ++i)
      {
	r->sig[SIGSZ-1-i]
	  = (((ofs + i >= SIGSZ ? 0 : a->sig[SIGSZ-1-i-ofs]) << n)
	     | ((ofs + i + 1 >= SIGSZ ? 0 : a->sig[SIGSZ-1-i-ofs-1])
		>> (HOST_BITS_PER_LONG - n)));
      }
}

/* Likewise, but N is specialized to 1.  */

static inline void
lshift_significand_1 (r, a)
     REAL_VALUE_TYPE *r;
     const REAL_VALUE_TYPE *a;
{
  unsigned int i;

  for (i = SIGSZ - 1; i > 0; --i)
    r->sig[i] = (a->sig[i] << 1) | (a->sig[i-1] >> (HOST_BITS_PER_LONG - 1));
  r->sig[0] = a->sig[0] << 1;
}

/* Add the significands of A and B, placing the result in R.  Return
   true if there was carry out of the most significant word.  */

static inline bool
add_significands (r, a, b)
     REAL_VALUE_TYPE *r;
     const REAL_VALUE_TYPE *a, *b;
{
  bool carry = false;
  int i;

  for (i = 0; i < SIGSZ; ++i)
    {
      unsigned long ai = a->sig[i];
      unsigned long ri = ai + b->sig[i];

      if (carry)
	{
	  carry = ri < ai;
	  carry |= ++ri == 0;
	}
      else
	carry = ri < ai;

      r->sig[i] = ri;
    }

  return carry;
}

/* Subtract the significands of A and B, placing the result in R.  CARRY is
   true if there's a borrow incoming to the least significant word.
   Return true if there was borrow out of the most significant word.  */

static inline bool
sub_significands (r, a, b, carry)
     REAL_VALUE_TYPE *r;
     const REAL_VALUE_TYPE *a, *b;
     int carry;
{
  int i;

  for (i = 0; i < SIGSZ; ++i)
    {
      unsigned long ai = a->sig[i];
      unsigned long ri = ai - b->sig[i];

      if (carry)
	{
	  carry = ri > ai;
	  carry |= ~--ri == 0;
	}
      else
	carry = ri > ai;

      r->sig[i] = ri;
    }

  return carry;
}  

/* Negate the significand A, placing the result in R.  */

static inline void
neg_significand (r, a)
     REAL_VALUE_TYPE *r;
     const REAL_VALUE_TYPE *a;
{
  bool carry = true;
  int i;

  for (i = 0; i < SIGSZ; ++i)
    {
      unsigned long ri, ai = a->sig[i];

      if (carry)
	{
	  if (ai)
	    {
	      ri = -ai;
	      carry = false;
	    }
	  else
	    ri = ai;
	}
      else
	ri = ~ai;

      r->sig[i] = ri;
    }
}  

/* Compare significands.  Return tri-state vs zero.  */

static inline int 
cmp_significands (a, b)
     const REAL_VALUE_TYPE *a, *b;
{
  int i;

  for (i = SIGSZ - 1; i >= 0; --i)
    {
      unsigned long ai = a->sig[i];
      unsigned long bi = b->sig[i];

      if (ai > bi)
	return 1;
      if (ai < bi)
	return -1;
    }

  return 0;
}

/* Return true if A is nonzero.  */

static inline int 
cmp_significand_0 (a)
     const REAL_VALUE_TYPE *a;
{
  int i;

  for (i = SIGSZ - 1; i >= 0; --i)
    if (a->sig[i])
      return 1;

  return 0;
}

/* Set bit N of the significand of R.  */

static inline void
set_significand_bit (r, n)
     REAL_VALUE_TYPE *r;
     unsigned int n;
{
  r->sig[n / HOST_BITS_PER_LONG]
    |= (unsigned long)1 << (n % HOST_BITS_PER_LONG);
}

/* Clear bit N of the significand of R.  */

static inline void
clear_significand_bit (r, n)
     REAL_VALUE_TYPE *r;
     unsigned int n;
{
  r->sig[n / HOST_BITS_PER_LONG]
    &= ~((unsigned long)1 << (n % HOST_BITS_PER_LONG));
}

/* Test bit N of the significand of R.  */

static inline bool
test_significand_bit (r, n)
     REAL_VALUE_TYPE *r;
     unsigned int n;
{
  /* ??? Compiler bug here if we return this expression directly.
     The conversion to bool strips the "&1" and we wind up testing
     e.g. 2 != 0 -> true.  Seen in gcc version 3.2 20020520.  */
  int t = (r->sig[n / HOST_BITS_PER_LONG] >> (n % HOST_BITS_PER_LONG)) & 1;
  return t;
}

/* Clear bits 0..N-1 of the significand of R.  */

static void
clear_significand_below (r, n)
     REAL_VALUE_TYPE *r;
     unsigned int n;
{
  int i, w = n / HOST_BITS_PER_LONG;

  for (i = 0; i < w; ++i)
    r->sig[i] = 0;

  r->sig[w] &= ~(((unsigned long)1 << (n % HOST_BITS_PER_LONG)) - 1);
}

/* Divide the significands of A and B, placing the result in R.  Return
   true if the division was inexact.  */

static inline bool
div_significands (r, a, b)
     REAL_VALUE_TYPE *r;
     const REAL_VALUE_TYPE *a, *b;
{
  REAL_VALUE_TYPE u;
  int i, bit = SIGNIFICAND_BITS - 1;
  unsigned long msb, inexact;

  u = *a;
  memset (r->sig, 0, sizeof (r->sig));

  msb = 0;
  goto start;
  do
    {
      msb = u.sig[SIGSZ-1] & SIG_MSB;
      lshift_significand_1 (&u, &u);
    start:
      if (msb || cmp_significands (&u, b) >= 0)
	{
	  sub_significands (&u, &u, b, 0);
	  set_significand_bit (r, bit);
	}
    }
  while (--bit >= 0);

  for (i = 0, inexact = 0; i < SIGSZ; i++)
    inexact |= u.sig[i];

  return inexact != 0;
}

/* Adjust the exponent and significand of R such that the most
   significant bit is set.  We underflow to zero and overflow to
   infinity here, without denormals.  (The intermediate representation
   exponent is large enough to handle target denormals normalized.)  */

static void
normalize (r)
     REAL_VALUE_TYPE *r;
{
  int shift = 0, exp;
  int i, j;

  /* Find the first word that is nonzero.  */
  for (i = SIGSZ - 1; i >= 0; i--)
    if (r->sig[i] == 0)
      shift += HOST_BITS_PER_LONG;
    else
      break;

  /* Zero significand flushes to zero.  */
  if (i < 0)
    {
      r->class = rvc_zero;
      r->exp = 0;
      return;
    }

  /* Find the first bit that is nonzero.  */
  for (j = 0; ; j++)
    if (r->sig[i] & ((unsigned long)1 << (HOST_BITS_PER_LONG - 1 - j)))
      break;
  shift += j;

  if (shift > 0)
    {
      exp = r->exp - shift;
      if (exp > MAX_EXP)
	get_inf (r, r->sign);
      else if (exp < -MAX_EXP)
	get_zero (r, r->sign);
      else
	{
	  r->exp = exp;
	  lshift_significand (r, r, shift);
	}
    }
}

/* Return R = A + (SUBTRACT_P ? -B : B).  */

static void
do_add (r, a, b, subtract_p)
     REAL_VALUE_TYPE *r;
     const REAL_VALUE_TYPE *a, *b;
     int subtract_p;
{
  int dexp, sign, exp;
  REAL_VALUE_TYPE t;
  bool inexact = false;

  /* Determine if we need to add or subtract.  */
  sign = a->sign;
  subtract_p = (sign ^ b->sign) ^ subtract_p;

  switch (CLASS2 (a->class, b->class))
    {
    case CLASS2 (rvc_zero, rvc_zero):
      /* -0 + -0 = -0, -0 - +0 = -0; all other cases yield +0.  */
      get_zero (r, sign & !subtract_p);
      return;

    case CLASS2 (rvc_zero, rvc_normal):
    case CLASS2 (rvc_zero, rvc_inf):
    case CLASS2 (rvc_zero, rvc_nan):
      /* 0 + ANY = ANY.  */
    case CLASS2 (rvc_normal, rvc_nan):
    case CLASS2 (rvc_inf, rvc_nan):
    case CLASS2 (rvc_nan, rvc_nan):
      /* ANY + NaN = NaN.  */
    case CLASS2 (rvc_normal, rvc_inf):
      /* R + Inf = Inf.  */
      *r = *b;
      r->sign = sign ^ subtract_p;
      return;

    case CLASS2 (rvc_normal, rvc_zero):
    case CLASS2 (rvc_inf, rvc_zero):
    case CLASS2 (rvc_nan, rvc_zero):
      /* ANY + 0 = ANY.  */
    case CLASS2 (rvc_nan, rvc_normal):
    case CLASS2 (rvc_nan, rvc_inf):
      /* NaN + ANY = NaN.  */
    case CLASS2 (rvc_inf, rvc_normal):
      /* Inf + R = Inf.  */
      *r = *a;
      return;

    case CLASS2 (rvc_inf, rvc_inf):
      if (subtract_p)
	/* Inf - Inf = NaN.  */
	get_canonical_qnan (r, 0);
      else
	/* Inf + Inf = Inf.  */
	*r = *a;
      return;

    case CLASS2 (rvc_normal, rvc_normal):
      break;

    default:
      abort ();
    }

  /* Swap the arguments such that A has the larger exponent.  */
  dexp = a->exp - b->exp;
  if (dexp < 0)
    {
      const REAL_VALUE_TYPE *t;
      t = a, a = b, b = t;
      dexp = -dexp;
      sign ^= subtract_p;
    }
  exp = a->exp;

  /* If the exponents are not identical, we need to shift the
     significand of B down.  */
  if (dexp > 0)
    {
      /* If the exponents are too far apart, the significands
	 do not overlap, which makes the subtraction a noop.  */
      if (dexp >= SIGNIFICAND_BITS)
	{
	  *r = *a;
	  r->sign = sign;
	  return;
	}

      inexact |= sticky_rshift_significand (&t, b, dexp);
      b = &t;
    }

  if (subtract_p)
    {
      if (sub_significands (r, a, b, inexact))
	{
	  /* We got a borrow out of the subtraction.  That means that
	     A and B had the same exponent, and B had the larger
	     significand.  We need to swap the sign and negate the
	     significand.  */
	  sign ^= 1;
	  neg_significand (r, r);
	}
    }
  else
    {
      if (add_significands (r, a, b))
	{
	  /* We got carry out of the addition.  This means we need to
	     shift the significand back down one bit and increase the
	     exponent.  */
	  inexact |= sticky_rshift_significand (r, r, 1);
	  r->sig[SIGSZ-1] |= SIG_MSB;
	  if (++exp > MAX_EXP)
	    {
	      get_inf (r, sign);
	      return;
	    }
	}
    }

  r->class = rvc_normal;
  r->sign = sign;
  r->exp = exp;

  /* Re-normalize the result.  */
  normalize (r);

  /* Special case: if the subtraction results in zero, the result
     is positive.  */
  if (r->class == rvc_zero)
    r->sign = 0;
  else
    r->sig[0] |= inexact;
}

/* Return R = A * B.  */

static void
do_multiply (r, a, b)
     REAL_VALUE_TYPE *r;
     const REAL_VALUE_TYPE *a, *b;
{
  REAL_VALUE_TYPE u, t, *rr;
  unsigned int i, j, k;
  int sign = a->sign ^ b->sign;

  switch (CLASS2 (a->class, b->class))
    {
    case CLASS2 (rvc_zero, rvc_zero):
    case CLASS2 (rvc_zero, rvc_normal):
    case CLASS2 (rvc_normal, rvc_zero):
      /* +-0 * ANY = 0 with appropriate sign.  */
      get_zero (r, sign);
      return;

    case CLASS2 (rvc_zero, rvc_nan):
    case CLASS2 (rvc_normal, rvc_nan):
    case CLASS2 (rvc_inf, rvc_nan):
    case CLASS2 (rvc_nan, rvc_nan):
      /* ANY * NaN = NaN.  */
      *r = *b;
      r->sign = sign;
      return;

    case CLASS2 (rvc_nan, rvc_zero):
    case CLASS2 (rvc_nan, rvc_normal):
    case CLASS2 (rvc_nan, rvc_inf):
      /* NaN * ANY = NaN.  */
      *r = *a;
      r->sign = sign;
      return;

    case CLASS2 (rvc_zero, rvc_inf):
    case CLASS2 (rvc_inf, rvc_zero):
      /* 0 * Inf = NaN */
      get_canonical_qnan (r, sign);
      return;

    case CLASS2 (rvc_inf, rvc_inf):
    case CLASS2 (rvc_normal, rvc_inf):
    case CLASS2 (rvc_inf, rvc_normal):
      /* Inf * Inf = Inf, R * Inf = Inf */
    overflow:
      get_inf (r, sign);
      return;

    case CLASS2 (rvc_normal, rvc_normal):
      break;

    default:
      abort ();
    }

  if (r == a || r == b)
    rr = &t;
  else
    rr = r;
  get_zero (rr, 0);

  /* Collect all the partial products.  Since we don't have sure access
     to a widening multiply, we split each long into two half-words.

     Consider the long-hand form of a four half-word multiplication:

		 A  B  C  D
	      *  E  F  G  H
	     --------------
	        DE DF DG DH
	     CE CF CG CH
	  BE BF BG BH
       AE AF AG AH

     We construct partial products of the widened half-word products
     that are known to not overlap, e.g. DF+DH.  Each such partial
     product is given its proper exponent, which allows us to sum them
     and obtain the finished product.  */

  for (i = 0; i < SIGSZ * 2; ++i)
    {
      unsigned long ai = a->sig[i / 2];
      if (i & 1)
	ai >>= HOST_BITS_PER_LONG / 2;
      else
	ai &= ((unsigned long)1 << (HOST_BITS_PER_LONG / 2)) - 1;

      if (ai == 0)
	continue;

      for (j = 0; j < 2; ++j)
	{
	  int exp = (a->exp - (2*SIGSZ-1-i)*(HOST_BITS_PER_LONG/2)
		     + (b->exp - (1-j)*(HOST_BITS_PER_LONG/2)));

	  if (exp > MAX_EXP)
	    goto overflow;
	  if (exp < -MAX_EXP)
	    /* Would underflow to zero, which we shouldn't bother adding.  */
	    continue;

	  u.class = rvc_normal;
	  u.sign = 0;
	  u.exp = exp;

	  for (k = j; k < SIGSZ * 2; k += 2)
	    {
	      unsigned long bi = b->sig[k / 2];
	      if (k & 1)
		bi >>= HOST_BITS_PER_LONG / 2;
	      else
		bi &= ((unsigned long)1 << (HOST_BITS_PER_LONG / 2)) - 1;

	      u.sig[k / 2] = ai * bi;
	    }

	  normalize (&u);
	  do_add (rr, rr, &u, 0);
	}
    }

  rr->sign = sign;
  if (rr != r)
    *r = t;
}

/* Return R = A / B.  */

static void
do_divide (r, a, b)
     REAL_VALUE_TYPE *r;
     const REAL_VALUE_TYPE *a, *b;
{
  int exp, sign = a->sign ^ b->sign;
  REAL_VALUE_TYPE t, *rr;
  bool inexact;

  switch (CLASS2 (a->class, b->class))
    {
    case CLASS2 (rvc_zero, rvc_zero):
      /* 0 / 0 = NaN.  */
    case CLASS2 (rvc_inf, rvc_inf):
      /* Inf / Inf = NaN.  */
      get_canonical_qnan (r, sign);
      return;

    case CLASS2 (rvc_zero, rvc_normal):
    case CLASS2 (rvc_zero, rvc_inf):
      /* 0 / ANY = 0.  */
    case CLASS2 (rvc_normal, rvc_inf):
      /* R / Inf = 0.  */
    underflow:
      get_zero (r, sign);
      return;

    case CLASS2 (rvc_normal, rvc_zero):
      /* R / 0 = Inf.  */
    case CLASS2 (rvc_inf, rvc_zero):
      /* Inf / 0 = Inf.  */
      get_inf (r, sign);
      return;

    case CLASS2 (rvc_zero, rvc_nan):
    case CLASS2 (rvc_normal, rvc_nan):
    case CLASS2 (rvc_inf, rvc_nan):
    case CLASS2 (rvc_nan, rvc_nan):
      /* ANY / NaN = NaN.  */
      *r = *b;
      r->sign = sign;
      return;

    case CLASS2 (rvc_nan, rvc_zero):
    case CLASS2 (rvc_nan, rvc_normal):
    case CLASS2 (rvc_nan, rvc_inf):
      /* NaN / ANY = NaN.  */
      *r = *a;
      r->sign = sign;
      return;

    case CLASS2 (rvc_inf, rvc_normal):
      /* Inf / R = Inf.  */
    overflow:
      get_inf (r, sign);
      return;

    case CLASS2 (rvc_normal, rvc_normal):
      break;

    default:
      abort ();
    }

  if (r == a || r == b)
    rr = &t;
  else
    rr = r;

  rr->class = rvc_normal;
  rr->sign = sign;

  exp = a->exp - b->exp + 1;
  if (exp > MAX_EXP)
    goto overflow;
  if (exp < -MAX_EXP)
    goto underflow;
  rr->exp = exp;

  inexact = div_significands (rr, a, b);

  /* Re-normalize the result.  */
  normalize (rr);
  rr->sig[0] |= inexact;

  if (rr != r)
    *r = t;
}

/* Return a tri-state comparison of A vs B.  Return NAN_RESULT if
   one of the two operands is a NaN.  */

static int
do_compare (a, b, nan_result)
     const REAL_VALUE_TYPE *a, *b;
     int nan_result;
{
  int ret;

  switch (CLASS2 (a->class, b->class))
    {
    case CLASS2 (rvc_zero, rvc_zero):
      /* Sign of zero doesn't matter for compares.  */
      return 0;

    case CLASS2 (rvc_inf, rvc_zero):
    case CLASS2 (rvc_inf, rvc_normal):
    case CLASS2 (rvc_normal, rvc_zero):
      return (a->sign ? -1 : 1);

    case CLASS2 (rvc_inf, rvc_inf):
      return -a->sign - -b->sign;

    case CLASS2 (rvc_zero, rvc_normal):
    case CLASS2 (rvc_zero, rvc_inf):
    case CLASS2 (rvc_normal, rvc_inf):
      return (b->sign ? 1 : -1);

    case CLASS2 (rvc_zero, rvc_nan):
    case CLASS2 (rvc_normal, rvc_nan):
    case CLASS2 (rvc_inf, rvc_nan):
    case CLASS2 (rvc_nan, rvc_nan):
    case CLASS2 (rvc_nan, rvc_zero):
    case CLASS2 (rvc_nan, rvc_normal):
    case CLASS2 (rvc_nan, rvc_inf):
      return nan_result;

    case CLASS2 (rvc_normal, rvc_normal):
      break;

    default:
      abort ();
    }

  if (a->sign != b->sign)
    return -a->sign - -b->sign;

  if (a->exp > b->exp)
    ret = 1;
  else if (a->exp < b->exp)
    ret = -1;
  else
    ret = cmp_significands (a, b);

  return (a->sign ? -ret : ret);
}

/* Return A truncated to an integral value toward zero.  */

static void
do_fix_trunc (r, a)
     REAL_VALUE_TYPE *r;
     const REAL_VALUE_TYPE *a;
{
  *r = *a;

  switch (r->class)
    {
    case rvc_zero:
    case rvc_inf:
    case rvc_nan:
      break;

    case rvc_normal:
      if (r->exp <= 0)
	get_zero (r, r->sign);
      else if (r->exp < SIGNIFICAND_BITS)
	clear_significand_below (r, SIGNIFICAND_BITS - r->exp);
      break;

    default:
      abort ();
    }
}

/* Perform the binary or unary operation described by CODE.
   For a unary operation, leave OP1 NULL.  */

void
real_arithmetic (r, icode, op0, op1)
     REAL_VALUE_TYPE *r;
     int icode;
     const REAL_VALUE_TYPE *op0, *op1;
{
  enum tree_code code = icode;

  switch (code)
    {
    case PLUS_EXPR:
      do_add (r, op0, op1, 0);
      break;

    case MINUS_EXPR:
      do_add (r, op0, op1, 1);
      break;

    case MULT_EXPR:
      do_multiply (r, op0, op1);
      break;

    case RDIV_EXPR:
      do_divide (r, op0, op1);
      break;

    case MIN_EXPR:
      if (op1->class == rvc_nan)
	*r = *op1;
      else if (do_compare (op0, op1, -1) < 0)
	*r = *op0;
      else
	*r = *op1;
      break;

    case MAX_EXPR:
      if (op1->class == rvc_nan)
	*r = *op1;
      else if (do_compare (op0, op1, 1) < 0)
	*r = *op1;
      else
	*r = *op0;
      break;

    case NEGATE_EXPR:
      *r = *op0;
      r->sign ^= 1;
      break;

    case ABS_EXPR:
      *r = *op0;
      r->sign = 0;
      break;

    case FIX_TRUNC_EXPR:
      do_fix_trunc (r, op0);
      break;

    default:
      abort ();
    }
}

/* Legacy.  Similar, but return the result directly.  */

REAL_VALUE_TYPE
real_arithmetic2 (icode, op0, op1)
     int icode;
     const REAL_VALUE_TYPE *op0, *op1;
{
  REAL_VALUE_TYPE r;
  real_arithmetic (&r, icode, op0, op1);
  return r;
}

bool
real_compare (icode, op0, op1)
     int icode;
     const REAL_VALUE_TYPE *op0, *op1;
{
  enum tree_code code = icode;

  switch (code)
    {
    case LT_EXPR:
      return do_compare (op0, op1, 1) < 0;
    case LE_EXPR:
      return do_compare (op0, op1, 1) <= 0;
    case GT_EXPR:
      return do_compare (op0, op1, -1) > 0;
    case GE_EXPR:
      return do_compare (op0, op1, -1) >= 0;
    case EQ_EXPR:
      return do_compare (op0, op1, -1) == 0;
    case NE_EXPR:
      return do_compare (op0, op1, -1) != 0;
    case UNORDERED_EXPR:
      return op0->class == rvc_nan || op1->class == rvc_nan;
    case ORDERED_EXPR:
      return op0->class != rvc_nan && op1->class != rvc_nan;
    case UNLT_EXPR:
      return do_compare (op0, op1, -1) < 0;
    case UNLE_EXPR:
      return do_compare (op0, op1, -1) <= 0;
    case UNGT_EXPR:
      return do_compare (op0, op1, 1) > 0;
    case UNGE_EXPR:
      return do_compare (op0, op1, 1) >= 0;
    case UNEQ_EXPR:
      return do_compare (op0, op1, 0) == 0;

    default:
      abort ();
    }
}

/* Return floor log2(R).  */

int
real_exponent (r)
     const REAL_VALUE_TYPE *r;
{
  switch (r->class)
    {
    case rvc_zero:
      return 0;
    case rvc_inf:
    case rvc_nan:
      return (unsigned int)-1 >> 1;
    case rvc_normal:
      return r->exp;
    default:
      abort ();
    }
}

/* R = OP0 * 2**EXP.  */

void
real_ldexp (r, op0, exp)
     REAL_VALUE_TYPE *r;
     const REAL_VALUE_TYPE *op0;
     int exp;
{
  *r = *op0;
  switch (r->class)
    {
    case rvc_zero:
    case rvc_inf:
    case rvc_nan:
      break;

    case rvc_normal:
      exp += op0->exp;
      if (exp > MAX_EXP)
	get_inf (r, r->sign);
      else if (exp < -MAX_EXP)
	get_zero (r, r->sign);
      else
	r->exp = exp;
      break;

    default:
      abort ();
    }
}

/* Determine whether a floating-point value X is infinite.  */

bool
real_isinf (r)
     const REAL_VALUE_TYPE *r;
{
  return (r->class == rvc_inf);
}

/* Determine whether a floating-point value X is a NaN.  */

bool
real_isnan (r)
     const REAL_VALUE_TYPE *r;
{
  return (r->class == rvc_nan);
}

/* Determine whether a floating-point value X is negative.  */

bool
real_isneg (r)
     const REAL_VALUE_TYPE *r;
{
  return r->sign;
}

/* Determine whether a floating-point value X is minus zero.  */

bool
real_isnegzero (r)
     const REAL_VALUE_TYPE *r;
{
  return r->sign && r->class == rvc_zero;
}

/* Compare two floating-point objects for bitwise identity.  */

extern bool
real_identical (a, b)
     const REAL_VALUE_TYPE *a, *b;
{
  int i;

  if (a->class != b->class)
    return false;
  if (a->sign != b->sign)
    return false;

  switch (a->class)
    {
    case rvc_zero:
    case rvc_inf:
      break;

    case rvc_normal:
      if (a->exp != b->exp)
 	return false;
      /* FALLTHRU */
    case rvc_nan:
      for (i = 0; i < SIGSZ; ++i)
	if (a->sig[i] != b->sig[i])
	  return false;
      break;

    default:
      abort ();
    }

  return true;
}

/* Try to change R into its exact multiplicative inverse in machine
   mode MODE.  Return true if successful.  */

bool
exact_real_inverse (mode, r)
     enum machine_mode mode;
     REAL_VALUE_TYPE *r;
{
  const REAL_VALUE_TYPE *one = real_digit (1);
  REAL_VALUE_TYPE u;
  int i;
  
  if (r->class != rvc_normal)
    return false;

  /* Check for a power of two: all significand bits zero except the MSB.  */
  for (i = 0; i < SIGSZ-1; ++i)
    if (r->sig[i] != 0)
      return false;
  if (r->sig[SIGSZ-1] != SIG_MSB)
    return false;

  /* Find the inverse and truncate to the required mode.  */
  do_divide (&u, one, r);
  real_convert (&u, mode, &u);
  
  /* The rounding may have overflowed.  */
  if (u.class != rvc_normal)
    return false;
  for (i = 0; i < SIGSZ-1; ++i)
    if (u.sig[i] != 0)
      return false;
  if (u.sig[SIGSZ-1] != SIG_MSB)
    return false;

  *r = u;
  return true;
}

/* Render R as an integer.  */

HOST_WIDE_INT
real_to_integer (r)
     const REAL_VALUE_TYPE *r;
{
  unsigned HOST_WIDE_INT i;

  switch (r->class)
    {
    case rvc_zero:
    underflow:
      return 0;

    case rvc_inf:
    case rvc_nan:
    overflow:
      i = (unsigned HOST_WIDE_INT) 1 << (HOST_BITS_PER_WIDE_INT - 1);
      if (!r->sign)
	i--;
      return i;

    case rvc_normal:
      if (r->exp <= 0)
	goto underflow;
      if (r->exp > HOST_BITS_PER_WIDE_INT)
	goto overflow;

      if (HOST_BITS_PER_WIDE_INT == HOST_BITS_PER_LONG)
	i = r->sig[SIGSZ-1];
      else if (HOST_BITS_PER_WIDE_INT == 2*HOST_BITS_PER_LONG)
	{
	  i = r->sig[SIGSZ-1];
	  i = i << (HOST_BITS_PER_LONG - 1) << 1;
	  i |= r->sig[SIGSZ-2];
	}
      else
	abort ();

      i >>= HOST_BITS_PER_WIDE_INT - r->exp;

      if (r->sign)
	i = -i;
      return i;

    default:
      abort ();
    }
}

/* Likewise, but to an integer pair, HI+LOW.  */

void
real_to_integer2 (plow, phigh, r)
     HOST_WIDE_INT *plow, *phigh;
     const REAL_VALUE_TYPE *r;
{
  REAL_VALUE_TYPE t;
  HOST_WIDE_INT low, high;
  int exp;

  switch (r->class)
    {
    case rvc_zero:
    underflow:
      low = high = 0;
      break;

    case rvc_inf:
    case rvc_nan:
    overflow:
      high = (unsigned HOST_WIDE_INT) 1 << (HOST_BITS_PER_WIDE_INT - 1);
      if (r->sign)
	low = 0;
      else
	{
	  high--;
	  low = -1;
	}
      break;

    case rvc_normal:
      exp = r->exp;
      if (exp <= 0)
	goto underflow;
      if (exp > 2*HOST_BITS_PER_WIDE_INT)
	goto overflow;

      rshift_significand (&t, r, 2*HOST_BITS_PER_WIDE_INT - exp);
      if (HOST_BITS_PER_WIDE_INT == HOST_BITS_PER_LONG)
	{
	  high = t.sig[SIGSZ-1];
	  low = t.sig[SIGSZ-2];
	}
      else if (HOST_BITS_PER_WIDE_INT == 2*HOST_BITS_PER_LONG)
	{
	  high = t.sig[SIGSZ-1];
	  high = high << (HOST_BITS_PER_LONG - 1) << 1;
	  high |= t.sig[SIGSZ-2];

	  low = t.sig[SIGSZ-3];
	  low = low << (HOST_BITS_PER_LONG - 1) << 1;
	  low |= t.sig[SIGSZ-4];
	}
      else
	abort ();

      if (r->sign)
	{
	  if (low == 0)
	    high = -high;
	  else
	    low = -low, high = ~high;
	}
      break;

    default:
      abort ();
    }

  *plow = low;
  *phigh = high;
}

/* A subroutine of real_to_decimal.  Compute the quotient and remainder
   of NUM / DEN.  Return the quotient and place the remainder in NUM.
   It is expected that NUM / DEN are close enough that the quotient is
   small.  */

static unsigned long
rtd_divmod (num, den)
     REAL_VALUE_TYPE *num, *den;
{
  unsigned long q, msb;
  int expn = num->exp, expd = den->exp;

  if (expn < expd)
    return 0;

  q = msb = 0;
  goto start;
  do
    {
      msb = num->sig[SIGSZ-1] & SIG_MSB;
      q <<= 1;
      lshift_significand_1 (num, num);
    start:
      if (msb || cmp_significands (num, den) >= 0)
	{
	  sub_significands (num, num, den, 0);
	  q |= 1;
	}
    }
  while (--expn >= expd);

  num->exp = expd;
  normalize (num);

  return q;
}

/* Render R as a decimal floating point constant.  Emit DIGITS significant
   digits in the result, bounded by BUF_SIZE.  If DIGITS is 0, choose the
   maximum for the representation.  If CROP_TRAILING_ZEROS, strip trailing
   zeros.  */

#define M_LOG10_2	0.30102999566398119521

void
real_to_decimal (str, r_orig, buf_size, digits, crop_trailing_zeros)
     char *str;
     const REAL_VALUE_TYPE *r_orig;
     size_t buf_size, digits;
     int crop_trailing_zeros;
{
  const REAL_VALUE_TYPE *one, *ten;
  REAL_VALUE_TYPE r, pten, u, v;
  int dec_exp, cmp_one, digit;
  size_t max_digits;
  char *p, *first, *last;
  bool sign;

  r = *r_orig;
  switch (r.class)
    {
    case rvc_zero:
      strcpy (str, (r.sign ? "-0.0" : "0.0"));
      return;
    case rvc_normal:
      break;
    case rvc_inf:
      strcpy (str, (r.sign ? "-Inf" : "+Inf"));
      return;
    case rvc_nan:
      /* ??? Print the significand as well, if not canonical?  */
      strcpy (str, (r.sign ? "-NaN" : "+NaN"));
      return;
    default:
      abort ();
    }

  /* Bound the number of digits printed by the size of the representation.  */
  max_digits = SIGNIFICAND_BITS * M_LOG10_2;
  if (digits == 0 || digits > max_digits)
    digits = max_digits;

  /* Estimate the decimal exponent, and compute the length of the string it
     will print as.  Be conservative and add one to account for possible
     overflow or rounding error.  */
  dec_exp = r.exp * M_LOG10_2;
  for (max_digits = 1; dec_exp ; max_digits++)
    dec_exp /= 10;

  /* Bound the number of digits printed by the size of the output buffer.  */
  max_digits = buf_size - 1 - 1 - 2 - max_digits - 1;
  if (max_digits > buf_size)
    abort ();
  if (digits > max_digits)
    digits = max_digits;

  one = real_digit (1);
  ten = ten_to_ptwo (0);

  sign = r.sign;
  r.sign = 0;

  dec_exp = 0;
  pten = *one;

  cmp_one = do_compare (&r, one, 0);
  if (cmp_one > 0)
    {
      int m;

      /* Number is greater than one.  Convert significand to an integer
	 and strip trailing decimal zeros.  */

      u = r;
      u.exp = SIGNIFICAND_BITS - 1;

      /* Largest M, such that 10**2**M fits within SIGNIFICAND_BITS.  */
      m = floor_log2 (max_digits);

      /* Iterate over the bits of the possible powers of 10 that might
	 be present in U and eliminate them.  That is, if we find that
	 10**2**M divides U evenly, keep the division and increase 
	 DEC_EXP by 2**M.  */
      do
	{
	  REAL_VALUE_TYPE t;

	  do_divide (&t, &u, ten_to_ptwo (m));
	  do_fix_trunc (&v, &t);
	  if (cmp_significands (&v, &t) == 0)
	    {
	      u = t;
	      dec_exp += 1 << m;
	    }
	}
      while (--m >= 0);

      /* Revert the scaling to integer that we performed earlier.  */
      u.exp += r.exp - (SIGNIFICAND_BITS - 1);
      r = u;

      /* Find power of 10.  Do this by dividing out 10**2**M when
	 this is larger than the current remainder.  Fill PTEN with 
	 the power of 10 that we compute.  */
      if (r.exp > 0)
	{
	  m = floor_log2 ((int)(r.exp * M_LOG10_2)) + 1;
	  do
	    {
	      const REAL_VALUE_TYPE *ptentwo = ten_to_ptwo (m);
	      if (do_compare (&u, ptentwo, 0) >= 0)
	        {
	          do_divide (&u, &u, ptentwo);
	          do_multiply (&pten, &pten, ptentwo);
	          dec_exp += 1 << m;
	        }
	    }
          while (--m >= 0);
	}
      else
	/* We managed to divide off enough tens in the above reduction
	   loop that we've now got a negative exponent.  Fall into the
	   less-than-one code to compute the proper value for PTEN.  */
	cmp_one = -1;
    }
  if (cmp_one < 0)
    {
      int m;

      /* Number is less than one.  Pad significand with leading
	 decimal zeros.  */

      v = r;
      while (1)
	{
	  /* Stop if we'd shift bits off the bottom.  */
	  if (v.sig[0] & 7)
	    break;

	  do_multiply (&u, &v, ten);

	  /* Stop if we're now >= 1.  */
	  if (u.exp > 0)
	    break;

	  v = u;
	  dec_exp -= 1;
	}
      r = v;

      /* Find power of 10.  Do this by multiplying in P=10**2**M when
	 the current remainder is smaller than 1/P.  Fill PTEN with the
	 power of 10 that we compute.  */
      m = floor_log2 ((int)(-r.exp * M_LOG10_2)) + 1;
      do
	{
	  const REAL_VALUE_TYPE *ptentwo = ten_to_ptwo (m);
	  const REAL_VALUE_TYPE *ptenmtwo = ten_to_mptwo (m);

	  if (do_compare (&v, ptenmtwo, 0) <= 0)
	    {
	      do_multiply (&v, &v, ptentwo);
	      do_multiply (&pten, &pten, ptentwo);
	      dec_exp -= 1 << m;
	    }
	}
      while (--m >= 0);

      /* Invert the positive power of 10 that we've collected so far.  */
      do_divide (&pten, one, &pten);
    }

  p = str;
  if (sign)
    *p++ = '-';
  first = p++;

  /* At this point, PTEN should contain the nearest power of 10 smaller
     than R, such that this division produces the first digit.

     Using a divide-step primitive that returns the complete integral
     remainder avoids the rounding error that would be produced if
     we were to use do_divide here and then simply multiply by 10 for
     each subsequent digit.  */

  digit = rtd_divmod (&r, &pten);

  /* Be prepared for error in that division via underflow ...  */
  if (digit == 0 && cmp_significand_0 (&r))
    {
      /* Multiply by 10 and try again.  */
      do_multiply (&r, &r, ten);
      digit = rtd_divmod (&r, &pten);
      dec_exp -= 1;
      if (digit == 0)
	abort ();
    }

  /* ... or overflow.  */
  if (digit == 10)
    {
      *p++ = '1';
      if (--digits > 0)
	*p++ = '0';
      dec_exp += 1;
    }
  else if (digit > 10)
    abort ();
  else
    *p++ = digit + '0';

  /* Generate subsequent digits.  */
  while (--digits > 0)
    {
      do_multiply (&r, &r, ten);
      digit = rtd_divmod (&r, &pten);
      *p++ = digit + '0';
    }
  last = p;

  /* Generate one more digit with which to do rounding.  */
  do_multiply (&r, &r, ten);
  digit = rtd_divmod (&r, &pten);

  /* Round the result.  */
  if (digit == 5)
    {
      /* Round to nearest.  If R is nonzero there are additional
	 nonzero digits to be extracted.  */
      if (cmp_significand_0 (&r))
	digit++;
      /* Round to even.  */
      else if ((p[-1] - '0') & 1)
	digit++;
    }
  if (digit > 5)
    {
      while (p > first)
	{
	  digit = *--p;
	  if (digit == '9')
	    *p = '0';
	  else
	    {
	      *p = digit + 1;
	      break;
	    }
	}

      /* Carry out of the first digit.  This means we had all 9's and
	 now have all 0's.  "Prepend" a 1 by overwriting the first 0.  */
      if (p == first)
	{
	  first[1] = '1';
	  dec_exp++;
	}
    }
  
  /* Insert the decimal point.  */
  first[0] = first[1];
  first[1] = '.';

  /* If requested, drop trailing zeros.  Never crop past "1.0".  */
  if (crop_trailing_zeros)
    while (last > first + 3 && last[-1] == '0')
      last--;

  /* Append the exponent.  */
  sprintf (last, "e%+d", dec_exp);
}

/* Render R as a hexadecimal floating point constant.  Emit DIGITS
   significant digits in the result, bounded by BUF_SIZE.  If DIGITS is 0,
   choose the maximum for the representation.  If CROP_TRAILING_ZEROS,
   strip trailing zeros.  */

void
real_to_hexadecimal (str, r, buf_size, digits, crop_trailing_zeros)
     char *str;
     const REAL_VALUE_TYPE *r;
     size_t buf_size, digits;
     int crop_trailing_zeros;
{
  int i, j, exp = r->exp;
  char *p, *first;
  char exp_buf[16];
  size_t max_digits;

  switch (r->class)
    {
    case rvc_zero:
      exp = 0;
      break;
    case rvc_normal:
      break;
    case rvc_inf:
      strcpy (str, (r->sign ? "-Inf" : "+Inf"));
      return;
    case rvc_nan:
      /* ??? Print the significand as well, if not canonical?  */
      strcpy (str, (r->sign ? "-NaN" : "+NaN"));
      return;
    default:
      abort ();
    }

  if (digits == 0)
    digits = SIGNIFICAND_BITS / 4;

  /* Bound the number of digits printed by the size of the output buffer.  */

  sprintf (exp_buf, "p%+d", exp);
  max_digits = buf_size - strlen (exp_buf) - r->sign - 4 - 1;
  if (max_digits > buf_size)
    abort ();
  if (digits > max_digits)
    digits = max_digits;

  p = str;
  if (r->sign)
    *p++ = '-';
  *p++ = '0';
  *p++ = 'x';
  *p++ = '0';
  *p++ = '.';
  first = p;

  for (i = SIGSZ - 1; i >= 0; --i)
    for (j = HOST_BITS_PER_LONG - 4; j >= 0; j -= 4)
      {
	*p++ = "0123456789abcdef"[(r->sig[i] >> j) & 15];
	if (--digits == 0)
	  goto out;
      }

 out:
  if (crop_trailing_zeros)
    while (p > first + 1 && p[-1] == '0')
      p--;

  sprintf (p, "p%+d", exp);
}

/* Initialize R from a decimal or hexadecimal string.  The string is
   assumed to have been syntax checked already.  */

void
real_from_string (r, str)
     REAL_VALUE_TYPE *r;
     const char *str;
{
  int exp = 0;
  bool sign = false;

  get_zero (r, 0);

  if (*str == '-')
    {
      sign = true;
      str++;
    }
  else if (*str == '+')
    str++;

  if (str[0] == '0' && str[1] == 'x')
    {
      /* Hexadecimal floating point.  */
      int pos = SIGNIFICAND_BITS - 4, d;

      str += 2;

      while (*str == '0')
	str++;
      while (1)
	{
	  d = hex_value (*str);
	  if (d == _hex_bad)
	    break;
	  if (pos >= 0)
	    {
	      r->sig[pos / HOST_BITS_PER_LONG]
		|= (unsigned long) d << (pos % HOST_BITS_PER_LONG);
	      pos -= 4;
	    }
	  exp += 4;
	  str++;
	}
      if (*str == '.')
	{
	  str++;
	  if (pos == SIGNIFICAND_BITS - 4)
	    {
	      while (*str == '0')
		str++, exp -= 4;
	    }
	  while (1)
	    {
	      d = hex_value (*str);
	      if (d == _hex_bad)
		break;
	      if (pos >= 0)
		{
		  r->sig[pos / HOST_BITS_PER_LONG]
		    |= (unsigned long) d << (pos % HOST_BITS_PER_LONG);
		  pos -= 4;
		}
	      str++;
	    }
	}
      if (*str == 'p' || *str == 'P')
	{
	  bool exp_neg = false;

	  str++;
	  if (*str == '-')
	    {
	      exp_neg = true;
	      str++;
	    }
	  else if (*str == '+')
	    str++;

	  d = 0;
	  while (ISDIGIT (*str))
	    {
	      d *= 10;
	      d += *str - '0';
	      if (d > MAX_EXP)
		{
		  /* Overflowed the exponent.  */
		  if (exp_neg)
		    goto underflow;
		  else
		    goto overflow;
		}
	      str++;
	    }
	  if (exp_neg)
	    d = -d;

	  exp += d;
	}

      r->class = rvc_normal;
      r->exp = exp;

      normalize (r);
    }
  else
    {
      /* Decimal floating point.  */
      const REAL_VALUE_TYPE *ten = ten_to_ptwo (0);
      int d;

      while (*str == '0')
	str++;
      while (ISDIGIT (*str))
	{
	  d = *str++ - '0';
	  do_multiply (r, r, ten);
	  if (d)
	    do_add (r, r, real_digit (d), 0);
	}
      if (*str == '.')
	{
	  str++;
	  if (r->class == rvc_zero)
	    {
	      while (*str == '0')
		str++, exp--;
	    }
	  while (ISDIGIT (*str))
	    {
	      d = *str++ - '0';
	      do_multiply (r, r, ten);
	      if (d)
	        do_add (r, r, real_digit (d), 0);
	      exp--;
	    }
	}

      if (*str == 'e' || *str == 'E')
	{
	  bool exp_neg = false;

	  str++;
	  if (*str == '-')
	    {
	      exp_neg = true;
	      str++;
	    }
	  else if (*str == '+')
	    str++;

	  d = 0;
	  while (ISDIGIT (*str))
	    {
	      d *= 10;
	      d += *str - '0';
	      if (d > MAX_EXP)
		{
		  /* Overflowed the exponent.  */
		  if (exp_neg)
		    goto underflow;
		  else
		    goto overflow;
		}
	      str++;
	    }
	  if (exp_neg)
	    d = -d;
	  exp += d;
	}

      if (exp)
	times_pten (r, exp);
    }

  r->sign = sign;
  return;

 underflow:
  get_zero (r, sign);
  return;

 overflow:
  get_inf (r, sign);
  return;
}

/* Legacy.  Similar, but return the result directly.  */

REAL_VALUE_TYPE
real_from_string2 (s, mode)
     const char *s;
     enum machine_mode mode;
{
  REAL_VALUE_TYPE r;

  real_from_string (&r, s);
  if (mode != VOIDmode)
    real_convert (&r, mode, &r);

  return r;
}

/* Initialize R from the integer pair HIGH+LOW.  */

void
real_from_integer (r, mode, low, high, unsigned_p)
     REAL_VALUE_TYPE *r;
     enum machine_mode mode;
     unsigned HOST_WIDE_INT low;
     HOST_WIDE_INT high;
     int unsigned_p;
{
  if (low == 0 && high == 0)
    get_zero (r, 0);
  else
    {
      r->class = rvc_normal;
      r->sign = high < 0 && !unsigned_p;
      r->exp = 2 * HOST_BITS_PER_WIDE_INT;

      if (r->sign)
	{
	  high = ~high;
	  if (low == 0)
	    high += 1;
	  else
	    low = -low;
	}

      if (HOST_BITS_PER_LONG == HOST_BITS_PER_WIDE_INT)
	{
	  r->sig[SIGSZ-1] = high;
	  r->sig[SIGSZ-2] = low;
	  memset (r->sig, 0, sizeof(long)*(SIGSZ-2));
	}
      else if (HOST_BITS_PER_LONG*2 == HOST_BITS_PER_WIDE_INT)
	{
	  r->sig[SIGSZ-1] = high >> (HOST_BITS_PER_LONG - 1) >> 1;
	  r->sig[SIGSZ-2] = high;
	  r->sig[SIGSZ-3] = low >> (HOST_BITS_PER_LONG - 1) >> 1;
	  r->sig[SIGSZ-4] = low;
	  if (SIGSZ > 4)
	    memset (r->sig, 0, sizeof(long)*(SIGSZ-4));
	}
      else
	abort ();

      normalize (r);
    }

  if (mode != VOIDmode)
    real_convert (r, mode, r);
}

/* Returns 10**2**N.  */

static const REAL_VALUE_TYPE *
ten_to_ptwo (n)
     int n;
{
  static REAL_VALUE_TYPE tens[EXP_BITS];

  if (n < 0 || n >= EXP_BITS)
    abort ();

  if (tens[n].class == rvc_zero)
    {
      if (n < (HOST_BITS_PER_WIDE_INT == 64 ? 5 : 4))
	{
	  HOST_WIDE_INT t = 10;
	  int i;

	  for (i = 0; i < n; ++i)
	    t *= t;

	  real_from_integer (&tens[n], VOIDmode, t, 0, 1);
	}
      else
	{
	  const REAL_VALUE_TYPE *t = ten_to_ptwo (n - 1);
	  do_multiply (&tens[n], t, t);
	}
    }

  return &tens[n];
}

/* Returns 10**(-2**N).  */

static const REAL_VALUE_TYPE *
ten_to_mptwo (n)
     int n;
{
  static REAL_VALUE_TYPE tens[EXP_BITS];

  if (n < 0 || n >= EXP_BITS)
    abort ();

  if (tens[n].class == rvc_zero)
    do_divide (&tens[n], real_digit (1), ten_to_ptwo (n));

  return &tens[n];
}

/* Returns N.  */

static const REAL_VALUE_TYPE *
real_digit (n)
     int n;
{
  static REAL_VALUE_TYPE num[10];

  if (n < 0 || n > 9)
    abort ();

  if (n > 0 && num[n].class == rvc_zero)
    real_from_integer (&num[n], VOIDmode, n, 0, 1);

  return &num[n];
}

/* Multiply R by 10**EXP.  */

static void
times_pten (r, exp)
     REAL_VALUE_TYPE *r;
     int exp;
{
  REAL_VALUE_TYPE pten, *rr;
  bool negative = (exp < 0);
  int i;

  if (negative)
    {
      exp = -exp;
      pten = *real_digit (1);
      rr = &pten;
    }
  else
    rr = r;

  for (i = 0; exp > 0; ++i, exp >>= 1)
    if (exp & 1)
      do_multiply (rr, rr, ten_to_ptwo (i));

  if (negative)
    do_divide (r, r, &pten);
}

/* Fills R with +Inf.  */

void
real_inf (r)
     REAL_VALUE_TYPE *r;
{
  get_inf (r, 0);
}

/* Fills R with a NaN whose significand is described by STR.  If QUIET,
   we force a QNaN, else we force an SNaN.  The string, if not empty,
   is parsed as a number and placed in the significand.  Return true
   if the string was successfully parsed.  */

bool
real_nan (r, str, quiet, mode)
     REAL_VALUE_TYPE *r;
     const char *str;
     int quiet;
     enum machine_mode mode;
{
  const struct real_format *fmt;

  fmt = real_format_for_mode[mode - QFmode];
  if (fmt == NULL)
    abort ();

  if (*str == 0)
    {
      if (quiet)
	get_canonical_qnan (r, 0);
      else
	get_canonical_snan (r, 0);
    }
  else
    {
      int base = 10, d;
      bool neg = false;

      memset (r, 0, sizeof (*r));
      r->class = rvc_nan;

      /* Parse akin to strtol into the significand of R.  */

      while (ISSPACE (*str))
	str++;
      if (*str == '-')
	str++, neg = true;
      else if (*str == '+')
	str++;
      if (*str == '0')
	{
	  if (*++str == 'x')
	    str++, base = 16;
	  else
	    base = 8;
	}

      while ((d = hex_value (*str)) < base)
	{
	  REAL_VALUE_TYPE u;

	  switch (base)
	    {
	    case 8:
	      lshift_significand (r, r, 3);
	      break;
	    case 16:
	      lshift_significand (r, r, 4);
	      break;
	    case 10:
	      lshift_significand_1 (&u, r);
	      lshift_significand (r, r, 3);
	      add_significands (r, r, &u);
	      break;
	    default:
	      abort ();
	    }

	  get_zero (&u, 0);
	  u.sig[0] = d;
	  add_significands (r, r, &u);

	  str++;
	}

      /* Must have consumed the entire string for success.  */
      if (*str != 0)
	return false;

      /* Shift the significand into place such that the bits
	 are in the most significant bits for the format.  */
      lshift_significand (r, r, SIGNIFICAND_BITS - fmt->p);

      /* Our MSB is always unset for NaNs.  */
      r->sig[SIGSZ-1] &= ~SIG_MSB;

      /* Force quiet or signalling NaN.  */
      if (quiet)
	r->sig[SIGSZ-1] |= SIG_MSB >> 1;
      else
	r->sig[SIGSZ-1] &= ~(SIG_MSB >> 1);

      /* Force at least one bit of the significand set.  */
      for (d = 0; d < SIGSZ; ++d)
	if (r->sig[d])
	  break;
      if (d == SIGSZ)
	r->sig[SIGSZ-1] |= SIG_MSB >> 2;

      /* Our intermediate format forces QNaNs to have MSB-1 set.
	 If the target format has QNaNs with the top bit unset,
	 mirror the output routines and invert the top two bits.  */
      if (!fmt->qnan_msb_set)
	r->sig[SIGSZ-1] ^= (SIG_MSB >> 1) | (SIG_MSB >> 2);
    }

  return true;
}

/* Fills R with 2**N.  */

void
real_2expN (r, n)
     REAL_VALUE_TYPE *r;
     int n;
{
  memset (r, 0, sizeof (*r));

  n++;
  if (n > MAX_EXP)
    r->class = rvc_inf;
  else if (n < -MAX_EXP)
    ;
  else
    {
      r->class = rvc_normal;
      r->exp = n;
      r->sig[SIGSZ-1] = SIG_MSB;
    }
}


static void
round_for_format (fmt, r)
     const struct real_format *fmt;
     REAL_VALUE_TYPE *r;
{
  int p2, np2, i, w;
  unsigned long sticky;
  bool guard, lsb;
  int emin2m1, emax2;

  p2 = fmt->p * fmt->log2_b;
  emin2m1 = (fmt->emin - 1) * fmt->log2_b;
  emax2 = fmt->emax * fmt->log2_b;

  np2 = SIGNIFICAND_BITS - p2;
  switch (r->class)
    {
    underflow:
      get_zero (r, r->sign);
    case rvc_zero:
      if (!fmt->has_signed_zero)
	r->sign = 0;
      return;

    overflow:
      get_inf (r, r->sign);
    case rvc_inf:
      return;

    case rvc_nan:
      clear_significand_below (r, np2);

      /* If we've cleared the entire significand, we need one bit
	 set for this to continue to be a NaN.  */
      for (i = 0; i < SIGSZ; ++i)
	if (r->sig[i])
	  break;
      if (i == SIGSZ)
	r->sig[SIGSZ-1] = SIG_MSB >> 2;
      return;

    case rvc_normal:
      break;

    default:
      abort ();
    }

  /* If we're not base2, normalize the exponent to a multiple of
     the true base.  */
  if (fmt->log2_b != 1)
    {
      int shift = r->exp & (fmt->log2_b - 1);
      if (shift)
	{
	  shift = fmt->log2_b - shift;
	  r->sig[0] |= sticky_rshift_significand (r, r, shift);
	  r->exp += shift;
	}
    }

  /* Check the range of the exponent.  If we're out of range,
     either underflow or overflow.  */
  if (r->exp > emax2)
    goto overflow;
  else if (r->exp <= emin2m1)
    {
      int diff;

      if (!fmt->has_denorm)
	{
	  /* Don't underflow completely until we've had a chance to round.  */
	  if (r->exp < emin2m1)
	    goto underflow;
	}
      else
	{
	  diff = emin2m1 - r->exp + 1;
	  if (diff > p2)
	    goto underflow;

	  /* De-normalize the significand.  */
	  r->sig[0] |= sticky_rshift_significand (r, r, diff);
	  r->exp += diff;
	}
    }

  /* There are P2 true significand bits, followed by one guard bit,
     followed by one sticky bit, followed by stuff.  Fold nonzero
     stuff into the sticky bit.  */

  sticky = 0;
  for (i = 0, w = (np2 - 1) / HOST_BITS_PER_LONG; i < w; ++i)
    sticky |= r->sig[i];
  sticky |=
    r->sig[w] & (((unsigned long)1 << ((np2 - 1) % HOST_BITS_PER_LONG)) - 1);

  guard = test_significand_bit (r, np2 - 1);
  lsb = test_significand_bit (r, np2);

  /* Round to even.  */
  if (guard && (sticky || lsb))
    {
      REAL_VALUE_TYPE u;
      get_zero (&u, 0);
      set_significand_bit (&u, np2);

      if (add_significands (r, r, &u))
	{
	  /* Overflow.  Means the significand had been all ones, and
	     is now all zeros.  Need to increase the exponent, and
	     possibly re-normalize it.  */
	  if (++r->exp > emax2)
	    goto overflow;
	  r->sig[SIGSZ-1] = SIG_MSB;

	  if (fmt->log2_b != 1)
	    {
	      int shift = r->exp & (fmt->log2_b - 1);
	      if (shift)
		{
		  shift = fmt->log2_b - shift;
		  rshift_significand (r, r, shift);
		  r->exp += shift;
		  if (r->exp > emax2)
		    goto overflow;
		}
	    }
	}
    }

  /* Catch underflow that we deferred until after rounding.  */
  if (r->exp <= emin2m1)
    goto underflow;

  /* Clear out trailing garbage.  */
  clear_significand_below (r, np2);
}

/* Extend or truncate to a new mode.  */

void
real_convert (r, mode, a)
     REAL_VALUE_TYPE *r;
     enum machine_mode mode;
     const REAL_VALUE_TYPE *a;
{
  const struct real_format *fmt;

  fmt = real_format_for_mode[mode - QFmode];
  if (fmt == NULL)
    abort ();

  *r = *a;
  round_for_format (fmt, r);

  /* round_for_format de-normalizes denormals.  Undo just that part.  */
  if (r->class == rvc_normal)
    normalize (r);
}

/* Legacy.  Likewise, except return the struct directly.  */

REAL_VALUE_TYPE
real_value_truncate (mode, a)
     enum machine_mode mode;
     REAL_VALUE_TYPE a;
{
  REAL_VALUE_TYPE r;
  real_convert (&r, mode, &a);
  return r;
}

/* Return true if truncating to MODE is exact.  */

bool
exact_real_truncate (mode, a)
     enum machine_mode mode;
     const REAL_VALUE_TYPE *a;
{
  REAL_VALUE_TYPE t;
  real_convert (&t, mode, a);
  return real_identical (&t, a);
}

/* Write R to the given target format.  Place the words of the result
   in target word order in BUF.  There are always 32 bits in each
   long, no matter the size of the host long.

   Legacy: return word 0 for implementing REAL_VALUE_TO_TARGET_SINGLE.  */

long
real_to_target_fmt (buf, r_orig, fmt)
     long *buf;
     const REAL_VALUE_TYPE *r_orig;
     const struct real_format *fmt;
{
  REAL_VALUE_TYPE r;
  long buf1;

  r = *r_orig;
  round_for_format (fmt, &r);

  if (!buf)
    buf = &buf1;
  (*fmt->encode) (fmt, buf, &r);

  return *buf;
}

/* Similar, but look up the format from MODE.  */

long
real_to_target (buf, r, mode)
     long *buf;
     const REAL_VALUE_TYPE *r;
     enum machine_mode mode;
{
  const struct real_format *fmt;

  fmt = real_format_for_mode[mode - QFmode];
  if (fmt == NULL)
    abort ();

  return real_to_target_fmt (buf, r, fmt);
}

/* Read R from the given target format.  Read the words of the result
   in target word order in BUF.  There are always 32 bits in each
   long, no matter the size of the host long.  */

void
real_from_target_fmt (r, buf, fmt)
     REAL_VALUE_TYPE *r;
     const long *buf;
     const struct real_format *fmt;
{
  (*fmt->decode) (fmt, r, buf);
}     

/* Similar, but look up the format from MODE.  */

void
real_from_target (r, buf, mode)
     REAL_VALUE_TYPE *r;
     const long *buf;
     enum machine_mode mode;
{
  const struct real_format *fmt;

  fmt = real_format_for_mode[mode - QFmode];
  if (fmt == NULL)
    abort ();

  (*fmt->decode) (fmt, r, buf);
}     

/* Return the number of bits in the significand for MODE.  */
/* ??? Legacy.  Should get access to real_format directly.  */

int
significand_size (mode)
     enum machine_mode mode;
{
  const struct real_format *fmt;

  fmt = real_format_for_mode[mode - QFmode];
  if (fmt == NULL)
    return 0;

  return fmt->p * fmt->log2_b;
}

/* Return a hash value for the given real value.  */
/* ??? The "unsigned int" return value is intended to be hashval_t,
   but I didn't want to pull hashtab.h into real.h.  */

unsigned int
real_hash (r)
     const REAL_VALUE_TYPE *r;
{
  unsigned int h;
  size_t i;

  h = r->class | (r->sign << 2);
  switch (r->class)
    {
    case rvc_zero:
    case rvc_inf:
      break;

    case rvc_normal:
      h |= r->exp << 3;
      /* FALLTHRU */

    case rvc_nan:
      if (sizeof(unsigned long) > sizeof(unsigned int))
	for (i = 0; i < SIGSZ; ++i)
	  {
	    unsigned long s = r->sig[i];
	    h ^= s ^ (s >> (HOST_BITS_PER_LONG / 2));
	  }
      else
	for (i = 0; i < SIGSZ; ++i)
	  h ^= r->sig[i];
      break;

    default:
      abort ();
    }

  return h;
}

/* IEEE single-precision format.  */

static void encode_ieee_single PARAMS ((const struct real_format *fmt,
					long *, const REAL_VALUE_TYPE *));
static void decode_ieee_single PARAMS ((const struct real_format *,
					REAL_VALUE_TYPE *, const long *));

static void
encode_ieee_single (fmt, buf, r)
     const struct real_format *fmt;
     long *buf;
     const REAL_VALUE_TYPE *r;
{
  unsigned long image, sig, exp;
  bool denormal = (r->sig[SIGSZ-1] & SIG_MSB) == 0;

  image = r->sign << 31;
  sig = (r->sig[SIGSZ-1] >> (HOST_BITS_PER_LONG - 24)) & 0x7fffff;

  switch (r->class)
    {
    case rvc_zero:
      break;

    case rvc_inf:
      if (fmt->has_inf)
	image |= 255 << 23;
      else
	image |= 0x7fffffff;
      break;

    case rvc_nan:
      if (fmt->has_nans)
	{
	  image |= 255 << 23;
	  image |= sig;
	  if (!fmt->qnan_msb_set)
	    image ^= 1 << 23 | 1 << 22;
	}
      else
	image |= 0x7fffffff;
      break;

    case rvc_normal:
      /* Recall that IEEE numbers are interpreted as 1.F x 2**exp,
	 whereas the intermediate representation is 0.F x 2**exp.
	 Which means we're off by one.  */
      if (denormal)
	exp = 0;
      else
      exp = r->exp + 127 - 1;
      image |= exp << 23;
      image |= sig;
      break;

    default:
      abort ();
    }

  buf[0] = image;
}

static void
decode_ieee_single (fmt, r, buf)
     const struct real_format *fmt;
     REAL_VALUE_TYPE *r;
     const long *buf;
{
  unsigned long image = buf[0] & 0xffffffff;
  bool sign = (image >> 31) & 1;
  int exp = (image >> 23) & 0xff;

  memset (r, 0, sizeof (*r));
  image <<= HOST_BITS_PER_LONG - 24;
  image &= ~SIG_MSB;

  if (exp == 0)
    {
      if (image && fmt->has_denorm)
	{
	  r->class = rvc_normal;
	  r->sign = sign;
	  r->exp = -126;
	  r->sig[SIGSZ-1] = image << 1;
	  normalize (r);
	}
      else if (fmt->has_signed_zero)
	r->sign = sign;
    }
  else if (exp == 255 && (fmt->has_nans || fmt->has_inf))
    {
      if (image)
	{
	  r->class = rvc_nan;
	  r->sign = sign;
	  if (!fmt->qnan_msb_set)
	    image ^= (SIG_MSB >> 1 | SIG_MSB >> 2);
	  r->sig[SIGSZ-1] = image;
	}
      else
	{
	  r->class = rvc_inf;
	  r->sign = sign;
	}
    }
  else
    {
      r->class = rvc_normal;
      r->sign = sign;
      r->exp = exp - 127 + 1;
      r->sig[SIGSZ-1] = image | SIG_MSB;
    }
}

const struct real_format ieee_single_format = 
  {
    encode_ieee_single,
    decode_ieee_single,
    2,
    1,
    24,
    -125,
    128,
    true,
    true,
    true,
    true,
    true
  };


/* IEEE double-precision format.  */

static void encode_ieee_double PARAMS ((const struct real_format *fmt,
					long *, const REAL_VALUE_TYPE *));
static void decode_ieee_double PARAMS ((const struct real_format *,
					REAL_VALUE_TYPE *, const long *));

static void
encode_ieee_double (fmt, buf, r)
     const struct real_format *fmt;
     long *buf;
     const REAL_VALUE_TYPE *r;
{
  unsigned long image_lo, image_hi, sig_lo, sig_hi, exp;
  bool denormal = (r->sig[SIGSZ-1] & SIG_MSB) == 0;

  image_hi = r->sign << 31;
  image_lo = 0;

  if (HOST_BITS_PER_LONG == 64)
    {
      sig_hi = r->sig[SIGSZ-1];
      sig_lo = (sig_hi >> (64 - 53)) & 0xffffffff;
      sig_hi = (sig_hi >> (64 - 53 + 1) >> 31) & 0xfffff;
    }
  else
    {
      sig_hi = r->sig[SIGSZ-1];
      sig_lo = r->sig[SIGSZ-2];
      sig_lo = (sig_hi << 21) | (sig_lo >> 11);
      sig_hi = (sig_hi >> 11) & 0xfffff;
    }

  switch (r->class)
    {
    case rvc_zero:
      break;

    case rvc_inf:
      if (fmt->has_inf)
	image_hi |= 2047 << 20;
      else
	{
	  image_hi |= 0x7fffffff;
	  image_lo = 0xffffffff;
	}
      break;

    case rvc_nan:
      if (fmt->has_nans)
	{
	  image_hi |= 2047 << 20;
	  image_hi |= sig_hi;
	  if (!fmt->qnan_msb_set)
	    image_hi ^= 1 << 19 | 1 << 18;
	  image_lo = sig_lo;
	}
      else
	{
	  image_hi |= 0x7fffffff;
	  image_lo = 0xffffffff;
	}
      break;

    case rvc_normal:
      /* Recall that IEEE numbers are interpreted as 1.F x 2**exp,
	 whereas the intermediate representation is 0.F x 2**exp.
	 Which means we're off by one.  */
      if (denormal)
	exp = 0;
      else
	exp = r->exp + 1023 - 1;
      image_hi |= exp << 20;
      image_hi |= sig_hi;
      image_lo = sig_lo;
      break;

    default:
      abort ();
    }

  if (FLOAT_WORDS_BIG_ENDIAN)
    buf[0] = image_hi, buf[1] = image_lo;
  else
    buf[0] = image_lo, buf[1] = image_hi;
}

static void
decode_ieee_double (fmt, r, buf)
     const struct real_format *fmt;
     REAL_VALUE_TYPE *r;
     const long *buf;
{
  unsigned long image_hi, image_lo;
  bool sign;
  int exp;

  if (FLOAT_WORDS_BIG_ENDIAN)
    image_hi = buf[0], image_lo = buf[1];
  else
    image_lo = buf[0], image_hi = buf[1];
  image_lo &= 0xffffffff;
  image_hi &= 0xffffffff;

  sign = (image_hi >> 31) & 1;
  exp = (image_hi >> 20) & 0x7ff;

  memset (r, 0, sizeof (*r));

  image_hi <<= 32 - 21;
  image_hi |= image_lo >> 21;
  image_hi &= 0x7fffffff;
  image_lo <<= 32 - 21;

  if (exp == 0)
    {
      if ((image_hi || image_lo) && fmt->has_denorm)
	{
	  r->class = rvc_normal;
	  r->sign = sign;
	  r->exp = -1022;
	  if (HOST_BITS_PER_LONG == 32)
	    {
	      image_hi = (image_hi << 1) | (image_lo >> 31);
	      image_lo <<= 1;
	      r->sig[SIGSZ-1] = image_hi;
	      r->sig[SIGSZ-2] = image_lo;
	    }
	  else
	    {
	      image_hi = (image_hi << 31 << 2) | (image_lo << 1);
	      r->sig[SIGSZ-1] = image_hi;
	    }
	  normalize (r);
	}
      else if (fmt->has_signed_zero)
	r->sign = sign;
    }
  else if (exp == 2047 && (fmt->has_nans || fmt->has_inf))
    {
      if (image_hi || image_lo)
	{
	  r->class = rvc_nan;
	  r->sign = sign;
	  if (HOST_BITS_PER_LONG == 32)
	    {
	      r->sig[SIGSZ-1] = image_hi;
	      r->sig[SIGSZ-2] = image_lo;
	    }
	  else
	    r->sig[SIGSZ-1] = (image_hi << 31 << 1) | image_lo;

	  if (!fmt->qnan_msb_set)
	    r->sig[SIGSZ-1] ^= (SIG_MSB >> 1 | SIG_MSB >> 2);
	}
      else
	{
	  r->class = rvc_inf;
	  r->sign = sign;
	}
    }
  else
    {
      r->class = rvc_normal;
      r->sign = sign;
      r->exp = exp - 1023 + 1;
      if (HOST_BITS_PER_LONG == 32)
	{
	  r->sig[SIGSZ-1] = image_hi | SIG_MSB;
	  r->sig[SIGSZ-2] = image_lo;
	}
      else
	r->sig[SIGSZ-1] = (image_hi << 31 << 1) | image_lo | SIG_MSB;
    }
}

const struct real_format ieee_double_format = 
  {
    encode_ieee_double,
    decode_ieee_double,
    2,
    1,
    53,
    -1021,
    1024,
    true,
    true,
    true,
    true,
    true
  };


/* IEEE extended double precision format.  This comes in three
   flavours: Intel's as a 12 byte image, Intel's as a 16 byte image,
   and Motorola's.  */

static void encode_ieee_extended PARAMS ((const struct real_format *fmt,
					  long *, const REAL_VALUE_TYPE *));
static void decode_ieee_extended PARAMS ((const struct real_format *,
					  REAL_VALUE_TYPE *, const long *));

static void encode_ieee_extended_128 PARAMS ((const struct real_format *fmt,
					      long *,
					      const REAL_VALUE_TYPE *));
static void decode_ieee_extended_128 PARAMS ((const struct real_format *,
					      REAL_VALUE_TYPE *,
					      const long *));

static void
encode_ieee_extended (fmt, buf, r)
     const struct real_format *fmt;
     long *buf;
     const REAL_VALUE_TYPE *r;
{
  unsigned long image_hi, sig_hi, sig_lo;
  bool denormal = (r->sig[SIGSZ-1] & SIG_MSB) == 0;

  image_hi = r->sign << 15;
  sig_hi = sig_lo = 0;

  switch (r->class)
    {
    case rvc_zero:
      break;

    case rvc_inf:
      if (fmt->has_inf)
	{
	  image_hi |= 32767;

	  /* Intel requires the explicit integer bit to be set, otherwise
	     it considers the value a "pseudo-infinity".  Motorola docs
	     say it doesn't care.  */
	  sig_hi = 0x80000000;
	}
      else
	{
	  image_hi |= 32767;
	  sig_lo = sig_hi = 0xffffffff;
	}
      break;

    case rvc_nan:
      if (fmt->has_nans)
	{
	  image_hi |= 32767;
	  if (HOST_BITS_PER_LONG == 32)
	    {
	      sig_hi = r->sig[SIGSZ-1];
	      sig_lo = r->sig[SIGSZ-2];
	    }
	  else
	    {
	      sig_lo = r->sig[SIGSZ-1];
	      sig_hi = sig_lo >> 31 >> 1;
	      sig_lo &= 0xffffffff;
	    }
	  if (!fmt->qnan_msb_set)
	    sig_hi ^= 1 << 30 | 1 << 29;

	  /* Intel requires the explicit integer bit to be set, otherwise
	     it considers the value a "pseudo-nan".  Motorola docs say it
	     doesn't care.  */
	  sig_hi |= 0x80000000;
	}
      else
	{
	  image_hi |= 32767;
	  sig_lo = sig_hi = 0xffffffff;
	}
      break;

    case rvc_normal:
      {
	int exp = r->exp;

	/* Recall that IEEE numbers are interpreted as 1.F x 2**exp,
	   whereas the intermediate representation is 0.F x 2**exp.
	   Which means we're off by one. 

	   Except for Motorola, which consider exp=0 and explicit
	   integer bit set to continue to be normalized.  In theory
	   this descrepency has been taken care of by the difference
	   in fmt->emin in round_for_format.  */

	if (denormal)
	  exp = 0;
	else
	  {
	    exp += 16383 - 1;
	    if (exp < 0)
	      abort ();
	  }
	image_hi |= exp;

	if (HOST_BITS_PER_LONG == 32)
	  {
	    sig_hi = r->sig[SIGSZ-1];
	    sig_lo = r->sig[SIGSZ-2];
	  }
	else
	  {
	    sig_lo = r->sig[SIGSZ-1];
	    sig_hi = sig_lo >> 31 >> 1;
	    sig_lo &= 0xffffffff;
	  }
      }
      break;

    default:
      abort ();
    }

  if (FLOAT_WORDS_BIG_ENDIAN)
    buf[0] = image_hi << 16, buf[1] = sig_hi, buf[2] = sig_lo;
  else
    buf[0] = sig_lo, buf[1] = sig_hi, buf[2] = image_hi;
}

static void
encode_ieee_extended_128 (fmt, buf, r)
     const struct real_format *fmt;
     long *buf;
     const REAL_VALUE_TYPE *r;
{
  buf[3 * !FLOAT_WORDS_BIG_ENDIAN] = 0;
  encode_ieee_extended (fmt, buf+!!FLOAT_WORDS_BIG_ENDIAN, r);
}

static void
decode_ieee_extended (fmt, r, buf)
     const struct real_format *fmt;
     REAL_VALUE_TYPE *r;
     const long *buf;
{
  unsigned long image_hi, sig_hi, sig_lo;
  bool sign;
  int exp;

  if (FLOAT_WORDS_BIG_ENDIAN)
    image_hi = buf[0] >> 16, sig_hi = buf[1], sig_lo = buf[2];
  else
    sig_lo = buf[0], sig_hi = buf[1], image_hi = buf[2];
  sig_lo &= 0xffffffff;
  sig_hi &= 0xffffffff;
  image_hi &= 0xffffffff;

  sign = (image_hi >> 15) & 1;
  exp = image_hi & 0x7fff;

  memset (r, 0, sizeof (*r));

  if (exp == 0)
    {
      if ((sig_hi || sig_lo) && fmt->has_denorm)
	{
	  r->class = rvc_normal;
	  r->sign = sign;

	  /* When the IEEE format contains a hidden bit, we know that
	     it's zero at this point, and so shift up the significand
	     and decrease the exponent to match.  In this case, Motorola
	     defines the explicit integer bit to be valid, so we don't
	     know whether the msb is set or not.  */
	  r->exp = fmt->emin;
	  if (HOST_BITS_PER_LONG == 32)
	    {
	      r->sig[SIGSZ-1] = sig_hi;
	      r->sig[SIGSZ-2] = sig_lo;
	    }
	  else
	    r->sig[SIGSZ-1] = (sig_hi << 31 << 1) | sig_lo;

	  normalize (r);
	}
      else if (fmt->has_signed_zero)
	r->sign = sign;
    }
  else if (exp == 32767 && (fmt->has_nans || fmt->has_inf))
    {
      /* See above re "pseudo-infinities" and "pseudo-nans".
	 Short summary is that the MSB will likely always be
	 set, and that we don't care about it.  */
      sig_hi &= 0x7fffffff;

      if (sig_hi || sig_lo)
	{
	  r->class = rvc_nan;
	  r->sign = sign;
	  if (HOST_BITS_PER_LONG == 32)
	    {
	      r->sig[SIGSZ-1] = sig_hi;
	      r->sig[SIGSZ-2] = sig_lo;
	    }
	  else
	    r->sig[SIGSZ-1] = (sig_hi << 31 << 1) | sig_lo;

	  if (!fmt->qnan_msb_set)
	    r->sig[SIGSZ-1] ^= (SIG_MSB >> 1 | SIG_MSB >> 2);
	}
      else
	{
	  r->class = rvc_inf;
	  r->sign = sign;
	}
    }
  else
    {
      r->class = rvc_normal;
      r->sign = sign;
      r->exp = exp - 16383 + 1;
      if (HOST_BITS_PER_LONG == 32)
	{
	  r->sig[SIGSZ-1] = sig_hi;
	  r->sig[SIGSZ-2] = sig_lo;
	}
      else
	r->sig[SIGSZ-1] = (sig_hi << 31 << 1) | sig_lo;
    }
}

static void
decode_ieee_extended_128 (fmt, r, buf)
     const struct real_format *fmt;
     REAL_VALUE_TYPE *r;
     const long *buf;
{
  decode_ieee_extended (fmt, r, buf+!!FLOAT_WORDS_BIG_ENDIAN);
}

const struct real_format ieee_extended_motorola_format = 
  {
    encode_ieee_extended,
    decode_ieee_extended,
    2,
    1,
    64,
    -16382,
    16384,
    true,
    true,
    true,
    true,
    true
  };

const struct real_format ieee_extended_intel_96_format = 
  {
    encode_ieee_extended,
    decode_ieee_extended,
    2,
    1,
    64,
    -16381,
    16384,
    true,
    true,
    true,
    true,
    true
  };

const struct real_format ieee_extended_intel_128_format = 
  {
    encode_ieee_extended_128,
    decode_ieee_extended_128,
    2,
    1,
    64,
    -16381,
    16384,
    true,
    true,
    true,
    true,
    true
  };

/* The following caters to i386 systems that set the rounding precision
   to 53 bits instead of 64, e.g. FreeBSD.  */
const struct real_format ieee_extended_intel_96_round_53_format = 
  {
    encode_ieee_extended,
    decode_ieee_extended,
    2,
    1,
    53,
    -16381,
    16384,
    true,
    true,
    true,
    true,
    true
  };

/* IBM 128-bit extended precision format: a pair of IEEE double precision
   numbers whose sum is equal to the extended precision value.  The number
   with greater magnitude is first.  This format has the same magnitude
   range as an IEEE double precision value, but effectively 106 bits of
   significand precision.  Infinity and NaN are represented by their IEEE
   double precision value stored in the first number, the second number is
   ignored.  Zeroes, Infinities, and NaNs are set in both doubles
   due to precedent.  */

static void encode_ibm_extended PARAMS ((const struct real_format *fmt,
					 long *, const REAL_VALUE_TYPE *));
static void decode_ibm_extended PARAMS ((const struct real_format *,
					 REAL_VALUE_TYPE *, const long *));

static void
encode_ibm_extended (fmt, buf, r)
     const struct real_format *fmt ATTRIBUTE_UNUSED;
     long *buf;
     const REAL_VALUE_TYPE *r;
{
  REAL_VALUE_TYPE u, v;

  switch (r->class)
    {
    case rvc_zero:
      /* Both doubles have sign bit set.  */
      buf[0] = FLOAT_WORDS_BIG_ENDIAN ? r->sign << 31 : 0;
      buf[1] = FLOAT_WORDS_BIG_ENDIAN ? 0 : r->sign << 31;
      buf[2] = buf[0];
      buf[3] = buf[1];
      break;

    case rvc_inf:
    case rvc_nan:
      /* Both doubles set to Inf / NaN.  */
      encode_ieee_double (&ieee_double_format, &buf[0], r);
      buf[2] = buf[0];
      buf[3] = buf[1];
      return;
      
    case rvc_normal:
      /* u = IEEE double precision portion of significand.  */
      u = *r;
      clear_significand_below (&u, SIGNIFICAND_BITS - 53);

      normalize (&u);
      /* If the upper double is zero, we have a denormal double, so
	 move it to the first double and leave the second as zero.  */
      if (u.class == rvc_zero)
	{
	  v = u;
	  u = *r;
	  normalize (&u);
	}
      else
	{
	  /* v = remainder containing additional 53 bits of significand.  */
	  do_add (&v, r, &u, 1);
	  round_for_format (&ieee_double_format, &v);
	}

      round_for_format (&ieee_double_format, &u);

      encode_ieee_double (&ieee_double_format, &buf[0], &u);
      encode_ieee_double (&ieee_double_format, &buf[2], &v);
      break;

    default:
      abort ();
    }
}

static void
decode_ibm_extended (fmt, r, buf)
     const struct real_format *fmt ATTRIBUTE_UNUSED;
     REAL_VALUE_TYPE *r;
     const long *buf;
{
  REAL_VALUE_TYPE u, v;

  decode_ieee_double (&ieee_double_format, &u, &buf[0]);

  if (u.class != rvc_zero && u.class != rvc_inf && u.class != rvc_nan)
    {
      decode_ieee_double (&ieee_double_format, &v, &buf[2]);
      do_add (r, &u, &v, 0);
    }
  else
    *r = u;
}

const struct real_format ibm_extended_format = 
  {
    encode_ibm_extended,
    decode_ibm_extended,
    2,
    1,
    53 + 53,
    -1021 + 53,
    1024,
    true,
    true,
    true,
    true,
    true
  };


/* IEEE quad precision format.  */

static void encode_ieee_quad PARAMS ((const struct real_format *fmt,
				      long *, const REAL_VALUE_TYPE *));
static void decode_ieee_quad PARAMS ((const struct real_format *,
				      REAL_VALUE_TYPE *, const long *));

static void
encode_ieee_quad (fmt, buf, r)
     const struct real_format *fmt;
     long *buf;
     const REAL_VALUE_TYPE *r;
{
  unsigned long image3, image2, image1, image0, exp;
  bool denormal = (r->sig[SIGSZ-1] & SIG_MSB) == 0;
  REAL_VALUE_TYPE u;

  image3 = r->sign << 31;
  image2 = 0;
  image1 = 0;
  image0 = 0;

  rshift_significand (&u, r, SIGNIFICAND_BITS - 113);

  switch (r->class)
    {
    case rvc_zero:
      break;

    case rvc_inf:
      if (fmt->has_inf)
	image3 |= 32767 << 16;
      else
	{
	  image3 |= 0x7fffffff;
	  image2 = 0xffffffff;
	  image1 = 0xffffffff;
	  image0 = 0xffffffff;
	}
      break;

    case rvc_nan:
      if (fmt->has_nans)
	{
	  image3 |= 32767 << 16;

	  if (HOST_BITS_PER_LONG == 32)
	    {
	      image0 = u.sig[0];
	      image1 = u.sig[1];
	      image2 = u.sig[2];
	      image3 |= u.sig[3] & 0xffff;
	    }
	  else
	    {
	      image0 = u.sig[0];
	      image1 = image0 >> 31 >> 1;
	      image2 = u.sig[1];
	      image3 |= (image2 >> 31 >> 1) & 0xffff;
	      image0 &= 0xffffffff;
	      image2 &= 0xffffffff;
	    }

	  if (!fmt->qnan_msb_set)
	    image3 ^= 1 << 15 | 1 << 14;
	}
      else
	{
	  image3 |= 0x7fffffff;
	  image2 = 0xffffffff;
	  image1 = 0xffffffff;
	  image0 = 0xffffffff;
	}
      break;

    case rvc_normal:
      /* Recall that IEEE numbers are interpreted as 1.F x 2**exp,
	 whereas the intermediate representation is 0.F x 2**exp.
	 Which means we're off by one.  */
      if (denormal)
	exp = 0;
      else
	exp = r->exp + 16383 - 1;
      image3 |= exp << 16;

      if (HOST_BITS_PER_LONG == 32)
	{
	  image0 = u.sig[0];
	  image1 = u.sig[1];
	  image2 = u.sig[2];
	  image3 |= u.sig[3] & 0xffff;
	}
      else
	{
	  image0 = u.sig[0];
	  image1 = image0 >> 31 >> 1;
	  image2 = u.sig[1];
	  image3 |= (image2 >> 31 >> 1) & 0xffff;
	  image0 &= 0xffffffff;
	  image2 &= 0xffffffff;
	}
      break;

    default:
      abort ();
    }

  if (FLOAT_WORDS_BIG_ENDIAN)
    {
      buf[0] = image3;
      buf[1] = image2;
      buf[2] = image1;
      buf[3] = image0;
    }
  else
    {
      buf[0] = image0;
      buf[1] = image1;
      buf[2] = image2;
      buf[3] = image3;
    }
}

static void
decode_ieee_quad (fmt, r, buf)
     const struct real_format *fmt;
     REAL_VALUE_TYPE *r;
     const long *buf;
{
  unsigned long image3, image2, image1, image0;
  bool sign;
  int exp;

  if (FLOAT_WORDS_BIG_ENDIAN)
    {
      image3 = buf[0];
      image2 = buf[1];
      image1 = buf[2];
      image0 = buf[3];
    }
  else
    {
      image0 = buf[0];
      image1 = buf[1];
      image2 = buf[2];
      image3 = buf[3];
    }
  image0 &= 0xffffffff;
  image1 &= 0xffffffff;
  image2 &= 0xffffffff;

  sign = (image3 >> 31) & 1;
  exp = (image3 >> 16) & 0x7fff;
  image3 &= 0xffff;

  memset (r, 0, sizeof (*r));

  if (exp == 0)
    {
      if ((image3 | image2 | image1 | image0) && fmt->has_denorm)
	{
	  r->class = rvc_normal;
	  r->sign = sign;

	  r->exp = -16382 + (SIGNIFICAND_BITS - 112);
	  if (HOST_BITS_PER_LONG == 32)
	    {
	      r->sig[0] = image0;
	      r->sig[1] = image1;
	      r->sig[2] = image2;
	      r->sig[3] = image3;
	    }
	  else
	    {
	      r->sig[0] = (image1 << 31 << 1) | image0;
	      r->sig[1] = (image3 << 31 << 1) | image2;
	    }

	  normalize (r);
	}
      else if (fmt->has_signed_zero)
	r->sign = sign;
    }
  else if (exp == 32767 && (fmt->has_nans || fmt->has_inf))
    {
      if (image3 | image2 | image1 | image0)
	{
	  r->class = rvc_nan;
	  r->sign = sign;

	  if (HOST_BITS_PER_LONG == 32)
	    {
	      r->sig[0] = image0;
	      r->sig[1] = image1;
	      r->sig[2] = image2;
	      r->sig[3] = image3;
	    }
	  else
	    {
	      r->sig[0] = (image1 << 31 << 1) | image0;
	      r->sig[1] = (image3 << 31 << 1) | image2;
	    }
	  lshift_significand (r, r, SIGNIFICAND_BITS - 113);

	  if (!fmt->qnan_msb_set)
	    r->sig[SIGSZ-1] ^= (SIG_MSB >> 1 | SIG_MSB >> 2);
	}
      else
	{
	  r->class = rvc_inf;
	  r->sign = sign;
	}
    }
  else
    {
      r->class = rvc_normal;
      r->sign = sign;
      r->exp = exp - 16383 + 1;

      if (HOST_BITS_PER_LONG == 32)
	{
	  r->sig[0] = image0;
	  r->sig[1] = image1;
	  r->sig[2] = image2;
	  r->sig[3] = image3;
	}
      else
	{
	  r->sig[0] = (image1 << 31 << 1) | image0;
	  r->sig[1] = (image3 << 31 << 1) | image2;
	}
      lshift_significand (r, r, SIGNIFICAND_BITS - 113);
      r->sig[SIGSZ-1] |= SIG_MSB;
    }
}

const struct real_format ieee_quad_format = 
  {
    encode_ieee_quad,
    decode_ieee_quad,
    2,
    1,
    113,
    -16381,
    16384,
    true,
    true,
    true,
    true,
    true
  };

/* Descriptions of VAX floating point formats can be found beginning at

   http://www.openvms.compaq.com:8000/73final/4515/4515pro_013.html#f_floating_point_format

   The thing to remember is that they're almost IEEE, except for word
   order, exponent bias, and the lack of infinities, nans, and denormals.

   We don't implement the H_floating format here, simply because neither
   the VAX or Alpha ports use it.  */
   
static void encode_vax_f PARAMS ((const struct real_format *fmt,
				  long *, const REAL_VALUE_TYPE *));
static void decode_vax_f PARAMS ((const struct real_format *,
				  REAL_VALUE_TYPE *, const long *));
static void encode_vax_d PARAMS ((const struct real_format *fmt,
				  long *, const REAL_VALUE_TYPE *));
static void decode_vax_d PARAMS ((const struct real_format *,
				  REAL_VALUE_TYPE *, const long *));
static void encode_vax_g PARAMS ((const struct real_format *fmt,
				  long *, const REAL_VALUE_TYPE *));
static void decode_vax_g PARAMS ((const struct real_format *,
				  REAL_VALUE_TYPE *, const long *));

static void
encode_vax_f (fmt, buf, r)
     const struct real_format *fmt ATTRIBUTE_UNUSED;
     long *buf;
     const REAL_VALUE_TYPE *r;
{
  unsigned long sign, exp, sig, image;

  sign = r->sign << 15;

  switch (r->class)
    {
    case rvc_zero:
      image = 0;
      break;

    case rvc_inf:
    case rvc_nan:
      image = 0xffff7fff | sign;
      break;

    case rvc_normal:
      sig = (r->sig[SIGSZ-1] >> (HOST_BITS_PER_LONG - 24)) & 0x7fffff;
      exp = r->exp + 128;

      image = (sig << 16) & 0xffff0000;
      image |= sign;
      image |= exp << 7;
      image |= sig >> 16;
      break;

    default:
      abort ();
    }

  buf[0] = image;
}

static void
decode_vax_f (fmt, r, buf)
     const struct real_format *fmt ATTRIBUTE_UNUSED;
     REAL_VALUE_TYPE *r;
     const long *buf;
{
  unsigned long image = buf[0] & 0xffffffff;
  int exp = (image >> 7) & 0xff;

  memset (r, 0, sizeof (*r));

  if (exp != 0)
    {
      r->class = rvc_normal;
      r->sign = (image >> 15) & 1;
      r->exp = exp - 128;

      image = ((image & 0x7f) << 16) | ((image >> 16) & 0xffff);
      r->sig[SIGSZ-1] = (image << (HOST_BITS_PER_LONG - 24)) | SIG_MSB;
    }
}

static void
encode_vax_d (fmt, buf, r)
     const struct real_format *fmt ATTRIBUTE_UNUSED;
     long *buf;
     const REAL_VALUE_TYPE *r;
{
  unsigned long image0, image1, sign = r->sign << 15;

  switch (r->class)
    {
    case rvc_zero:
      image0 = image1 = 0;
      break;

    case rvc_inf:
    case rvc_nan:
      image0 = 0xffff7fff | sign;
      image1 = 0xffffffff;
      break;

    case rvc_normal:
      /* Extract the significand into straight hi:lo.  */
      if (HOST_BITS_PER_LONG == 64)
	{
	  image0 = r->sig[SIGSZ-1];
	  image1 = (image0 >> (64 - 56)) & 0xffffffff;
	  image0 = (image0 >> (64 - 56 + 1) >> 31) & 0x7fffff;
	}
      else
	{
	  image0 = r->sig[SIGSZ-1];
	  image1 = r->sig[SIGSZ-2];
	  image1 = (image0 << 24) | (image1 >> 8);
	  image0 = (image0 >> 8) & 0xffffff;
	}

      /* Rearrange the half-words of the significand to match the
	 external format.  */
      image0 = ((image0 << 16) | (image0 >> 16)) & 0xffff007f;
      image1 = ((image1 << 16) | (image1 >> 16)) & 0xffffffff;

      /* Add the sign and exponent.  */
      image0 |= sign;
      image0 |= (r->exp + 128) << 7;
      break;

    default:
      abort ();
    }

  if (FLOAT_WORDS_BIG_ENDIAN)
    buf[0] = image1, buf[1] = image0;
  else
    buf[0] = image0, buf[1] = image1;
}

static void
decode_vax_d (fmt, r, buf)
     const struct real_format *fmt ATTRIBUTE_UNUSED;
     REAL_VALUE_TYPE *r;
     const long *buf;
{
  unsigned long image0, image1;
  int exp;

  if (FLOAT_WORDS_BIG_ENDIAN)
    image1 = buf[0], image0 = buf[1];
  else
    image0 = buf[0], image1 = buf[1];
  image0 &= 0xffffffff;
  image1 &= 0xffffffff;

  exp = (image0 >> 7) & 0x7f;

  memset (r, 0, sizeof (*r));

  if (exp != 0)
    {
      r->class = rvc_normal;
      r->sign = (image0 >> 15) & 1;
      r->exp = exp - 128;

      /* Rearrange the half-words of the external format into
	 proper ascending order.  */
      image0 = ((image0 & 0x7f) << 16) | ((image0 >> 16) & 0xffff);
      image1 = ((image1 & 0xffff) << 16) | ((image1 >> 16) & 0xffff);

      if (HOST_BITS_PER_LONG == 64)
	{
	  image0 = (image0 << 31 << 1) | image1;
	  image0 <<= 64 - 56;
	  image0 |= SIG_MSB;
	  r->sig[SIGSZ-1] = image0;
	}
      else
	{
	  r->sig[SIGSZ-1] = image0;
	  r->sig[SIGSZ-2] = image1;
	  lshift_significand (r, r, 2*HOST_BITS_PER_LONG - 56);
	  r->sig[SIGSZ-1] |= SIG_MSB;
	}
    }
}

static void
encode_vax_g (fmt, buf, r)
     const struct real_format *fmt ATTRIBUTE_UNUSED;
     long *buf;
     const REAL_VALUE_TYPE *r;
{
  unsigned long image0, image1, sign = r->sign << 15;

  switch (r->class)
    {
    case rvc_zero:
      image0 = image1 = 0;
      break;

    case rvc_inf:
    case rvc_nan:
      image0 = 0xffff7fff | sign;
      image1 = 0xffffffff;
      break;

    case rvc_normal:
      /* Extract the significand into straight hi:lo.  */
      if (HOST_BITS_PER_LONG == 64)
	{
	  image0 = r->sig[SIGSZ-1];
	  image1 = (image0 >> (64 - 53)) & 0xffffffff;
	  image0 = (image0 >> (64 - 53 + 1) >> 31) & 0xfffff;
	}
      else
	{
	  image0 = r->sig[SIGSZ-1];
	  image1 = r->sig[SIGSZ-2];
	  image1 = (image0 << 21) | (image1 >> 11);
	  image0 = (image0 >> 11) & 0xfffff;
	}

      /* Rearrange the half-words of the significand to match the
	 external format.  */
      image0 = ((image0 << 16) | (image0 >> 16)) & 0xffff000f;
      image1 = ((image1 << 16) | (image1 >> 16)) & 0xffffffff;

      /* Add the sign and exponent.  */
      image0 |= sign;
      image0 |= (r->exp + 1024) << 4;
      break;

    default:
      abort ();
    }

  if (FLOAT_WORDS_BIG_ENDIAN)
    buf[0] = image1, buf[1] = image0;
  else
    buf[0] = image0, buf[1] = image1;
}

static void
decode_vax_g (fmt, r, buf)
     const struct real_format *fmt ATTRIBUTE_UNUSED;
     REAL_VALUE_TYPE *r;
     const long *buf;
{
  unsigned long image0, image1;
  int exp;

  if (FLOAT_WORDS_BIG_ENDIAN)
    image1 = buf[0], image0 = buf[1];
  else
    image0 = buf[0], image1 = buf[1];
  image0 &= 0xffffffff;
  image1 &= 0xffffffff;

  exp = (image0 >> 4) & 0x7ff;

  memset (r, 0, sizeof (*r));

  if (exp != 0)
    {
      r->class = rvc_normal;
      r->sign = (image0 >> 15) & 1;
      r->exp = exp - 1024;

      /* Rearrange the half-words of the external format into
	 proper ascending order.  */
      image0 = ((image0 & 0xf) << 16) | ((image0 >> 16) & 0xffff);
      image1 = ((image1 & 0xffff) << 16) | ((image1 >> 16) & 0xffff);

      if (HOST_BITS_PER_LONG == 64)
	{
	  image0 = (image0 << 31 << 1) | image1;
	  image0 <<= 64 - 53;
	  image0 |= SIG_MSB;
	  r->sig[SIGSZ-1] = image0;
	}
      else
	{
	  r->sig[SIGSZ-1] = image0;
	  r->sig[SIGSZ-2] = image1;
	  lshift_significand (r, r, 64 - 53);
	  r->sig[SIGSZ-1] |= SIG_MSB;
	}
    }
}

const struct real_format vax_f_format = 
  {
    encode_vax_f,
    decode_vax_f,
    2,
    1,
    24,
    -127,
    127,
    false,
    false,
    false,
    false,
    false
  };

const struct real_format vax_d_format = 
  {
    encode_vax_d,
    decode_vax_d,
    2,
    1,
    56,
    -127,
    127,
    false,
    false,
    false,
    false,
    false
  };

const struct real_format vax_g_format = 
  {
    encode_vax_g,
    decode_vax_g,
    2,
    1,
    53,
    -1023,
    1023,
    false,
    false,
    false,
    false,
    false
  };

/* A good reference for these can be found in chapter 9 of
   "ESA/390 Principles of Operation", IBM document number SA22-7201-01.
   An on-line version can be found here:

   http://publibz.boulder.ibm.com/cgi-bin/bookmgr_OS390/BOOKS/DZ9AR001/9.1?DT=19930923083613
*/

static void encode_i370_single PARAMS ((const struct real_format *fmt,
					long *, const REAL_VALUE_TYPE *));
static void decode_i370_single PARAMS ((const struct real_format *,
					REAL_VALUE_TYPE *, const long *));
static void encode_i370_double PARAMS ((const struct real_format *fmt,
					long *, const REAL_VALUE_TYPE *));
static void decode_i370_double PARAMS ((const struct real_format *,
					REAL_VALUE_TYPE *, const long *));

static void
encode_i370_single (fmt, buf, r)
     const struct real_format *fmt ATTRIBUTE_UNUSED;
     long *buf;
     const REAL_VALUE_TYPE *r;
{
  unsigned long sign, exp, sig, image;

  sign = r->sign << 31;

  switch (r->class)
    {
    case rvc_zero:
      image = 0;
      break;

    case rvc_inf:
    case rvc_nan:
      image = 0x7fffffff | sign;
      break;

    case rvc_normal:
      sig = (r->sig[SIGSZ-1] >> (HOST_BITS_PER_LONG - 24)) & 0xffffff;
      exp = ((r->exp / 4) + 64) << 24;
      image = sign | exp | sig;
      break;

    default:
      abort ();
    }

  buf[0] = image;
}

static void
decode_i370_single (fmt, r, buf)
     const struct real_format *fmt ATTRIBUTE_UNUSED;
     REAL_VALUE_TYPE *r;
     const long *buf;
{
  unsigned long sign, sig, image = buf[0];
  int exp;

  sign = (image >> 31) & 1;
  exp = (image >> 24) & 0x7f;
  sig = image & 0xffffff;

  memset (r, 0, sizeof (*r));

  if (exp || sig)
    {
      r->class = rvc_normal;
      r->sign = sign;
      r->exp = (exp - 64) * 4;
      r->sig[SIGSZ-1] = sig << (HOST_BITS_PER_LONG - 24);
      normalize (r);
    }
}

static void
encode_i370_double (fmt, buf, r)
     const struct real_format *fmt ATTRIBUTE_UNUSED;
     long *buf;
     const REAL_VALUE_TYPE *r;
{
  unsigned long sign, exp, image_hi, image_lo;

  sign = r->sign << 31;

  switch (r->class)
    {
    case rvc_zero:
      image_hi = image_lo = 0;
      break;

    case rvc_inf:
    case rvc_nan:
      image_hi = 0x7fffffff | sign;
      image_lo = 0xffffffff;
      break;

    case rvc_normal:
      if (HOST_BITS_PER_LONG == 64)
	{
	  image_hi = r->sig[SIGSZ-1];
	  image_lo = (image_hi >> (64 - 56)) & 0xffffffff;
	  image_hi = (image_hi >> (64 - 56 + 1) >> 31) & 0xffffff;
	}
      else
	{
	  image_hi = r->sig[SIGSZ-1];
	  image_lo = r->sig[SIGSZ-2];
	  image_lo = (image_lo >> 8) | (image_hi << 24);
	  image_hi >>= 8;
	}

      exp = ((r->exp / 4) + 64) << 24;
      image_hi |= sign | exp;
      break;

    default:
      abort ();
    }

  if (FLOAT_WORDS_BIG_ENDIAN)
    buf[0] = image_hi, buf[1] = image_lo;
  else
    buf[0] = image_lo, buf[1] = image_hi;
}

static void
decode_i370_double (fmt, r, buf)
     const struct real_format *fmt ATTRIBUTE_UNUSED;
     REAL_VALUE_TYPE *r;
     const long *buf;
{
  unsigned long sign, image_hi, image_lo;
  int exp;

  if (FLOAT_WORDS_BIG_ENDIAN)
    image_hi = buf[0], image_lo = buf[1];
  else
    image_lo = buf[0], image_hi = buf[1];

  sign = (image_hi >> 31) & 1;
  exp = (image_hi >> 24) & 0x7f;
  image_hi &= 0xffffff;
  image_lo &= 0xffffffff;

  memset (r, 0, sizeof (*r));

  if (exp || image_hi || image_lo)
    {
      r->class = rvc_normal;
      r->sign = sign;
      r->exp = (exp - 64) * 4 + (SIGNIFICAND_BITS - 56);

      if (HOST_BITS_PER_LONG == 32)
	{
	  r->sig[0] = image_lo;
	  r->sig[1] = image_hi;
	}
      else
	r->sig[0] = image_lo | (image_hi << 31 << 1);

      normalize (r);
    }
}

const struct real_format i370_single_format =
  {
    encode_i370_single,
    decode_i370_single,
    16,
    4,
    6,
    -64,
    63,
    false,
    false,
    false, /* ??? The encoding does allow for "unnormals".  */
    false, /* ??? The encoding does allow for "unnormals".  */
    false
  };

const struct real_format i370_double_format =
  {
    encode_i370_double,
    decode_i370_double,
    16,
    4,
    14,
    -64,
    63,
    false,
    false,
    false, /* ??? The encoding does allow for "unnormals".  */
    false, /* ??? The encoding does allow for "unnormals".  */
    false
  };

/* The "twos-complement" c4x format is officially defined as

	x = s(~s).f * 2**e

   This is rather misleading.  One must remember that F is signed.
   A better description would be

	x = -1**s * ((s + 1 + .f) * 2**e

   So if we have a (4 bit) fraction of .1000 with a sign bit of 1,
   that's -1 * (1+1+(-.5)) == -1.5.  I think.

   The constructions here are taken from Tables 5-1 and 5-2 of the
   TMS320C4x User's Guide wherein step-by-step instructions for
   conversion from IEEE are presented.  That's close enough to our
   internal representation so as to make things easy.

   See http://www-s.ti.com/sc/psheets/spru063c/spru063c.pdf  */

static void encode_c4x_single PARAMS ((const struct real_format *fmt,
				       long *, const REAL_VALUE_TYPE *));
static void decode_c4x_single PARAMS ((const struct real_format *,
				       REAL_VALUE_TYPE *, const long *));
static void encode_c4x_extended PARAMS ((const struct real_format *fmt,
					 long *, const REAL_VALUE_TYPE *));
static void decode_c4x_extended PARAMS ((const struct real_format *,
					 REAL_VALUE_TYPE *, const long *));

static void
encode_c4x_single (fmt, buf, r)
     const struct real_format *fmt ATTRIBUTE_UNUSED;
     long *buf;
     const REAL_VALUE_TYPE *r;
{
  unsigned long image, exp, sig;
  
  switch (r->class)
    {
    case rvc_zero:
      exp = -128;
      sig = 0;
      break;

    case rvc_inf:
    case rvc_nan:
      exp = 127;
      sig = 0x800000 - r->sign;
      break;

    case rvc_normal:
      exp = r->exp - 1;
      sig = (r->sig[SIGSZ-1] >> (HOST_BITS_PER_LONG - 24)) & 0x7fffff;
      if (r->sign)
	{
	  if (sig)
	    sig = -sig;
	  else
	    exp--;
	  sig |= 0x800000;
	}
      break;

    default:
      abort ();
    }

  image = ((exp & 0xff) << 24) | (sig & 0xffffff);
  buf[0] = image;
}

static void
decode_c4x_single (fmt, r, buf)
     const struct real_format *fmt ATTRIBUTE_UNUSED;
     REAL_VALUE_TYPE *r;
     const long *buf;
{
  unsigned long image = buf[0];
  unsigned long sig;
  int exp, sf;

  exp = (((image >> 24) & 0xff) ^ 0x80) - 0x80;
  sf = ((image & 0xffffff) ^ 0x800000) - 0x800000;

  memset (r, 0, sizeof (*r));

  if (exp != -128)
    {
      r->class = rvc_normal;

      sig = sf & 0x7fffff;
      if (sf < 0)
	{
	  r->sign = 1;
	  if (sig)
	    sig = -sig;
	  else
	    exp++;
	}
      sig = (sig << (HOST_BITS_PER_LONG - 24)) | SIG_MSB;

      r->exp = exp + 1;
      r->sig[SIGSZ-1] = sig;
    }
}

static void
encode_c4x_extended (fmt, buf, r)
     const struct real_format *fmt ATTRIBUTE_UNUSED;
     long *buf;
     const REAL_VALUE_TYPE *r;
{
  unsigned long exp, sig;
  
  switch (r->class)
    {
    case rvc_zero:
      exp = -128;
      sig = 0;
      break;

    case rvc_inf:
    case rvc_nan:
      exp = 127;
      sig = 0x80000000 - r->sign;
      break;

    case rvc_normal:
      exp = r->exp - 1;

      sig = r->sig[SIGSZ-1];
      if (HOST_BITS_PER_LONG == 64)
	sig = sig >> 1 >> 31;
      sig &= 0x7fffffff;

      if (r->sign)
	{
	  if (sig)
	    sig = -sig;
	  else
	    exp--;
	  sig |= 0x80000000;
	}
      break;

    default:
      abort ();
    }

  exp = (exp & 0xff) << 24;
  sig &= 0xffffffff;

  if (FLOAT_WORDS_BIG_ENDIAN)
    buf[0] = exp, buf[1] = sig;
  else
    buf[0] = sig, buf[0] = exp;
}

static void
decode_c4x_extended (fmt, r, buf)
     const struct real_format *fmt ATTRIBUTE_UNUSED;
     REAL_VALUE_TYPE *r;
     const long *buf;
{
  unsigned long sig;
  int exp, sf;

  if (FLOAT_WORDS_BIG_ENDIAN)
    exp = buf[0], sf = buf[1];
  else
    sf = buf[0], exp = buf[1];

  exp = (((exp >> 24) & 0xff) & 0x80) - 0x80;
  sf = ((sf & 0xffffffff) ^ 0x80000000) - 0x80000000;

  memset (r, 0, sizeof (*r));

  if (exp != -128)
    {
      r->class = rvc_normal;

      sig = sf & 0x7fffffff;
      if (sf < 0)
	{
	  r->sign = 1;
	  if (sig)
	    sig = -sig;
	  else
	    exp++;
	}
      if (HOST_BITS_PER_LONG == 64)
	sig = sig << 1 << 31;
      sig |= SIG_MSB;

      r->exp = exp + 1;
      r->sig[SIGSZ-1] = sig;
    }
}

const struct real_format c4x_single_format = 
  {
    encode_c4x_single,
    decode_c4x_single,
    2,
    1,
    24,
    -126,
    128,
    false,
    false,
    false,
    false,
    false
  };

const struct real_format c4x_extended_format = 
  {
    encode_c4x_extended,
    decode_c4x_extended,
    2,
    1,
    32,
    -126,
    128,
    false,
    false,
    false,
    false,
    false
  };


/* A synthetic "format" for internal arithmetic.  It's the size of the
   internal significand minus the two bits needed for proper rounding.
   The encode and decode routines exist only to satisfy our paranoia
   harness.  */

static void encode_internal PARAMS ((const struct real_format *fmt,
				     long *, const REAL_VALUE_TYPE *));
static void decode_internal PARAMS ((const struct real_format *,
				     REAL_VALUE_TYPE *, const long *));

static void
encode_internal (fmt, buf, r)
     const struct real_format *fmt ATTRIBUTE_UNUSED;
     long *buf;
     const REAL_VALUE_TYPE *r;
{
  memcpy (buf, r, sizeof (*r));
}

static void
decode_internal (fmt, r, buf)
     const struct real_format *fmt ATTRIBUTE_UNUSED;
     REAL_VALUE_TYPE *r;
     const long *buf;
{
  memcpy (r, buf, sizeof (*r));
}

const struct real_format real_internal_format = 
  {
    encode_internal,
    decode_internal,
    2,
    1,
    SIGNIFICAND_BITS - 2,
    -MAX_EXP,
    MAX_EXP,
    true,
    true,
    false,
    true,
    true 
  };

/* Set up default mode to format mapping for IEEE.  Everyone else has
   to set these values in OVERRIDE_OPTIONS.  */

const struct real_format *real_format_for_mode[TFmode - QFmode + 1] =
{
  NULL,				/* QFmode */
  NULL,				/* HFmode */
  NULL,				/* TQFmode */
  &ieee_single_format,		/* SFmode */
  &ieee_double_format,		/* DFmode */

  /* We explicitly don't handle XFmode.  There are two formats,
     pretty much equally common.  Choose one in OVERRIDE_OPTIONS.  */
  NULL,				/* XFmode */
  &ieee_quad_format		/* TFmode */
};
