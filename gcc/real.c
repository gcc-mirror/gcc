/* real.c - software floating point emulation.
   Copyright (C) 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000, 2002 Free Software Foundation, Inc.
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

   Both of these requirements are easily satisfied.  The largest
   target significand is 113 bits; we store 128.  The smallest
   denormal number fits in 17 exponent bits; we store 29.

   Target floating point models that use base 16 instead of base 2
   (i.e. IBM 370), are handled during round_for_format, in which we
   canonicalize the exponent to be a multiple of 4 (log2(16)), and
   adjust the significand to match.  */


/* Enumerate the special cases of numbers that we encounter.  */
enum real_value_class {
  rvc_zero,
  rvc_normal,
  rvc_inf,
  rvc_nan
};

/* Used to classify two numbers simultaneously.  */
#define CLASS2(A, B)  ((A) << 2 | (B))

/* An expanded form of the represented number.  */

#define SIGNIFICAND_BITS	128
#define EXP_BITS		(32 - 3)
#define MAX_EXP			((1 << (EXP_BITS - 1)) - 1)
#define SIGSZ			(SIGNIFICAND_BITS / HOST_BITS_PER_LONG)
#define SIG_MSB			((unsigned long)1 << (HOST_BITS_PER_LONG - 1))

#if HOST_BITS_PER_LONG != 64 && HOST_BITS_PER_LONG != 32
 #error "Some constant folding done by hand to avoid shift count warnings"
#endif

struct real_value
{
  enum real_value_class class : 2;
  unsigned int sign : 1;
  int exp : EXP_BITS;
  unsigned long sig[SIGSZ];
};

/* Describes the properties of the specific target format in use.  */
struct real_format
{
  /* Move to and from the target bytes.  */
  void (*encode) (const struct real_format *, long *,
		  const struct real_value *);
  void (*decode) (const struct real_format *, struct real_value *,
		  const long *);

  /* The radix of the exponent and digits of the significand.  */
  int b;

  /* log2(b).  */
  int log2_b;

  /* Size of the significand in digits of radix B.  */
  int p;

  /* The minimum negative integer, x, such that b**(x-1) is normalized.  */
  int emin;

  /* The maximum integer, x, such that b**(x-1) is representable.  */
  int emax;

  /* Properties of the format.  */
  bool has_nans;
  bool has_inf;
  bool has_denorm;
  bool has_signed_zero;
  bool qnan_msb_set;
};


static const struct real_format *fmt_for_mode[TFmode - QFmode + 1];


static void get_zero PARAMS ((struct real_value *, int));
static void get_canonical_qnan PARAMS ((struct real_value *, int));
static void get_canonical_snan PARAMS ((struct real_value *, int));
static void get_inf PARAMS ((struct real_value *, int));
static void sticky_rshift_significand PARAMS ((struct real_value *,
					       const struct real_value *,
					       unsigned int));
static void rshift_significand PARAMS ((struct real_value *,
					const struct real_value *,
					unsigned int));
static void lshift_significand PARAMS ((struct real_value *,
					const struct real_value *,
					unsigned int));
static void lshift_significand_1 PARAMS ((struct real_value *,
					  const struct real_value *));
static bool add_significands PARAMS ((struct real_value *r,
				      const struct real_value *,
				      const struct real_value *));
static bool sub_significands PARAMS ((struct real_value *,
				      const struct real_value *,
				      const struct real_value *));
static void neg_significand PARAMS ((struct real_value *,
				     const struct real_value *));
static int cmp_significands PARAMS ((const struct real_value *,
				     const struct real_value *));
static void set_significand_bit PARAMS ((struct real_value *, unsigned int));
static void clear_significand_bit PARAMS ((struct real_value *, unsigned int));
static bool test_significand_bit PARAMS ((struct real_value *, unsigned int));
static void clear_significand_below PARAMS ((struct real_value *,
					     unsigned int));
static bool div_significands PARAMS ((struct real_value *,
				      const struct real_value *,
				      const struct real_value *));
static void normalize PARAMS ((struct real_value *));

static void do_add PARAMS ((struct real_value *, const struct real_value *,
			    const struct real_value *, int));
static void do_multiply PARAMS ((struct real_value *,
				 const struct real_value *,
				 const struct real_value *));
static void do_divide PARAMS ((struct real_value *, const struct real_value *,
			       const struct real_value *));
static int do_compare PARAMS ((const struct real_value *,
			       const struct real_value *, int));
static void do_fix_trunc PARAMS ((struct real_value *,
				  const struct real_value *));

static const struct real_value * ten_to_ptwo PARAMS ((int));
static const struct real_value * real_digit PARAMS ((int));

static void round_for_format PARAMS ((const struct real_format *,
				      struct real_value *));

/* Initialize R with a positive zero.  */

static inline void
get_zero (r, sign)
     struct real_value *r;
     int sign;
{
  memset (r, 0, sizeof (*r));
  r->sign = sign;
}

/* Initialize R with the canonical quiet NaN.  */

static inline void
get_canonical_qnan (r, sign)
     struct real_value *r;
     int sign;
{
  memset (r, 0, sizeof (*r));
  r->class = rvc_nan;
  r->sign = sign;
  r->sig[SIGSZ-1] = SIG_MSB >> 1;
}

static inline void
get_canonical_snan (r, sign)
     struct real_value *r;
     int sign;
{
  memset (r, 0, sizeof (*r));
  r->class = rvc_nan;
  r->sign = sign;
  r->sig[SIGSZ-1] = SIG_MSB >> 2;
}

static inline void
get_inf (r, sign)
     struct real_value *r;
     int sign;
{
  memset (r, 0, sizeof (*r));
  r->class = rvc_inf;
  r->sign = sign;
}


/* Right-shift the significand of A by N bits; put the result in the
   significand of R.  If any one bits are shifted out, set the least
   significant bit of R.  */

static void
sticky_rshift_significand (r, a, n)
     struct real_value *r;
     const struct real_value *a;
     unsigned int n;
{
  unsigned long sticky = 0;
  unsigned int i, ofs = 0;

  if (n >= HOST_BITS_PER_LONG)
    {
      for (i = 0, ofs = n / HOST_BITS_PER_LONG; i < ofs; ++i)
	sticky |= a->sig[i];
      n -= ofs * HOST_BITS_PER_LONG;
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

  r->sig[0] |= (sticky != 0);
}

/* Right-shift the significand of A by N bits; put the result in the
   significand of R.  */

static void
rshift_significand (r, a, n)
     struct real_value *r;
     const struct real_value *a;
     unsigned int n;
{
  unsigned int i, ofs = n / HOST_BITS_PER_LONG;

  n -= ofs * HOST_BITS_PER_LONG;
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
     struct real_value *r;
     const struct real_value *a;
     unsigned int n;
{
  unsigned int i, ofs = n / HOST_BITS_PER_LONG;

  n -= ofs * HOST_BITS_PER_LONG;
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
     struct real_value *r;
     const struct real_value *a;
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
     struct real_value *r;
     const struct real_value *a, *b;
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

/* Subtract the significands of A and B, placing the result in R.
   Return true if there was carry out of the most significant word.  */

static inline bool
sub_significands (r, a, b)
     struct real_value *r;
     const struct real_value *a, *b;
{
  bool carry = false;
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
     struct real_value *r;
     const struct real_value *a;
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
     const struct real_value *a, *b;
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

/* Set bit N of the significand of R.  */

static inline void
set_significand_bit (r, n)
     struct real_value *r;
     unsigned int n;
{
  r->sig[n / HOST_BITS_PER_LONG]
    |= (unsigned long)1 << (n % HOST_BITS_PER_LONG);
}

/* Clear bit N of the significand of R.  */

static inline void
clear_significand_bit (r, n)
     struct real_value *r;
     unsigned int n;
{
  r->sig[n / HOST_BITS_PER_LONG]
    &= ~((unsigned long)1 << (n % HOST_BITS_PER_LONG));
}

/* Test bit N of the significand of R.  */

static inline bool
test_significand_bit (r, n)
     struct real_value *r;
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
     struct real_value *r;
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
     struct real_value *r;
     const struct real_value *a, *b;
{
  struct real_value u;
  int bit = SIGNIFICAND_BITS - 1;
  int i;
  long inexact;

  u = *a;
  memset (r->sig, 0, sizeof (r->sig));

  goto start;
  do
    {
      if ((u.sig[SIGSZ-1] & SIG_MSB) == 0)
	{
	  lshift_significand_1 (&u, &u);
	start:
	  if (cmp_significands (&u, b) >= 0)
	    {
	      sub_significands (&u, &u, b);
	      set_significand_bit (r, bit);
	    }
	}
      else
	{
	  /* We lose a bit here, and thus know the next quotient bit
	     will be one.  */
	  lshift_significand_1 (&u, &u);
	  sub_significands (&u, &u, b);
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
     struct real_value *r;
{
  int shift = 0, exp;
  int i, j;

  /* Find the first word that is non-zero.  */
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

  /* Find the first bit that is non-zero.  */
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
     struct real_value *r;
     const struct real_value *a, *b;
     int subtract_p;
{
  int dexp, sign, exp;
  struct real_value t;

  /* Determine if we need to add or subtract.  */
  sign = a->sign;
  subtract_p = (sign ^ b->sign) ^ subtract_p;

  switch (CLASS2 (a->class, b->class))
    {
    case CLASS2 (rvc_zero, rvc_zero):
      /* +-0 +/- +-0 = +0.  */
      get_zero (r, 0);
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
      const struct real_value *t;
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

      sticky_rshift_significand (&t, b, dexp);
      b = &t;
    }

  if (subtract_p)
    {
      if (sub_significands (r, a, b))
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
	  sticky_rshift_significand (r, r, 1);
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
}

/* Return R = A * B.  */

static void
do_multiply (r, a, b)
     struct real_value *r;
     const struct real_value *a, *b;
{
  struct real_value u, t, *rr;
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

  u.class = rvc_normal;
  u.sign = 0;

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
     struct real_value *r;
     const struct real_value *a, *b;
{
  int exp, sign = a->sign ^ b->sign;
  struct real_value t, *rr;
  bool inexact;

  switch (CLASS2 (a->class, b->class))
    {
    case CLASS2 (rvc_zero, rvc_zero):
      /* 0 / 0 = NaN.  */
    case CLASS2 (rvc_inf, rvc_zero):
      /* Inf / 0 = NaN.  */
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
  rr->sig[0] |= inexact;

  /* Re-normalize the result.  */
  normalize (rr);

  if (rr != r)
    *r = t;
}

/* Return a tri-state comparison of A vs B.  Return NAN_RESULT if
   one of the two operands is a NaN.  */

static int
do_compare (a, b, nan_result)
     const struct real_value *a, *b;
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

void
do_fix_trunc (r, a)
     struct real_value *r;
     const struct real_value *a;
{
  *r = *a;

  switch (a->class)
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
real_arithmetic (tr, icode, top0, top1)
     REAL_VALUE_TYPE *tr;
     int icode;
     const REAL_VALUE_TYPE *top0, *top1;
{
  struct real_value *r = (struct real_value *) tr;
  const struct real_value *op0 = (const struct real_value *) top0;
  const struct real_value *op1 = (const struct real_value *) top1;
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
real_arithmetic2 (icode, top0, top1)
     int icode;
     const REAL_VALUE_TYPE *top0, *top1;
{
  REAL_VALUE_TYPE r;
  real_arithmetic (&r, icode, top0, top1);
  return r;
}

bool
real_compare (icode, top0, top1)
     int icode;
     const REAL_VALUE_TYPE *top0, *top1;
{
  enum tree_code code = icode;
  const struct real_value *op0 = (const struct real_value *) top0;
  const struct real_value *op1 = (const struct real_value *) top1;

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
real_exponent (tr)
     const REAL_VALUE_TYPE *tr;
{
  const struct real_value *r = (const struct real_value *) tr;

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
real_ldexp (tr, top0, exp)
     REAL_VALUE_TYPE *tr;
     const REAL_VALUE_TYPE *top0;
     int exp;
{
  struct real_value *r = (struct real_value *) tr;
  const struct real_value *op0 = (const struct real_value *) top0;

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
real_isinf (tr)
     const REAL_VALUE_TYPE *tr;
{
  const struct real_value *r = (const struct real_value *) tr;
  return (r->class == rvc_inf);
}

/* Determine whether a floating-point value X is a NaN.  */

bool
real_isnan (tr)
     const REAL_VALUE_TYPE *tr;
{
  const struct real_value *r = (const struct real_value *) tr;
  return (r->class == rvc_nan);
}

/* Determine whether a floating-point value X is negative.  */

bool
real_isneg (tr)
     const REAL_VALUE_TYPE *tr;
{
  const struct real_value *r = (const struct real_value *) tr;
  return r->sign;
}

/* Determine whether a floating-point value X is minus zero.  */

bool
real_isnegzero (tr)
     const REAL_VALUE_TYPE *tr;
{
  const struct real_value *r = (const struct real_value *) tr;
  return r->sign && r->class == rvc_zero;
}

/* Compare two floating-point objects for bitwise identity.  */

extern bool
real_identical (ta, tb)
     const REAL_VALUE_TYPE *ta, *tb;
{
  const struct real_value *a = (const struct real_value *) ta;
  const struct real_value *b = (const struct real_value *) tb;
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
exact_real_inverse (mode, tr)
     enum machine_mode mode;
     REAL_VALUE_TYPE *tr;
{
  const struct real_value *one = real_digit (1);
  struct real_value *r = (struct real_value *) tr;
  struct real_value u;
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
  real_convert ((REAL_VALUE_TYPE *)&u, mode, (REAL_VALUE_TYPE *)&u);
  
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
real_to_integer (tr)
     const REAL_VALUE_TYPE *tr;
{
  const struct real_value *r = (const struct real_value *) tr;
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
real_to_integer2 (plow, phigh, tr)
     HOST_WIDE_INT *plow, *phigh;
     const REAL_VALUE_TYPE *tr;
{
  struct real_value r;
  HOST_WIDE_INT low, high;
  int exp;

  r = *(const struct real_value *) tr;
  switch (r.class)
    {
    case rvc_zero:
    underflow:
      low = high = 0;
      break;

    case rvc_inf:
    case rvc_nan:
    overflow:
      high = (unsigned HOST_WIDE_INT) 1 << (HOST_BITS_PER_WIDE_INT - 1);
      if (r.sign)
	low = 0;
      else
	{
	  high--;
	  low = -1;
	}
      break;

    case rvc_normal:
      exp = r.exp;
      if (exp <= 0)
	goto underflow;
      if (exp >= 2*HOST_BITS_PER_WIDE_INT)
	goto overflow;

      rshift_significand (&r, &r, 2*HOST_BITS_PER_WIDE_INT - exp);
      if (HOST_BITS_PER_WIDE_INT == HOST_BITS_PER_LONG)
	{
	  high = r.sig[SIGSZ-1];
	  low = r.sig[SIGSZ-2];
	}
      else if (HOST_BITS_PER_WIDE_INT == 2*HOST_BITS_PER_LONG)
	{
	  high = r.sig[SIGSZ-1];
	  high = high << (HOST_BITS_PER_LONG - 1) << 1;
	  high |= r.sig[SIGSZ-2];

	  low = r.sig[SIGSZ-3];
	  low = low << (HOST_BITS_PER_LONG - 1) << 1;
	  low |= r.sig[SIGSZ-4];
	}
      else
	abort ();

      if (r.sign)
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

/* Render R as a decimal floating point constant.  Emit DIGITS
   significant digits in the result.  If DIGITS <= 0, choose the
   maximum for the representation.  */

#define M_LOG10_2	0.30102999566398119521

void
real_to_decimal (str, r_orig, digits)
     char *str;
     const REAL_VALUE_TYPE *r_orig;
     int digits;
{
  struct real_value r;
  const struct real_value *one, *ten;
  int dec_exp, max_digits, d, cmp_half;
  char *p, *first, *last;
  bool sign;

  r = *(const struct real_value *)r_orig;
  switch (r.class)
    {
    case rvc_zero:
      strcpy (str, (r.sign ? "-0.0" : "0.0"));
      return;
    case rvc_normal:
      break;
    case rvc_inf:
      strcpy (str, (r.sign ? "+Inf" : "-Inf"));
      return;
    case rvc_nan:
      /* ??? Print the significand as well, if not canonical?  */
      strcpy (str, (r.sign ? "+NaN" : "-NaN"));
      return;
    default:
      abort ();
    }

  max_digits = SIGNIFICAND_BITS * M_LOG10_2;
  if (digits <= 0 || digits > max_digits)
    digits = max_digits;

  one = real_digit (1);
  ten = ten_to_ptwo (0);

  sign = r.sign;
  r.sign = 0;

  /* Estimate the decimal exponent.  */
  dec_exp = r.exp * M_LOG10_2;
  
  /* Scale the number such that it is in [1, 10).  */
  if (dec_exp > 0)
    {
      int i;
      for (i = EXP_BITS - 1; i >= 0; --i)
	if (dec_exp & (1 << i))
	  do_divide (&r, &r, ten_to_ptwo (i));
    }
  else if (dec_exp < 0)
    {
      int i, pos_exp = -(--dec_exp);

      for (i = EXP_BITS - 1; i >= 0; --i)
	if (pos_exp & (1 << i))
	  do_multiply (&r, &r, ten_to_ptwo (i));
    }

  /* Assert that the number is in the proper range.  Round-off can
     prevent the above from working exactly.  */
  if (do_compare (&r, one, -1) < 0)
    {
      do_multiply (&r, &r, ten);
      dec_exp--;
    }
  else if (do_compare (&r, ten, 1) >= 0)
    {
      do_divide (&r, &r, ten);
      dec_exp++;
    }

  p = str;
  if (sign)
    *p++ = '-';
  first = p++;
  while (1)
    {
      d = real_to_integer ((const REAL_VALUE_TYPE *) &r);
      do_add (&r, &r, real_digit (d), 1);

      *p++ = d + '0';
      if (--digits == 0)
	break;
      do_multiply (&r, &r, ten);
    }
  last = p;

  /* Round the result.  Compare R vs 0.5 by doing R*2 vs 1.0.  */
  r.exp += 1;
  cmp_half = do_compare (&r, one, -1);
  if (cmp_half == 0)
    /* Round to even.  */
    cmp_half += d & 1;
  if (cmp_half > 0)
    {
      while (p > first)
	{
	  d = *--p;
	  if (d == '9')
	    *p = '0';
	  else
	    {
	      *p = d + 1;
	      break;
	    }
	}

      if (p == first)
	{
	  first[1] = '1';
	  dec_exp++;
	}
    }
  
  first[0] = first[1];
  first[1] = '.';

  sprintf (last, "e%+d", dec_exp);
}

/* Render R as a hexadecimal floating point constant.  Emit DIGITS
   significant digits in the result.  If DIGITS <= 0, choose the maximum
   for the representation.  */

void
real_to_hexadecimal (str, tr, digits)
     char *str;
     const REAL_VALUE_TYPE *tr;
     int digits;
{
  struct real_value r;
  int i, j;
  char *p;

  r = *(const struct real_value *) tr;

  switch (r.class)
    {
    case rvc_zero:
      r.exp = 0;
      break;
    case rvc_normal:
      break;
    case rvc_inf:
      strcpy (str, (r.sign ? "+Inf" : "-Inf"));
      return;
    case rvc_nan:
      /* ??? Print the significand as well, if not canonical?  */
      strcpy (str, (r.sign ? "+NaN" : "-NaN"));
      return;
    default:
      abort ();
    }

  if (digits <= 0)
    digits = SIGNIFICAND_BITS / 4;

  p = str;
  if (r.sign)
    *p++ = '-';
  *p++ = '0';
  *p++ = 'x';
  *p++ = '0';
  *p++ = '.';

  for (i = SIGSZ - 1; i >= 0; --i)
    for (j = HOST_BITS_PER_LONG - 4; j >= 0; j -= 4)
      {
	*p++ = "0123456789abcdef"[(r.sig[i] >> j) & 15];
	if (--digits == 0)
	  goto out;
      }
 out:
  sprintf (p, "p%+d", r.exp);
}

/* Initialize R from a decimal or hexadecimal string.  The string is
   assumed to have been syntax checked already.  */

void
real_from_string (tr, str)
     REAL_VALUE_TYPE *tr;
     const char *str;
{
  struct real_value *r = (struct real_value *) tr;
  int exp = 0;

  get_zero (r, 0);

  if (*str == '-')
    {
      r->sign = 1;
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
	  int exp_neg = 0;

	  str++;
	  if (*str == '-')
	    {
	      exp_neg = 1;
	      str++;
	    }
	  else if (*str == '+')
	    str++;

	  d = 0;
	  while (ISDIGIT (*str))
	    {
	      int t = d;
	      d *= 10;
	      d += *str - '0';
	      if (d < t)
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
      if (r->exp != exp)
	{
	  if (exp < 0)
	    goto underflow;
	  else
	    goto overflow;
	}

      normalize (r);
    }
  else
    {
      /* Decimal floating point.  */
      const struct real_value *ten = ten_to_ptwo (0);
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
	  int exp_neg = 0;

	  str++;
	  if (*str == '-')
	    {
	      exp_neg = 1;
	      str++;
	    }
	  else if (*str == '+')
	    str++;

	  d = 0;
	  while (ISDIGIT (*str))
	    {
	      int t = d;
	      d *= 10;
	      d += *str - '0';
	      if (d < t)
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

      if (exp < 0)
	{
	  exp = -exp;
	  for (d = 0; d < EXP_BITS; ++d)
	    if (exp & (1 << d))
	      do_divide (r, r, ten_to_ptwo (d));
	}
      else if (exp > 0)
	{
	  for (d = 0; d < EXP_BITS; ++d)
	    if (exp & (1 << d))
	      do_multiply (r, r, ten_to_ptwo (d));
	}
    }

  return;

 underflow:
  get_zero (r, r->sign);
  return;

 overflow:
  get_inf (r, r->sign);
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
real_from_integer (tr, mode, low, high, unsigned_p)
     REAL_VALUE_TYPE *tr;
     enum machine_mode mode;
     unsigned HOST_WIDE_INT low;
     HOST_WIDE_INT high;
     int unsigned_p;
{
  struct real_value *r = (struct real_value *) tr;

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
    real_convert (tr, mode, tr);
}

/* Returns 10**2**n.  */

static const struct real_value *
ten_to_ptwo (n)
     int n;
{
  static struct real_value tens[EXP_BITS];

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

	  real_from_integer ((REAL_VALUE_TYPE *) &tens[n], VOIDmode, t, 0, 1);
	}
      else
	{
	  const struct real_value *t = ten_to_ptwo (n - 1);
	  do_multiply (&tens[n], t, t);
	}
    }

  return &tens[n];
}

/* Returns N.  */

static const struct real_value *
real_digit (n)
     int n;
{
  static struct real_value num[10];

  if (n < 0 || n > 9)
    abort ();

  if (n > 0 && num[n].class == rvc_zero)
    real_from_integer ((REAL_VALUE_TYPE *) &num[n], VOIDmode, n, 0, 1);

  return &num[n];
}

/* Fills R with +Inf.  */

void
real_inf (tr)
     REAL_VALUE_TYPE *tr;
{
  get_inf ((struct real_value *)tr, 0);
}

/* Fills R with a NaN whose significand is described by STR.  If QUIET,
   we force a QNaN, else we force an SNaN.  The string, if not empty,
   is parsed as a number and placed in the significand.  Return true
   if the string was successfully parsed.  */

bool
real_nan (tr, str, quiet, mode)
     REAL_VALUE_TYPE *tr;
     const char *str;
     int quiet;
     enum machine_mode mode;
{
  struct real_value *r = (struct real_value *) tr;
  const struct real_format *fmt;

  fmt = fmt_for_mode[mode - QFmode];
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
	  struct real_value u;

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
real_2expN (tr, n)
     REAL_VALUE_TYPE *tr;
     int n;
{
  struct real_value *r = (struct real_value *) tr;

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
     struct real_value *r;
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
	  sticky_rshift_significand (r, r, shift);
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
          sticky_rshift_significand (r, r, diff);
          r->exp += diff;
        }
    }

  /* There are P2 true significand bits, followed by one guard bit,
     followed by one sticky bit, followed by stuff.  Fold non-zero
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
      struct real_value u;
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
		  sticky_rshift_significand (r, r, shift);
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
real_convert (tr, mode, ta)
     REAL_VALUE_TYPE *tr;
     enum machine_mode mode;
     const REAL_VALUE_TYPE *ta;
{
  struct real_value *r = (struct real_value *)tr;
  const struct real_value *a = (const struct real_value *)ta;
  const struct real_format *fmt;

  fmt = fmt_for_mode[mode - QFmode];
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
exact_real_truncate (mode, ta)
     enum machine_mode mode;
     const REAL_VALUE_TYPE *ta;
{
  REAL_VALUE_TYPE t;
  real_convert (&t, mode, ta);
  return real_identical (&t, ta);
}

/* Write R to the target format of MODE.  Place the words of the 
   result in target word order in BUF.  There are always 32 bits
   in each long, no matter the size of the host long.

   Legacy: return word 0 for implementing REAL_VALUE_TO_TARGET_SINGLE.  */

long
real_to_target (buf, tr, mode)
     long *buf;
     const REAL_VALUE_TYPE *tr;
     enum machine_mode mode;
{
  struct real_value r;
  const struct real_format *fmt;
  long buf1;

  r = *(const struct real_value *) tr;

  fmt = fmt_for_mode[mode - QFmode];
  if (fmt == NULL)
    abort ();

  round_for_format (fmt, &r);
  if (!buf)
    buf = &buf1;
  (*fmt->encode) (fmt, buf, &r);

  return *buf;
}

/* Read R from the target format of MODE.  Read the words of the
   result in target word order in BUF.  There are always 32 bits
   in each long, no matter the size of the host long.  */

void
real_from_target (tr, buf, mode)
     REAL_VALUE_TYPE *tr;
     const long *buf;
     enum machine_mode mode;
{
  struct real_value *r = (struct real_value *) tr;
  const struct real_format *fmt;

  fmt = fmt_for_mode[mode - QFmode];
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

  fmt = fmt_for_mode[mode - QFmode];
  if (fmt == NULL)
    return 0;

  return fmt->p * fmt->log2_b;
}

/* IEEE single-precision format.  */

static void encode_ieee_single PARAMS ((const struct real_format *fmt,
					long *, const struct real_value *));
static void decode_ieee_single PARAMS ((const struct real_format *,
					struct real_value *, const long *));

static void
encode_ieee_single (fmt, buf, r)
     const struct real_format *fmt;
     long *buf;
     const struct real_value *r;
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
    }

  buf[0] = image;
}

static void
decode_ieee_single (fmt, r, buf)
     const struct real_format *fmt;
     struct real_value *r;
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

const struct real_format ieee_single = 
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
					long *, const struct real_value *));
static void decode_ieee_double PARAMS ((const struct real_format *,
					struct real_value *, const long *));

static void
encode_ieee_double (fmt, buf, r)
     const struct real_format *fmt;
     long *buf;
     const struct real_value *r;
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
    }

  if (FLOAT_WORDS_BIG_ENDIAN)
    buf[0] = image_hi, buf[1] = image_lo;
  else
    buf[0] = image_lo, buf[1] = image_hi;
}

static void
decode_ieee_double (fmt, r, buf)
     const struct real_format *fmt;
     struct real_value *r;
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

const struct real_format ieee_double = 
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
					  long *, const struct real_value *));
static void decode_ieee_extended PARAMS ((const struct real_format *,
					  struct real_value *, const long *));

static void encode_ieee_extended_128 PARAMS ((const struct real_format *fmt,
					      long *,
					      const struct real_value *));
static void decode_ieee_extended_128 PARAMS ((const struct real_format *,
					      struct real_value *,
					      const long *));

static void
encode_ieee_extended (fmt, buf, r)
     const struct real_format *fmt;
     long *buf;
     const struct real_value *r;
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
     const struct real_value *r;
{
  buf[3 * !FLOAT_WORDS_BIG_ENDIAN] = 0;
  encode_ieee_extended (fmt, buf+!!FLOAT_WORDS_BIG_ENDIAN, r);
}

static void
decode_ieee_extended (fmt, r, buf)
     const struct real_format *fmt;
     struct real_value *r;
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
     struct real_value *r;
     const long *buf;
{
  decode_ieee_extended (fmt, r, buf+!!FLOAT_WORDS_BIG_ENDIAN);
}

const struct real_format ieee_extended_motorola = 
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

const struct real_format ieee_extended_intel_96 = 
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

const struct real_format ieee_extended_intel_128 = 
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


/* IEEE quad precision format.  */

static void encode_ieee_quad PARAMS ((const struct real_format *fmt,
				      long *, const struct real_value *));
static void decode_ieee_quad PARAMS ((const struct real_format *,
				      struct real_value *, const long *));

static void
encode_ieee_quad (fmt, buf, r)
     const struct real_format *fmt;
     long *buf;
     const struct real_value *r;
{
  unsigned long image3, image2, image1, image0, exp;
  bool denormal = (r->sig[SIGSZ-1] & SIG_MSB) == 0;
  struct real_value u;

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
     struct real_value *r;
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

const struct real_format ieee_quad = 
  {
    encode_ieee_quad,
    decode_ieee_quad,
    2,
    1,
    113,
    -16382,
    16384,
    true,
    true,
    true,
    true,
    true
  };


/* The VAX floating point formats.  */

static void encode_vax_f PARAMS ((const struct real_format *fmt,
				  long *, const struct real_value *));
static void decode_vax_f PARAMS ((const struct real_format *,
				  struct real_value *, const long *));
static void encode_vax_d PARAMS ((const struct real_format *fmt,
				  long *, const struct real_value *));
static void decode_vax_d PARAMS ((const struct real_format *,
				  struct real_value *, const long *));
static void encode_vax_g PARAMS ((const struct real_format *fmt,
				  long *, const struct real_value *));
static void decode_vax_g PARAMS ((const struct real_format *,
				  struct real_value *, const long *));

static void
encode_vax_f (fmt, buf, r)
     const struct real_format *fmt ATTRIBUTE_UNUSED;
     long *buf;
     const struct real_value *r;
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
    }

  buf[0] = image;
}

static void
decode_vax_f (fmt, r, buf)
     const struct real_format *fmt ATTRIBUTE_UNUSED;
     struct real_value *r;
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
     const struct real_value *r;
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
    }

  if (FLOAT_WORDS_BIG_ENDIAN)
    buf[0] = image1, buf[1] = image0;
  else
    buf[0] = image0, buf[1] = image1;
}

static void
decode_vax_d (fmt, r, buf)
     const struct real_format *fmt ATTRIBUTE_UNUSED;
     struct real_value *r;
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
     const struct real_value *r;
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
    }

  if (FLOAT_WORDS_BIG_ENDIAN)
    buf[0] = image1, buf[1] = image0;
  else
    buf[0] = image0, buf[1] = image1;
}

static void
decode_vax_g (fmt, r, buf)
     const struct real_format *fmt ATTRIBUTE_UNUSED;
     struct real_value *r;
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


/* The IBM S/390 floating point formats.  A good reference for these can
   be found in chapter 9 of "ESA/390 Principles of Operation", IBM document
   number SA22-7201-01.  An on-line version can be found here:

   http://publibz.boulder.ibm.com/cgi-bin/bookmgr_OS390/BOOKS/DZ9AR001/9.1?DT=19930923083613
*/

static void encode_i370_single PARAMS ((const struct real_format *fmt,
					long *, const struct real_value *));
static void decode_i370_single PARAMS ((const struct real_format *,
					struct real_value *, const long *));
static void encode_i370_double PARAMS ((const struct real_format *fmt,
					long *, const struct real_value *));
static void decode_i370_double PARAMS ((const struct real_format *,
					struct real_value *, const long *));

static void
encode_i370_single (fmt, buf, r)
     const struct real_format *fmt ATTRIBUTE_UNUSED;
     long *buf;
     const struct real_value *r;
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
    }

  buf[0] = image;
}

static void
decode_i370_single (fmt, r, buf)
     const struct real_format *fmt ATTRIBUTE_UNUSED;
     struct real_value *r;
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
     const struct real_value *r;
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
    }

  if (FLOAT_WORDS_BIG_ENDIAN)
    buf[0] = image_hi, buf[1] = image_lo;
  else
    buf[0] = image_lo, buf[1] = image_hi;
}

static void
decode_i370_double (fmt, r, buf)
     const struct real_format *fmt ATTRIBUTE_UNUSED;
     struct real_value *r;
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

const struct real_format i370_single =
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

const struct real_format i370_double =
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


/* TMS320C[34]x twos complement floating point format.  */

static void encode_c4x_single PARAMS ((const struct real_format *fmt,
				       long *, const struct real_value *));
static void decode_c4x_single PARAMS ((const struct real_format *,
				       struct real_value *, const long *));
static void encode_c4x_extended PARAMS ((const struct real_format *fmt,
					 long *, const struct real_value *));
static void decode_c4x_extended PARAMS ((const struct real_format *,
					 struct real_value *, const long *));

static void
encode_c4x_single (fmt, buf, r)
     const struct real_format *fmt ATTRIBUTE_UNUSED;
     long *buf;
     const struct real_value *r;
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
    }

  image = ((exp & 0xff) << 24) | (sig & 0xffffff);
  buf[0] = image;
}

static void
decode_c4x_single (fmt, r, buf)
     const struct real_format *fmt ATTRIBUTE_UNUSED;
     struct real_value *r;
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
     const struct real_value *r;
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
     struct real_value *r;
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

const struct real_format c4x_single = 
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

const struct real_format c4x_extended = 
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


/* Initialize things at start of compilation.  */

static const struct real_format * format_for_size PARAMS ((int));

static const struct real_format *
format_for_size (size)
     int size;
{
#ifndef TARGET_G_FORMAT
#define TARGET_G_FORMAT 0
#endif

  switch (TARGET_FLOAT_FORMAT)
    {
    case IEEE_FLOAT_FORMAT:
      switch (size)
	{
	case 32:
	  return &ieee_single;

	case 64:
	  return &ieee_double;

	case 96:
	  if (!INTEL_EXTENDED_IEEE_FORMAT)
	    return &ieee_extended_motorola;
	  else
	    return &ieee_extended_intel_96;

	case 128:
	  if (!INTEL_EXTENDED_IEEE_FORMAT)
	    return &ieee_quad;
	  else
	    return &ieee_extended_intel_128;
	}
      break;

    case VAX_FLOAT_FORMAT:
      switch (size)
	{
	case 32:
	  return &vax_f_format;

	case 64:
	  if (TARGET_G_FORMAT)
	    return &vax_g_format;
	  else
	    return &vax_d_format;
	}
      break;

    case IBM_FLOAT_FORMAT:
      switch (size)
	{
	case 32:
	  return &i370_single;
	case 64:
	  return &i370_double;
	}
      break;

    case C4X_FLOAT_FORMAT:
      switch (size)
	{
	case 32:
	  return &c4x_single;
	case 64:
	  return &c4x_extended;
	}
      break;
    }

  abort ();
}

void
init_real_once ()
{
  int i;

  /* Set up the mode->format table.  */
  for (i = 0; i < 3; ++i)
    {
      enum machine_mode mode;
      int size;

      if (i == 0)
	size = FLOAT_TYPE_SIZE;
      else if (i == 1)
	size = DOUBLE_TYPE_SIZE;
      else
	size = LONG_DOUBLE_TYPE_SIZE;

      mode = mode_for_size (size, MODE_FLOAT, 0);
      if (mode == BLKmode)
	abort ();

      fmt_for_mode[mode - QFmode] = format_for_size (size);
    }
}
