/* Operations with long integers.
   Copyright (C) 2006, 2007, 2009, 2010 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.

GCC is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"			/* For SHIFT_COUNT_TRUNCATED.  */
#include "tree.h"

/* We know that A1 + B1 = SUM1, using 2's complement arithmetic and ignoring
   overflow.  Suppose A, B and SUM have the same respective signs as A1, B1,
   and SUM1.  Then this yields nonzero if overflow occurred during the
   addition.

   Overflow occurs if A and B have the same sign, but A and SUM differ in
   sign.  Use `^' to test whether signs differ, and `< 0' to isolate the
   sign.  */
#define OVERFLOW_SUM_SIGN(a, b, sum) ((~((a) ^ (b)) & ((a) ^ (sum))) < 0)

/* To do constant folding on INTEGER_CST nodes requires two-word arithmetic.
   We do that by representing the two-word integer in 4 words, with only
   HOST_BITS_PER_WIDE_INT / 2 bits stored in each word, as a positive
   number.  The value of the word is LOWPART + HIGHPART * BASE.  */

#define LOWPART(x) \
  ((x) & (((unsigned HOST_WIDE_INT) 1 << (HOST_BITS_PER_WIDE_INT / 2)) - 1))
#define HIGHPART(x) \
  ((unsigned HOST_WIDE_INT) (x) >> HOST_BITS_PER_WIDE_INT / 2)
#define BASE ((unsigned HOST_WIDE_INT) 1 << HOST_BITS_PER_WIDE_INT / 2)

/* Unpack a two-word integer into 4 words.
   LOW and HI are the integer, as two `HOST_WIDE_INT' pieces.
   WORDS points to the array of HOST_WIDE_INTs.  */

static void
encode (HOST_WIDE_INT *words, unsigned HOST_WIDE_INT low, HOST_WIDE_INT hi)
{
  words[0] = LOWPART (low);
  words[1] = HIGHPART (low);
  words[2] = LOWPART (hi);
  words[3] = HIGHPART (hi);
}

/* Pack an array of 4 words into a two-word integer.
   WORDS points to the array of words.
   The integer is stored into *LOW and *HI as two `HOST_WIDE_INT' pieces.  */

static void
decode (HOST_WIDE_INT *words, unsigned HOST_WIDE_INT *low,
	HOST_WIDE_INT *hi)
{
  *low = words[0] + words[1] * BASE;
  *hi = words[2] + words[3] * BASE;
}

/* Add two doubleword integers with doubleword result.
   Return nonzero if the operation overflows according to UNSIGNED_P.
   Each argument is given as two `HOST_WIDE_INT' pieces.
   One argument is L1 and H1; the other, L2 and H2.
   The value is stored as two `HOST_WIDE_INT' pieces in *LV and *HV.  */

int
add_double_with_sign (unsigned HOST_WIDE_INT l1, HOST_WIDE_INT h1,
		      unsigned HOST_WIDE_INT l2, HOST_WIDE_INT h2,
		      unsigned HOST_WIDE_INT *lv, HOST_WIDE_INT *hv,
		      bool unsigned_p)
{
  unsigned HOST_WIDE_INT l;
  HOST_WIDE_INT h;

  l = l1 + l2;
  h = (HOST_WIDE_INT) ((unsigned HOST_WIDE_INT) h1
		       + (unsigned HOST_WIDE_INT) h2
		       + (l < l1));

  *lv = l;
  *hv = h;

  if (unsigned_p)
    return ((unsigned HOST_WIDE_INT) h < (unsigned HOST_WIDE_INT) h1
	    || (h == h1
		&& l < l1));
  else
    return OVERFLOW_SUM_SIGN (h1, h2, h);
}

/* Negate a doubleword integer with doubleword result.
   Return nonzero if the operation overflows, assuming it's signed.
   The argument is given as two `HOST_WIDE_INT' pieces in L1 and H1.
   The value is stored as two `HOST_WIDE_INT' pieces in *LV and *HV.  */

int
neg_double (unsigned HOST_WIDE_INT l1, HOST_WIDE_INT h1,
	    unsigned HOST_WIDE_INT *lv, HOST_WIDE_INT *hv)
{
  if (l1 == 0)
    {
      *lv = 0;
      *hv = - h1;
      return (*hv & h1) < 0;
    }
  else
    {
      *lv = -l1;
      *hv = ~h1;
      return 0;
    }
}

/* Multiply two doubleword integers with doubleword result.
   Return nonzero if the operation overflows according to UNSIGNED_P.
   Each argument is given as two `HOST_WIDE_INT' pieces.
   One argument is L1 and H1; the other, L2 and H2.
   The value is stored as two `HOST_WIDE_INT' pieces in *LV and *HV.  */

int
mul_double_with_sign (unsigned HOST_WIDE_INT l1, HOST_WIDE_INT h1,
		      unsigned HOST_WIDE_INT l2, HOST_WIDE_INT h2,
		      unsigned HOST_WIDE_INT *lv, HOST_WIDE_INT *hv,
		      bool unsigned_p)
{
  HOST_WIDE_INT arg1[4];
  HOST_WIDE_INT arg2[4];
  HOST_WIDE_INT prod[4 * 2];
  unsigned HOST_WIDE_INT carry;
  int i, j, k;
  unsigned HOST_WIDE_INT toplow, neglow;
  HOST_WIDE_INT tophigh, neghigh;

  encode (arg1, l1, h1);
  encode (arg2, l2, h2);

  memset (prod, 0, sizeof prod);

  for (i = 0; i < 4; i++)
    {
      carry = 0;
      for (j = 0; j < 4; j++)
	{
	  k = i + j;
	  /* This product is <= 0xFFFE0001, the sum <= 0xFFFF0000.  */
	  carry += arg1[i] * arg2[j];
	  /* Since prod[p] < 0xFFFF, this sum <= 0xFFFFFFFF.  */
	  carry += prod[k];
	  prod[k] = LOWPART (carry);
	  carry = HIGHPART (carry);
	}
      prod[i + 4] = carry;
    }

  decode (prod, lv, hv);
  decode (prod + 4, &toplow, &tophigh);

  /* Unsigned overflow is immediate.  */
  if (unsigned_p)
    return (toplow | tophigh) != 0;

  /* Check for signed overflow by calculating the signed representation of the
     top half of the result; it should agree with the low half's sign bit.  */
  if (h1 < 0)
    {
      neg_double (l2, h2, &neglow, &neghigh);
      add_double (neglow, neghigh, toplow, tophigh, &toplow, &tophigh);
    }
  if (h2 < 0)
    {
      neg_double (l1, h1, &neglow, &neghigh);
      add_double (neglow, neghigh, toplow, tophigh, &toplow, &tophigh);
    }
  return (*hv < 0 ? ~(toplow & tophigh) : toplow | tophigh) != 0;
}

/* Shift the doubleword integer in L1, H1 right by COUNT places
   keeping only PREC bits of result.  ARITH nonzero specifies
   arithmetic shifting; otherwise use logical shift.
   Store the value as two `HOST_WIDE_INT' pieces in *LV and *HV.  */

static void
rshift_double (unsigned HOST_WIDE_INT l1, HOST_WIDE_INT h1,
	       unsigned HOST_WIDE_INT count, unsigned int prec,
	       unsigned HOST_WIDE_INT *lv, HOST_WIDE_INT *hv,
	       bool arith)
{
  unsigned HOST_WIDE_INT signmask;

  signmask = (arith
	      ? -((unsigned HOST_WIDE_INT) h1 >> (HOST_BITS_PER_WIDE_INT - 1))
	      : 0);

  if (SHIFT_COUNT_TRUNCATED)
    count %= prec;

  if (count >= 2 * HOST_BITS_PER_WIDE_INT)
    {
      /* Shifting by the host word size is undefined according to the
	 ANSI standard, so we must handle this as a special case.  */
      *hv = 0;
      *lv = 0;
    }
  else if (count >= HOST_BITS_PER_WIDE_INT)
    {
      *hv = 0;
      *lv = (unsigned HOST_WIDE_INT) h1 >> (count - HOST_BITS_PER_WIDE_INT);
    }
  else
    {
      *hv = (unsigned HOST_WIDE_INT) h1 >> count;
      *lv = ((l1 >> count)
	     | ((unsigned HOST_WIDE_INT) h1
		<< (HOST_BITS_PER_WIDE_INT - count - 1) << 1));
    }

  /* Zero / sign extend all bits that are beyond the precision.  */

  if (count >= prec)
    {
      *hv = signmask;
      *lv = signmask;
    }
  else if ((prec - count) >= 2 * HOST_BITS_PER_WIDE_INT)
    ;
  else if ((prec - count) >= HOST_BITS_PER_WIDE_INT)
    {
      *hv &= ~((HOST_WIDE_INT) (-1) << (prec - count - HOST_BITS_PER_WIDE_INT));
      *hv |= signmask << (prec - count - HOST_BITS_PER_WIDE_INT);
    }
  else
    {
      *hv = signmask;
      *lv &= ~((unsigned HOST_WIDE_INT) (-1) << (prec - count));
      *lv |= signmask << (prec - count);
    }
}

/* Shift the doubleword integer in L1, H1 left by COUNT places
   keeping only PREC bits of result.
   Shift right if COUNT is negative.
   ARITH nonzero specifies arithmetic shifting; otherwise use logical shift.
   Store the value as two `HOST_WIDE_INT' pieces in *LV and *HV.  */

void
lshift_double (unsigned HOST_WIDE_INT l1, HOST_WIDE_INT h1,
	       HOST_WIDE_INT count, unsigned int prec,
	       unsigned HOST_WIDE_INT *lv, HOST_WIDE_INT *hv, bool arith)
{
  unsigned HOST_WIDE_INT signmask;

  if (count < 0)
    {
      rshift_double (l1, h1, absu_hwi (count), prec, lv, hv, arith);
      return;
    }

  if (SHIFT_COUNT_TRUNCATED)
    count %= prec;

  if (count >= 2 * HOST_BITS_PER_WIDE_INT)
    {
      /* Shifting by the host word size is undefined according to the
	 ANSI standard, so we must handle this as a special case.  */
      *hv = 0;
      *lv = 0;
    }
  else if (count >= HOST_BITS_PER_WIDE_INT)
    {
      *hv = l1 << (count - HOST_BITS_PER_WIDE_INT);
      *lv = 0;
    }
  else
    {
      *hv = (((unsigned HOST_WIDE_INT) h1 << count)
	     | (l1 >> (HOST_BITS_PER_WIDE_INT - count - 1) >> 1));
      *lv = l1 << count;
    }

  /* Sign extend all bits that are beyond the precision.  */

  signmask = -((prec > HOST_BITS_PER_WIDE_INT
		? ((unsigned HOST_WIDE_INT) *hv
		   >> (prec - HOST_BITS_PER_WIDE_INT - 1))
		: (*lv >> (prec - 1))) & 1);

  if (prec >= 2 * HOST_BITS_PER_WIDE_INT)
    ;
  else if (prec >= HOST_BITS_PER_WIDE_INT)
    {
      *hv &= ~((HOST_WIDE_INT) (-1) << (prec - HOST_BITS_PER_WIDE_INT));
      *hv |= signmask << (prec - HOST_BITS_PER_WIDE_INT);
    }
  else
    {
      *hv = signmask;
      *lv &= ~((unsigned HOST_WIDE_INT) (-1) << prec);
      *lv |= signmask << prec;
    }
}

/* Divide doubleword integer LNUM, HNUM by doubleword integer LDEN, HDEN
   for a quotient (stored in *LQUO, *HQUO) and remainder (in *LREM, *HREM).
   CODE is a tree code for a kind of division, one of
   TRUNC_DIV_EXPR, FLOOR_DIV_EXPR, CEIL_DIV_EXPR, ROUND_DIV_EXPR
   or EXACT_DIV_EXPR
   It controls how the quotient is rounded to an integer.
   Return nonzero if the operation overflows.
   UNS nonzero says do unsigned division.  */

int
div_and_round_double (unsigned code, int uns,
		      /* num == numerator == dividend */
		      unsigned HOST_WIDE_INT lnum_orig,
		      HOST_WIDE_INT hnum_orig,
		      /* den == denominator == divisor */
		      unsigned HOST_WIDE_INT lden_orig,
		      HOST_WIDE_INT hden_orig,
		      unsigned HOST_WIDE_INT *lquo,
		      HOST_WIDE_INT *hquo, unsigned HOST_WIDE_INT *lrem,
		      HOST_WIDE_INT *hrem)
{
  int quo_neg = 0;
  HOST_WIDE_INT num[4 + 1];	/* extra element for scaling.  */
  HOST_WIDE_INT den[4], quo[4];
  int i, j;
  unsigned HOST_WIDE_INT work;
  unsigned HOST_WIDE_INT carry = 0;
  unsigned HOST_WIDE_INT lnum = lnum_orig;
  HOST_WIDE_INT hnum = hnum_orig;
  unsigned HOST_WIDE_INT lden = lden_orig;
  HOST_WIDE_INT hden = hden_orig;
  int overflow = 0;

  if (hden == 0 && lden == 0)
    overflow = 1, lden = 1;

  /* Calculate quotient sign and convert operands to unsigned.  */
  if (!uns)
    {
      if (hnum < 0)
	{
	  quo_neg = ~ quo_neg;
	  /* (minimum integer) / (-1) is the only overflow case.  */
	  if (neg_double (lnum, hnum, &lnum, &hnum)
	      && ((HOST_WIDE_INT) lden & hden) == -1)
	    overflow = 1;
	}
      if (hden < 0)
	{
	  quo_neg = ~ quo_neg;
	  neg_double (lden, hden, &lden, &hden);
	}
    }

  if (hnum == 0 && hden == 0)
    {				/* single precision */
      *hquo = *hrem = 0;
      /* This unsigned division rounds toward zero.  */
      *lquo = lnum / lden;
      goto finish_up;
    }

  if (hnum == 0)
    {				/* trivial case: dividend < divisor */
      /* hden != 0 already checked.  */
      *hquo = *lquo = 0;
      *hrem = hnum;
      *lrem = lnum;
      goto finish_up;
    }

  memset (quo, 0, sizeof quo);

  memset (num, 0, sizeof num);	/* to zero 9th element */
  memset (den, 0, sizeof den);

  encode (num, lnum, hnum);
  encode (den, lden, hden);

  /* Special code for when the divisor < BASE.  */
  if (hden == 0 && lden < (unsigned HOST_WIDE_INT) BASE)
    {
      /* hnum != 0 already checked.  */
      for (i = 4 - 1; i >= 0; i--)
	{
	  work = num[i] + carry * BASE;
	  quo[i] = work / lden;
	  carry = work % lden;
	}
    }
  else
    {
      /* Full double precision division,
	 with thanks to Don Knuth's "Seminumerical Algorithms".  */
      int num_hi_sig, den_hi_sig;
      unsigned HOST_WIDE_INT quo_est, scale;

      /* Find the highest nonzero divisor digit.  */
      for (i = 4 - 1;; i--)
	if (den[i] != 0)
	  {
	    den_hi_sig = i;
	    break;
	  }

      /* Insure that the first digit of the divisor is at least BASE/2.
	 This is required by the quotient digit estimation algorithm.  */

      scale = BASE / (den[den_hi_sig] + 1);
      if (scale > 1)
	{		/* scale divisor and dividend */
	  carry = 0;
	  for (i = 0; i <= 4 - 1; i++)
	    {
	      work = (num[i] * scale) + carry;
	      num[i] = LOWPART (work);
	      carry = HIGHPART (work);
	    }

	  num[4] = carry;
	  carry = 0;
	  for (i = 0; i <= 4 - 1; i++)
	    {
	      work = (den[i] * scale) + carry;
	      den[i] = LOWPART (work);
	      carry = HIGHPART (work);
	      if (den[i] != 0) den_hi_sig = i;
	    }
	}

      num_hi_sig = 4;

      /* Main loop */
      for (i = num_hi_sig - den_hi_sig - 1; i >= 0; i--)
	{
	  /* Guess the next quotient digit, quo_est, by dividing the first
	     two remaining dividend digits by the high order quotient digit.
	     quo_est is never low and is at most 2 high.  */
	  unsigned HOST_WIDE_INT tmp;

	  num_hi_sig = i + den_hi_sig + 1;
	  work = num[num_hi_sig] * BASE + num[num_hi_sig - 1];
	  if (num[num_hi_sig] != den[den_hi_sig])
	    quo_est = work / den[den_hi_sig];
	  else
	    quo_est = BASE - 1;

	  /* Refine quo_est so it's usually correct, and at most one high.  */
	  tmp = work - quo_est * den[den_hi_sig];
	  if (tmp < BASE
	      && (den[den_hi_sig - 1] * quo_est
		  > (tmp * BASE + num[num_hi_sig - 2])))
	    quo_est--;

	  /* Try QUO_EST as the quotient digit, by multiplying the
	     divisor by QUO_EST and subtracting from the remaining dividend.
	     Keep in mind that QUO_EST is the I - 1st digit.  */

	  carry = 0;
	  for (j = 0; j <= den_hi_sig; j++)
	    {
	      work = quo_est * den[j] + carry;
	      carry = HIGHPART (work);
	      work = num[i + j] - LOWPART (work);
	      num[i + j] = LOWPART (work);
	      carry += HIGHPART (work) != 0;
	    }

	  /* If quo_est was high by one, then num[i] went negative and
	     we need to correct things.  */
	  if (num[num_hi_sig] < (HOST_WIDE_INT) carry)
	    {
	      quo_est--;
	      carry = 0;		/* add divisor back in */
	      for (j = 0; j <= den_hi_sig; j++)
		{
		  work = num[i + j] + den[j] + carry;
		  carry = HIGHPART (work);
		  num[i + j] = LOWPART (work);
		}

	      num [num_hi_sig] += carry;
	    }

	  /* Store the quotient digit.  */
	  quo[i] = quo_est;
	}
    }

  decode (quo, lquo, hquo);

 finish_up:
  /* If result is negative, make it so.  */
  if (quo_neg)
    neg_double (*lquo, *hquo, lquo, hquo);

  /* Compute trial remainder:  rem = num - (quo * den)  */
  mul_double (*lquo, *hquo, lden_orig, hden_orig, lrem, hrem);
  neg_double (*lrem, *hrem, lrem, hrem);
  add_double (lnum_orig, hnum_orig, *lrem, *hrem, lrem, hrem);

  switch (code)
    {
    case TRUNC_DIV_EXPR:
    case TRUNC_MOD_EXPR:	/* round toward zero */
    case EXACT_DIV_EXPR:	/* for this one, it shouldn't matter */
      return overflow;

    case FLOOR_DIV_EXPR:
    case FLOOR_MOD_EXPR:	/* round toward negative infinity */
      if (quo_neg && (*lrem != 0 || *hrem != 0))   /* ratio < 0 && rem != 0 */
	{
	  /* quo = quo - 1;  */
	  add_double (*lquo, *hquo, (HOST_WIDE_INT) -1, (HOST_WIDE_INT)  -1,
		      lquo, hquo);
	}
      else
	return overflow;
      break;

    case CEIL_DIV_EXPR:
    case CEIL_MOD_EXPR:		/* round toward positive infinity */
      if (!quo_neg && (*lrem != 0 || *hrem != 0))  /* ratio > 0 && rem != 0 */
	{
	  add_double (*lquo, *hquo, (HOST_WIDE_INT) 1, (HOST_WIDE_INT) 0,
		      lquo, hquo);
	}
      else
	return overflow;
      break;

    case ROUND_DIV_EXPR:
    case ROUND_MOD_EXPR:	/* round to closest integer */
      {
	unsigned HOST_WIDE_INT labs_rem = *lrem;
	HOST_WIDE_INT habs_rem = *hrem;
	unsigned HOST_WIDE_INT labs_den = lden, ltwice;
	HOST_WIDE_INT habs_den = hden, htwice;

	/* Get absolute values.  */
	if (*hrem < 0)
	  neg_double (*lrem, *hrem, &labs_rem, &habs_rem);
	if (hden < 0)
	  neg_double (lden, hden, &labs_den, &habs_den);

	/* If (2 * abs (lrem) >= abs (lden)), adjust the quotient.  */
	mul_double ((HOST_WIDE_INT) 2, (HOST_WIDE_INT) 0,
		    labs_rem, habs_rem, &ltwice, &htwice);

	if (((unsigned HOST_WIDE_INT) habs_den
	     < (unsigned HOST_WIDE_INT) htwice)
	    || (((unsigned HOST_WIDE_INT) habs_den
		 == (unsigned HOST_WIDE_INT) htwice)
		&& (labs_den <= ltwice)))
	  {
	    if (*hquo < 0)
	      /* quo = quo - 1;  */
	      add_double (*lquo, *hquo,
			  (HOST_WIDE_INT) -1, (HOST_WIDE_INT) -1, lquo, hquo);
	    else
	      /* quo = quo + 1; */
	      add_double (*lquo, *hquo, (HOST_WIDE_INT) 1, (HOST_WIDE_INT) 0,
			  lquo, hquo);
	  }
	else
	  return overflow;
      }
      break;

    default:
      gcc_unreachable ();
    }

  /* Compute true remainder:  rem = num - (quo * den)  */
  mul_double (*lquo, *hquo, lden_orig, hden_orig, lrem, hrem);
  neg_double (*lrem, *hrem, lrem, hrem);
  add_double (lnum_orig, hnum_orig, *lrem, *hrem, lrem, hrem);
  return overflow;
}


/* Returns mask for PREC bits.  */

double_int
double_int_mask (unsigned prec)
{
  unsigned HOST_WIDE_INT m;
  double_int mask;

  if (prec > HOST_BITS_PER_WIDE_INT)
    {
      prec -= HOST_BITS_PER_WIDE_INT;
      m = ((unsigned HOST_WIDE_INT) 2 << (prec - 1)) - 1;
      mask.high = (HOST_WIDE_INT) m;
      mask.low = ALL_ONES;
    }
  else
    {
      mask.high = 0;
      mask.low = ((unsigned HOST_WIDE_INT) 2 << (prec - 1)) - 1;
    }

  return mask;
}

/* Clears the bits of CST over the precision PREC.  If UNS is false, the bits
   outside of the precision are set to the sign bit (i.e., the PREC-th one),
   otherwise they are set to zero.

   This corresponds to returning the value represented by PREC lowermost bits
   of CST, with the given signedness.  */

double_int
double_int_ext (double_int cst, unsigned prec, bool uns)
{
  if (uns)
    return double_int_zext (cst, prec);
  else
    return double_int_sext (cst, prec);
}

/* The same as double_int_ext with UNS = true.  */

double_int
double_int_zext (double_int cst, unsigned prec)
{
  double_int mask = double_int_mask (prec);
  double_int r;

  r.low = cst.low & mask.low;
  r.high = cst.high & mask.high;

  return r;
}

/* The same as double_int_ext with UNS = false.  */

double_int
double_int_sext (double_int cst, unsigned prec)
{
  double_int mask = double_int_mask (prec);
  double_int r;
  unsigned HOST_WIDE_INT snum;

  if (prec <= HOST_BITS_PER_WIDE_INT)
    snum = cst.low;
  else
    {
      prec -= HOST_BITS_PER_WIDE_INT;
      snum = (unsigned HOST_WIDE_INT) cst.high;
    }
  if (((snum >> (prec - 1)) & 1) == 1)
    {
      r.low = cst.low | ~mask.low;
      r.high = cst.high | ~mask.high;
    }
  else
    {
      r.low = cst.low & mask.low;
      r.high = cst.high & mask.high;
    }

  return r;
}

/* Returns true if CST fits in signed HOST_WIDE_INT.  */

bool
double_int_fits_in_shwi_p (double_int cst)
{
  if (cst.high == 0)
    return (HOST_WIDE_INT) cst.low >= 0;
  else if (cst.high == -1)
    return (HOST_WIDE_INT) cst.low < 0;
  else
    return false;
}

/* Returns true if CST fits in HOST_WIDE_INT if UNS is false, or in
   unsigned HOST_WIDE_INT if UNS is true.  */

bool
double_int_fits_in_hwi_p (double_int cst, bool uns)
{
  if (uns)
    return double_int_fits_in_uhwi_p (cst);
  else
    return double_int_fits_in_shwi_p (cst);
}

/* Returns A * B.  */

double_int
double_int_mul (double_int a, double_int b)
{
  double_int ret;
  mul_double (a.low, a.high, b.low, b.high, &ret.low, &ret.high);
  return ret;
}

/* Returns A * B. If the operation overflows according to UNSIGNED_P,
   *OVERFLOW is set to nonzero.  */

double_int
double_int_mul_with_sign (double_int a, double_int b,
                          bool unsigned_p, int *overflow)
{
  double_int ret;
  *overflow = mul_double_with_sign (a.low, a.high, b.low, b.high,
                                    &ret.low, &ret.high, unsigned_p);
  return ret;
}

/* Returns A + B.  */

double_int
double_int_add (double_int a, double_int b)
{
  double_int ret;
  add_double (a.low, a.high, b.low, b.high, &ret.low, &ret.high);
  return ret;
}

/* Returns A - B.  */

double_int
double_int_sub (double_int a, double_int b)
{
  double_int ret;
  neg_double (b.low, b.high, &b.low, &b.high);
  add_double (a.low, a.high, b.low, b.high, &ret.low, &ret.high);
  return ret;
}

/* Returns -A.  */

double_int
double_int_neg (double_int a)
{
  double_int ret;
  neg_double (a.low, a.high, &ret.low, &ret.high);
  return ret;
}

/* Returns A / B (computed as unsigned depending on UNS, and rounded as
   specified by CODE).  CODE is enum tree_code in fact, but double_int.h
   must be included before tree.h.  The remainder after the division is
   stored to MOD.  */

double_int
double_int_divmod (double_int a, double_int b, bool uns, unsigned code,
		   double_int *mod)
{
  double_int ret;

  div_and_round_double (code, uns, a.low, a.high,
			b.low, b.high, &ret.low, &ret.high,
			&mod->low, &mod->high);
  return ret;
}

/* The same as double_int_divmod with UNS = false.  */

double_int
double_int_sdivmod (double_int a, double_int b, unsigned code, double_int *mod)
{
  return double_int_divmod (a, b, false, code, mod);
}

/* The same as double_int_divmod with UNS = true.  */

double_int
double_int_udivmod (double_int a, double_int b, unsigned code, double_int *mod)
{
  return double_int_divmod (a, b, true, code, mod);
}

/* Returns A / B (computed as unsigned depending on UNS, and rounded as
   specified by CODE).  CODE is enum tree_code in fact, but double_int.h
   must be included before tree.h.  */

double_int
double_int_div (double_int a, double_int b, bool uns, unsigned code)
{
  double_int mod;

  return double_int_divmod (a, b, uns, code, &mod);
}

/* The same as double_int_div with UNS = false.  */

double_int
double_int_sdiv (double_int a, double_int b, unsigned code)
{
  return double_int_div (a, b, false, code);
}

/* The same as double_int_div with UNS = true.  */

double_int
double_int_udiv (double_int a, double_int b, unsigned code)
{
  return double_int_div (a, b, true, code);
}

/* Returns A % B (computed as unsigned depending on UNS, and rounded as
   specified by CODE).  CODE is enum tree_code in fact, but double_int.h
   must be included before tree.h.  */

double_int
double_int_mod (double_int a, double_int b, bool uns, unsigned code)
{
  double_int mod;

  double_int_divmod (a, b, uns, code, &mod);
  return mod;
}

/* The same as double_int_mod with UNS = false.  */

double_int
double_int_smod (double_int a, double_int b, unsigned code)
{
  return double_int_mod (a, b, false, code);
}

/* The same as double_int_mod with UNS = true.  */

double_int
double_int_umod (double_int a, double_int b, unsigned code)
{
  return double_int_mod (a, b, true, code);
}

/* Set BITPOS bit in A.  */
double_int
double_int_setbit (double_int a, unsigned bitpos)
{
  if (bitpos < HOST_BITS_PER_WIDE_INT)
    a.low |= (unsigned HOST_WIDE_INT) 1 << bitpos;
  else
    a.high |= (HOST_WIDE_INT) 1 <<  (bitpos - HOST_BITS_PER_WIDE_INT);
 
  return a;
}

/* Count trailing zeros in A.  */
int
double_int_ctz (double_int a)
{
  unsigned HOST_WIDE_INT w = a.low ? a.low : (unsigned HOST_WIDE_INT) a.high;
  unsigned bits = a.low ? 0 : HOST_BITS_PER_WIDE_INT;
  if (!w)
    return HOST_BITS_PER_DOUBLE_INT;
  bits += ctz_hwi (w);
  return bits;
}

/* Shift A left by COUNT places keeping only PREC bits of result.  Shift
   right if COUNT is negative.  ARITH true specifies arithmetic shifting;
   otherwise use logical shift.  */

double_int
double_int_lshift (double_int a, HOST_WIDE_INT count, unsigned int prec, bool arith)
{
  double_int ret;
  lshift_double (a.low, a.high, count, prec, &ret.low, &ret.high, arith);
  return ret;
}

/* Shift A rigth by COUNT places keeping only PREC bits of result.  Shift
   left if COUNT is negative.  ARITH true specifies arithmetic shifting;
   otherwise use logical shift.  */

double_int
double_int_rshift (double_int a, HOST_WIDE_INT count, unsigned int prec, bool arith)
{
  double_int ret;
  lshift_double (a.low, a.high, -count, prec, &ret.low, &ret.high, arith);
  return ret;
}

/* Rotate  A left by COUNT places keeping only PREC bits of result.
   Rotate right if COUNT is negative.  */

double_int
double_int_lrotate (double_int a, HOST_WIDE_INT count, unsigned int prec)
{
  double_int t1, t2;

  count %= prec;
  if (count < 0)
    count += prec;

  t1 = double_int_lshift (a, count, prec, false);
  t2 = double_int_rshift (a, prec - count, prec, false);

  return double_int_ior (t1, t2);
}

/* Rotate A rigth by COUNT places keeping only PREC bits of result.
   Rotate right if COUNT is negative.  */

double_int
double_int_rrotate (double_int a, HOST_WIDE_INT count, unsigned int prec)
{
  double_int t1, t2;

  count %= prec;
  if (count < 0)
    count += prec;

  t1 = double_int_rshift (a, count, prec, false);
  t2 = double_int_lshift (a, prec - count, prec, false);

  return double_int_ior (t1, t2);
}

/* Returns -1 if A < B, 0 if A == B and 1 if A > B.  Signedness of the
   comparison is given by UNS.  */

int
double_int_cmp (double_int a, double_int b, bool uns)
{
  if (uns)
    return double_int_ucmp (a, b);
  else
    return double_int_scmp (a, b);
}

/* Compares two unsigned values A and B.  Returns -1 if A < B, 0 if A == B,
   and 1 if A > B.  */

int
double_int_ucmp (double_int a, double_int b)
{
  if ((unsigned HOST_WIDE_INT) a.high < (unsigned HOST_WIDE_INT) b.high)
    return -1;
  if ((unsigned HOST_WIDE_INT) a.high > (unsigned HOST_WIDE_INT) b.high)
    return 1;
  if (a.low < b.low)
    return -1;
  if (a.low > b.low)
    return 1;

  return 0;
}

/* Compares two signed values A and B.  Returns -1 if A < B, 0 if A == B,
   and 1 if A > B.  */

int
double_int_scmp (double_int a, double_int b)
{
  if (a.high < b.high)
    return -1;
  if (a.high > b.high)
    return 1;
  if (a.low < b.low)
    return -1;
  if (a.low > b.low)
    return 1;

  return 0;
}

/* Compares two values A and B.  Returns max value.  Signedness of the
   comparison is given by UNS.  */

double_int
double_int_max (double_int a, double_int b, bool uns)
{
  return (double_int_cmp (a, b, uns) == 1) ? a : b;
}

/* Compares two signed values A and B.  Returns max value.  */

double_int double_int_smax (double_int a, double_int b)
{
  return (double_int_scmp (a, b) == 1) ? a : b;
}

/* Compares two unsigned values A and B.  Returns max value.  */

double_int double_int_umax (double_int a, double_int b)
{
  return (double_int_ucmp (a, b) == 1) ? a : b;
}

/* Compares two values A and B.  Returns mix value.  Signedness of the
   comparison is given by UNS.  */

double_int double_int_min (double_int a, double_int b, bool uns)
{
  return (double_int_cmp (a, b, uns) == -1) ? a : b;
}

/* Compares two signed values A and B.  Returns min value.  */

double_int double_int_smin (double_int a, double_int b)
{
  return (double_int_scmp (a, b) == -1) ? a : b;
}

/* Compares two unsigned values A and B.  Returns min value.  */

double_int double_int_umin (double_int a, double_int b)
{
  return (double_int_ucmp (a, b) == -1) ? a : b;
}

/* Splits last digit of *CST (taken as unsigned) in BASE and returns it.  */

static unsigned
double_int_split_digit (double_int *cst, unsigned base)
{
  unsigned HOST_WIDE_INT resl, reml;
  HOST_WIDE_INT resh, remh;

  div_and_round_double (FLOOR_DIV_EXPR, true, cst->low, cst->high, base, 0,
			&resl, &resh, &reml, &remh);
  cst->high = resh;
  cst->low = resl;

  return reml;
}

/* Dumps CST to FILE.  If UNS is true, CST is considered to be unsigned,
   otherwise it is signed.  */

void
dump_double_int (FILE *file, double_int cst, bool uns)
{
  unsigned digits[100], n;
  int i;

  if (double_int_zero_p (cst))
    {
      fprintf (file, "0");
      return;
    }

  if (!uns && double_int_negative_p (cst))
    {
      fprintf (file, "-");
      cst = double_int_neg (cst);
    }

  for (n = 0; !double_int_zero_p (cst); n++)
    digits[n] = double_int_split_digit (&cst, 10);
  for (i = n - 1; i >= 0; i--)
    fprintf (file, "%u", digits[i]);
}


/* Sets RESULT to VAL, taken unsigned if UNS is true and as signed
   otherwise.  */

void
mpz_set_double_int (mpz_t result, double_int val, bool uns)
{
  bool negate = false;
  unsigned HOST_WIDE_INT vp[2];

  if (!uns && double_int_negative_p (val))
    {
      negate = true;
      val = double_int_neg (val);
    }

  vp[0] = val.low;
  vp[1] = (unsigned HOST_WIDE_INT) val.high;
  mpz_import (result, 2, -1, sizeof (HOST_WIDE_INT), 0, 0, vp);

  if (negate)
    mpz_neg (result, result);
}

/* Returns VAL converted to TYPE.  If WRAP is true, then out-of-range
   values of VAL will be wrapped; otherwise, they will be set to the
   appropriate minimum or maximum TYPE bound.  */

double_int
mpz_get_double_int (const_tree type, mpz_t val, bool wrap)
{
  unsigned HOST_WIDE_INT *vp;
  size_t count, numb;
  double_int res;

  if (!wrap)
    {
      mpz_t min, max;

      mpz_init (min);
      mpz_init (max);
      get_type_static_bounds (type, min, max);

      if (mpz_cmp (val, min) < 0)
	mpz_set (val, min);
      else if (mpz_cmp (val, max) > 0)
	mpz_set (val, max);

      mpz_clear (min);
      mpz_clear (max);
    }

  /* Determine the number of unsigned HOST_WIDE_INT that are required
     for representing the value.  The code to calculate count is
     extracted from the GMP manual, section "Integer Import and Export":
     http://gmplib.org/manual/Integer-Import-and-Export.html  */
  numb = 8*sizeof(HOST_WIDE_INT);
  count = (mpz_sizeinbase (val, 2) + numb-1) / numb;
  if (count < 2)
    count = 2;
  vp = (unsigned HOST_WIDE_INT *) alloca (count * sizeof(HOST_WIDE_INT));

  vp[0] = 0;
  vp[1] = 0;
  mpz_export (vp, &count, -1, sizeof (HOST_WIDE_INT), 0, 0, val);

  gcc_assert (wrap || count <= 2);

  res.low = vp[0];
  res.high = (HOST_WIDE_INT) vp[1];

  res = double_int_ext (res, TYPE_PRECISION (type), TYPE_UNSIGNED (type));
  if (mpz_sgn (val) < 0)
    res = double_int_neg (res);

  return res;
}
