/* Fold a constant sub-tree into a single node for C-compiler
   Copyright (C) 1987, 1988, 1992 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/*@@ Fix lossage on folding division of big integers.  */

/*@@ This file should be rewritten to use an arbitary precision
  @@ representation for "struct tree_int_cst" and "struct tree_real_cst".
  @@ Perhaps the routines could also be used for bc/dc, and made a lib.
  @@ The routines that translate from the ap rep should
  @@ warn if precision et. al. is lost.
  @@ This would also make life easier when this technology is used
  @@ for cross-compilers.  */


/* The entry points in this file are fold, size_int and size_binop.

   fold takes a tree as argument and returns a simplified tree.

   size_binop takes a tree code for an arithmetic operation
   and two operands that are trees, and produces a tree for the
   result, assuming the type comes from `sizetype'.

   size_int takes an integer value, and creates a tree constant
   with type from `sizetype'.  */
   
#include <stdio.h>
#include <setjmp.h>
#include "config.h"
#include "flags.h"
#include "tree.h"

void lshift_double ();
void rshift_double ();
void lrotate_double ();
void rrotate_double ();
static tree const_binop ();

/* To do constant folding on INTEGER_CST nodes requires 64-bit arithmetic.
   We do that by representing the 64-bit integer as 8 shorts,
   with only 8 bits stored in each short, as a positive number.  */

/* Unpack a 64-bit integer into 8 shorts.
   LOW and HI are the integer, as two `int' pieces.
   SHORTS points to the array of shorts.  */

static void
encode (shorts, low, hi)
     short *shorts;
     int low, hi;
{
  shorts[0] = low & 0xff;
  shorts[1] = (low >> 8) & 0xff;
  shorts[2] = (low >> 16) & 0xff;
  shorts[3] = (low >> 24) & 0xff;
  shorts[4] = hi & 0xff;
  shorts[5] = (hi >> 8) & 0xff;
  shorts[6] = (hi >> 16) & 0xff;
  shorts[7] = (hi >> 24) & 0xff;
}

/* Pack an array of 8 shorts into a 64-bit integer.
   SHORTS points to the array of shorts.
   The integer is stored into *LOW and *HI as two `int' pieces.  */

static void
decode (shorts, low, hi)
     short *shorts;
     int *low, *hi;
{
  /* The casts in the following statement should not be
     needed, but they get around bugs in some C compilers.  */
  *low = (((long)shorts[3] << 24) | ((long)shorts[2] << 16)
	  | ((long)shorts[1] << 8) | (long)shorts[0]);
  *hi = (((long)shorts[7] << 24) | ((long)shorts[6] << 16)
	 | ((long)shorts[5] << 8) | (long)shorts[4]);
}

/* Make the integer constant T valid for its type
   by setting to 0 or 1 all the bits in the constant
   that don't belong in the type.  */

static void
force_fit_type (t)
     tree t;
{
  register int prec = TYPE_PRECISION (TREE_TYPE (t));

  if (TREE_CODE (TREE_TYPE (t)) == POINTER_TYPE)
    prec = POINTER_SIZE;

  /* First clear all bits that are beyond the type's precision.  */

  if (prec == 2 * HOST_BITS_PER_INT)
    ;
  else if (prec > HOST_BITS_PER_INT)
    {
      TREE_INT_CST_HIGH (t)
	&= ~((-1) << (prec - HOST_BITS_PER_INT));
    }
  else
    {
      TREE_INT_CST_HIGH (t) = 0;
      if (prec < HOST_BITS_PER_INT)
	TREE_INT_CST_LOW (t)
	  &= ~((-1) << prec);
    }

  /* If it's a signed type and value's sign bit is set, extend the sign.  */

  if (! TREE_UNSIGNED (TREE_TYPE (t))
      && prec != 2 * HOST_BITS_PER_INT
      && (prec > HOST_BITS_PER_INT
	  ? TREE_INT_CST_HIGH (t) & (1 << (prec - HOST_BITS_PER_INT - 1))
	  : TREE_INT_CST_LOW (t) & (1 << (prec - 1))))
    {
      /* Value is negative:
	 set to 1 all the bits that are outside this type's precision.  */
      if (prec > HOST_BITS_PER_INT)
	{
	  TREE_INT_CST_HIGH (t)
	    |= ((-1) << (prec - HOST_BITS_PER_INT));
	}
      else
	{
	  TREE_INT_CST_HIGH (t) = -1;
	  if (prec < HOST_BITS_PER_INT)
	    TREE_INT_CST_LOW (t)
	      |= ((-1) << prec);
	}
    }
}

/* Add two 64-bit integers with 64-bit result.
   Each argument is given as two `int' pieces.
   One argument is L1 and H1; the other, L2 and H2.
   The value is stored as two `int' pieces in *LV and *HV.
   We use the 8-shorts representation internally.  */

void
add_double (l1, h1, l2, h2, lv, hv)
     int l1, h1, l2, h2;
     int *lv, *hv;
{
  short arg1[8];
  short arg2[8];
  register int carry = 0;
  register int i;

  encode (arg1, l1, h1);
  encode (arg2, l2, h2);

  for (i = 0; i < 8; i++)
    {
      carry += arg1[i] + arg2[i];
      arg1[i] = carry & 0xff;
      carry >>= 8;
    }

  decode (arg1, lv, hv);
}

/* Negate a 64-bit integers with 64-bit result.
   The argument is given as two `int' pieces in L1 and H1.
   The value is stored as two `int' pieces in *LV and *HV.
   We use the 8-shorts representation internally.  */

void
neg_double (l1, h1, lv, hv)
     int l1, h1;
     int *lv, *hv;
{
  if (l1 == 0)
    {
      *lv = 0;
      *hv = - h1;
    }
  else
    {
      *lv = - l1;
      *hv = ~ h1;
    }
}

/* Multiply two 64-bit integers with 64-bit result.
   Each argument is given as two `int' pieces.
   One argument is L1 and H1; the other, L2 and H2.
   The value is stored as two `int' pieces in *LV and *HV.
   We use the 8-shorts representation internally.  */

void
mul_double (l1, h1, l2, h2, lv, hv)
     int l1, h1, l2, h2;
     int *lv, *hv;
{
  short arg1[8];
  short arg2[8];
  short prod[16];
  register int carry = 0;
  register int i, j, k;

  /* These two cases are used extensively, arising from pointer
     combinations.  */
  if (h2 == 0)
    {
      if (l2 == 2)
	{
	  unsigned temp = l1 + l1;
	  *hv = h1 * 2 + (temp < l1);
	  *lv = temp;
	  return;
	}
      if (l2 == 4)
	{
	  unsigned temp = l1 + l1;
	  h1 = h1 * 4 + ((temp < l1) << 1);
	  l1 = temp;
	  temp += temp;
	  h1 += (temp < l1);
	  *lv = temp;
	  *hv = h1;
	  return;
	}
      if (l2 == 8)
	{
	  unsigned temp = l1 + l1;
	  h1 = h1 * 8 + ((temp < l1) << 2);
	  l1 = temp;
	  temp += temp;
	  h1 += (temp < l1) << 1;
	  l1 = temp;
	  temp += temp;
	  h1 += (temp < l1);
	  *lv = temp;
	  *hv = h1;
	  return;
	}
    }

  encode (arg1, l1, h1);
  encode (arg2, l2, h2);

  bzero (prod, sizeof prod);

  for (i = 0; i < 8; i++)
    for (j = 0; j < 8; j++)
      {
	k = i + j;
	carry = arg1[i] * arg2[j];
	while (carry)
	  {
	    carry += prod[k];
	    prod[k] = carry & 0xff;
	    carry >>= 8;
	    k++;
	  }
      }

  decode (prod, lv, hv);	/* @@decode ignores prod[8] -> prod[15] */
}

/* Shift the 64-bit integer in L1, H1 left by COUNT places
   keeping only PREC bits of result.
   Shift right if COUNT is negative.
   ARITH nonzero specifies arithmetic shifting; otherwise use logical shift.
   Store the value as two `int' pieces in *LV and *HV.  */

void
lshift_double (l1, h1, count, prec, lv, hv, arith)
     int l1, h1, count, prec;
     int *lv, *hv;
     int arith;
{
  short arg1[8];
  register int i;
  register int carry;

  if (count < 0)
    {
      rshift_double (l1, h1, - count, prec, lv, hv, arith);
      return;
    }

  encode (arg1, l1, h1);

  if (count > prec)
    count = prec;

  while (count > 0)
    {
      carry = 0;
      for (i = 0; i < 8; i++)
	{
	  carry += arg1[i] << 1;
	  arg1[i] = carry & 0xff;
	  carry >>= 8;
	}
      count--;
    }

  decode (arg1, lv, hv);
}

/* Shift the 64-bit integer in L1, H1 right by COUNT places
   keeping only PREC bits of result.  COUNT must be positive.
   ARITH nonzero specifies arithmetic shifting; otherwise use logical shift.
   Store the value as two `int' pieces in *LV and *HV.  */

void
rshift_double (l1, h1, count, prec, lv, hv, arith)
     int l1, h1, count, prec;
     int *lv, *hv;
     int arith;
{
  short arg1[8];
  register int i;
  register int carry;

  encode (arg1, l1, h1);

  if (count > prec)
    count = prec;

  while (count > 0)
    {
      carry = arith && arg1[7] >> 7; 
     for (i = 7; i >= 0; i--)
	{
	  carry <<= 8;
	  carry += arg1[i];
	  arg1[i] = (carry >> 1) & 0xff;
	}
      count--;
    }

  decode (arg1, lv, hv);
}

/* Rotate the 64-bit integer in L1, H1 left by COUNT places
   keeping only PREC bits of result.
   Rotate right if COUNT is negative.
   Store the value as two `int' pieces in *LV and *HV.  */

void
lrotate_double (l1, h1, count, prec, lv, hv)
     int l1, h1, count, prec;
     int *lv, *hv;
{
  short arg1[8];
  register int i;
  register int carry;

  if (count < 0)
    {
      rrotate_double (l1, h1, - count, prec, lv, hv);
      return;
    }

  encode (arg1, l1, h1);

  if (count > prec)
    count = prec;

  carry = arg1[7] >> 7;
  while (count > 0)
    {
      for (i = 0; i < 8; i++)
	{
	  carry += arg1[i] << 1;
	  arg1[i] = carry & 0xff;
	  carry >>= 8;
	}
      count--;
    }

  decode (arg1, lv, hv);
}

/* Rotate the 64-bit integer in L1, H1 left by COUNT places
   keeping only PREC bits of result.  COUNT must be positive.
   Store the value as two `int' pieces in *LV and *HV.  */

void
rrotate_double (l1, h1, count, prec, lv, hv)
     int l1, h1, count, prec;
     int *lv, *hv;
{
  short arg1[8];
  register int i;
  register int carry;

  encode (arg1, l1, h1);

  if (count > prec)
    count = prec;

  carry = arg1[0] & 1;
  while (count > 0)
    {
      for (i = 7; i >= 0; i--)
	{
	  carry <<= 8;
	  carry += arg1[i];
	  arg1[i] = (carry >> 1) & 0xff;
	}
      count--;
    }

  decode (arg1, lv, hv);
}

/* Divide 64 bit integer LNUM, HNUM by 64 bit integer LDEN, HDEN
   for a quotient (stored in *LQUO, *HQUO) and remainder (in *LREM, *HREM).
   CODE is a tree code for a kind of division, one of
   TRUNC_DIV_EXPR, FLOOR_DIV_EXPR, CEIL_DIV_EXPR, ROUND_DIV_EXPR
   or EXACT_DIV_EXPR
   It controls how the quotient is rounded to a integer.
   UNS nonzero says do unsigned division.  */

static void
div_and_round_double (code, uns,
		      lnum_orig, hnum_orig, lden_orig, hden_orig,
		      lquo, hquo, lrem, hrem)
     enum tree_code code;
     int uns;
     int lnum_orig, hnum_orig;		/* num == numerator == dividend */
     int lden_orig, hden_orig;		/* den == denominator == divisor */
     int *lquo, *hquo, *lrem, *hrem;
{
  int quo_neg = 0;
  short num[9], den[8], quo[8];	/* extra element for scaling.  */
  register int i, j, work;
  register int carry = 0;
  unsigned int lnum = lnum_orig;
  int hnum = hnum_orig;
  unsigned int lden = lden_orig;
  int hden = hden_orig;

  if ((hden == 0) && (lden == 0))
    abort ();

  /* calculate quotient sign and convert operands to unsigned.  */
  if (!uns) 
    {
      if (hden < 0) 
	{
	  quo_neg = ~ quo_neg;
	  neg_double (lden, hden, &lden, &hden);
	}
      if (hnum < 0)
	{
	  quo_neg = ~ quo_neg;
	  neg_double (lnum, hnum, &lnum, &hnum);
	}
    }

  if (hnum == 0 && hden == 0)
    {				/* single precision */
      *hquo = *hrem = 0;
      *lquo = lnum / lden;	/* rounds toward zero since positive args */
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

  bzero (quo, sizeof quo);

  bzero (num, sizeof num);	/* to zero 9th element */
  bzero (den, sizeof den);

  encode (num, lnum, hnum); 
  encode (den, lden, hden);

  /* This code requires more than just hden == 0.
     We also have to require that we don't need more than three bytes
     to hold CARRY.  If we ever did need four bytes to hold it, we
     would lose part of it when computing WORK on the next round.  */
  if (hden == 0 && ((lden << 8) >> 8) == lden)
    {				/* simpler algorithm */
      /* hnum != 0 already checked.  */
      for (i = 7; i >= 0; i--)
	{
	  work = num[i] + (carry << 8);
	  quo[i] = work / lden;
	  carry = work % lden;
	}
    }
  else {			/* full double precision,
				   with thanks to Don Knuth's
				   "Semi-Numericial Algorithms".  */
#define BASE 256
    int quo_est, scale, num_hi_sig, den_hi_sig, quo_hi_sig;

    /* Find the highest non-zero divisor digit.  */
    for (i = 7; ; i--)
      if (den[i] != 0) {
	den_hi_sig = i;
	break;
      }
    for (i = 7; ; i--)
      if (num[i] != 0) {
	num_hi_sig = i;
	break;
      }
    quo_hi_sig = num_hi_sig - den_hi_sig + 1;

    /* Insure that the first digit of the divisor is at least BASE/2.
       This is required by the quotient digit estimation algorithm.  */

    scale = BASE / (den[den_hi_sig] + 1);
    if (scale > 1) {		/* scale divisor and dividend */
      carry = 0;
      for (i = 0; i <= 8; i++) {
	work = (num[i] * scale) + carry;
	num[i] = work & 0xff;
	carry = work >> 8;
	if (num[i] != 0) num_hi_sig = i;
      }
      carry = 0;
      for (i = 0; i <= 7; i++) {
	work = (den[i] * scale) + carry;
	den[i] = work & 0xff;
	carry = work >> 8;
	if (den[i] != 0) den_hi_sig = i;
      }
    }

    /* Main loop */
    for (i = quo_hi_sig; i > 0; i--) {
      /* quess the next quotient digit, quo_est, by dividing the first
	 two remaining dividend digits by the high order quotient digit.
	 quo_est is never low and is at most 2 high.  */

      int num_hi;		/* index of highest remaining dividend digit */

      num_hi = i + den_hi_sig;

      work = (num[num_hi] * BASE) + (num_hi > 0 ? num[num_hi - 1] : 0);
      if (num[num_hi] != den[den_hi_sig]) {
	quo_est = work / den[den_hi_sig];
      }
      else {
	quo_est = BASE - 1;
      }

      /* refine quo_est so it's usually correct, and at most one high.   */
      while ((den[den_hi_sig - 1] * quo_est)
	     > (((work - (quo_est * den[den_hi_sig])) * BASE)
		 + ((num_hi - 1) > 0 ? num[num_hi - 2] : 0)))
	quo_est--;

      /* Try QUO_EST as the quotient digit, by multiplying the
         divisor by QUO_EST and subtracting from the remaining dividend.
	 Keep in mind that QUO_EST is the I - 1st digit.  */

      carry = 0;

      for (j = 0; j <= den_hi_sig; j++)
	{
	  int digit;

	  work = num[i + j - 1] - (quo_est * den[j]) + carry;
	  digit = work & 0xff;
	  carry = work >> 8;
	  if (digit < 0)
	    {
	      digit += BASE;
	      carry--;
	    }
	  num[i + j - 1] = digit;
	}

      /* if quo_est was high by one, then num[i] went negative and
	 we need to correct things.  */

      if (num[num_hi] < 0)
	{
	  quo_est--;
	  carry = 0;		/* add divisor back in */
	  for (j = 0; j <= den_hi_sig; j++)
	    {
	      work = num[i + j - 1] + den[j] + carry;
	      if (work > BASE)
		{
		  work -= BASE;
		  carry = 1;
		}
	      else
		{
		  carry = 0;
		}
	      num[i + j - 1] = work;
	    }
	  num [num_hi] += carry;
	}

      /* store the quotient digit.  */
      quo[i - 1] = quo_est;
    }
  }

  decode (quo, lquo, hquo);

 finish_up:
  /* if result is negative, make it so.  */
  if (quo_neg)
    neg_double (*lquo, *hquo, lquo, hquo);

  /* compute trial remainder:  rem = num - (quo * den)  */
  mul_double (*lquo, *hquo, lden_orig, hden_orig, lrem, hrem);
  neg_double (*lrem, *hrem, lrem, hrem);
  add_double (lnum_orig, hnum_orig, *lrem, *hrem, lrem, hrem);

  switch (code)
    {
    case TRUNC_DIV_EXPR:
    case TRUNC_MOD_EXPR:	/* round toward zero */
    case EXACT_DIV_EXPR:	/* for this one, it shouldn't matter */
      return;

    case FLOOR_DIV_EXPR:
    case FLOOR_MOD_EXPR:	/* round toward negative infinity */
      if (quo_neg && (*lrem != 0 || *hrem != 0))   /* ratio < 0 && rem != 0 */
	{
	  /* quo = quo - 1;  */
	  add_double (*lquo, *hquo, -1, -1, lquo, hquo);
	}
      else return;
      break;

    case CEIL_DIV_EXPR:
    case CEIL_MOD_EXPR:		/* round toward positive infinity */
      if (!quo_neg && (*lrem != 0 || *hrem != 0))  /* ratio > 0 && rem != 0 */
	{
	  add_double (*lquo, *hquo, 1, 0, lquo, hquo);
	}
      else return;
      break;
    
    case ROUND_DIV_EXPR:
    case ROUND_MOD_EXPR:	/* round to closest integer */
      {
	int labs_rem = *lrem, habs_rem = *hrem;
	int labs_den = lden, habs_den = hden, ltwice, htwice;

	/* get absolute values */
	if (*hrem < 0) neg_double (*lrem, *hrem, &labs_rem, &habs_rem);
	if (hden < 0) neg_double (lden, hden, &labs_den, &habs_den);

	/* if (2 * abs (lrem) >= abs (lden)) */
	mul_double (2, 0, labs_rem, habs_rem, &ltwice, &htwice);
	if (((unsigned) habs_den < (unsigned) htwice)
	    || (((unsigned) habs_den == (unsigned) htwice)
		&& ((unsigned) labs_den < (unsigned) ltwice)))
	  {
	    if (*hquo < 0)
	      /* quo = quo - 1;  */
	      add_double (*lquo, *hquo, -1, -1, lquo, hquo);
	    else
	      /* quo = quo + 1; */
	      add_double (*lquo, *hquo, 1, 0, lquo, hquo);
	  }
	else return;
      }
      break;

    default:
      abort ();
    }

  /* compute true remainder:  rem = num - (quo * den)  */
  mul_double (*lquo, *hquo, lden_orig, hden_orig, lrem, hrem);
  neg_double (*lrem, *hrem, lrem, hrem);
  add_double (lnum_orig, hnum_orig, *lrem, *hrem, lrem, hrem);
}

#if TARGET_FLOAT_FORMAT == IEEE_FLOAT_FORMAT

/* Check for infinity in an IEEE double precision number.  */

int
target_isinf (x)
     REAL_VALUE_TYPE x;
{
  /* The IEEE 64-bit double format.  */
  union {
    REAL_VALUE_TYPE d;
    struct {
      unsigned sign      :  1;
      unsigned exponent  : 11;
      unsigned mantissa1 : 20;
      unsigned mantissa2;
    } little_endian;
    struct {
      unsigned mantissa2;
      unsigned mantissa1 : 20;
      unsigned exponent  : 11;
      unsigned sign      :  1;
    } big_endian;    
  } u;

  u.d = dconstm1;
  if (u.big_endian.sign == 1)
    {
      u.d = x;
      return (u.big_endian.exponent == 2047
	      && u.big_endian.mantissa1 == 0
	      && u.big_endian.mantissa2 == 0);
    }
  else
    {
      u.d = x;
      return (u.little_endian.exponent == 2047
	      && u.little_endian.mantissa1 == 0
	      && u.little_endian.mantissa2 == 0);
    }
}

/* Check whether an IEEE double precision number is a NaN.  */

int
target_isnan (x)
     REAL_VALUE_TYPE x;
{
  /* The IEEE 64-bit double format.  */
  union {
    REAL_VALUE_TYPE d;
    struct {
      unsigned sign      :  1;
      unsigned exponent  : 11;
      unsigned mantissa1 : 20;
      unsigned mantissa2;
    } little_endian;
    struct {
      unsigned mantissa2;
      unsigned mantissa1 : 20;
      unsigned exponent  : 11;
      unsigned sign      :  1;
    } big_endian;    
  } u;

  u.d = dconstm1;
  if (u.big_endian.sign == 1)
    {
      u.d = x;
      return (u.big_endian.exponent == 2047
	      && (u.big_endian.mantissa1 != 0
		  || u.big_endian.mantissa2 != 0));
    }
  else
    {
      u.d = x;
      return (u.little_endian.exponent == 2047
	      && (u.little_endian.mantissa1 != 0
		  || u.little_endian.mantissa2 != 0));
    }
}

/* Check for minus zero in an IEEE double precision number.  */

int
target_minus_zero (x)
     REAL_VALUE_TYPE x;
{
  REAL_VALUE_TYPE d1, d2;

  d1 = REAL_VALUE_NEGATE (x);
  d2 = dconst0;

  return !bcmp (&d1, &d2, sizeof (d1));
}
#else /* Target not IEEE */

/* Let's assume other float formats don't have infinity.
   (This can be overridden by redefining REAL_VALUE_ISINF.)  */

target_isinf (x)
     REAL_VALUE_TYPE x;
{
  return 0;
}

/* Let's assume other float formats don't have NaNs.
   (This can be overridden by redefining REAL_VALUE_ISNAN.)  */

target_isnan (x)
     REAL_VALUE_TYPE x;
{
  return 0;
}

/* Let's assume other float formats don't have minus zero.
   (This can be overridden by redefining REAL_VALUE_MINUS_ZERO.)  */

target_minus_zero (x)
     REAL_VALUE_TYPE x;
{
  return 0;
}
#endif /* Target not IEEE */

/* Split a tree IN into a constant and a variable part
   that could be combined with CODE to make IN.
   CODE must be a commutative arithmetic operation.
   Store the constant part into *CONP and the variable in &VARP.
   Return 1 if this was done; zero means the tree IN did not decompose
   this way.

   If CODE is PLUS_EXPR we also split trees that use MINUS_EXPR.
   Therefore, we must tell the caller whether the variable part
   was subtracted.  We do this by storing 1 or -1 into *VARSIGNP.
   The value stored is the coefficient for the variable term.
   The constant term we return should always be added;
   we negate it if necessary.  */

static int
split_tree (in, code, varp, conp, varsignp)
     tree in;
     enum tree_code code;
     tree *varp, *conp;
     int *varsignp;
{
  register tree outtype = TREE_TYPE (in);
  *varp = 0;
  *conp = 0;

  /* Strip any conversions that don't change the machine mode.  */
  while ((TREE_CODE (in) == NOP_EXPR
	  || TREE_CODE (in) == CONVERT_EXPR)
	 && (TYPE_MODE (TREE_TYPE (in))
	     == TYPE_MODE (TREE_TYPE (TREE_OPERAND (in, 0)))))
    in = TREE_OPERAND (in, 0);

  if (TREE_CODE (in) == code
      || (TREE_CODE (TREE_TYPE (in)) != REAL_TYPE
	  /* We can associate addition and subtraction together
	     (even though the C standard doesn't say so)
	     for integers because the value is not affected.
	     For reals, the value might be affected, so we can't.  */
	  &&
	  ((code == PLUS_EXPR && TREE_CODE (in) == MINUS_EXPR)
	   || (code == MINUS_EXPR && TREE_CODE (in) == PLUS_EXPR))))
    {
      enum tree_code code = TREE_CODE (TREE_OPERAND (in, 0));
      if (code == INTEGER_CST)
	{
	  *conp = TREE_OPERAND (in, 0);
	  *varp = TREE_OPERAND (in, 1);
	  if (TYPE_MODE (TREE_TYPE (*varp)) != TYPE_MODE (outtype)
	      && TREE_TYPE (*varp) != outtype)
	    *varp = convert (outtype, *varp);
	  *varsignp = (TREE_CODE (in) == MINUS_EXPR) ? -1 : 1;
	  return 1;
	}
      if (TREE_CONSTANT (TREE_OPERAND (in, 1)))
	{
	  *conp = TREE_OPERAND (in, 1);
	  *varp = TREE_OPERAND (in, 0);
	  *varsignp = 1;
	  if (TYPE_MODE (TREE_TYPE (*varp)) != TYPE_MODE (outtype)
	      && TREE_TYPE (*varp) != outtype)
	    *varp = convert (outtype, *varp);
	  if (TREE_CODE (in) == MINUS_EXPR)
	    {
	      /* If operation is subtraction and constant is second,
		 must negate it to get an additive constant.
		 And this cannot be done unless it is a manifest constant.
		 It could also be the address of a static variable.
		 We cannot negate that, so give up.  */
	      if (TREE_CODE (*conp) == INTEGER_CST)
		/* Subtracting from integer_zero_node loses for long long.  */
		*conp = fold (build1 (NEGATE_EXPR, TREE_TYPE (*conp), *conp));
	      else
		return 0;
	    }
	  return 1;
	}
      if (TREE_CONSTANT (TREE_OPERAND (in, 0)))
	{
	  *conp = TREE_OPERAND (in, 0);
	  *varp = TREE_OPERAND (in, 1);
	  if (TYPE_MODE (TREE_TYPE (*varp)) != TYPE_MODE (outtype)
	      && TREE_TYPE (*varp) != outtype)
	    *varp = convert (outtype, *varp);
	  *varsignp = (TREE_CODE (in) == MINUS_EXPR) ? -1 : 1;
	  return 1;
	}
    }
  return 0;
}

/* Combine two constants NUM and ARG2 under operation CODE
   to produce a new constant.
   We assume ARG1 and ARG2 have the same data type,
   or at least are the same kind of constant and the same machine mode.  */

/* Handle floating overflow for `const_binop'.  */
static jmp_buf const_binop_error;

static tree
const_binop (code, arg1, arg2)
     enum tree_code code;
     register tree arg1, arg2;
{
  if (TREE_CODE (arg1) == INTEGER_CST)
    {
      register int int1l = TREE_INT_CST_LOW (arg1);
      register int int1h = TREE_INT_CST_HIGH (arg1);
      int int2l = TREE_INT_CST_LOW (arg2);
      int int2h = TREE_INT_CST_HIGH (arg2);
      int low, hi;
      int garbagel, garbageh;
      register tree t;
      int uns = TREE_UNSIGNED (TREE_TYPE (arg1));

      switch (code)
	{
	case BIT_IOR_EXPR:
	  t = build_int_2 (int1l | int2l, int1h | int2h);
	  break;

	case BIT_XOR_EXPR:
	  t = build_int_2 (int1l ^ int2l, int1h ^ int2h);
	  break;

	case BIT_AND_EXPR:
	  t = build_int_2 (int1l & int2l, int1h & int2h);
	  break;

	case BIT_ANDTC_EXPR:
	  t = build_int_2 (int1l & ~int2l, int1h & ~int2h);
	  break;

	case RSHIFT_EXPR:
	  int2l = - int2l;
	case LSHIFT_EXPR:
	  lshift_double (int1l, int1h, int2l,
			 TYPE_PRECISION (TREE_TYPE (arg1)),
			 &low, &hi,
			 !uns);
	  t = build_int_2 (low, hi);
	  break;

	case RROTATE_EXPR:
	  int2l = - int2l;
	case LROTATE_EXPR:
	  lrotate_double (int1l, int1h, int2l,
			  TYPE_PRECISION (TREE_TYPE (arg1)),
			  &low, &hi);
	  t = build_int_2 (low, hi);
	  break;

	case PLUS_EXPR:
	  if (int1h == 0)
	    {
	      int2l += int1l;
	      if ((unsigned) int2l < int1l)
		int2h += 1;
	      t = build_int_2 (int2l, int2h);
	      break;
	    }
	  if (int2h == 0)
	    {
	      int1l += int2l;
	      if ((unsigned) int1l < int2l)
		int1h += 1;
	      t = build_int_2 (int1l, int1h);
	      break;
	    }
	  add_double (int1l, int1h, int2l, int2h, &low, &hi);
	  t = build_int_2 (low, hi);
	  break;

	case MINUS_EXPR:
	  if (int2h == 0 && int2l == 0)
	    {
	      t = build_int_2 (int1l, int1h);
	      break;
	    }
	  neg_double (int2l, int2h, &int2l, &int2h);
	  add_double (int1l, int1h, int2l, int2h, &low, &hi);
	  t = build_int_2 (low, hi);
	  break;

	case MULT_EXPR:
  /* Optimize simple cases.  */
	  if (int1h == 0)
	    {
	      unsigned temp;

	      switch (int1l)
		{
		case 0:
		  t = build_int_2 (0, 0);
		  goto got_it;
		case 1:
		  t = build_int_2 (int2l, int2h);
		  goto got_it;
		case 2:
		  temp = int2l + int2l;
		  int2h = int2h * 2 + (temp < int2l);
		  t = build_int_2 (temp, int2h);
		  goto got_it;
		case 3:
		  temp = int2l + int2l + int2l;
		  int2h = int2h * 3 + (temp < int2l);
		  t = build_int_2 (temp, int2h);
		  goto got_it;
		case 4:
		  temp = int2l + int2l;
		  int2h = int2h * 4 + ((temp < int2l) << 1);
		  int2l = temp;
		  temp += temp;
		  int2h += (temp < int2l);
		  t = build_int_2 (temp, int2h);
		  goto got_it;
		case 8:
		  temp = int2l + int2l;
		  int2h = int2h * 8 + ((temp < int2l) << 2);
		  int2l = temp;
		  temp += temp;
		  int2h += (temp < int2l) << 1;
		  int2l = temp;
		  temp += temp;
		  int2h += (temp < int2l);
		  t = build_int_2 (temp, int2h);
		  goto got_it;
		default:
		  break;
		}
	    }

	  if (int2h == 0)
	    {
	      if (int2l == 0)
		{
		  t = build_int_2 (0, 0);
		  break;
		}
	      if (int2l == 1)
		{
		  t = build_int_2 (int1l, int1h);
		  break;
		}
	    }

	  mul_double (int1l, int1h, int2l, int2h, &low, &hi);
	  t = build_int_2 (low, hi);
	  break;

	case TRUNC_DIV_EXPR:
	case FLOOR_DIV_EXPR: case CEIL_DIV_EXPR:
	case EXACT_DIV_EXPR:
	  /* This is a shortcut for a common special case.
	     It reduces the number of tree nodes generated
	     and saves time.  */
	  if (int2h == 0 && int2l > 0
	      && TREE_TYPE (arg1) == sizetype
	      && int1h == 0 && int1l >= 0)
	    {
	      if (code == CEIL_DIV_EXPR)
		int1l += int2l-1;
	      return size_int (int1l / int2l);
	    }
	case ROUND_DIV_EXPR: 
	  if (int2h == 0 && int2l == 1)
	    {
	      t = build_int_2 (int1l, int1h);
	      break;
	    }
	  if (int1l == int2l && int1h == int2h)
	    {
	      if ((int1l | int1h) == 0)
		abort ();
	      t = build_int_2 (1, 0);
	      break;
	    }
	  div_and_round_double (code, uns, int1l, int1h, int2l, int2h,
				&low, &hi, &garbagel, &garbageh);
	  t = build_int_2 (low, hi);
	  break;

	case TRUNC_MOD_EXPR: case ROUND_MOD_EXPR: 
	case FLOOR_MOD_EXPR: case CEIL_MOD_EXPR:
	  div_and_round_double (code, uns, int1l, int1h, int2l, int2h,
				&garbagel, &garbageh, &low, &hi);
	  t = build_int_2 (low, hi);
	  break;

	case MIN_EXPR:
	case MAX_EXPR:
	  if (uns)
	    {
	      low = (((unsigned) int1h < (unsigned) int2h)
		     || (((unsigned) int1h == (unsigned) int2h)
			 && ((unsigned) int1l < (unsigned) int2l)));
	    }
	  else
	    {
	      low = ((int1h < int2h)
		     || ((int1h == int2h)
			 && ((unsigned) int1l < (unsigned) int2l)));
	    }
	  if (low == (code == MIN_EXPR))
	    t = build_int_2 (int1l, int1h);
	  else
	    t = build_int_2 (int2l, int2h);
	  break;

	default:
	  abort ();
	}
    got_it:
      TREE_TYPE (t) = TREE_TYPE (arg1);
      force_fit_type (t);
      return t;
    }
#if ! defined (REAL_IS_NOT_DOUBLE) || defined (REAL_ARITHMETIC)
  if (TREE_CODE (arg1) == REAL_CST)
    {
      register REAL_VALUE_TYPE d1;
      register REAL_VALUE_TYPE d2;
      register REAL_VALUE_TYPE value;

      d1 = TREE_REAL_CST (arg1);
      d2 = TREE_REAL_CST (arg2);
      if (setjmp (const_binop_error))
	{
	  warning ("floating overflow in constant folding");
	  return build (code, TREE_TYPE (arg1), arg1, arg2);
	}
      set_float_handler (const_binop_error);

#ifdef REAL_ARITHMETIC
      REAL_ARITHMETIC (value, code, d1, d2);
#else
      switch (code)
	{
	case PLUS_EXPR:
	  value = d1 + d2;
	  break;

	case MINUS_EXPR:
	  value = d1 - d2;
	  break;

	case MULT_EXPR:
	  value = d1 * d2;
	  break;

	case RDIV_EXPR:
#ifndef REAL_INFINITY
	  if (d2 == 0)
	    abort ();
#endif

	  value = d1 / d2;
	  break;

	case MIN_EXPR:
	  value = MIN (d1, d2);
	  break;

	case MAX_EXPR:
	  value = MAX (d1, d2);
	  break;

	default:
	  abort ();
	}
#endif /* no REAL_ARITHMETIC */
      set_float_handler (0);
      return build_real (TREE_TYPE (arg1),
			 REAL_VALUE_TRUNCATE (TYPE_MODE (TREE_TYPE (arg1)), value));
    }
#endif /* not REAL_IS_NOT_DOUBLE, or REAL_ARITHMETIC */
  if (TREE_CODE (arg1) == COMPLEX_CST)
    {
      register tree r1 = TREE_REALPART (arg1);
      register tree i1 = TREE_IMAGPART (arg1);
      register tree r2 = TREE_REALPART (arg2);
      register tree i2 = TREE_IMAGPART (arg2);
      register tree t;

      switch (code)
	{
	case PLUS_EXPR:
	  t = build_complex (const_binop (PLUS_EXPR, r1, r2),
			     const_binop (PLUS_EXPR, i1, i2));
	  break;

	case MINUS_EXPR:
	  t = build_complex (const_binop (MINUS_EXPR, r1, r2),
			     const_binop (MINUS_EXPR, i1, i2));
	  break;

	case MULT_EXPR:
	  t = build_complex (const_binop (MINUS_EXPR,
					  const_binop (MULT_EXPR, r1, r2),
					  const_binop (MULT_EXPR, i1, i2)),
			     const_binop (PLUS_EXPR,
					  const_binop (MULT_EXPR, r1, i2),
					  const_binop (MULT_EXPR, i1, r2)));
	  break;

	case RDIV_EXPR:
	  {
	    register tree magsquared
	      = const_binop (PLUS_EXPR,
			     const_binop (MULT_EXPR, r2, r2),
			     const_binop (MULT_EXPR, i2, i2));
	    t = build_complex (const_binop (RDIV_EXPR,
					    const_binop (PLUS_EXPR,
							 const_binop (MULT_EXPR, r1, r2),
							 const_binop (MULT_EXPR, i1, i2)),
					    magsquared),
			       const_binop (RDIV_EXPR,
					    const_binop (MINUS_EXPR,
							 const_binop (MULT_EXPR, i1, r2),
							 const_binop (MULT_EXPR, r1, i2)),
					    magsquared));
	  }
	  break;

	default:
	  abort ();
	}
      TREE_TYPE (t) = TREE_TYPE (arg1);
      return t;
    }
  return 0;
}

/* Return an INTEGER_CST with value V and type from `sizetype'.  */

tree
size_int (number)
     unsigned int number;
{
  register tree t;
  /* Type-size nodes already made for small sizes.  */
  static tree size_table[2*HOST_BITS_PER_INT+1];

  if (number >= 0 && number < 2*HOST_BITS_PER_INT+1 && size_table[number] != 0)
    return size_table[number];
  if (number >= 0 && number < 2*HOST_BITS_PER_INT+1)
    {
      int temp = allocation_temporary_p ();

      push_obstacks_nochange ();
      /* Make this a permanent node.  */
      if (temp)
	end_temporary_allocation ();
      t = build_int_2 (number, 0);
      TREE_TYPE (t) = sizetype;
      size_table[number] = t;
      pop_obstacks ();
    }
  else
    {
      t = build_int_2 (number, 0);
      TREE_TYPE (t) = sizetype;
    }
  return t;
}

/* Combine operands OP1 and OP2 with arithmetic operation CODE.
   CODE is a tree code.  Data type is taken from `sizetype',
   If the operands are constant, so is the result.  */

tree
size_binop (code, arg0, arg1)
     enum tree_code code;
     tree arg0, arg1;
{
  /* Handle the special case of two integer constants faster.  */
  if (TREE_CODE (arg0) == INTEGER_CST && TREE_CODE (arg1) == INTEGER_CST)
    {
      /* And some specific cases even faster than that.  */
      if (code == PLUS_EXPR
	  && TREE_INT_CST_LOW (arg0) == 0
	  && TREE_INT_CST_HIGH (arg0) == 0)
	return arg1;
      if (code == MINUS_EXPR
	  && TREE_INT_CST_LOW (arg1) == 0
	  && TREE_INT_CST_HIGH (arg1) == 0)
	return arg0;
      if (code == MULT_EXPR
	  && TREE_INT_CST_LOW (arg0) == 1
	  && TREE_INT_CST_HIGH (arg0) == 0)
	return arg1;
      /* Handle general case of two integer constants.  */
      return const_binop (code, arg0, arg1);
    }

  if (arg0 == error_mark_node || arg1 == error_mark_node)
    return error_mark_node;

  return fold (build (code, sizetype, arg0, arg1));
}

/* Given T, a tree representing type conversion of ARG1, a constant,
   return a constant tree representing the result of conversion.  */

static tree
fold_convert (t, arg1)
     register tree t;
     register tree arg1;
{
  register tree type = TREE_TYPE (t);

  if (TREE_CODE (type) == POINTER_TYPE
      || TREE_CODE (type) == INTEGER_TYPE
      || TREE_CODE (type) == ENUMERAL_TYPE)
    {
      if (TREE_CODE (arg1) == INTEGER_CST)
	{
	  /* Given an integer constant, make new constant with new type,
	     appropriately sign-extended or truncated.  */
	  t = build_int_2 (TREE_INT_CST_LOW (arg1),
			   TREE_INT_CST_HIGH (arg1));
	  TREE_TYPE (t) = type;
	  force_fit_type (t);
	}
#if !defined (REAL_IS_NOT_DOUBLE) || defined (REAL_ARITHMETIC)
      else if (TREE_CODE (arg1) == REAL_CST)
	{
	  if (REAL_VALUES_LESS (real_value_from_int_cst (TYPE_MAX_VALUE (type)),
				TREE_REAL_CST (arg1))
	      || REAL_VALUES_LESS (TREE_REAL_CST (arg1),
				   real_value_from_int_cst (TYPE_MIN_VALUE (type))))
	    {
	      warning ("real constant out of range for integer conversion");
	      return t;
	    }
#ifndef REAL_ARITHMETIC
	  {
	    REAL_VALUE_TYPE d;
	    int low, high;
	    int half_word = 1 << (HOST_BITS_PER_INT / 2);

	    d = TREE_REAL_CST (arg1);
	    if (d < 0)
	      d = -d;

	    high = (int) (d / half_word / half_word);
	    d -= (REAL_VALUE_TYPE) high * half_word * half_word;
	    low = (unsigned) d;
	    if (TREE_REAL_CST (arg1) < 0)
	      neg_double (low, high, &low, &high);
	    t = build_int_2 (low, high);
	  }
#else
	  {
	    int low, high;
	    REAL_VALUE_TO_INT (low, high, TREE_REAL_CST (arg1));
	    t = build_int_2 (low, high);
	  }
#endif
	  TREE_TYPE (t) = type;
	  force_fit_type (t);
	}
#endif /* not REAL_IS_NOT_DOUBLE, or REAL_ARITHMETIC */
      TREE_TYPE (t) = type;
    }
  else if (TREE_CODE (type) == REAL_TYPE)
    {
#if !defined (REAL_IS_NOT_DOUBLE) || defined (REAL_ARITHMETIC)
      if (TREE_CODE (arg1) == INTEGER_CST)
	return build_real_from_int_cst (type, arg1);
#endif /* not REAL_IS_NOT_DOUBLE, or REAL_ARITHMETIC */
      if (TREE_CODE (arg1) == REAL_CST)
	return build_real (type, REAL_VALUE_TRUNCATE (TYPE_MODE (type),
						      TREE_REAL_CST (arg1)));
    }
  TREE_CONSTANT (t) = 1;
  return t;
}

/* Return an expr equal to X but certainly not valid as an lvalue.  */

tree
non_lvalue (x)
     tree x;
{
  tree result;

  /* These things are certainly not lvalues.  */
  if (TREE_CODE (x) == NON_LVALUE_EXPR
      || TREE_CODE (x) == INTEGER_CST
      || TREE_CODE (x) == REAL_CST
      || TREE_CODE (x) == STRING_CST
      || TREE_CODE (x) == ADDR_EXPR)
    return x;

  result = build1 (NON_LVALUE_EXPR, TREE_TYPE (x), x);
  TREE_CONSTANT (result) = TREE_CONSTANT (x);
  return result;
}

/* Return nonzero if two operands are necessarily equal. 
   If ONLY_CONST is non-zero, only return non-zero for constants.  */

int
operand_equal_p (arg0, arg1, only_const)
     tree arg0, arg1;
     int only_const;
{
  /* If both types don't have the same signedness, then we can't consider
     them equal.  We must check this before the STRIP_NOPS calls
     because they may change the signedness of the arguments.  */
  if (TREE_UNSIGNED (TREE_TYPE (arg0)) != TREE_UNSIGNED (TREE_TYPE (arg1)))
    return 0;

  STRIP_NOPS (arg0);
  STRIP_NOPS (arg1);

  /* If ARG0 and ARG1 are the same SAVE_EXPR, they are necessarily equal.
     We don't care about side effects in that case because the SAVE_EXPR
     takes care of that for us.  */
  if (TREE_CODE (arg0) == SAVE_EXPR && arg0 == arg1)
    return ! only_const;

  if (TREE_SIDE_EFFECTS (arg0) || TREE_SIDE_EFFECTS (arg1))
    return 0;

  if (TREE_CODE (arg0) == TREE_CODE (arg1)
      && TREE_CODE (arg0) == ADDR_EXPR
      && TREE_OPERAND (arg0, 0) == TREE_OPERAND (arg1, 0))
    return 1;

  if (TREE_CODE (arg0) == TREE_CODE (arg1)
      && TREE_CODE (arg0) == INTEGER_CST
      && TREE_INT_CST_LOW (arg0) == TREE_INT_CST_LOW (arg1)
      && TREE_INT_CST_HIGH (arg0) == TREE_INT_CST_HIGH (arg1))
    return 1;

  /* Detect when real constants are equal.
     But reject weird values because we can't be sure what to do with them.  */
  if (TREE_CODE (arg0) == TREE_CODE (arg1)
      && TREE_CODE (arg0) == REAL_CST
      && REAL_VALUES_EQUAL (TREE_REAL_CST (arg0), TREE_REAL_CST (arg1))
      && !REAL_VALUE_ISINF (TREE_REAL_CST (arg0))
      && !REAL_VALUE_ISNAN (TREE_REAL_CST (arg0)))
    return 1;

  if (only_const)
    return 0;

  if (arg0 == arg1)
    return 1;

  if (TREE_CODE (arg0) != TREE_CODE (arg1))
    return 0;
  /* This is needed for conversions and for COMPONENT_REF.
     Might as well play it safe and always test this.  */
  if (TYPE_MODE (TREE_TYPE (arg0)) != TYPE_MODE (TREE_TYPE (arg1)))
    return 0;

  switch (TREE_CODE_CLASS (TREE_CODE (arg0)))
    {
    case '1':
      /* Two conversions are equal only if signedness and modes match.  */
      if ((TREE_CODE (arg0) == NOP_EXPR || TREE_CODE (arg0) == CONVERT_EXPR)
	  && (TREE_UNSIGNED (TREE_TYPE (arg0))
	      != TREE_UNSIGNED (TREE_TYPE (arg1))))
	return 0;

      return operand_equal_p (TREE_OPERAND (arg0, 0),
			      TREE_OPERAND (arg1, 0), 0);

    case '<':
    case '2':
      return (operand_equal_p (TREE_OPERAND (arg0, 0),
			       TREE_OPERAND (arg1, 0), 0)
	      && operand_equal_p (TREE_OPERAND (arg0, 1),
				  TREE_OPERAND (arg1, 1), 0));

    case 'r':
      switch (TREE_CODE (arg0))
	{
	case INDIRECT_REF:
	  return operand_equal_p (TREE_OPERAND (arg0, 0),
				  TREE_OPERAND (arg1, 0), 0);

	case COMPONENT_REF:
	case ARRAY_REF:
	  return (operand_equal_p (TREE_OPERAND (arg0, 0),
				   TREE_OPERAND (arg1, 0), 0)
		  && operand_equal_p (TREE_OPERAND (arg0, 1),
				      TREE_OPERAND (arg1, 1), 0));

	case BIT_FIELD_REF:
	  return (operand_equal_p (TREE_OPERAND (arg0, 0),
				   TREE_OPERAND (arg1, 0), 0)
		  && operand_equal_p (TREE_OPERAND (arg0, 1),
				      TREE_OPERAND (arg1, 1), 0)
		  && operand_equal_p (TREE_OPERAND (arg0, 2),
				      TREE_OPERAND (arg1, 2), 0));
	}
      break;
    }

  return 0;
}

/* Return nonzero if comparing COMP1 with COMP2
   gives the same result as comparing OP1 with OP2.
   When in doubt, return 0.  */

static int 
comparison_equiv_p (comp1, comp2, op1, op2)
     tree comp1, comp2, op1, op2;
{
  int unsignedp1, unsignedp2;
  tree primop1, primop2;
  int correct_width;

  if (operand_equal_p (comp1, op1, 0)
      && operand_equal_p (comp2, op2, 0))
    return 1;

  if (TREE_CODE (TREE_TYPE (op1)) != INTEGER_TYPE)
    return 0;

  if (TREE_TYPE (op1) != TREE_TYPE (op2))
    return 0;

  if (TREE_TYPE (comp1) != TREE_TYPE (comp2))
    return 0;

  /* Duplicate what shorten_compare does to the comparison operands,
     and see if that gives the actual comparison operands, COMP1 and COMP2.  */

  /* Throw away any conversions to wider types
     already present in the operands.  */
  primop1 = get_narrower (op1, &unsignedp1);
  primop2 = get_narrower (op2, &unsignedp2);

  correct_width = TYPE_PRECISION (TREE_TYPE (op2));
  if (unsignedp1 == unsignedp2
      && TYPE_PRECISION (TREE_TYPE (primop1)) < correct_width
      && TYPE_PRECISION (TREE_TYPE (primop2)) < correct_width)
    {
      tree type = TREE_TYPE (comp1);

      /* Make sure shorter operand is extended the right way
	 to match the longer operand.  */
      primop1 = convert (signed_or_unsigned_type (unsignedp1, TREE_TYPE (primop1)),
			 primop1);
      primop2 = convert (signed_or_unsigned_type (unsignedp2, TREE_TYPE (primop2)),
			 primop2);

      primop1 = convert (type, primop1);
      primop2 = convert (type, primop2);

      if (operand_equal_p (comp1, primop1, 0)
	  && operand_equal_p (comp2, primop2, 0))
	return 1;
    }

  return 0;
}

/* Return a tree for the case when the result of an expression is RESULT
   converted to TYPE and OMITTED was previously an operand of the expression
   but is now not needed (e.g., we folded OMITTED * 0).

   If OMITTED has side effects, we must evaluate it.  Otherwise, just do
   the conversion of RESULT to TYPE.  */

static tree
omit_one_operand (type, result, omitted)
     tree type, result, omitted;
{
  tree t = convert (type, result);

  if (TREE_SIDE_EFFECTS (omitted))
    return build (COMPOUND_EXPR, type, omitted, t);

  return t;
}

/* Return a simplified tree node for the truth-negation of ARG
   (perhaps by altering ARG).  It is known that ARG is an operation that
   returns a truth value (0 or 1).  */

tree
invert_truthvalue (arg)
     tree arg;
{
  tree type = TREE_TYPE (arg);

  /* For floating-point comparisons, it isn't safe to invert the condition.
     So just enclose a TRUTH_NOT_EXPR around what we have.  */
  if (TREE_CODE_CLASS (TREE_CODE (arg)) == '<'
      && TREE_CODE (TREE_TYPE (TREE_OPERAND (arg, 0))) == REAL_TYPE)
    return build1 (TRUTH_NOT_EXPR, type, arg);

  switch (TREE_CODE (arg))
    {
    case NE_EXPR:
      TREE_SET_CODE (arg, EQ_EXPR);
      return arg;

    case EQ_EXPR:
      TREE_SET_CODE (arg, NE_EXPR);
      return arg;

    case GE_EXPR:
      TREE_SET_CODE (arg, LT_EXPR);
      return arg;

    case GT_EXPR:
      TREE_SET_CODE (arg, LE_EXPR);
      return arg;

    case LE_EXPR:
      TREE_SET_CODE (arg, GT_EXPR);
      return arg;

    case LT_EXPR:
      TREE_SET_CODE (arg, GE_EXPR);
      return arg;

    case INTEGER_CST:
      return convert (type, build_int_2 (TREE_INT_CST_LOW (arg) == 0
					 && TREE_INT_CST_HIGH (arg) == 0, 0));

    case TRUTH_AND_EXPR:
      return build (TRUTH_OR_EXPR, type,
		    invert_truthvalue (TREE_OPERAND (arg, 0)),
		    invert_truthvalue (TREE_OPERAND (arg, 1)));

    case TRUTH_OR_EXPR:
      return build (TRUTH_AND_EXPR, type,
		    invert_truthvalue (TREE_OPERAND (arg, 0)),
		    invert_truthvalue (TREE_OPERAND (arg, 1)));

    case TRUTH_ANDIF_EXPR:
      return build (TRUTH_ORIF_EXPR, type,
		    invert_truthvalue (TREE_OPERAND (arg, 0)),
		    invert_truthvalue (TREE_OPERAND (arg, 1)));

    case TRUTH_ORIF_EXPR:
      return build (TRUTH_ANDIF_EXPR, type,
		    invert_truthvalue (TREE_OPERAND (arg, 0)),
		    invert_truthvalue (TREE_OPERAND (arg, 1)));

    case TRUTH_NOT_EXPR:
      return TREE_OPERAND (arg, 0);

    case COND_EXPR:
      return build (COND_EXPR, type, TREE_OPERAND (arg, 0),
		    invert_truthvalue (TREE_OPERAND (arg, 1)),
		    invert_truthvalue (TREE_OPERAND (arg, 2)));

    case COMPOUND_EXPR:
      return build (COMPOUND_EXPR, type, TREE_OPERAND (arg, 0),
		    invert_truthvalue (TREE_OPERAND (arg, 1)));

    case NON_LVALUE_EXPR:
      return invert_truthvalue (TREE_OPERAND (arg, 0));

    case NOP_EXPR:
    case CONVERT_EXPR:
    case FLOAT_EXPR:
      return build1 (TREE_CODE (arg), type,
		     invert_truthvalue (TREE_OPERAND (arg, 0)));

    case BIT_AND_EXPR:
      if (! integer_onep (TREE_OPERAND (arg, 1)))
	abort ();
      return build (EQ_EXPR, type, arg, convert (type, integer_zero_node));
    }

  abort ();
}

/* Given a bit-wise operation CODE applied to ARG0 and ARG1, see if both
   operands are another bit-wise operation with a common input.  If so,
   distribute the bit operations to save an operation and possibly two if
   constants are involved.  For example, convert
   	(A | B) & (A | C) into A | (B & C)
   Further simplification will occur if B and C are constants.

   If this optimization cannot be done, 0 will be returned.  */

static tree
distribute_bit_expr (code, type, arg0, arg1)
     enum tree_code code;
     tree type;
     tree arg0, arg1;
{
  tree common;
  tree left, right;

  if (TREE_CODE (arg0) != TREE_CODE (arg1)
      || TREE_CODE (arg0) == code
      || (TREE_CODE (arg0) != BIT_AND_EXPR
	  && TREE_CODE (arg0) != BIT_IOR_EXPR))
    return 0;

  if (operand_equal_p (TREE_OPERAND (arg0, 0), TREE_OPERAND (arg1, 0), 0))
    {
      common = TREE_OPERAND (arg0, 0);
      left = TREE_OPERAND (arg0, 1);
      right = TREE_OPERAND (arg1, 1);
    }
  else if (operand_equal_p (TREE_OPERAND (arg0, 0), TREE_OPERAND (arg1, 1), 0))
    {
      common = TREE_OPERAND (arg0, 0);
      left = TREE_OPERAND (arg0, 1);
      right = TREE_OPERAND (arg1, 0);
    }
  else if (operand_equal_p (TREE_OPERAND (arg0, 1), TREE_OPERAND (arg1, 0), 0))
    {
      common = TREE_OPERAND (arg0, 1);
      left = TREE_OPERAND (arg0, 0);
      right = TREE_OPERAND (arg1, 1);
    }
  else if (operand_equal_p (TREE_OPERAND (arg0, 1), TREE_OPERAND (arg1, 1), 0))
    {
      common = TREE_OPERAND (arg0, 1);
      left = TREE_OPERAND (arg0, 0);
      right = TREE_OPERAND (arg1, 0);
    }
  else
    return 0;

  return fold (build (TREE_CODE (arg0), type, common,
		      fold (build (code, type, left, right))));
}

/* Return a BIT_FIELD_REF of type TYPE to refer to BITSIZE bits of INNER
   starting at BITPOS.  The field is unsigned if UNSIGNEDP is non-zero.  */

static tree
make_bit_field_ref (inner, type, bitsize, bitpos, unsignedp)
     tree inner;
     tree type;
     int bitsize, bitpos;
     int unsignedp;
{
  tree result = build (BIT_FIELD_REF, type, inner,
		       size_int (bitsize), size_int (bitpos));

  TREE_UNSIGNED (result) = unsignedp;

  return result;
}

/* Optimize a bit-field compare.

   There are two cases:  First is a compare against a constant and the
   second is a comparison of two items where the fields are at the same
   bit position relative to the start of a chunk (byte, halfword, word)
   large enough to contain it.  In these cases we can avoid the shift
   implicit in bitfield extractions.

   For constants, we emit a compare of the shifted constant with the
   BIT_AND_EXPR of a mask and a byte, halfword, or word of the operand being
   compared.  For two fields at the same position, we do the ANDs with the
   similar mask and compare the result of the ANDs.

   CODE is the comparison code, known to be either NE_EXPR or EQ_EXPR.
   COMPARE_TYPE is the type of the comparison, and LHS and RHS
   are the left and right operands of the comparison, respectively.

   If the optimization described above can be done, we return the resuling
   tree.  Otherwise we return zero.  */

static tree
optimize_bit_field_compare (code, compare_type, lhs, rhs)
     enum tree_code code;
     tree compare_type;
     tree lhs, rhs;
{
  int lbitpos, lbitsize, rbitpos, rbitsize;
  int lnbitpos, lnbitsize, rnbitpos, rnbitsize;
  tree type = TREE_TYPE (lhs);
  tree signed_type, unsigned_type;
  int const_p = TREE_CODE (rhs) == INTEGER_CST;
  enum machine_mode lmode, rmode, lnmode, rnmode;
  int lunsignedp, runsignedp;
  int lvolatilep = 0, rvolatilep = 0;
  tree linner, rinner;
  tree mask;

  /* Get all the information about the extractions being done.  If the bit size
     if the same as the size of the underlying object, we aren't doing an
     extraction at all and so can do nothing.  */
  linner = get_inner_reference (lhs, &lbitsize, &lbitpos, &lmode,
				&lunsignedp, &lvolatilep);
  if (lbitsize == GET_MODE_BITSIZE (lmode))
    return 0;

 if (!const_p)
   {
     /* If this is not a constant, we can only do something if bit positions,
	sizes, and signedness are the same.   */
     rinner = get_inner_reference (rhs, &rbitsize, &rbitpos,
				   &rmode, &runsignedp, &rvolatilep);

     if (lbitpos != rbitpos || lbitsize != rbitsize
	 || lunsignedp != runsignedp)
       return 0;
   }

  /* See if we can find a mode to refer to this field.  We should be able to,
     but fail if we can't.  */
  lnmode = get_best_mode (lbitsize, lbitpos,
			  TYPE_ALIGN (TREE_TYPE (linner)), word_mode,
			  lvolatilep);
  if (lnmode == VOIDmode)
    return 0;

  /* Set signed and unsigned types of the precision of this mode for the
     shifts below.  */
  signed_type = type_for_mode (lnmode, 0);
  unsigned_type = type_for_mode (lnmode, 1);

  if (! const_p)
    {
      rnmode = get_best_mode (rbitsize, rbitpos, 
			      TYPE_ALIGN (TREE_TYPE (rinner)), word_mode,
			      rvolatilep);
      if (rnmode == VOIDmode)
	return 0;
    }
    
  /* Compute the bit position and size for the new reference and our offset
     within it. If the new reference is the same size as the original, we
     won't optimize anything, so return zero.  */
  lnbitsize = GET_MODE_BITSIZE (lnmode);
  lnbitpos = lbitpos & ~ (lnbitsize - 1);
  lbitpos -= lnbitpos;
  if (lnbitsize == lbitsize)
    return 0;

  if (! const_p)
    {
      rnbitsize = GET_MODE_BITSIZE (rnmode);
      rnbitpos = rbitpos & ~ (rnbitsize - 1);
      rbitpos -= rnbitpos;
      if (rnbitsize == rbitsize)
	return 0;
    }

#if BYTES_BIG_ENDIAN
  lbitpos = lnbitsize - lbitsize - lbitpos;
  rbitpos = rnbitsize - rbitsize - rbitpos;
#endif

  /* Make the mask to be used against the extracted field.  */
  mask = convert (unsigned_type, build_int_2 (~0, ~0));
  mask = const_binop (LSHIFT_EXPR, mask, size_int (lnbitsize - lbitsize));
  mask = const_binop (RSHIFT_EXPR, mask,
		      size_int (lnbitsize - lbitsize - lbitpos));

  if (! const_p)
    /* If not comparing with constant, just rework the comparison
       and return.  */
    return build (code, compare_type,
		  build (BIT_AND_EXPR, type,
			 make_bit_field_ref (linner, type,
					     lnbitsize, lnbitpos, lunsignedp),
			 mask),
		  build (BIT_AND_EXPR, type,
			 make_bit_field_ref (rinner, type,
					     rnbitsize, rnbitpos, runsignedp),
			 mask));

  /* Otherwise, we are handling the constant case. See if the constant is too
     big for the field.  Warn and return a tree of for 0 (false) if so.  We do
     this not only for its own sake, but to avoid having to test for this
     error case below.  If we didn't, we might generate wrong code.

     For unsigned fields, the constant shifted right by the field length should
     be all zero.  For signed fields, the high-order bits should agree with 
     the sign bit.  */

  if (lunsignedp)
    {
      if (! integer_zerop (const_binop (RSHIFT_EXPR,
					convert (unsigned_type, rhs),
					size_int (lbitsize))))
	{
	  warning ("comparison is always %s due to width of bitfield",
		   code == NE_EXPR ? "one" : "zero");
	  return convert (compare_type,
			  (code == NE_EXPR
			   ? integer_one_node : integer_zero_node));
	}
    }
  else
    {
      tree tem = const_binop (RSHIFT_EXPR, convert (signed_type, rhs),
			      size_int (lbitsize - 1));
      if (! integer_zerop (tem) && ! integer_all_onesp (tem))
	{
	  warning ("comparison is always %s due to width of bitfield",
		   code == NE_EXPR ? "one" : "zero");
	  return convert (compare_type,
			  (code == NE_EXPR
			   ? integer_one_node : integer_zero_node));
	}
    }

  /* Single-bit compares should always be against zero.  */
  if (lbitsize == 1 && ! integer_zerop (rhs))
    {
      code = code == EQ_EXPR ? NE_EXPR : EQ_EXPR;
      rhs = convert (type, integer_zero_node);
    }

  /* Make a new bitfield reference, shift the constant over the
     appropriate number of bits and mask it with the computed mask
     (in case this was a signed field).  If we changed it, make a new one.  */
  lhs = make_bit_field_ref (linner, TREE_TYPE (lhs), lnbitsize, lnbitpos, 
			    lunsignedp);

  rhs = fold (build1 (NOP_EXPR, type,
		      const_binop (BIT_AND_EXPR,
				   const_binop (LSHIFT_EXPR,
						convert (unsigned_type, rhs),
						size_int (lbitpos)), mask)));

  return build (code, compare_type,
		build (BIT_AND_EXPR, type, lhs, mask),
		rhs);
}

/* Subroutine for the following routine: decode a field reference.

   If EXP is a comparison reference, we return the innermost reference.

   *PBITSIZE is set to the number of bits in the reference, *PBITPOS is
   set to the starting bit number.

   If the innermost field can be completely contained in a mode-sized
   unit, *PMODE is set to that mode.  Otherwise, it is set to VOIDmode.

   *PVOLATILEP is set to 1 if the any expression encountered is volatile;
   otherwise it is not changed.

   *PUNSIGNEDP is set to the signedness of the field.

   *PMASK is set to the mask used.  This is either contained in a
   BIT_AND_EXPR or derived from the width of the field.

   Return 0 if this is not a component reference or is one that we can't
   do anything with.  */

static tree
decode_field_reference (exp, pbitsize, pbitpos, pmode, punsignedp,
			pvolatilep, pmask)
     tree exp;
     int *pbitsize, *pbitpos;
     enum machine_mode *pmode;
     int *punsignedp, *pvolatilep;
     tree *pmask;
{
  tree mask = 0;
  tree inner;

  STRIP_NOPS (exp);

  if (TREE_CODE (exp) == BIT_AND_EXPR)
    {
      mask = TREE_OPERAND (exp, 1);
      exp = TREE_OPERAND (exp, 0);
      STRIP_NOPS (exp); STRIP_NOPS (mask);
      if (TREE_CODE (mask) != INTEGER_CST)
	return 0;
    }

  if (TREE_CODE (exp) != COMPONENT_REF && TREE_CODE (exp) != ARRAY_REF
      && TREE_CODE (exp) != BIT_FIELD_REF)
    return 0;

  inner = get_inner_reference (exp, pbitsize, pbitpos, pmode,
			       punsignedp, pvolatilep);
  
  if (mask == 0)
    {
      tree unsigned_type = type_for_size (*pbitsize, 1);
      int precision = TYPE_PRECISION (unsigned_type);

      mask = convert (unsigned_type, build_int_2 (~0, ~0));
      mask = const_binop (LSHIFT_EXPR, mask, size_int (precision - *pbitsize));
      mask = const_binop (RSHIFT_EXPR, mask, size_int (precision - *pbitsize));
    }

  *pmask = mask;
  return inner;
}

/* Return non-zero if MASK respresents a mask of SIZE ones in the low-order
   bit positions.  */

static int
all_ones_mask_p (mask, size)
     tree mask;
     int size;
{
  tree type = TREE_TYPE (mask);
  int precision = TYPE_PRECISION (type);

  return
    operand_equal_p (mask, 
		     const_binop (RSHIFT_EXPR,
				  const_binop (LSHIFT_EXPR,
					       convert (signed_type (type),
							build_int_2 (~0, ~0)),
					       size_int (precision - size)),
				  size_int (precision - size)), 0);
}

/* Try to merge two comparisons to the same innermost item.

   For example, if we have p->a == 2 && p->b == 4 and we can make an
   object large enough to span both A and B, we can do this with a comparison
   against the object ANDed with the a mask.

   If we have p->a == q->a && p->b == q->b, we may be able to use bit masking
   operations to do this with one comparison.

   We check for both normal comparisons and the BIT_AND_EXPRs made this by
   function and the one above.

   CODE is the logical operation being done.  It can be TRUTH_ANDIF_EXPR,
   TRUTH_AND_EXPR, TRUTH_ORIF_EXPR, or TRUTH_OR_EXPR.

   TRUTH_TYPE is the type of the logical operand and LHS and RHS are its
   two operands.

   We return the simplified tree or 0 if no optimization is possible.  */

static tree
merge_component_references (code, truth_type, lhs, rhs)
     enum tree_code code;
     tree truth_type, lhs, rhs;
{
  /* If this is the "or" of two comparisons, we can do something if we
     the comparisons are NE_EXPR.  If this is the "and", we can do something
     if the comparisons are EQ_EXPR.  I.e., 
     	(a->b == 2 && a->c == 4) can become (a->new == NEW).

     WANTED_CODE is this operation code.  For single bit fields, we can
     convert EQ_EXPR to NE_EXPR so we need not reject the "wrong"
     comparison for one-bit fields.  */

  enum tree_code wanted_code
    = (code == TRUTH_AND_EXPR || code == TRUTH_ANDIF_EXPR) ? EQ_EXPR : NE_EXPR;
  enum tree_code lcode, rcode;
  tree ll_inner, lr_inner, rl_inner, rr_inner;
  int ll_bitsize, ll_bitpos, lr_bitsize, lr_bitpos;
  int rl_bitsize, rl_bitpos, rr_bitsize, rr_bitpos;
  int xll_bitpos, xlr_bitpos, xrl_bitpos, xrr_bitpos;
  int lnbitsize, lnbitpos, rnbitsize, rnbitpos;
  int ll_unsignedp, lr_unsignedp, rl_unsignedp, rr_unsignedp;
  enum machine_mode ll_mode, lr_mode, rl_mode, rr_mode;
  enum machine_mode lnmode, rnmode;
  tree ll_mask, lr_mask, rl_mask, rr_mask;
  tree l_const = 0, r_const = 0;
  tree type, result;
  int first_bit, end_bit;
  int volatilep = 0;

  /* Start by getting the comparison codes and seeing if we may be able
     to do something.  Then get all the parameters for each side.  Fail
     if anything is volatile.  */

  lcode = TREE_CODE (lhs);
  rcode = TREE_CODE (rhs);
  if ((lcode != EQ_EXPR && lcode != NE_EXPR)
      || (rcode != EQ_EXPR && rcode != NE_EXPR)
      || TREE_SIDE_EFFECTS (lhs) || TREE_SIDE_EFFECTS (rhs))
    return 0;

  ll_inner = decode_field_reference (TREE_OPERAND (lhs, 0),
				     &ll_bitsize, &ll_bitpos, &ll_mode,
				     &ll_unsignedp, &volatilep, &ll_mask);
  lr_inner = decode_field_reference (TREE_OPERAND (lhs, 1),
				     &lr_bitsize, &lr_bitpos, &lr_mode,
				     &lr_unsignedp, &volatilep, &lr_mask);
  rl_inner = decode_field_reference (TREE_OPERAND (rhs, 0),
				     &rl_bitsize, &rl_bitpos, &rl_mode,
				     &rl_unsignedp, &volatilep, &rl_mask);
  rr_inner = decode_field_reference (TREE_OPERAND (rhs, 1),
				     &rr_bitsize, &rr_bitpos, &rr_mode,
				     &rr_unsignedp, &volatilep, &rr_mask);

  /* It must be true that the inner operation on the lhs of each
     comparison must be the same if we are to be able to do anything.
     Then see if we have constants.  If not, the same must be true for
     the rhs's.  */
  if (volatilep || ll_inner == 0 || rl_inner == 0
      || ! operand_equal_p (ll_inner, rl_inner, 0))
    return 0;

  if (TREE_CODE (TREE_OPERAND (lhs, 1)) == INTEGER_CST
      && TREE_CODE (TREE_OPERAND (rhs, 1)) == INTEGER_CST)
    l_const = TREE_OPERAND (lhs, 1), r_const = TREE_OPERAND (rhs, 1);
  else if (lr_inner == 0 || rr_inner == 0
	   || ! operand_equal_p (lr_inner, rr_inner, 0))
    return 0;

  /* If either comparison code is not correct for our logical operation,
     fail.  However, we can convert a one-bit comparison against zero into
     the opposite comparison against that bit being set in the field.  */
  if (lcode != wanted_code)
    {
      if (l_const && integer_zerop (l_const) && integer_pow2p (ll_mask))
	l_const = ll_mask;
      else
	return 0;
    }

  if (rcode != wanted_code)
    {
      if (r_const && integer_zerop (r_const) && integer_pow2p (rl_mask))
	r_const = rl_mask;
      else
	return 0;
    }

  /* See if we can find a mode that contains both fields being compared on
     the left.  If we can't, fail.  Otherwise, update all constants and masks
     to be relative to a field of that size.  */
  first_bit = MIN (ll_bitpos, rl_bitpos);
  end_bit = MAX (ll_bitpos + ll_bitsize, rl_bitpos + rl_bitsize);
  lnmode = get_best_mode (end_bit - first_bit, first_bit,
			  TYPE_ALIGN (TREE_TYPE (ll_inner)), word_mode,
			  volatilep);
  if (lnmode == VOIDmode)
    return 0;

  lnbitsize = GET_MODE_BITSIZE (lnmode);
  lnbitpos = first_bit & ~ (lnbitsize - 1);
  type = type_for_size (lnbitsize, 1);
  xll_bitpos = ll_bitpos - lnbitpos, xrl_bitpos = rl_bitpos - lnbitpos;

#if BYTES_BIG_ENDIAN
  xll_bitpos = lnbitsize - xll_bitpos - ll_bitsize;
  xrl_bitpos = lnbitsize - xrl_bitpos - rl_bitsize;
#endif

  ll_mask = const_binop (LSHIFT_EXPR, convert (type, ll_mask),
			 size_int (xll_bitpos));
  rl_mask = const_binop (LSHIFT_EXPR, convert (type, rl_mask),
			 size_int (xrl_bitpos));

  /* Make sure the constants are interpreted as unsigned, so we
     don't have sign bits outside the range of their type.  */

  if (l_const)
    {
      l_const = convert (unsigned_type (TREE_TYPE (l_const)), l_const);
      l_const = const_binop (LSHIFT_EXPR, convert (type, l_const),
			     size_int (xll_bitpos));
    }
  if (r_const)
    {
      r_const = convert (unsigned_type (TREE_TYPE (r_const)), r_const);
      r_const = const_binop (LSHIFT_EXPR, convert (type, r_const),
			     size_int (xrl_bitpos));
    }

  /* If the right sides are not constant, do the same for it.  Also,
     disallow this optimization if a size or signedness mismatch occurs
     between the left and right sides.  */
  if (l_const == 0)
    {
      if (ll_bitsize != lr_bitsize || rl_bitsize != rr_bitsize
	  || ll_unsignedp != lr_unsignedp || rl_unsignedp != rr_unsignedp)
	return 0;

      first_bit = MIN (lr_bitpos, rr_bitpos);
      end_bit = MAX (lr_bitpos + lr_bitsize, rr_bitpos + rr_bitsize);
      rnmode = get_best_mode (end_bit - first_bit, first_bit,
			      TYPE_ALIGN (TREE_TYPE (lr_inner)), word_mode,
			      volatilep);
      if (rnmode == VOIDmode)
	return 0;

      rnbitsize = GET_MODE_BITSIZE (rnmode);
      rnbitpos = first_bit & ~ (rnbitsize - 1);
      xlr_bitpos = lr_bitpos - rnbitpos, xrr_bitpos = rr_bitpos - rnbitpos;

#if BYTES_BIG_ENDIAN
      xlr_bitpos = rnbitsize - xlr_bitpos - lr_bitsize;
      xrr_bitpos = rnbitsize - xrr_bitpos - rr_bitsize;
#endif

      lr_mask = const_binop (LSHIFT_EXPR, convert (type, lr_mask),
			     size_int (xlr_bitpos));
      rr_mask = const_binop (LSHIFT_EXPR, convert (type, rr_mask),
			     size_int (xrr_bitpos));

      /* Make a mask that corresponds to both fields being compared.
	 Do this for both items being compared.  If the masks agree,
	 we can do this by masking both and comparing the masked
	 results.  */
      ll_mask = const_binop (BIT_IOR_EXPR, ll_mask, rl_mask);
      lr_mask = const_binop (BIT_IOR_EXPR, lr_mask, rr_mask);
      if (operand_equal_p (ll_mask, lr_mask, 0) && lnbitsize == rnbitsize)
	{
	  lhs = make_bit_field_ref (ll_inner, type, lnbitsize, lnbitpos,
				    ll_unsignedp || rl_unsignedp);
	  rhs = make_bit_field_ref (lr_inner, type, rnbitsize, rnbitpos,
				    lr_unsignedp || rr_unsignedp);
	  if (! all_ones_mask_p (ll_mask, lnbitsize))
	    {
	      lhs = build (BIT_AND_EXPR, type, lhs, ll_mask);
	      rhs = build (BIT_AND_EXPR, type, rhs, ll_mask);
	    }
	  return build (wanted_code, truth_type, lhs, rhs);
	}

      /* There is still another way we can do something:  If both pairs of
	 fields being compared are adjacent, we may be able to make a wider
	 field containing them both.  */
      if ((ll_bitsize + ll_bitpos == rl_bitpos
	   && lr_bitsize + lr_bitpos == rr_bitpos)
	  || (ll_bitpos == rl_bitpos + rl_bitsize
	      && lr_bitpos == rr_bitpos + rr_bitsize))
	return build (wanted_code, truth_type,
		      make_bit_field_ref (ll_inner, type,
					  ll_bitsize + rl_bitsize,
					  MIN (ll_bitpos, rl_bitpos),
					  ll_unsignedp),
		      make_bit_field_ref (lr_inner, type,
					  lr_bitsize + rr_bitsize,
					  MIN (lr_bitpos, rr_bitpos),
					  lr_unsignedp));

      return 0;
    }

  /* Handle the case of comparisons with constants.  If there is something in
     common between the masks, those bits of the constants must be the same.
     If not, the condition is always false.  Test for this to avoid generating
     incorrect code below.  */
  result = const_binop (BIT_AND_EXPR, ll_mask, rl_mask);
  if (! integer_zerop (result)
      && simple_cst_equal (const_binop (BIT_AND_EXPR, result, l_const),
			   const_binop (BIT_AND_EXPR, result, r_const)) != 1)
    {
      if (wanted_code == NE_EXPR)
	{
	  warning ("`or' of unmatched not-equal tests is always 1");
	  return convert (truth_type, integer_one_node);
	}
      else
	{
	  warning ("`and' of mutually exclusive equal-tests is always zero");
	  return convert (truth_type, integer_zero_node);
	}
    }

  /* Construct the expression we will return.  First get the component
     reference we will make.  Unless the mask is all ones the width of
     that field, perform the mask operation.  Then compare with the
     merged constant.  */
  result = make_bit_field_ref (ll_inner, type, lnbitsize, lnbitpos,
			       ll_unsignedp || rl_unsignedp);

  ll_mask = const_binop (BIT_IOR_EXPR, ll_mask, rl_mask);
  if (! all_ones_mask_p (ll_mask, lnbitsize))
    result = build (BIT_AND_EXPR, type, result, ll_mask);

  return build (wanted_code, truth_type, result,
		const_binop (BIT_IOR_EXPR, l_const, r_const));
}

/* Perform constant folding and related simplification of EXPR.
   The related simplifications include x*1 => x, x*0 => 0, etc.,
   and application of the associative law.
   NOP_EXPR conversions may be removed freely (as long as we
   are careful not to change the C type of the overall expression)
   We cannot simplify through a CONVERT_EXPR, FIX_EXPR or FLOAT_EXPR,
   but we can constant-fold them if they have constant operands.  */

tree
fold (expr) 
     tree expr;
{
  register tree t = expr;
  tree t1 = NULL_TREE;
  tree type = TREE_TYPE (expr);
  register tree arg0, arg1;
  register enum tree_code code = TREE_CODE (t);
  register int kind;

  /* WINS will be nonzero when the switch is done
     if all operands are constant.  */

  int wins = 1;

  /* Return right away if already constant.  */
  if (TREE_CONSTANT (t))
    {
      if (code == CONST_DECL)
	return DECL_INITIAL (t);
      return t;
    }
  
  kind = TREE_CODE_CLASS (code);
  if (kind == 'e' || kind == '<' || kind == '1' || kind == '2' || kind == 'r')
    {
      register int len = tree_code_length[(int) code];
      register int i;
      for (i = 0; i < len; i++)
	{
	  tree op = TREE_OPERAND (t, i);

	  if (op == 0)
	    continue;		/* Valid for CALL_EXPR, at least.  */

	  /* Strip any conversions that don't change the mode.  */
	  STRIP_NOPS (op);
	  
	  if (TREE_CODE (op) != INTEGER_CST
#if ! defined (REAL_IS_NOT_DOUBLE) || defined (REAL_ARITHMETIC)
	      && TREE_CODE (op) != REAL_CST
#endif /* not REAL_IS_NOT_DOUBLE, or REAL_ARITHMETIC */
	      )
	    /* Note that TREE_CONSTANT isn't enough:
	       static var addresses are constant but we can't
	       do arithmetic on them.  */
	    wins = 0;

	  if (i == 0)
	    arg0 = op;
	  else if (i == 1)
	    arg1 = op;
	}
    }

  /* If this is a commutative operation, and ARG0 is a constant, move it
     to ARG1 to reduce the number of tests below.  */
  if ((code == PLUS_EXPR || code == MULT_EXPR || code == MIN_EXPR
       || code == MAX_EXPR || code == BIT_IOR_EXPR || code == BIT_XOR_EXPR
       || code == BIT_AND_EXPR)
      && (TREE_CODE (arg0) == INTEGER_CST || TREE_CODE (arg0) == REAL_CST))
    {
      tree tem = arg0;
      arg0 = arg1; arg1 = tem;

      TREE_OPERAND (t, 0) = arg0;
      TREE_OPERAND (t, 1) = arg1;
    }

  /* Now WINS is set as described above,
     ARG0 is the first operand of EXPR,
     and ARG1 is the second operand (if it has more than one operand).

     First check for cases where an arithmetic operation is applied to a
     compound, conditional, or comparison operation.  Push the arithmetic
     operation inside the compound or conditional to see if any folding
     can then be done.  Convert comparison to conditional for this purpose.
     The also optimizes non-constant cases that used to be done in
     expand_expr.  */
  if (TREE_CODE_CLASS (code) == '1')
    {
      if (TREE_CODE (arg0) == COMPOUND_EXPR)
	return build (COMPOUND_EXPR, type, TREE_OPERAND (arg0, 0),
		      fold (build1 (code, type, TREE_OPERAND (arg0, 1))));
      else if (TREE_CODE (arg0) == COND_EXPR)
	return fold (build (COND_EXPR, type, TREE_OPERAND (arg0, 0),
			    fold (build1 (code, type, TREE_OPERAND (arg0, 1))),
			    fold (build1 (code, type, TREE_OPERAND (arg0, 2)))));
      else if (TREE_CODE_CLASS (TREE_CODE (arg0)) == '<') 
	return fold (build (COND_EXPR, type, arg0,
			    fold (build1 (code, type, integer_one_node)),
			    fold (build1 (code, type, integer_zero_node))));
   }
  else if (TREE_CODE_CLASS (code) == '2')
    {
      if (TREE_CODE (arg1) == COMPOUND_EXPR)
	return build (COMPOUND_EXPR, type, TREE_OPERAND (arg1, 0),
		      fold (build (code, type, arg0, TREE_OPERAND (arg1, 1))));
      else if (TREE_CODE (arg1) == COND_EXPR
	       || TREE_CODE_CLASS (TREE_CODE (arg1)) == '<')
	{
	  tree test, true_value, false_value;

	  if (TREE_CODE (arg1) == COND_EXPR)
	    {
	      test = TREE_OPERAND (arg1, 0);
	      true_value = TREE_OPERAND (arg1, 1);
	      false_value = TREE_OPERAND (arg1, 2);
	    }
	  else
	    {
	      test = arg1;
	      true_value = integer_one_node;
	      false_value = integer_zero_node;
	    }

	  if (TREE_CODE (arg0) != VAR_DECL && TREE_CODE (arg0) != PARM_DECL)
	    arg0 = save_expr (arg0);
	  test = fold (build (COND_EXPR, type, test,
			      fold (build (code, type, arg0, true_value)),
			      fold (build (code, type, arg0, false_value))));
	  if (TREE_CODE (arg0) == SAVE_EXPR)
	    return build (COMPOUND_EXPR, type,
			  convert (void_type_node, arg0), test);
	  else
	    return convert (type, test);
	}

      else if (TREE_CODE (arg0) == COMPOUND_EXPR)
	return build (COMPOUND_EXPR, type, TREE_OPERAND (arg0, 0),
		      fold (build (code, type, TREE_OPERAND (arg0, 1), arg1)));
      else if (TREE_CODE (arg0) == COND_EXPR
	       || TREE_CODE_CLASS (TREE_CODE (arg0)) == '<')
	{
	  tree test, true_value, false_value;

	  if (TREE_CODE (arg0) == COND_EXPR)
	    {
	      test = TREE_OPERAND (arg0, 0);
	      true_value = TREE_OPERAND (arg0, 1);
	      false_value = TREE_OPERAND (arg0, 2);
	    }
	  else
	    {
	      test = arg0;
	      true_value = integer_one_node;
	      false_value = integer_zero_node;
	    }

	  if (TREE_CODE (arg1) != VAR_DECL && TREE_CODE (arg1) != PARM_DECL)
	    arg1 = save_expr (arg1);
	  test = fold (build (COND_EXPR, type, test,
			      fold (build (code, type, true_value, arg1)),
			      fold (build (code, type, false_value, arg1))));
	  if (TREE_CODE (arg1) == SAVE_EXPR)
	    return build (COMPOUND_EXPR, type,
			  convert (void_type_node, arg1), test);
	  else
	    return convert (type, test);
	}
    }
	  
  switch (code)
    {
    case INTEGER_CST:
    case REAL_CST:
    case STRING_CST:
    case COMPLEX_CST:
    case CONSTRUCTOR:
      return t;

    case CONST_DECL:
      return fold (DECL_INITIAL (t));

    case NOP_EXPR:
    case FLOAT_EXPR:
    case CONVERT_EXPR:
    case FIX_TRUNC_EXPR:
      /* Other kinds of FIX are not handled properly by fold_convert.  */
      /* Two conversions in a row are not needed unless:
	 - the intermediate type is narrower than both initial and final, or
	 - the initial type is a pointer type and the precisions of the
	   intermediate and final types differ, or
	 - the final type is a pointer type and the precisions of the 
	  initial and intermediate types differ.  */
      if ((TREE_CODE (TREE_OPERAND (t, 0)) == NOP_EXPR
	   || TREE_CODE (TREE_OPERAND (t, 0)) == CONVERT_EXPR)
	  && (TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (t, 0)))
	      > TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (TREE_OPERAND (t, 0), 0)))
	      ||
	      TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (t, 0)))
	      > TYPE_PRECISION (TREE_TYPE (t)))
	  && ((TREE_UNSIGNED (TREE_TYPE (TREE_OPERAND (t, 0)))
	       && (TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (t, 0)))
		   > TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (TREE_OPERAND (t, 0), 0)))))
	      ==
	      (TREE_UNSIGNED (TREE_TYPE (t))
	       && (TYPE_PRECISION (TREE_TYPE (t))
		   > TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (t, 0))))))
	  && ! ((TREE_CODE (TREE_TYPE (TREE_OPERAND (TREE_OPERAND (t, 0), 0)))
		 == POINTER_TYPE)
		&& (TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (t, 0)))
		    != TYPE_PRECISION (TREE_TYPE (t))))
	  && ! (TREE_CODE (TREE_TYPE (t)) == POINTER_TYPE
		&& (TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (TREE_OPERAND (t, 0), 0)))
		    != TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (t, 0))))))
	return convert (TREE_TYPE (t), TREE_OPERAND (TREE_OPERAND (t, 0), 0));

      if (TREE_CODE (TREE_OPERAND (t, 0)) == MODIFY_EXPR
	  && TREE_CONSTANT (TREE_OPERAND (TREE_OPERAND (t, 0), 1)))
	{
	  /* Don't leave an assignment inside a conversion.  */
	  tree prev = TREE_OPERAND (t, 0);
	  TREE_OPERAND (t, 0) = TREE_OPERAND (prev, 1);
	  /* First do the assignment, then return converted constant.  */
	  t = build (COMPOUND_EXPR, TREE_TYPE (t), prev, fold (t));
	  TREE_USED (t) = 1;
	  return t;
	}
      if (!wins)
	{
	  TREE_CONSTANT (t) = TREE_CONSTANT (arg0);
	  return t;
	}
      return fold_convert (t, arg0);

#if 0  /* This loses on &"foo"[0].  */
    case ARRAY_REF:
	{
	  int i;

	  /* Fold an expression like: "foo"[2] */
	  if (TREE_CODE (arg0) == STRING_CST
	      && TREE_CODE (arg1) == INTEGER_CST
	      && !TREE_INT_CST_HIGH (arg1)
	      && (i = TREE_INT_CST_LOW (arg1)) < TREE_STRING_LENGTH (arg0))
	    {
	      t = build_int_2 (TREE_STRING_POINTER (arg0)[i], 0);
	      TREE_TYPE (t) = TREE_TYPE (TREE_TYPE (arg0));
	      force_fit_type (t);
	    }
	}
      return t;
#endif /* 0 */

    case RANGE_EXPR:
      TREE_CONSTANT (t) = wins;
      return t;

    case NEGATE_EXPR:
      if (wins)
	{
	  if (TREE_CODE (arg0) == INTEGER_CST)
	    {
	      if (TREE_INT_CST_LOW (arg0) == 0)
		t = build_int_2 (0, - TREE_INT_CST_HIGH (arg0));
	      else
		t = build_int_2 (- TREE_INT_CST_LOW (arg0),
				 ~ TREE_INT_CST_HIGH (arg0));
	      TREE_TYPE (t) = type;
	      force_fit_type (t);
	    }
	  else if (TREE_CODE (arg0) == REAL_CST)
	    t = build_real (type, REAL_VALUE_NEGATE (TREE_REAL_CST (arg0)));
	  TREE_TYPE (t) = type;
	}
      else if (TREE_CODE (arg0) == NEGATE_EXPR)
	return TREE_OPERAND (arg0, 0);

      /* Convert - (a - b) to (b - a) for non-floating-point.  */
      else if (TREE_CODE (arg0) == MINUS_EXPR && TREE_CODE (type) != REAL_TYPE)
	return build (MINUS_EXPR, type, TREE_OPERAND (arg0, 1),
		      TREE_OPERAND (arg0, 0));

      return t;

    case ABS_EXPR:
      if (wins)
	{
	  if (TREE_CODE (arg0) == INTEGER_CST)
	    {
	      if (! TREE_UNSIGNED (type)
		  && TREE_INT_CST_HIGH (arg0) < 0)
		{
		  if (TREE_INT_CST_LOW (arg0) == 0)
		    t = build_int_2 (0, - TREE_INT_CST_HIGH (arg0));
		  else
		    t = build_int_2 (- TREE_INT_CST_LOW (arg0),
				     ~ TREE_INT_CST_HIGH (arg0));
		}
	    }
	  else if (TREE_CODE (arg0) == REAL_CST)
	    {
	      if (REAL_VALUES_LESS (TREE_REAL_CST (arg0), dconst0))
		t = build_real (type,
				REAL_VALUE_NEGATE (TREE_REAL_CST (arg0)));
	    }
	  TREE_TYPE (t) = type;
	}
      else if (TREE_CODE (arg0) == ABS_EXPR || TREE_CODE (arg0) == NEGATE_EXPR)
	return build1 (ABS_EXPR, type, TREE_OPERAND (arg0, 0));
      return t;

    case BIT_NOT_EXPR:
      if (wins)
	{
	  if (TREE_CODE (arg0) == INTEGER_CST)
	    t = build_int_2 (~ TREE_INT_CST_LOW (arg0),
			     ~ TREE_INT_CST_HIGH (arg0));
	  TREE_TYPE (t) = type;
	  force_fit_type (t);
	}
      else if (TREE_CODE (arg0) == BIT_NOT_EXPR)
	return TREE_OPERAND (arg0, 0);
      return t;

    case PLUS_EXPR:
      /* A + (-B) -> A - B */
      if (TREE_CODE (arg1) == NEGATE_EXPR)
	return fold (build (MINUS_EXPR, type, arg0, TREE_OPERAND (arg1, 0)));
      else if (TREE_CODE (type) != REAL_TYPE)
	{
	  if (integer_zerop (arg1))
	    return non_lvalue (convert (type, arg0));

	  /* If we are adding two BIT_AND_EXPR's, both of which are and'ing
	     with a constant, and the two constants have no bits in common,
	     we should treat this as a BIT_IOR_EXPR since this may produce more
	     simplifications.  */
	  if (TREE_CODE (arg0) == BIT_AND_EXPR
	      && TREE_CODE (arg1) == BIT_AND_EXPR
	      && TREE_CODE (TREE_OPERAND (arg0, 1)) == INTEGER_CST
	      && TREE_CODE (TREE_OPERAND (arg1, 1)) == INTEGER_CST
	      && integer_zerop (const_binop (BIT_AND_EXPR,
					     TREE_OPERAND (arg0, 1),
					     TREE_OPERAND (arg1, 1))))
	    {
	      code = BIT_IOR_EXPR;
	      goto bit_ior;
	    }
	}
      /* In IEEE floating point, x+0 may not equal x.  */
      else if (TARGET_FLOAT_FORMAT != IEEE_FLOAT_FORMAT
	       && real_zerop (arg1))
	return non_lvalue (convert (type, arg0));
    associate:
      /* In most languages, can't associate operations on floats
	 through parentheses.  Rather than remember where the parentheses
	 were, we don't associate floats at all.  It shouldn't matter much.  */
      if (TREE_CODE (type) == REAL_TYPE)
	goto binary;
      /* The varsign == -1 cases happen only for addition and subtraction.
	 It says that the arg that was split was really CON minus VAR.
	 The rest of the code applies to all associative operations.  */
      if (!wins)
	{
	  tree var, con, tem;
	  int varsign;

	  if (split_tree (arg0, code, &var, &con, &varsign))
	    {
	      if (varsign == -1)
		{
		  /* EXPR is (CON-VAR) +- ARG1.  */
		  /* If it is + and VAR==ARG1, return just CONST.  */
		  if (code == PLUS_EXPR && operand_equal_p (var, arg1, 0))
		    return convert (TREE_TYPE (t), con);
		    
		  /* Otherwise return (CON +- ARG1) - VAR.  */
		  TREE_SET_CODE (t, MINUS_EXPR);
		  TREE_OPERAND (t, 1) = var;
		  TREE_OPERAND (t, 0)
		    = fold (build (code, TREE_TYPE (t), con, arg1));
		}
	      else
		{
		  /* EXPR is (VAR+CON) +- ARG1.  */
		  /* If it is - and VAR==ARG1, return just CONST.  */
		  if (code == MINUS_EXPR && operand_equal_p (var, arg1, 0))
		    return convert (TREE_TYPE (t), con);
		    
		  /* Otherwise return VAR +- (ARG1 +- CON).  */
		  TREE_OPERAND (t, 1) = tem
		    = fold (build (code, TREE_TYPE (t), arg1, con));
		  TREE_OPERAND (t, 0) = var;
		  if (integer_zerop (tem)
		      && (code == PLUS_EXPR || code == MINUS_EXPR))
		    return convert (type, var);
		  /* If we have x +/- (c - d) [c an explicit integer]
		     change it to x -/+ (d - c) since if d is relocatable
		     then the latter can be a single immediate insn
		     and the former cannot.  */
		  if (TREE_CODE (tem) == MINUS_EXPR
		      && TREE_CODE (TREE_OPERAND (tem, 0)) == INTEGER_CST)
		    {
		      tree tem1 = TREE_OPERAND (tem, 1);
		      TREE_OPERAND (tem, 1) = TREE_OPERAND (tem, 0);
		      TREE_OPERAND (tem, 0) = tem1;
		      TREE_SET_CODE (t,
				     (code == PLUS_EXPR ? MINUS_EXPR : PLUS_EXPR));
		    }
		}
	      return t;
	    }

	  if (split_tree (arg1, code, &var, &con, &varsign))
	    {
	      /* EXPR is ARG0 +- (CON +- VAR).  */
	      if (varsign == -1)
		TREE_SET_CODE (t,
			       (code == PLUS_EXPR ? MINUS_EXPR : PLUS_EXPR));
	      if (TREE_CODE (t) == MINUS_EXPR
		  && operand_equal_p (var, arg0, 0))
		{
		  /* If VAR and ARG0 cancel, return just CON or -CON.  */
		  if (code == PLUS_EXPR)
		    return convert (TREE_TYPE (t), con);
		  return fold (build1 (NEGATE_EXPR, TREE_TYPE (t),
				       convert (TREE_TYPE (t), con)));
		}
	      TREE_OPERAND (t, 0)
		= fold (build (code, TREE_TYPE (t), arg0, con));
	      TREE_OPERAND (t, 1) = var;
	      if (integer_zerop (TREE_OPERAND (t, 0))
		  && TREE_CODE (t) == PLUS_EXPR)
		return convert (TREE_TYPE (t), var);
	      return t;
	    }
	}
    binary:
#if defined (REAL_IS_NOT_DOUBLE) && ! defined (REAL_ARITHMETIC)
      if (TREE_CODE (arg1) == REAL_CST)
	return t;
#endif /* REAL_IS_NOT_DOUBLE, and no REAL_ARITHMETIC */
      if (wins)
	t1 = const_binop (code, arg0, arg1);
      if (t1 != NULL_TREE)
	{
	  /* The return value should always have
	     the same type as the original expression.  */
	  TREE_TYPE (t1) = TREE_TYPE (t);
	  return t1;
	}
      return t;

    case MINUS_EXPR:
      if (TREE_CODE (type) != REAL_TYPE)
	{
	  if (! wins && integer_zerop (arg0))
	    return build1 (NEGATE_EXPR, type, arg1);
	  if (integer_zerop (arg1))
	    return non_lvalue (convert (type, arg0));
	}
      /* Convert A - (-B) to A + B.  */
      else if (TREE_CODE (arg1) == NEGATE_EXPR)
	return fold (build (PLUS_EXPR, type, arg0, TREE_OPERAND (arg1, 0)));
      else
	{
	  if (! wins && real_zerop (arg0))
	    return build1 (NEGATE_EXPR, type, arg1);
	  /* In IEEE floating point, x-0 may not equal x.  */
	  if (real_zerop (arg1) && TARGET_FLOAT_FORMAT != IEEE_FLOAT_FORMAT)
	    return non_lvalue (convert (type, arg0));
	}
      /* Fold &x - &x.  This can happen from &x.foo - &x. 
	 Note that can't be done for certain floats even in non-IEEE formats.
	 Also note that operand_equal_p is always false is an operand
	 is volatile.  */

      if (operand_equal_p (arg0, arg1,
			   TREE_CODE (type) == REAL_TYPE))
	return convert (type, integer_zero_node);
      goto associate;

    case MULT_EXPR:
      if (TREE_CODE (type) != REAL_TYPE)
	{
	  if (integer_zerop (arg1))
	    return omit_one_operand (type, arg1, arg0);
	  if (integer_onep (arg1))
	    return non_lvalue (convert (type, arg0));

	  /* (a * (1 << b)) is (a << b)  */
	  if (TREE_CODE (arg1) == LSHIFT_EXPR
	      && integer_onep (TREE_OPERAND (arg1, 0)))
	    return fold (build (LSHIFT_EXPR, type, arg0,
				TREE_OPERAND (arg1, 1)));
	  if (TREE_CODE (arg0) == LSHIFT_EXPR
	      && integer_onep (TREE_OPERAND (arg0, 0)))
	    return fold (build (LSHIFT_EXPR, type, arg1,
				TREE_OPERAND (arg0, 1)));
	}
      /* In IEEE floating point, these optimizations are not correct.  */
      else
	{
	  if (TARGET_FLOAT_FORMAT != IEEE_FLOAT_FORMAT
	      && real_zerop (arg1))
	    return omit_one_operand (type, arg1, arg0);
	  /* In IEEE floating point, x*1 is not equivalent to x for nans.
	     However, ANSI says we can drop signals,
	     so we can do this anyway.  */
	  if (real_onep (arg1))
	    return non_lvalue (convert (type, arg0));
	  /* x*2 is x+x */
	  if (! wins && real_twop (arg1))
	    {
	      tree arg = save_expr (arg0);
	      return build (PLUS_EXPR, type, arg, arg);
	    }
	}
      goto associate;

    case BIT_IOR_EXPR:
    bit_ior:
      if (integer_all_onesp (arg1))
	return omit_one_operand (type, arg1, arg0);
      if (integer_zerop (arg1))
	return non_lvalue (convert (type, arg0));
      t1 = distribute_bit_expr (code, type, arg0, arg1);
      if (t1 != NULL_TREE)
	return t1;
      goto associate;

    case BIT_XOR_EXPR:
      if (integer_zerop (arg1))
	return non_lvalue (convert (type, arg0));
      if (integer_all_onesp (arg1))
	return fold (build1 (BIT_NOT_EXPR, type, arg0));
      goto associate;

    case BIT_AND_EXPR:
    bit_and:
      if (integer_all_onesp (arg1))
	return non_lvalue (convert (type, arg0));
      if (integer_zerop (arg1))
	return omit_one_operand (type, arg1, arg0);
      t1 = distribute_bit_expr (code, type, arg0, arg1);
      if (t1 != NULL_TREE)
	return t1;
      /* Simplify ((int)c & 0x377) into (int)c, if c is unsigned char.  */
      if (TREE_CODE (arg0) == INTEGER_CST && TREE_CODE (arg1) == NOP_EXPR
	  && TREE_UNSIGNED (TREE_TYPE (TREE_OPERAND (arg1, 0))))
	{
	  int prec = TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (arg1, 0)));
	  if (prec < BITS_PER_WORD && prec < HOST_BITS_PER_INT
	      && (~TREE_INT_CST_LOW (arg0) & ((1 << prec) - 1)) == 0)
	    return build1 (NOP_EXPR, type, TREE_OPERAND (arg1, 0));
	}
      if (TREE_CODE (arg1) == INTEGER_CST && TREE_CODE (arg0) == NOP_EXPR
	  && TREE_UNSIGNED (TREE_TYPE (TREE_OPERAND (arg0, 0))))
	{
	  int prec = TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (arg0, 0)));
	  if (prec < BITS_PER_WORD && prec < HOST_BITS_PER_INT
	      && (~TREE_INT_CST_LOW (arg1) & ((1 << prec) - 1)) == 0)
	    return build1 (NOP_EXPR, type, TREE_OPERAND (arg0, 0));
	}
      goto associate;

    case BIT_ANDTC_EXPR:
      if (integer_all_onesp (arg0))
	return non_lvalue (convert (type, arg1));
      if (integer_zerop (arg0))
	return omit_one_operand (type, arg0, arg1);
      if (TREE_CODE (arg1) == INTEGER_CST)
	{
	  arg1 = fold (build1 (BIT_NOT_EXPR, type, arg1));
	  code = BIT_AND_EXPR;
	  goto bit_and;
	}
      goto binary;

    case TRUNC_DIV_EXPR:
    case ROUND_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case CEIL_DIV_EXPR:
    case EXACT_DIV_EXPR:
    case RDIV_EXPR:
      if (integer_onep (arg1))
	return non_lvalue (convert (type, arg0));
      if (integer_zerop (arg1))
	return t;
#if !defined (REAL_IS_NOT_DOUBLE) || defined (REAL_ARITHMETIC)
#ifndef REAL_INFINITY
      if (TREE_CODE (arg1) == REAL_CST
	  && real_zerop (arg1))
	return t;
#endif
#endif /* not REAL_IS_NOT_DOUBLE, or REAL_ARITHMETIC */

      goto binary;

    case CEIL_MOD_EXPR:
    case FLOOR_MOD_EXPR:
    case ROUND_MOD_EXPR:
    case TRUNC_MOD_EXPR:
      if (integer_onep (arg1))
	return omit_one_operand (type, integer_zero_node, arg0);
      if (integer_zerop (arg1))
	return t;
      goto binary;

    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
    case LROTATE_EXPR:
    case RROTATE_EXPR:
      if (integer_zerop (arg1))
	return non_lvalue (convert (type, arg0));
      /* Since negative shift count is not well-defined,
	 don't try to compute it in the compiler.  */
      if (tree_int_cst_lt (arg1, integer_zero_node))
	return t;
      goto binary;

    case MIN_EXPR:
      if (operand_equal_p (arg0, arg1, 0))
	return arg0;
      if (TREE_CODE (type) == INTEGER_TYPE
	  && operand_equal_p (arg1, TYPE_MIN_VALUE (type), 1))
	return omit_one_operand (type, arg1, arg0);
      goto associate;

    case MAX_EXPR:
      if (operand_equal_p (arg0, arg1, 0))
	return arg0;
      if (TREE_CODE (type) == INTEGER_TYPE
	  && operand_equal_p (arg1, TYPE_MAX_VALUE (type), 1))
	return omit_one_operand (type, arg1, arg0);
      goto associate;

    case TRUTH_NOT_EXPR:
      /* Note that the operand of this must be an int
	 and its values must be 0 or 1.
	 ("true" is a fixed value perhaps depending on the language,
	 but we don't handle values other than 1 correctly yet.)  */
      return invert_truthvalue (arg0);

    case TRUTH_ANDIF_EXPR:
      /* Note that the operands of this must be ints
	 and their values must be 0 or 1.
	 ("true" is a fixed value perhaps depending on the language.)  */
      /* If first arg is constant zero, return it.  */
      if (TREE_CODE (arg0) == INTEGER_CST && integer_zerop (arg0))
	return arg0;
    case TRUTH_AND_EXPR:
      /* If either arg is constant true, drop it.  */
      if (TREE_CODE (arg0) == INTEGER_CST && ! integer_zerop (arg0))
	return non_lvalue (arg1);
      if (TREE_CODE (arg1) == INTEGER_CST && ! integer_zerop (arg1))
	return non_lvalue (arg0);
      /* Both known to be zero => return zero.  */
      if (TREE_CODE (arg0) == INTEGER_CST && TREE_CODE (arg1) == INTEGER_CST)
	return arg0;

    truth_andor:
      /* Check for the possibility of merging component references.  If our
	 lhs is another similar operation, try to merge its rhs with our
	 rhs.  Then try to merge our lhs and rhs.  */
      if (optimize)
	{
	  tree tem;

	  if (TREE_CODE (arg0) == code)
	    {
	      tem = merge_component_references (code, type,
						TREE_OPERAND (arg0, 1), arg1);
	      if (tem)
		return fold (build (code, type, TREE_OPERAND (arg0, 0), tem));
	    }

	  tem = merge_component_references (code, type, arg0, arg1);
	  if (tem)
	    return tem;
	}
      return t;

    case TRUTH_ORIF_EXPR:
      /* Note that the operands of this must be ints
	 and their values must be 0 or true.
	 ("true" is a fixed value perhaps depending on the language.)  */
      /* If first arg is constant true, return it.  */
      if (TREE_CODE (arg0) == INTEGER_CST && ! integer_zerop (arg0))
	return arg0;
    case TRUTH_OR_EXPR:
      /* If either arg is constant zero, drop it.  */
      if (TREE_CODE (arg0) == INTEGER_CST && integer_zerop (arg0))
	return non_lvalue (arg1);
      if (TREE_CODE (arg1) == INTEGER_CST && integer_zerop (arg1))
	return non_lvalue (arg0);
      /* Both known to be true => return true.  */
      if (TREE_CODE (arg0) == INTEGER_CST && TREE_CODE (arg1) == INTEGER_CST)
	return arg0;
      goto truth_andor;

    case EQ_EXPR:
    case NE_EXPR:
    case LT_EXPR:
    case GT_EXPR:
    case LE_EXPR:
    case GE_EXPR:
      /* If one arg is a constant integer, put it last.  */
      if (TREE_CODE (arg0) == INTEGER_CST
	  && TREE_CODE (arg1) != INTEGER_CST)
	{
	  TREE_OPERAND (t, 0) = arg1;
	  TREE_OPERAND (t, 1) = arg0;
	  arg0 = TREE_OPERAND (t, 0);
	  arg1 = TREE_OPERAND (t, 1);
	  switch (code)
	    {
	    case GT_EXPR:
	      code = LT_EXPR;
	      break;
	    case GE_EXPR:
	      code = LE_EXPR;
	      break;
	    case LT_EXPR:
	      code = GT_EXPR;
	      break;
	    case LE_EXPR:
	      code = GE_EXPR;
	      break;
	    }
	  TREE_SET_CODE (t, code);
	}

      /* Convert foo++ == CONST into ++foo == CONST + INCR.
	 First, see if one arg is constant; find the constant arg
	 and the other one.  */
      {
	tree constop = 0, varop;
	tree *constoploc;

	if (TREE_CONSTANT (arg1))
	  constoploc = &TREE_OPERAND (t, 1), constop = arg1, varop = arg0;
	if (TREE_CONSTANT (arg0))
	  constoploc = &TREE_OPERAND (t, 0), constop = arg0, varop = arg1;

	if (constop && TREE_CODE (varop) == POSTINCREMENT_EXPR)
	  {
	    tree newconst
	      = fold (build (PLUS_EXPR, TREE_TYPE (varop),
			     constop, TREE_OPERAND (varop, 1)));
	    /* This optimization is invalid for ordered comparisons
	       if CONST+INCR overflows or if foo+incr might overflow.
	       For pointer types we assume overflow doesn't happen.  */
	    if (TREE_CODE (TREE_TYPE (varop)) == POINTER_TYPE
		|| code == EQ_EXPR || code == NE_EXPR)
	      {
		/* This optimization is invalid for floating point
		   if adding one to the constant does not change it.  */
		if (TREE_CODE (TREE_TYPE (newconst)) != REAL_TYPE
		    || !REAL_VALUES_EQUAL (TREE_REAL_CST (newconst),
					   TREE_REAL_CST (constop)))
		  {
		    TREE_SET_CODE (varop, PREINCREMENT_EXPR);
		    *constoploc = newconst;
		    return t;
		  }
	      }
	  }
	else if (constop && TREE_CODE (varop) == POSTDECREMENT_EXPR)
	  {
	    tree newconst
	      = fold (build (MINUS_EXPR, TREE_TYPE (varop),
			     constop, TREE_OPERAND (varop, 1)));
	    if (TREE_CODE (TREE_TYPE (varop)) == POINTER_TYPE
		|| code == EQ_EXPR || code == NE_EXPR)
	      {
		if (TREE_CODE (TREE_TYPE (newconst)) != REAL_TYPE
		    || !REAL_VALUES_EQUAL (TREE_REAL_CST (newconst),
					   TREE_REAL_CST (constop)))
		  {
		    TREE_SET_CODE (varop, PREDECREMENT_EXPR);
		    *constoploc = newconst;
		    return t;
		  }
	      }
	  }
      }

      /* Change X >= CST to X > (CST - 1) if CST is positive.  */
      if (TREE_CODE (arg1) == INTEGER_CST
	  && TREE_CODE (arg0) != INTEGER_CST
	  && ! tree_int_cst_lt (arg1, integer_one_node))
	{
	  switch (TREE_CODE (t))
	    {
	    case GE_EXPR:
	      code = GT_EXPR;
	      TREE_SET_CODE (t, code);
	      arg1 = const_binop (MINUS_EXPR, arg1, integer_one_node);
	      TREE_OPERAND (t, 1) = arg1;
	      break;

	    case LT_EXPR:
	      code = LE_EXPR;
	      TREE_SET_CODE (t, code);
	      arg1 = const_binop (MINUS_EXPR, arg1, integer_one_node);
	      TREE_OPERAND (t, 1) = arg1;
	    }
	}

      /* If we are comparing the result of a comparison to a constant,
	 we can often simplify this, since the comparison result is known to
	 be either 0 or 1.  We can ignore conversions if the LHS is a
	 comparison.  */

      if (TREE_CODE (arg1) == INTEGER_CST)
	{
	  tree comparison = arg0;

	  while (TREE_CODE (comparison) == NOP_EXPR
		 || TREE_CODE (comparison) == CONVERT_EXPR)
	    comparison = TREE_OPERAND (comparison, 0);

	  if (TREE_CODE_CLASS (TREE_CODE (comparison)) == '<'
	      || TREE_CODE (comparison) == TRUTH_ANDIF_EXPR
	      || TREE_CODE (comparison) == TRUTH_ORIF_EXPR
	      || TREE_CODE (comparison) == TRUTH_AND_EXPR
	      || TREE_CODE (comparison) == TRUTH_OR_EXPR
	      || TREE_CODE (comparison) == TRUTH_NOT_EXPR)
	    {
	      /* We do different things depending on whether the
		 constant being compared against is < 0, == 0, == 1, or > 1.
		 Each of those cases, in order, corresponds to one
		 character in a string.  The value of the character is
		 the result to return.  A '0' or '1' means return always true
		 or always false, respectively; 'c' means return the result
		 of the comparison, and 'i' means return the result of the
		 inverted comparison.  */

	      char *actions, action;

	      switch (code)
		{
		case EQ_EXPR:
		  actions = "0ic0";
		  break;
		case NE_EXPR:
		  actions = "1ci1";
		  break;
		case LE_EXPR:
		  actions = "0i11";
		  break;
		case LT_EXPR:
		  actions = "00i1";
		  break;
		case GE_EXPR:
		  actions = "11c0";
		  break;
		case GT_EXPR:
		  actions = "1c00";
		  break;
		}

	      if (tree_int_cst_lt (arg1, integer_zero_node))
		action = actions[0];
	      else if (integer_zerop (arg1))
		action = actions[1];
	      else if (integer_onep (arg1))
		action = actions[2];
	      else
		action = actions[3];

	      switch (action)
		{
		case '0':
		  return omit_one_operand (type, integer_zero_node,
					   comparison);

		case '1':
		  return omit_one_operand (type, integer_one_node, comparison);

		case 'c':
		  return convert (type, comparison);

		case 'i':
		  return convert (type, invert_truthvalue (comparison));
		  
		default:
		  abort ();
		}
	    }
	}
		     
      /* If this is an EQ or NE comparison with zero and ARG0 is
	 (1 << foo) & bar, convert it to (bar >> foo) & 1.  Both require
	 two operations, but the latter can be done in one less insn
	 one machine that have only two-operand insns or on which a
	 constant cannot be the first operand.  */
      if (integer_zerop (arg1) && (code == EQ_EXPR || code == NE_EXPR)
	  && TREE_CODE (arg0) == BIT_AND_EXPR)
	{
	  if (TREE_CODE (TREE_OPERAND (arg0, 0)) == LSHIFT_EXPR
	      && integer_onep (TREE_OPERAND (TREE_OPERAND (arg0, 0), 0)))
	    return
	      fold (build (code, type,
			   build (BIT_AND_EXPR, TREE_TYPE (arg0),
				  build (RSHIFT_EXPR,
					 TREE_TYPE (TREE_OPERAND (arg0, 0)),
					 TREE_OPERAND (arg0, 1),
					 TREE_OPERAND (TREE_OPERAND (arg0, 0), 1)),
				  convert (TREE_TYPE (arg0),
					   integer_one_node)),
			   arg1));
	  else if (TREE_CODE (TREE_OPERAND (arg0, 1)) == LSHIFT_EXPR
		   && integer_onep (TREE_OPERAND (TREE_OPERAND (arg0, 1), 0)))
	    return
	      fold (build (code, type,
			   build (BIT_AND_EXPR, TREE_TYPE (arg0),
				  build (RSHIFT_EXPR,
					 TREE_TYPE (TREE_OPERAND (arg0, 1)),
					 TREE_OPERAND (arg0, 0),
					 TREE_OPERAND (TREE_OPERAND (arg0, 1), 1)),
				  convert (TREE_TYPE (arg0),
					   integer_one_node)),
			   arg1));
	}

      /* If this is an NE comparison of zero with an AND of one, remove the
	 comparison since the AND will give the correct value.  */
      if (code == NE_EXPR && integer_zerop (arg1)
	  && TREE_CODE (arg0) == BIT_AND_EXPR
	  && integer_onep (TREE_OPERAND (arg0, 1)))
	return convert (type, arg0);

      /* If we have (A & C) == C where C is a power of 2, convert this into
	 (A & C) != 0.  Similarly for NE_EXPR.  */
      if ((code == EQ_EXPR || code == NE_EXPR)
	  && TREE_CODE (arg0) == BIT_AND_EXPR
	  && integer_pow2p (TREE_OPERAND (arg0, 1))
	  && operand_equal_p (TREE_OPERAND (arg0, 1), arg1, 0))
	return build (code == EQ_EXPR ? NE_EXPR : EQ_EXPR, type,
		      arg0, integer_zero_node);

      /* Simplify comparison of an integer with itself.
	 (This may not be safe with IEEE floats if they are nans.)  */
      if (operand_equal_p (arg0, arg1, 0)
	  && TREE_CODE (TREE_TYPE (arg1)) == INTEGER_TYPE)
	{
	  switch (code)
	    {
	    case EQ_EXPR:
	    case GE_EXPR:
	    case LE_EXPR:
	      t = build_int_2 (1, 0);
	      TREE_TYPE (t) = type;
	      return t;
	    case NE_EXPR:
	    case GT_EXPR:
	    case LT_EXPR:
	      t = build_int_2 (0, 0);
	      TREE_TYPE (t) = type;
	      return t;
	    }
	}

      /* An unsigned comparison against 0 can be simplified.  */
      if (integer_zerop (arg1)
	  && (TREE_CODE (TREE_TYPE (arg1)) == INTEGER_TYPE
	      || TREE_CODE (TREE_TYPE (arg1)) == POINTER_TYPE)
	  && TREE_UNSIGNED (TREE_TYPE (arg1)))
	{
	  switch (TREE_CODE (t))
	    {
	    case GT_EXPR:
	      TREE_SET_CODE (t, NE_EXPR);
	      break;
	    case LE_EXPR:
	      TREE_SET_CODE (t, EQ_EXPR);
	      break;
	    case GE_EXPR:
	      return omit_one_operand (integer_type_node,
				       integer_one_node, arg0);
	    case LT_EXPR:
	      return omit_one_operand (integer_type_node,
				       integer_zero_node, arg0);
	    }
	}

      /* To compute GT, swap the arguments and do LT.
	 To compute GE, do LT and invert the result.
	 To compute LE, swap the arguments, do LT and invert the result.
	 To compute NE, do EQ and invert the result.  */
      if (code == LE_EXPR || code == GT_EXPR)
	{
	  register tree temp = arg0;
	  arg0 = arg1;
	  arg1 = temp;
	}

      /* Compute a result for LT or EQ if args permit;
	 otherwise return T.  */
      if (TREE_CODE (arg0) == INTEGER_CST
	  && TREE_CODE (arg1) == INTEGER_CST)
	{
	  if (code == EQ_EXPR || code == NE_EXPR)
	    t = build_int_2
	      (TREE_INT_CST_LOW (arg0) == TREE_INT_CST_LOW (arg1)
	       && TREE_INT_CST_HIGH (arg0) == TREE_INT_CST_HIGH (arg1),
	       0);
	  else
	    t = build_int_2 ((TREE_UNSIGNED (TREE_TYPE (arg0))
			      ? INT_CST_LT_UNSIGNED (arg0, arg1)
			      : INT_CST_LT (arg0, arg1)),
			     0);
	}
      /* Assume a nonexplicit constant cannot equal an explicit one,
	 since such code would be undefined anyway.
	 Exception: on sysvr4, using #pragma weak,
	 a label can come out as 0.  */
      else if (TREE_CODE (arg1) == INTEGER_CST
	       && !integer_zerop (arg1)
	       && TREE_CONSTANT (arg0)
	       && TREE_CODE (arg0) == ADDR_EXPR
	       && (code == EQ_EXPR || code == NE_EXPR))
	{
	  t = build_int_2 (0, 0);
	}
      /* Two real constants can be compared explicitly.  */
      else if (TREE_CODE (arg0) == REAL_CST
	       && TREE_CODE (arg1) == REAL_CST)
	{
	  if (code == EQ_EXPR || code == NE_EXPR)
	    t = build_int_2 (REAL_VALUES_EQUAL (TREE_REAL_CST (arg0),
						TREE_REAL_CST (arg1)),
			     0);
	  else
	    t = build_int_2 (REAL_VALUES_LESS (TREE_REAL_CST (arg0),
					       TREE_REAL_CST (arg1)),
			     0);
	}
      else if ((TREE_CODE (arg0) == COMPONENT_REF
		|| TREE_CODE (arg0) == BIT_FIELD_REF)
	       && (code == EQ_EXPR || code == NE_EXPR)
	       /* Handle the constant case even without -O
		  to make sure the warnings are given.  */
	       && (optimize || TREE_CODE (arg1) == INTEGER_CST))
	{
	  tree tem = optimize_bit_field_compare (code, type, arg0, arg1);
	  return tem ? tem : t;
	}

      /* If what we want is other than LT or EQ, invert the result.  */
      if ((code == GE_EXPR || code == LE_EXPR || code == NE_EXPR)
	  && TREE_CODE (t) == INTEGER_CST)
	TREE_INT_CST_LOW (t) ^= 1;
      TREE_TYPE (t) = type;
      return t;

    case COND_EXPR:
      if (TREE_CODE (arg0) == INTEGER_CST)
	return TREE_OPERAND (t, (integer_zerop (arg0) ? 2 : 1));
      else if (operand_equal_p (arg1, TREE_OPERAND (expr, 2), 0))
	return omit_one_operand (type, arg1, arg0);
      else if (integer_onep (TREE_OPERAND (t, 1))
	       && integer_zerop (TREE_OPERAND (t, 2))
	       /* If we try to convert TREE_OPERAND (t, 0) to our type, the
		  call to fold will try to move the conversion inside 
		  a COND, which will recurse.  In that case, the COND_EXPR
		  is probably the best choice, so leave it alone.  */
	       && type == TREE_TYPE (arg0))
	return arg0;
      else if (integer_zerop (arg1) && integer_onep (TREE_OPERAND (t, 2)))
	return convert (type, invert_truthvalue (arg0));

      /* If we have (a >= 0 ? a : -a) or the same with ">", this is an
	 absolute value expression.  */

      if ((TREE_CODE (arg0) == GE_EXPR || TREE_CODE (arg0) == GT_EXPR)
	  && integer_zerop (TREE_OPERAND (arg0, 1))
	  && TREE_CODE (TREE_OPERAND (t, 2)) == NEGATE_EXPR
	  && operand_equal_p (TREE_OPERAND (arg0, 0), arg1, 0)
	  && operand_equal_p (TREE_OPERAND (TREE_OPERAND (t, 2), 0), arg1, 0))
	return fold (build1 (ABS_EXPR, type, arg1));

      /* Similarly for (a <= 0 ? -a : a).  */

      if ((TREE_CODE (arg0) == LE_EXPR || TREE_CODE (arg0) == LT_EXPR)
	  && integer_zerop (TREE_OPERAND (arg0, 1))
	  && TREE_CODE (arg1) == NEGATE_EXPR
	  && operand_equal_p (TREE_OPERAND (arg0, 0), TREE_OPERAND (t, 2), 0)
	  && operand_equal_p (TREE_OPERAND (arg1, 0), TREE_OPERAND (t, 2), 0))
	return fold (build1 (ABS_EXPR, type, TREE_OPERAND (t, 2)));

      /* If we have a GT, GE, LT, or LE comparison, this might be a MIN or
	 MAX test.  If so, make a MIN_EXPR or MAX_EXPR.  */

      if (TREE_CODE (arg0) == GT_EXPR || TREE_CODE (arg0) == GE_EXPR
	  || TREE_CODE (arg0) == LT_EXPR || TREE_CODE (arg0) == LE_EXPR)
	{
	  tree hi_true, lo_true;

	  if (TREE_CODE (arg0) == GT_EXPR || TREE_CODE (arg0) == GE_EXPR)
	    hi_true = TREE_OPERAND (arg0, 0), lo_true = TREE_OPERAND (arg0, 1);
	  else
	    hi_true = TREE_OPERAND (arg0, 1), lo_true = TREE_OPERAND (arg0, 0);

	  if (comparison_equiv_p (hi_true, lo_true, arg1, TREE_OPERAND (t, 2)))
	    /* We use arg1 and the other arg because they must have the same
	       type as the intended result.
	       The values being compared might have a narrower type.  */
	    return fold (build (MAX_EXPR, type, arg1, TREE_OPERAND (t, 2)));
	  else if (comparison_equiv_p (lo_true, hi_true,
				       arg1, TREE_OPERAND (t, 2)))
	    return fold (build (MIN_EXPR, type, arg1, TREE_OPERAND (t, 2)));
	}

      /* Look for cases when we are comparing some expression A for equality
	 with zero and the result is to be zero if A is zero.  In that case,
	 check to see if the value of A is the same as the value to be
	 returned when A is non-zero.

	 There are two cases:  One is where we have (A ? A : 0) and the
	 other is when a single bit is tested (e.g., A & 2 ? 2 : 0).
	 In these cases, the result of the conditional is simply A. 

	 Start by setting ARG1 to be the true value and ARG0 to be the thing
	 compared with zero.  Then check for the two cases above.  */

      if (integer_zerop (TREE_OPERAND (t, 2))
	  && TREE_CODE (arg0) == NE_EXPR
	  && integer_zerop (TREE_OPERAND (arg0, 1))
	  && ! TREE_SIDE_EFFECTS (arg1))
	;
      else if (integer_zerop (arg1)
	       && TREE_CODE (arg0) == EQ_EXPR
	       && integer_zerop (TREE_OPERAND (arg0, 1))
	       && ! TREE_SIDE_EFFECTS (TREE_OPERAND (t, 2)))
	arg1 = TREE_OPERAND (t, 2);
      else
	return t;

      arg0 = TREE_OPERAND (arg0, 0);

      STRIP_NOPS (arg1);
      if (operand_equal_p (arg0, arg1, 0)
	  || (TREE_CODE (arg1) == INTEGER_CST
	      && integer_pow2p (arg1)
	      && TREE_CODE (arg0) == BIT_AND_EXPR
	      && operand_equal_p (TREE_OPERAND (arg0, 1), arg1, 0)))
	return convert (type, arg0);
      return t;

    case COMPOUND_EXPR:
      if (!TREE_SIDE_EFFECTS (arg0))
	return arg1;
      return t;

    default:
      return t;
    } /* switch (code) */
}
