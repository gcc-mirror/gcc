/* Fold a constant sub-tree into a single node for C-compiler
   Copyright (C) 1987, 88, 92, 93, 94, 1995 Free Software Foundation, Inc.

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
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/*@@ This file should be rewritten to use an arbitrary precision
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

/* Handle floating overflow for `const_binop'.  */
static jmp_buf float_error;

static void encode	PROTO((HOST_WIDE_INT *, HOST_WIDE_INT, HOST_WIDE_INT));
static void decode	PROTO((HOST_WIDE_INT *, HOST_WIDE_INT *, HOST_WIDE_INT *));
int div_and_round_double PROTO((enum tree_code, int, HOST_WIDE_INT,
				       HOST_WIDE_INT, HOST_WIDE_INT,
				       HOST_WIDE_INT, HOST_WIDE_INT *,
				       HOST_WIDE_INT *, HOST_WIDE_INT *,
				       HOST_WIDE_INT *));
static int split_tree	PROTO((tree, enum tree_code, tree *, tree *, int *));
static tree const_binop PROTO((enum tree_code, tree, tree, int));
static tree fold_convert PROTO((tree, tree));
static enum tree_code invert_tree_comparison PROTO((enum tree_code));
static enum tree_code swap_tree_comparison PROTO((enum tree_code));
static int truth_value_p PROTO((enum tree_code));
static int operand_equal_for_comparison_p PROTO((tree, tree, tree));
static int twoval_comparison_p PROTO((tree, tree *, tree *, int *));
static tree eval_subst	PROTO((tree, tree, tree, tree, tree));
static tree omit_one_operand PROTO((tree, tree, tree));
static tree pedantic_omit_one_operand PROTO((tree, tree, tree));
static tree distribute_bit_expr PROTO((enum tree_code, tree, tree, tree));
static tree make_bit_field_ref PROTO((tree, tree, int, int, int));
static tree optimize_bit_field_compare PROTO((enum tree_code, tree,
					      tree, tree));
static tree decode_field_reference PROTO((tree, int *, int *,
					  enum machine_mode *, int *,
					  int *, tree *, tree *));
static int all_ones_mask_p PROTO((tree, int));
static int simple_operand_p PROTO((tree));
static tree range_test	PROTO((enum tree_code, tree, enum tree_code,
			       enum tree_code, tree, tree, tree));
static tree unextend	PROTO((tree, int, int, tree));
static tree fold_truthop PROTO((enum tree_code, tree, tree, tree));
static tree strip_compound_expr PROTO((tree, tree));

#ifndef BRANCH_COST
#define BRANCH_COST 1
#endif

/* Yield nonzero if a signed left shift of A by B bits overflows.  */
#define left_shift_overflows(a, b)  ((a)  !=  ((a) << (b)) >> (b))

/* Suppose A1 + B1 = SUM1, using 2's complement arithmetic ignoring overflow.
   Suppose A, B and SUM have the same respective signs as A1, B1, and SUM1.
   Then this yields nonzero if overflow occurred during the addition.
   Overflow occurs if A and B have the same sign, but A and SUM differ in sign.
   Use `^' to test whether signs differ, and `< 0' to isolate the sign.  */
#define overflow_sum_sign(a, b, sum) ((~((a) ^ (b)) & ((a) ^ (sum))) < 0)

/* To do constant folding on INTEGER_CST nodes requires two-word arithmetic.
   We do that by representing the two-word integer in 4 words, with only
   HOST_BITS_PER_WIDE_INT/2 bits stored in each word, as a positive number.  */

#define LOWPART(x) \
  ((x) & (((unsigned HOST_WIDE_INT) 1 << (HOST_BITS_PER_WIDE_INT/2)) - 1))
#define HIGHPART(x) \
  ((unsigned HOST_WIDE_INT) (x) >> HOST_BITS_PER_WIDE_INT/2)
#define BASE ((unsigned HOST_WIDE_INT) 1 << HOST_BITS_PER_WIDE_INT/2)

/* Unpack a two-word integer into 4 words.
   LOW and HI are the integer, as two `HOST_WIDE_INT' pieces.
   WORDS points to the array of HOST_WIDE_INTs.  */

static void
encode (words, low, hi)
     HOST_WIDE_INT *words;
     HOST_WIDE_INT low, hi;
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
decode (words, low, hi)
     HOST_WIDE_INT *words;
     HOST_WIDE_INT *low, *hi;
{
  *low = words[0] | words[1] * BASE;
  *hi = words[2] | words[3] * BASE;
}

/* Make the integer constant T valid for its type
   by setting to 0 or 1 all the bits in the constant
   that don't belong in the type.
   Yield 1 if a signed overflow occurs, 0 otherwise.
   If OVERFLOW is nonzero, a signed overflow has already occurred
   in calculating T, so propagate it.

   Make the real constant T valid for its type by calling CHECK_FLOAT_VALUE,
   if it exists.  */

int
force_fit_type (t, overflow)
     tree t;
     int overflow;
{
  HOST_WIDE_INT low, high;
  register int prec;

  if (TREE_CODE (t) == REAL_CST)
    {
#ifdef CHECK_FLOAT_VALUE
      CHECK_FLOAT_VALUE (TYPE_MODE (TREE_TYPE (t)), TREE_REAL_CST (t),
			 overflow);
#endif
      return overflow;
    }

  else if (TREE_CODE (t) != INTEGER_CST)
    return overflow;

  low = TREE_INT_CST_LOW (t);
  high = TREE_INT_CST_HIGH (t);

  if (TREE_CODE (TREE_TYPE (t)) == POINTER_TYPE)
    prec = POINTER_SIZE;
  else
    prec = TYPE_PRECISION (TREE_TYPE (t));

  /* First clear all bits that are beyond the type's precision.  */

  if (prec == 2 * HOST_BITS_PER_WIDE_INT)
    ;
  else if (prec > HOST_BITS_PER_WIDE_INT)
    {
      TREE_INT_CST_HIGH (t)
	&= ~((HOST_WIDE_INT) (-1) << (prec - HOST_BITS_PER_WIDE_INT));
    }
  else
    {
      TREE_INT_CST_HIGH (t) = 0;
      if (prec < HOST_BITS_PER_WIDE_INT)
	TREE_INT_CST_LOW (t) &= ~((HOST_WIDE_INT) (-1) << prec);
    }

  /* Unsigned types do not suffer sign extension or overflow.  */
  if (TREE_UNSIGNED (TREE_TYPE (t)))
    return overflow;

  /* If the value's sign bit is set, extend the sign.  */
  if (prec != 2 * HOST_BITS_PER_WIDE_INT
      && (prec > HOST_BITS_PER_WIDE_INT
	  ? (TREE_INT_CST_HIGH (t)
	     & ((HOST_WIDE_INT) 1 << (prec - HOST_BITS_PER_WIDE_INT - 1)))
	  : TREE_INT_CST_LOW (t) & ((HOST_WIDE_INT) 1 << (prec - 1))))
    {
      /* Value is negative:
	 set to 1 all the bits that are outside this type's precision.  */
      if (prec > HOST_BITS_PER_WIDE_INT)
	{
	  TREE_INT_CST_HIGH (t)
	    |= ((HOST_WIDE_INT) (-1) << (prec - HOST_BITS_PER_WIDE_INT));
	}
      else
	{
	  TREE_INT_CST_HIGH (t) = -1;
	  if (prec < HOST_BITS_PER_WIDE_INT)
	    TREE_INT_CST_LOW (t) |= ((HOST_WIDE_INT) (-1) << prec);
	}
    }

  /* Yield nonzero if signed overflow occurred.  */
  return
    ((overflow | (low ^ TREE_INT_CST_LOW (t)) | (high ^ TREE_INT_CST_HIGH (t)))
     != 0);
}

/* Add two doubleword integers with doubleword result.
   Each argument is given as two `HOST_WIDE_INT' pieces.
   One argument is L1 and H1; the other, L2 and H2.
   The value is stored as two `HOST_WIDE_INT' pieces in *LV and *HV.  */

int
add_double (l1, h1, l2, h2, lv, hv)
     HOST_WIDE_INT l1, h1, l2, h2;
     HOST_WIDE_INT *lv, *hv;
{
  HOST_WIDE_INT l, h;

  l = l1 + l2;
  h = h1 + h2 + ((unsigned HOST_WIDE_INT) l < l1);

  *lv = l;
  *hv = h;
  return overflow_sum_sign (h1, h2, h);
}

/* Negate a doubleword integer with doubleword result.
   Return nonzero if the operation overflows, assuming it's signed.
   The argument is given as two `HOST_WIDE_INT' pieces in L1 and H1.
   The value is stored as two `HOST_WIDE_INT' pieces in *LV and *HV.  */

int
neg_double (l1, h1, lv, hv)
     HOST_WIDE_INT l1, h1;
     HOST_WIDE_INT *lv, *hv;
{
  if (l1 == 0)
    {
      *lv = 0;
      *hv = - h1;
      return (*hv & h1) < 0;
    }
  else
    {
      *lv = - l1;
      *hv = ~ h1;
      return 0;
    }
}

/* Multiply two doubleword integers with doubleword result.
   Return nonzero if the operation overflows, assuming it's signed.
   Each argument is given as two `HOST_WIDE_INT' pieces.
   One argument is L1 and H1; the other, L2 and H2.
   The value is stored as two `HOST_WIDE_INT' pieces in *LV and *HV.  */

int
mul_double (l1, h1, l2, h2, lv, hv)
     HOST_WIDE_INT l1, h1, l2, h2;
     HOST_WIDE_INT *lv, *hv;
{
  HOST_WIDE_INT arg1[4];
  HOST_WIDE_INT arg2[4];
  HOST_WIDE_INT prod[4 * 2];
  register unsigned HOST_WIDE_INT carry;
  register int i, j, k;
  HOST_WIDE_INT toplow, tophigh, neglow, neghigh;

  encode (arg1, l1, h1);
  encode (arg2, l2, h2);

  bzero ((char *) prod, sizeof prod);

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

  decode (prod, lv, hv);	/* This ignores prod[4] through prod[4*2-1] */

  /* Check for overflow by calculating the top half of the answer in full;
     it should agree with the low half's sign bit.  */
  decode (prod+4, &toplow, &tophigh);
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

/* Shift the doubleword integer in L1, H1 left by COUNT places
   keeping only PREC bits of result.
   Shift right if COUNT is negative.
   ARITH nonzero specifies arithmetic shifting; otherwise use logical shift.
   Store the value as two `HOST_WIDE_INT' pieces in *LV and *HV.  */

void
lshift_double (l1, h1, count, prec, lv, hv, arith)
     HOST_WIDE_INT l1, h1, count;
     int prec;
     HOST_WIDE_INT *lv, *hv;
     int arith;
{
  if (count < 0)
    {
      rshift_double (l1, h1, - count, prec, lv, hv, arith);
      return;
    }
  
#ifdef SHIFT_COUNT_TRUNCATED
  if (SHIFT_COUNT_TRUNCATED)
    count %= prec;
#endif

  if (count >= HOST_BITS_PER_WIDE_INT)
    {
      *hv = (unsigned HOST_WIDE_INT) l1 << count - HOST_BITS_PER_WIDE_INT;
      *lv = 0;
    }
  else
    {
      *hv = (((unsigned HOST_WIDE_INT) h1 << count)
	     | ((unsigned HOST_WIDE_INT) l1 >> HOST_BITS_PER_WIDE_INT - count - 1 >> 1));
      *lv = (unsigned HOST_WIDE_INT) l1 << count;
    }
}

/* Shift the doubleword integer in L1, H1 right by COUNT places
   keeping only PREC bits of result.  COUNT must be positive.
   ARITH nonzero specifies arithmetic shifting; otherwise use logical shift.
   Store the value as two `HOST_WIDE_INT' pieces in *LV and *HV.  */

void
rshift_double (l1, h1, count, prec, lv, hv, arith)
     HOST_WIDE_INT l1, h1, count;
     int prec;
     HOST_WIDE_INT *lv, *hv;
     int arith;
{
  unsigned HOST_WIDE_INT signmask;
  signmask = (arith
	      ? -((unsigned HOST_WIDE_INT) h1 >> (HOST_BITS_PER_WIDE_INT - 1))
	      : 0);

#ifdef SHIFT_COUNT_TRUNCATED
  if (SHIFT_COUNT_TRUNCATED)
    count %= prec;
#endif

  if (count >= HOST_BITS_PER_WIDE_INT)
    {
      *hv = signmask;
      *lv = ((signmask << 2 * HOST_BITS_PER_WIDE_INT - count - 1 << 1)
	     | ((unsigned HOST_WIDE_INT) h1 >> count - HOST_BITS_PER_WIDE_INT));
    }
  else
    {
      *lv = (((unsigned HOST_WIDE_INT) l1 >> count)
	     | ((unsigned HOST_WIDE_INT) h1 << HOST_BITS_PER_WIDE_INT - count - 1 << 1));
      *hv = ((signmask << HOST_BITS_PER_WIDE_INT - count)
	     | ((unsigned HOST_WIDE_INT) h1 >> count));
    }
}

/* Rotate the doubleword integer in L1, H1 left by COUNT places
   keeping only PREC bits of result.
   Rotate right if COUNT is negative.
   Store the value as two `HOST_WIDE_INT' pieces in *LV and *HV.  */

void
lrotate_double (l1, h1, count, prec, lv, hv)
     HOST_WIDE_INT l1, h1, count;
     int prec;
     HOST_WIDE_INT *lv, *hv;
{
  HOST_WIDE_INT s1l, s1h, s2l, s2h;

  count %= prec;
  if (count < 0)
    count += prec;

  lshift_double (l1, h1, count, prec, &s1l, &s1h, 0);
  rshift_double (l1, h1, prec - count, prec, &s2l, &s2h, 0);
  *lv = s1l | s2l;
  *hv = s1h | s2h;
}

/* Rotate the doubleword integer in L1, H1 left by COUNT places
   keeping only PREC bits of result.  COUNT must be positive.
   Store the value as two `HOST_WIDE_INT' pieces in *LV and *HV.  */

void
rrotate_double (l1, h1, count, prec, lv, hv)
     HOST_WIDE_INT l1, h1, count;
     int prec;
     HOST_WIDE_INT *lv, *hv;
{
  HOST_WIDE_INT s1l, s1h, s2l, s2h;

  count %= prec;
  if (count < 0)
    count += prec;

  rshift_double (l1, h1, count, prec, &s1l, &s1h, 0);
  lshift_double (l1, h1, prec - count, prec, &s2l, &s2h, 0);
  *lv = s1l | s2l;
  *hv = s1h | s2h;
}

/* Divide doubleword integer LNUM, HNUM by doubleword integer LDEN, HDEN
   for a quotient (stored in *LQUO, *HQUO) and remainder (in *LREM, *HREM).
   CODE is a tree code for a kind of division, one of
   TRUNC_DIV_EXPR, FLOOR_DIV_EXPR, CEIL_DIV_EXPR, ROUND_DIV_EXPR
   or EXACT_DIV_EXPR
   It controls how the quotient is rounded to a integer.
   Return nonzero if the operation overflows.
   UNS nonzero says do unsigned division.  */

int
div_and_round_double (code, uns,
		      lnum_orig, hnum_orig, lden_orig, hden_orig,
		      lquo, hquo, lrem, hrem)
     enum tree_code code;
     int uns;
     HOST_WIDE_INT lnum_orig, hnum_orig; /* num == numerator == dividend */
     HOST_WIDE_INT lden_orig, hden_orig; /* den == denominator == divisor */
     HOST_WIDE_INT *lquo, *hquo, *lrem, *hrem;
{
  int quo_neg = 0;
  HOST_WIDE_INT num[4 + 1];	/* extra element for scaling.  */
  HOST_WIDE_INT den[4], quo[4];
  register int i, j;
  unsigned HOST_WIDE_INT work;
  register unsigned HOST_WIDE_INT carry = 0;
  HOST_WIDE_INT lnum = lnum_orig;
  HOST_WIDE_INT hnum = hnum_orig;
  HOST_WIDE_INT lden = lden_orig;
  HOST_WIDE_INT hden = hden_orig;
  int overflow = 0;

  if ((hden == 0) && (lden == 0))
    abort ();

  /* calculate quotient sign and convert operands to unsigned.  */
  if (!uns) 
    {
      if (hnum < 0)
	{
	  quo_neg = ~ quo_neg;
	  /* (minimum integer) / (-1) is the only overflow case.  */
	  if (neg_double (lnum, hnum, &lnum, &hnum) && (lden & hden) == -1)
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
      *lquo = lnum / (unsigned HOST_WIDE_INT) lden;
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

  bzero ((char *) quo, sizeof quo);

  bzero ((char *) num, sizeof num);	/* to zero 9th element */
  bzero ((char *) den, sizeof den);

  encode (num, lnum, hnum); 
  encode (den, lden, hden);

  /* Special code for when the divisor < BASE.  */
  if (hden == 0 && lden < BASE)
    {
      /* hnum != 0 already checked.  */
      for (i = 4 - 1; i >= 0; i--)
	{
	  work = num[i] + carry * BASE;
	  quo[i] = work / (unsigned HOST_WIDE_INT) lden;
	  carry = work % (unsigned HOST_WIDE_INT) lden;
	}
    }
  else
    {
      /* Full double precision division,
	 with thanks to Don Knuth's "Seminumerical Algorithms".  */
    int num_hi_sig, den_hi_sig;
    unsigned HOST_WIDE_INT quo_est, scale;

    /* Find the highest non-zero divisor digit.  */
    for (i = 4 - 1; ; i--)
      if (den[i] != 0) {
	den_hi_sig = i;
	break;
      }

    /* Insure that the first digit of the divisor is at least BASE/2.
       This is required by the quotient digit estimation algorithm.  */

    scale = BASE / (den[den_hi_sig] + 1);
    if (scale > 1) {		/* scale divisor and dividend */
      carry = 0;
      for (i = 0; i <= 4 - 1; i++) {
	work = (num[i] * scale) + carry;
	num[i] = LOWPART (work);
	carry = HIGHPART (work);
      } num[4] = carry;
      carry = 0;
      for (i = 0; i <= 4 - 1; i++) {
	work = (den[i] * scale) + carry;
	den[i] = LOWPART (work);
	carry = HIGHPART (work);
	if (den[i] != 0) den_hi_sig = i;
      }
    }

    num_hi_sig = 4;

    /* Main loop */
    for (i = num_hi_sig - den_hi_sig - 1; i >= 0; i--) {
      /* guess the next quotient digit, quo_est, by dividing the first
	 two remaining dividend digits by the high order quotient digit.
	 quo_est is never low and is at most 2 high.  */
      unsigned HOST_WIDE_INT tmp;

      num_hi_sig = i + den_hi_sig + 1;
      work = num[num_hi_sig] * BASE + num[num_hi_sig - 1];
      if (num[num_hi_sig] != den[den_hi_sig])
	quo_est = work / den[den_hi_sig];
      else
	quo_est = BASE - 1;

      /* refine quo_est so it's usually correct, and at most one high.   */
      tmp = work - quo_est * den[den_hi_sig];
      if (tmp < BASE
	  && den[den_hi_sig - 1] * quo_est > (tmp * BASE + num[num_hi_sig - 2]))
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

      /* if quo_est was high by one, then num[i] went negative and
	 we need to correct things.  */

      if (num[num_hi_sig] < carry)
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

      /* store the quotient digit.  */
      quo[i] = quo_est;
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
      return overflow;

    case FLOOR_DIV_EXPR:
    case FLOOR_MOD_EXPR:	/* round toward negative infinity */
      if (quo_neg && (*lrem != 0 || *hrem != 0))   /* ratio < 0 && rem != 0 */
	{
	  /* quo = quo - 1;  */
	  add_double (*lquo, *hquo, (HOST_WIDE_INT) -1, (HOST_WIDE_INT)  -1,
		      lquo, hquo);
	}
      else return overflow;
      break;

    case CEIL_DIV_EXPR:
    case CEIL_MOD_EXPR:		/* round toward positive infinity */
      if (!quo_neg && (*lrem != 0 || *hrem != 0))  /* ratio > 0 && rem != 0 */
	{
	  add_double (*lquo, *hquo, (HOST_WIDE_INT) 1, (HOST_WIDE_INT) 0,
		      lquo, hquo);
	}
      else return overflow;
      break;
    
    case ROUND_DIV_EXPR:
    case ROUND_MOD_EXPR:	/* round to closest integer */
      {
	HOST_WIDE_INT labs_rem = *lrem, habs_rem = *hrem;
	HOST_WIDE_INT labs_den = lden, habs_den = hden, ltwice, htwice;

	/* get absolute values */
	if (*hrem < 0) neg_double (*lrem, *hrem, &labs_rem, &habs_rem);
	if (hden < 0) neg_double (lden, hden, &labs_den, &habs_den);

	/* if (2 * abs (lrem) >= abs (lden)) */
	mul_double ((HOST_WIDE_INT) 2, (HOST_WIDE_INT) 0,
		    labs_rem, habs_rem, &ltwice, &htwice);
	if (((unsigned HOST_WIDE_INT) habs_den
	     < (unsigned HOST_WIDE_INT) htwice)
	    || (((unsigned HOST_WIDE_INT) habs_den
		 == (unsigned HOST_WIDE_INT) htwice)
		&& ((HOST_WIDE_INT unsigned) labs_den
		    < (unsigned HOST_WIDE_INT) ltwice)))
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
	else return overflow;
      }
      break;

    default:
      abort ();
    }

  /* compute true remainder:  rem = num - (quo * den)  */
  mul_double (*lquo, *hquo, lden_orig, hden_orig, lrem, hrem);
  neg_double (*lrem, *hrem, lrem, hrem);
  add_double (lnum_orig, hnum_orig, *lrem, *hrem, lrem, hrem);
  return overflow;
}

#ifndef REAL_ARITHMETIC
/* Effectively truncate a real value to represent the nearest possible value
   in a narrower mode.  The result is actually represented in the same data
   type as the argument, but its value is usually different.

   A trap may occur during the FP operations and it is the responsibility
   of the calling function to have a handler established.  */

REAL_VALUE_TYPE
real_value_truncate (mode, arg)
     enum machine_mode mode;
     REAL_VALUE_TYPE arg;
{
  return REAL_VALUE_TRUNCATE (mode, arg);
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

/* Check for a negative IEEE double precision number.  */

int
target_negative (x)
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
      return u.big_endian.sign;
    }
  else
    {
      u.d = x;
      return u.little_endian.sign;
    }
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
   (This can be overridden by redefining REAL_VALUE_NEGATIVE.)  */

target_negative (x)
     REAL_VALUE_TYPE x;
{
  return x < 0;
}
#endif /* Target not IEEE */
#endif /* no REAL_ARITHMETIC */

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
      || (! FLOAT_TYPE_P (TREE_TYPE (in))
	  /* We can associate addition and subtraction together
	     (even though the C standard doesn't say so)
	     for integers because the value is not affected.
	     For reals, the value might be affected, so we can't.  */
	  && ((code == PLUS_EXPR && TREE_CODE (in) == MINUS_EXPR)
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
   or at least are the same kind of constant and the same machine mode.

   If NOTRUNC is nonzero, do not truncate the result to fit the data type.  */

static tree
const_binop (code, arg1, arg2, notrunc)
     enum tree_code code;
     register tree arg1, arg2;
     int notrunc;
{
  if (TREE_CODE (arg1) == INTEGER_CST)
    {
      register HOST_WIDE_INT int1l = TREE_INT_CST_LOW (arg1);
      register HOST_WIDE_INT int1h = TREE_INT_CST_HIGH (arg1);
      HOST_WIDE_INT int2l = TREE_INT_CST_LOW (arg2);
      HOST_WIDE_INT int2h = TREE_INT_CST_HIGH (arg2);
      HOST_WIDE_INT low, hi;
      HOST_WIDE_INT garbagel, garbageh;
      register tree t;
      int uns = TREE_UNSIGNED (TREE_TYPE (arg1));
      int overflow = 0;

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
	  /* It's unclear from the C standard whether shifts can overflow.
	     The following code ignores overflow; perhaps a C standard
	     interpretation ruling is needed.  */
	  lshift_double (int1l, int1h, int2l,
			 TYPE_PRECISION (TREE_TYPE (arg1)),
			 &low, &hi,
			 !uns);
	  t = build_int_2 (low, hi);
	  TREE_TYPE (t) = TREE_TYPE (arg1);
	  if (!notrunc)
	    force_fit_type (t, 0);
	  TREE_OVERFLOW (t) = TREE_OVERFLOW (arg1) | TREE_OVERFLOW (arg2);
	  TREE_CONSTANT_OVERFLOW (t)
	    = TREE_CONSTANT_OVERFLOW (arg1) | TREE_CONSTANT_OVERFLOW (arg2);
	  return t;

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
	      if ((unsigned HOST_WIDE_INT) int2l < int1l)
		{
		  hi = int2h++;
		  overflow = int2h < hi;
		}
	      t = build_int_2 (int2l, int2h);
	      break;
	    }
	  if (int2h == 0)
	    {
	      int1l += int2l;
	      if ((unsigned HOST_WIDE_INT) int1l < int2l)
		{
		  hi = int1h++;
		  overflow = int1h < hi;
		}
	      t = build_int_2 (int1l, int1h);
	      break;
	    }
	  overflow = add_double (int1l, int1h, int2l, int2h, &low, &hi);
	  t = build_int_2 (low, hi);
	  break;

	case MINUS_EXPR:
	  if (int2h == 0 && int2l == 0)
	    {
	      t = build_int_2 (int1l, int1h);
	      break;
	    }
	  neg_double (int2l, int2h, &low, &hi);
	  add_double (int1l, int1h, low, hi, &low, &hi);
	  overflow = overflow_sum_sign (hi, int2h, int1h);
	  t = build_int_2 (low, hi);
	  break;

	case MULT_EXPR:
	  overflow = mul_double (int1l, int1h, int2l, int2h, &low, &hi);
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
	  overflow = div_and_round_double (code, uns,
					   int1l, int1h, int2l, int2h,
					   &low, &hi, &garbagel, &garbageh);
	  t = build_int_2 (low, hi);
	  break;

	case TRUNC_MOD_EXPR: case ROUND_MOD_EXPR: 
	case FLOOR_MOD_EXPR: case CEIL_MOD_EXPR:
	  overflow = div_and_round_double (code, uns,
					   int1l, int1h, int2l, int2h,
					   &garbagel, &garbageh, &low, &hi);
	  t = build_int_2 (low, hi);
	  break;

	case MIN_EXPR:
	case MAX_EXPR:
	  if (uns)
	    {
	      low = (((unsigned HOST_WIDE_INT) int1h
		      < (unsigned HOST_WIDE_INT) int2h)
		     || (((unsigned HOST_WIDE_INT) int1h
			  == (unsigned HOST_WIDE_INT) int2h)
			 && ((unsigned HOST_WIDE_INT) int1l
			     < (unsigned HOST_WIDE_INT) int2l)));
	    }
	  else
	    {
	      low = ((int1h < int2h)
		     || ((int1h == int2h)
			 && ((unsigned HOST_WIDE_INT) int1l
			     < (unsigned HOST_WIDE_INT) int2l)));
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
      TREE_OVERFLOW (t)
	= ((notrunc ? !uns && overflow : force_fit_type (t, overflow && !uns))
	   | TREE_OVERFLOW (arg1)
	   | TREE_OVERFLOW (arg2));
      TREE_CONSTANT_OVERFLOW (t) = (TREE_OVERFLOW (t)
				    | TREE_CONSTANT_OVERFLOW (arg1)
				    | TREE_CONSTANT_OVERFLOW (arg2));
      return t;
    }
#if ! defined (REAL_IS_NOT_DOUBLE) || defined (REAL_ARITHMETIC)
  if (TREE_CODE (arg1) == REAL_CST)
    {
      REAL_VALUE_TYPE d1;
      REAL_VALUE_TYPE d2;
      int overflow = 0;
      REAL_VALUE_TYPE value;
      tree t;

      d1 = TREE_REAL_CST (arg1);
      d2 = TREE_REAL_CST (arg2);

      /* If either operand is a NaN, just return it.  Otherwise, set up
	 for floating-point trap; we return an overflow.  */
      if (REAL_VALUE_ISNAN (d1))
	return arg1;
      else if (REAL_VALUE_ISNAN (d2))
	return arg2;
      else if (setjmp (float_error))
	{
	  t = copy_node (arg1);
	  overflow = 1;
	  goto got_float;
	}

      set_float_handler (float_error);

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
      t = build_real (TREE_TYPE (arg1),
		      real_value_truncate (TYPE_MODE (TREE_TYPE (arg1)), value));
    got_float:
      set_float_handler (NULL_PTR);

      TREE_OVERFLOW (t)
	= (force_fit_type (t, overflow)
	   | TREE_OVERFLOW (arg1) | TREE_OVERFLOW (arg2));
      TREE_CONSTANT_OVERFLOW (t)
	= TREE_OVERFLOW (t)
	  | TREE_CONSTANT_OVERFLOW (arg1)
	  | TREE_CONSTANT_OVERFLOW (arg2);
      return t;
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
	  t = build_complex (const_binop (PLUS_EXPR, r1, r2, notrunc),
			     const_binop (PLUS_EXPR, i1, i2, notrunc));
	  break;

	case MINUS_EXPR:
	  t = build_complex (const_binop (MINUS_EXPR, r1, r2, notrunc),
			     const_binop (MINUS_EXPR, i1, i2, notrunc));
	  break;

	case MULT_EXPR:
	  t = build_complex (const_binop (MINUS_EXPR,
					  const_binop (MULT_EXPR,
						       r1, r2, notrunc),
					  const_binop (MULT_EXPR,
						       i1, i2, notrunc),
					  notrunc),
			     const_binop (PLUS_EXPR,
					  const_binop (MULT_EXPR,
						       r1, i2, notrunc),
					  const_binop (MULT_EXPR,
						       i1, r2, notrunc),
					  notrunc));
	  break;

	case RDIV_EXPR:
	  {
	    register tree magsquared
	      = const_binop (PLUS_EXPR,
			     const_binop (MULT_EXPR, r2, r2, notrunc),
			     const_binop (MULT_EXPR, i2, i2, notrunc),
			     notrunc);

	    t = build_complex
	      (const_binop (INTEGRAL_TYPE_P (TREE_TYPE (r1))
			    ? TRUNC_DIV_EXPR : RDIV_EXPR,
			    const_binop (PLUS_EXPR,
					 const_binop (MULT_EXPR, r1, r2,
						      notrunc),
					 const_binop (MULT_EXPR, i1, i2,
						      notrunc),
					 notrunc),
			    magsquared, notrunc),
	       const_binop (INTEGRAL_TYPE_P (TREE_TYPE (r1))
			    ? TRUNC_DIV_EXPR : RDIV_EXPR,
			    const_binop (MINUS_EXPR,
					 const_binop (MULT_EXPR, i1, r2,
						      notrunc),
					 const_binop (MULT_EXPR, r1, i2,
						      notrunc),
					 notrunc),
			    magsquared, notrunc));
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
     unsigned HOST_WIDE_INT number;
{
  register tree t;
  /* Type-size nodes already made for small sizes.  */
  static tree size_table[2*HOST_BITS_PER_WIDE_INT + 1];

  if (number < 2*HOST_BITS_PER_WIDE_INT + 1
      && size_table[number] != 0)
    return size_table[number];
  if (number < 2*HOST_BITS_PER_WIDE_INT + 1)
    {
      push_obstacks_nochange ();
      /* Make this a permanent node.  */
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
      return const_binop (code, arg0, arg1, 0);
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
  int overflow = 0;

  if (TREE_CODE (type) == POINTER_TYPE || INTEGRAL_TYPE_P (type))
    {
      if (TREE_CODE (arg1) == INTEGER_CST)
	{
	  /* If we would build a constant wider than GCC supports,
	     leave the conversion unfolded.  */
	  if (TYPE_PRECISION (type) > 2 * HOST_BITS_PER_WIDE_INT)
	    return t;

	  /* Given an integer constant, make new constant with new type,
	     appropriately sign-extended or truncated.  */
	  t = build_int_2 (TREE_INT_CST_LOW (arg1),
			   TREE_INT_CST_HIGH (arg1));
	  TREE_TYPE (t) = type;
	  /* Indicate an overflow if (1) ARG1 already overflowed,
	     or (2) force_fit_type indicates an overflow.
	     Tell force_fit_type that an overflow has already occurred
	     if ARG1 is a too-large unsigned value and T is signed.  */
	  TREE_OVERFLOW (t)
	    = (TREE_OVERFLOW (arg1)
	       | force_fit_type (t,
				 (TREE_INT_CST_HIGH (arg1) < 0
				  & (TREE_UNSIGNED (type)
				     < TREE_UNSIGNED (TREE_TYPE (arg1))))));
	  TREE_CONSTANT_OVERFLOW (t)
	    = TREE_OVERFLOW (t) | TREE_CONSTANT_OVERFLOW (arg1);
	}
#if !defined (REAL_IS_NOT_DOUBLE) || defined (REAL_ARITHMETIC)
      else if (TREE_CODE (arg1) == REAL_CST)
	{
	  /* Don't initialize these, use assignments.
	     Initialized local aggregates don't work on old compilers.  */
	  REAL_VALUE_TYPE x;
	  REAL_VALUE_TYPE l;
	  REAL_VALUE_TYPE u;

	  x = TREE_REAL_CST (arg1);
	  l = real_value_from_int_cst (TYPE_MIN_VALUE (type));
	  u = real_value_from_int_cst (TYPE_MAX_VALUE (type));
	  /* See if X will be in range after truncation towards 0.
	     To compensate for truncation, move the bounds away from 0,
	     but reject if X exactly equals the adjusted bounds.  */
#ifdef REAL_ARITHMETIC
	  REAL_ARITHMETIC (l, MINUS_EXPR, l, dconst1);
	  REAL_ARITHMETIC (u, PLUS_EXPR, u, dconst1);
#else
	  l--;
	  u++;
#endif
	  /* If X is a NaN, use zero instead and show we have an overflow.
	     Otherwise, range check.  */
	  if (REAL_VALUE_ISNAN (x))
	    overflow = 1, x = dconst0;
	  else if (! (REAL_VALUES_LESS (l, x) && REAL_VALUES_LESS (x, u)))
	    overflow = 1;

#ifndef REAL_ARITHMETIC
	  {
	    HOST_WIDE_INT low, high;
	    HOST_WIDE_INT half_word
	      = (HOST_WIDE_INT) 1 << (HOST_BITS_PER_WIDE_INT / 2);

	    if (x < 0)
	      x = -x;

	    high = (HOST_WIDE_INT) (x / half_word / half_word);
	    x -= (REAL_VALUE_TYPE) high * half_word * half_word;
	    if (x >= (REAL_VALUE_TYPE) half_word * half_word / 2)
	      {
		low = x - (REAL_VALUE_TYPE) half_word * half_word / 2;
		low |= (HOST_WIDE_INT) -1 << (HOST_BITS_PER_WIDE_INT - 1);
	      }
	    else
	      low = (HOST_WIDE_INT) x;
	    if (TREE_REAL_CST (arg1) < 0)
	      neg_double (low, high, &low, &high);
	    t = build_int_2 (low, high);
	  }
#else
	  {
	    HOST_WIDE_INT low, high;
	    REAL_VALUE_TO_INT (&low, &high, x);
	    t = build_int_2 (low, high);
	  }
#endif
	  TREE_TYPE (t) = type;
	  TREE_OVERFLOW (t)
	    = TREE_OVERFLOW (arg1) | force_fit_type (t, overflow);
	  TREE_CONSTANT_OVERFLOW (t)
	    = TREE_OVERFLOW (t) | TREE_CONSTANT_OVERFLOW (arg1);
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
	{
	  if (REAL_VALUE_ISNAN (TREE_REAL_CST (arg1)))
	    return arg1;
	  else if (setjmp (float_error))
	    {
	      overflow = 1;
	      t = copy_node (arg1);
	      goto got_it;
	    }
	  set_float_handler (float_error);

	  t = build_real (type, real_value_truncate (TYPE_MODE (type),
						     TREE_REAL_CST (arg1)));
	  set_float_handler (NULL_PTR);

	got_it:
	  TREE_OVERFLOW (t)
	    = TREE_OVERFLOW (arg1) | force_fit_type (t, overflow);
	  TREE_CONSTANT_OVERFLOW (t)
	    = TREE_OVERFLOW (t) | TREE_CONSTANT_OVERFLOW (arg1);
	  return t;
	}
    }
  TREE_CONSTANT (t) = 1;
  return t;
}

/* Return an expr equal to X but certainly not valid as an lvalue.
   Also make sure it is not valid as an null pointer constant.  */

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
    {
      if (TREE_CODE (x) == INTEGER_CST && integer_zerop (x))
	{
	  /* Use NOP_EXPR instead of NON_LVALUE_EXPR
	     so convert_for_assignment won't strip it.
	     This is so this 0 won't be treated as a null pointer constant.  */
	  result = build1 (NOP_EXPR, TREE_TYPE (x), x);
	  TREE_CONSTANT (result) = TREE_CONSTANT (x);
	  return result;
	}
      return x;
    }

  result = build1 (NON_LVALUE_EXPR, TREE_TYPE (x), x);
  TREE_CONSTANT (result) = TREE_CONSTANT (x);
  return result;
}

/* Nonzero means lvalues are limited to those valid in pedantic ANSI C.
   Zero means allow extended lvalues.  */

int pedantic_lvalues;

/* When pedantic, return an expr equal to X but certainly not valid as a
   pedantic lvalue.  Otherwise, return X.  */

tree
pedantic_non_lvalue (x)
     tree x;
{
  if (pedantic_lvalues)
    return non_lvalue (x);
  else
    return x;
}

/* Given a tree comparison code, return the code that is the logical inverse
   of the given code.  It is not safe to do this for floating-point
   comparisons, except for NE_EXPR and EQ_EXPR.  */

static enum tree_code
invert_tree_comparison (code)
     enum tree_code code;
{
  switch (code)
    {
    case EQ_EXPR:
      return NE_EXPR;
    case NE_EXPR:
      return EQ_EXPR;
    case GT_EXPR:
      return LE_EXPR;
    case GE_EXPR:
      return LT_EXPR;
    case LT_EXPR:
      return GE_EXPR;
    case LE_EXPR:
      return GT_EXPR;
    default:
      abort ();
    }
}

/* Similar, but return the comparison that results if the operands are
   swapped.  This is safe for floating-point.  */

static enum tree_code
swap_tree_comparison (code)
     enum tree_code code;
{
  switch (code)
    {
    case EQ_EXPR:
    case NE_EXPR:
      return code;
    case GT_EXPR:
      return LT_EXPR;
    case GE_EXPR:
      return LE_EXPR;
    case LT_EXPR:
      return GT_EXPR;
    case LE_EXPR:
      return GE_EXPR;
    default:
      abort ();
    }
}

/* Return nonzero if CODE is a tree code that represents a truth value.  */

static int
truth_value_p (code)
     enum tree_code code;
{
  return (TREE_CODE_CLASS (code) == '<'
	  || code == TRUTH_AND_EXPR || code == TRUTH_ANDIF_EXPR
	  || code == TRUTH_OR_EXPR || code == TRUTH_ORIF_EXPR
	  || code == TRUTH_XOR_EXPR || code == TRUTH_NOT_EXPR);
}

/* Return nonzero if two operands are necessarily equal.
   If ONLY_CONST is non-zero, only return non-zero for constants.
   This function tests whether the operands are indistinguishable;
   it does not test whether they are equal using C's == operation.
   The distinction is important for IEEE floating point, because
   (1) -0.0 and 0.0 are distinguishable, but -0.0==0.0, and
   (2) two NaNs may be indistinguishable, but NaN!=NaN.  */

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

  /* Detect when real constants are equal.  */
  if (TREE_CODE (arg0) == TREE_CODE (arg1)
      && TREE_CODE (arg0) == REAL_CST)
    return !bcmp ((char *) &TREE_REAL_CST (arg0),
		  (char *) &TREE_REAL_CST (arg1),
		  sizeof (REAL_VALUE_TYPE));

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

/* Similar to operand_equal_p, but see if ARG0 might have been made by
   shorten_compare from ARG1 when ARG1 was being compared with OTHER. 

   When in doubt, return 0.  */

static int 
operand_equal_for_comparison_p (arg0, arg1, other)
     tree arg0, arg1;
     tree other;
{
  int unsignedp1, unsignedpo;
  tree primarg1, primother;
  unsigned correct_width;

  if (operand_equal_p (arg0, arg1, 0))
    return 1;

  if (! INTEGRAL_TYPE_P (TREE_TYPE (arg0))
      || ! INTEGRAL_TYPE_P (TREE_TYPE (arg1)))
    return 0;

  /* Duplicate what shorten_compare does to ARG1 and see if that gives the
     actual comparison operand, ARG0.

     First throw away any conversions to wider types
     already present in the operands.  */

  primarg1 = get_narrower (arg1, &unsignedp1);
  primother = get_narrower (other, &unsignedpo);

  correct_width = TYPE_PRECISION (TREE_TYPE (arg1));
  if (unsignedp1 == unsignedpo
      && TYPE_PRECISION (TREE_TYPE (primarg1)) < correct_width
      && TYPE_PRECISION (TREE_TYPE (primother)) < correct_width)
    {
      tree type = TREE_TYPE (arg0);

      /* Make sure shorter operand is extended the right way
	 to match the longer operand.  */
      primarg1 = convert (signed_or_unsigned_type (unsignedp1,
						  TREE_TYPE (primarg1)),
			 primarg1);

      if (operand_equal_p (arg0, convert (type, primarg1), 0))
	return 1;
    }

  return 0;
}

/* See if ARG is an expression that is either a comparison or is performing
   arithmetic on comparisons.  The comparisons must only be comparing
   two different values, which will be stored in *CVAL1 and *CVAL2; if
   they are non-zero it means that some operands have already been found.
   No variables may be used anywhere else in the expression except in the
   comparisons.  If SAVE_P is true it means we removed a SAVE_EXPR around
   the expression and save_expr needs to be called with CVAL1 and CVAL2.

   If this is true, return 1.  Otherwise, return zero.  */

static int
twoval_comparison_p (arg, cval1, cval2, save_p)
     tree arg;
     tree *cval1, *cval2;
     int *save_p;
{
  enum tree_code code = TREE_CODE (arg);
  char class = TREE_CODE_CLASS (code);

  /* We can handle some of the 'e' cases here.  */
  if (class == 'e' && code == TRUTH_NOT_EXPR)
    class = '1';
  else if (class == 'e'
	   && (code == TRUTH_ANDIF_EXPR || code == TRUTH_ORIF_EXPR
	       || code == COMPOUND_EXPR))
    class = '2';

  /* ??? Disable this since the SAVE_EXPR might already be in use outside
     the expression.  There may be no way to make this work, but it needs
     to be looked at again for 2.6.  */
#if 0
  else if (class == 'e' && code == SAVE_EXPR && SAVE_EXPR_RTL (arg) == 0)
    {
      /* If we've already found a CVAL1 or CVAL2, this expression is
	 two complex to handle.  */
      if (*cval1 || *cval2)
	return 0;

      class = '1';
      *save_p = 1;
    }
#endif

  switch (class)
    {
    case '1':
      return twoval_comparison_p (TREE_OPERAND (arg, 0), cval1, cval2, save_p);

    case '2':
      return (twoval_comparison_p (TREE_OPERAND (arg, 0), cval1, cval2, save_p)
	      && twoval_comparison_p (TREE_OPERAND (arg, 1),
				      cval1, cval2, save_p));

    case 'c':
      return 1;

    case 'e':
      if (code == COND_EXPR)
	return (twoval_comparison_p (TREE_OPERAND (arg, 0),
				     cval1, cval2, save_p)
		&& twoval_comparison_p (TREE_OPERAND (arg, 1),
					cval1, cval2, save_p)
		&& twoval_comparison_p (TREE_OPERAND (arg, 2),
					cval1, cval2, save_p));
      return 0;
	  
    case '<':
      /* First see if we can handle the first operand, then the second.  For
	 the second operand, we know *CVAL1 can't be zero.  It must be that
	 one side of the comparison is each of the values; test for the
	 case where this isn't true by failing if the two operands
	 are the same.  */

      if (operand_equal_p (TREE_OPERAND (arg, 0),
			   TREE_OPERAND (arg, 1), 0))
	return 0;

      if (*cval1 == 0)
	*cval1 = TREE_OPERAND (arg, 0);
      else if (operand_equal_p (*cval1, TREE_OPERAND (arg, 0), 0))
	;
      else if (*cval2 == 0)
	*cval2 = TREE_OPERAND (arg, 0);
      else if (operand_equal_p (*cval2, TREE_OPERAND (arg, 0), 0))
	;
      else
	return 0;

      if (operand_equal_p (*cval1, TREE_OPERAND (arg, 1), 0))
	;
      else if (*cval2 == 0)
	*cval2 = TREE_OPERAND (arg, 1);
      else if (operand_equal_p (*cval2, TREE_OPERAND (arg, 1), 0))
	;
      else
	return 0;

      return 1;
    }

  return 0;
}

/* ARG is a tree that is known to contain just arithmetic operations and
   comparisons.  Evaluate the operations in the tree substituting NEW0 for
   any occurrence of OLD0 as an operand of a comparison and likewise for
   NEW1 and OLD1.  */

static tree
eval_subst (arg, old0, new0, old1, new1)
     tree arg;
     tree old0, new0, old1, new1;
{
  tree type = TREE_TYPE (arg);
  enum tree_code code = TREE_CODE (arg);
  char class = TREE_CODE_CLASS (code);

  /* We can handle some of the 'e' cases here.  */
  if (class == 'e' && code == TRUTH_NOT_EXPR)
    class = '1';
  else if (class == 'e'
	   && (code == TRUTH_ANDIF_EXPR || code == TRUTH_ORIF_EXPR))
    class = '2';

  switch (class)
    {
    case '1':
      return fold (build1 (code, type,
			   eval_subst (TREE_OPERAND (arg, 0),
				       old0, new0, old1, new1)));

    case '2':
      return fold (build (code, type,
			  eval_subst (TREE_OPERAND (arg, 0),
				      old0, new0, old1, new1),
			  eval_subst (TREE_OPERAND (arg, 1),
				      old0, new0, old1, new1)));

    case 'e':
      switch (code)
	{
	case SAVE_EXPR:
	  return eval_subst (TREE_OPERAND (arg, 0), old0, new0, old1, new1);

	case COMPOUND_EXPR:
	  return eval_subst (TREE_OPERAND (arg, 1), old0, new0, old1, new1);

	case COND_EXPR:
	  return fold (build (code, type,
			      eval_subst (TREE_OPERAND (arg, 0),
					  old0, new0, old1, new1),
			      eval_subst (TREE_OPERAND (arg, 1),
					  old0, new0, old1, new1),
			      eval_subst (TREE_OPERAND (arg, 2),
					  old0, new0, old1, new1)));
	}

    case '<':
      {
	tree arg0 = TREE_OPERAND (arg, 0);
	tree arg1 = TREE_OPERAND (arg, 1);

	/* We need to check both for exact equality and tree equality.  The
	   former will be true if the operand has a side-effect.  In that
	   case, we know the operand occurred exactly once.  */

	if (arg0 == old0 || operand_equal_p (arg0, old0, 0))
	  arg0 = new0;
	else if (arg0 == old1 || operand_equal_p (arg0, old1, 0))
	  arg0 = new1;

	if (arg1 == old0 || operand_equal_p (arg1, old0, 0))
	  arg1 = new0;
	else if (arg1 == old1 || operand_equal_p (arg1, old1, 0))
	  arg1 = new1;

	return fold (build (code, type, arg0, arg1));
      }
    }

  return arg;
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

  return non_lvalue (t);
}

/* Similar, but call pedantic_non_lvalue instead of non_lvalue.  */

static tree
pedantic_omit_one_operand (type, result, omitted)
     tree type, result, omitted;
{
  tree t = convert (type, result);

  if (TREE_SIDE_EFFECTS (omitted))
    return build (COMPOUND_EXPR, type, omitted, t);

  return pedantic_non_lvalue (t);
}



/* Return a simplified tree node for the truth-negation of ARG.  This
   never alters ARG itself.  We assume that ARG is an operation that
   returns a truth value (0 or 1).  */

tree
invert_truthvalue (arg)
     tree arg;
{
  tree type = TREE_TYPE (arg);
  enum tree_code code = TREE_CODE (arg);

  if (code == ERROR_MARK)
    return arg;

  /* If this is a comparison, we can simply invert it, except for
     floating-point non-equality comparisons, in which case we just
     enclose a TRUTH_NOT_EXPR around what we have.  */

  if (TREE_CODE_CLASS (code) == '<')
    {
      if (FLOAT_TYPE_P (TREE_TYPE (TREE_OPERAND (arg, 0)))
	  && code != NE_EXPR && code != EQ_EXPR)
	return build1 (TRUTH_NOT_EXPR, type, arg);
      else
	return build (invert_tree_comparison (code), type,
		      TREE_OPERAND (arg, 0), TREE_OPERAND (arg, 1));
    }

  switch (code)
    {
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

    case TRUTH_XOR_EXPR:
      /* Here we can invert either operand.  We invert the first operand
	 unless the second operand is a TRUTH_NOT_EXPR in which case our
	 result is the XOR of the first operand with the inside of the
	 negation of the second operand.  */

      if (TREE_CODE (TREE_OPERAND (arg, 1)) == TRUTH_NOT_EXPR)
	return build (TRUTH_XOR_EXPR, type, TREE_OPERAND (arg, 0),
		      TREE_OPERAND (TREE_OPERAND (arg, 1), 0));
      else
	return build (TRUTH_XOR_EXPR, type,
		      invert_truthvalue (TREE_OPERAND (arg, 0)),
		      TREE_OPERAND (arg, 1));

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
      if (!integer_onep (TREE_OPERAND (arg, 1)))
	break;
      return build (EQ_EXPR, type, arg, convert (type, integer_zero_node));

    case SAVE_EXPR:
      return build1 (TRUTH_NOT_EXPR, type, arg);

    case CLEANUP_POINT_EXPR:
      return build1 (CLEANUP_POINT_EXPR, type,
		     invert_truthvalue (TREE_OPERAND (arg, 0)));
    }
  if (TREE_CODE (TREE_TYPE (arg)) != BOOLEAN_TYPE)
    abort ();
  return build1 (TRUTH_NOT_EXPR, type, arg);
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

   If the optimization described above can be done, we return the resulting
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
  tree offset;

  /* Get all the information about the extractions being done.  If the bit size
     if the same as the size of the underlying object, we aren't doing an
     extraction at all and so can do nothing.  */
  linner = get_inner_reference (lhs, &lbitsize, &lbitpos, &offset, &lmode,
				&lunsignedp, &lvolatilep);
  if (linner == lhs || lbitsize == GET_MODE_BITSIZE (lmode) || lbitsize < 0
      || offset != 0)
    return 0;

 if (!const_p)
   {
     /* If this is not a constant, we can only do something if bit positions,
	sizes, and signedness are the same.   */
     rinner = get_inner_reference (rhs, &rbitsize, &rbitpos, &offset,
				   &rmode, &runsignedp, &rvolatilep);

     if (rinner == rhs || lbitpos != rbitpos || lbitsize != rbitsize
	 || lunsignedp != runsignedp || offset != 0)
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

  if (BYTES_BIG_ENDIAN)
    lbitpos = lnbitsize - lbitsize - lbitpos;

  /* Make the mask to be used against the extracted field.  */
  mask = build_int_2 (~0, ~0);
  TREE_TYPE (mask) = unsigned_type;
  force_fit_type (mask, 0);
  mask = convert (unsigned_type, mask);
  mask = const_binop (LSHIFT_EXPR, mask, size_int (lnbitsize - lbitsize), 0);
  mask = const_binop (RSHIFT_EXPR, mask,
		      size_int (lnbitsize - lbitsize - lbitpos), 0);

  if (! const_p)
    /* If not comparing with constant, just rework the comparison
       and return.  */
    return build (code, compare_type,
		  build (BIT_AND_EXPR, unsigned_type,
			 make_bit_field_ref (linner, unsigned_type,
					     lnbitsize, lnbitpos, 1),
			 mask),
		  build (BIT_AND_EXPR, unsigned_type,
			 make_bit_field_ref (rinner, unsigned_type,
					     rnbitsize, rnbitpos, 1),
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
					size_int (lbitsize), 0)))
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
			      size_int (lbitsize - 1), 0);
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
  lhs = make_bit_field_ref (linner, unsigned_type, lnbitsize, lnbitpos, 1);
  if (lvolatilep)
    {
      TREE_SIDE_EFFECTS (lhs) = 1;
      TREE_THIS_VOLATILE (lhs) = 1;
    }

  rhs = fold (const_binop (BIT_AND_EXPR,
			   const_binop (LSHIFT_EXPR,
					convert (unsigned_type, rhs),
					size_int (lbitpos), 0),
			   mask, 0));

  return build (code, compare_type,
		build (BIT_AND_EXPR, unsigned_type, lhs, mask),
		rhs);
}

/* Subroutine for fold_truthop: decode a field reference.

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

   *PAND_MASK is set the the mask found in a BIT_AND_EXPR, if any.

   Return 0 if this is not a component reference or is one that we can't
   do anything with.  */

static tree
decode_field_reference (exp, pbitsize, pbitpos, pmode, punsignedp,
			pvolatilep, pmask, pand_mask)
     tree exp;
     int *pbitsize, *pbitpos;
     enum machine_mode *pmode;
     int *punsignedp, *pvolatilep;
     tree *pmask;
     tree *pand_mask;
{
  tree and_mask = 0;
  tree mask, inner, offset;
  tree unsigned_type;
  int precision;

  /* All the optimizations using this function assume integer fields.  
     There are problems with FP fields since the type_for_size call
     below can fail for, e.g., XFmode.  */
  if (! INTEGRAL_TYPE_P (TREE_TYPE (exp)))
    return 0;

  STRIP_NOPS (exp);

  if (TREE_CODE (exp) == BIT_AND_EXPR)
    {
      and_mask = TREE_OPERAND (exp, 1);
      exp = TREE_OPERAND (exp, 0);
      STRIP_NOPS (exp); STRIP_NOPS (and_mask);
      if (TREE_CODE (and_mask) != INTEGER_CST)
	return 0;
    }


  inner = get_inner_reference (exp, pbitsize, pbitpos, &offset, pmode,
			       punsignedp, pvolatilep);
  if ((inner == exp && and_mask == 0)
      || *pbitsize < 0 || offset != 0)
    return 0;
  
  /* Compute the mask to access the bitfield.  */
  unsigned_type = type_for_size (*pbitsize, 1);
  precision = TYPE_PRECISION (unsigned_type);

  mask = build_int_2 (~0, ~0);
  TREE_TYPE (mask) = unsigned_type;
  force_fit_type (mask, 0);
  mask = const_binop (LSHIFT_EXPR, mask, size_int (precision - *pbitsize), 0);
  mask = const_binop (RSHIFT_EXPR, mask, size_int (precision - *pbitsize), 0);

  /* Merge it with the mask we found in the BIT_AND_EXPR, if any.  */
  if (and_mask != 0)
    mask = fold (build (BIT_AND_EXPR, unsigned_type,
			convert (unsigned_type, and_mask), mask));

  *pmask = mask;
  *pand_mask = and_mask;
  return inner;
}

/* Return non-zero if MASK represents a mask of SIZE ones in the low-order
   bit positions.  */

static int
all_ones_mask_p (mask, size)
     tree mask;
     int size;
{
  tree type = TREE_TYPE (mask);
  int precision = TYPE_PRECISION (type);
  tree tmask;

  tmask = build_int_2 (~0, ~0);
  TREE_TYPE (tmask) = signed_type (type);
  force_fit_type (tmask, 0);
  return
    tree_int_cst_equal (mask, 
			const_binop (RSHIFT_EXPR,
				     const_binop (LSHIFT_EXPR, tmask,
						  size_int (precision - size),
						  0),
				     size_int (precision - size), 0));
}

/* Subroutine for fold_truthop: determine if an operand is simple enough
   to be evaluated unconditionally.  */

static int 
simple_operand_p (exp)
     tree exp;
{
  /* Strip any conversions that don't change the machine mode.  */
  while ((TREE_CODE (exp) == NOP_EXPR
	  || TREE_CODE (exp) == CONVERT_EXPR)
	 && (TYPE_MODE (TREE_TYPE (exp))
	     == TYPE_MODE (TREE_TYPE (TREE_OPERAND (exp, 0)))))
    exp = TREE_OPERAND (exp, 0);

  return (TREE_CODE_CLASS (TREE_CODE (exp)) == 'c'
	  || (TREE_CODE_CLASS (TREE_CODE (exp)) == 'd'
	      && ! TREE_ADDRESSABLE (exp)
	      && ! TREE_THIS_VOLATILE (exp)
	      && ! DECL_NONLOCAL (exp)
	      /* Don't regard global variables as simple.  They may be
		 allocated in ways unknown to the compiler (shared memory,
		 #pragma weak, etc).  */
	      && ! TREE_PUBLIC (exp)
	      && ! DECL_EXTERNAL (exp)
	      /* Loading a static variable is unduly expensive, but global
		 registers aren't expensive.  */
	      && (! TREE_STATIC (exp) || DECL_REGISTER (exp))));
}

/* Subroutine for fold_truthop: try to optimize a range test.

   For example, "i >= 2 && i =< 9" can be done as "(unsigned) (i - 2) <= 7".

   JCODE is the logical combination of the two terms.  It is TRUTH_AND_EXPR
   (representing TRUTH_ANDIF_EXPR and TRUTH_AND_EXPR) or TRUTH_OR_EXPR
   (representing TRUTH_ORIF_EXPR and TRUTH_OR_EXPR).  TYPE is the type of
   the result.

   VAR is the value being tested.  LO_CODE and HI_CODE are the comparison
   operators comparing VAR to LO_CST and HI_CST.  LO_CST is known to be no
   larger than HI_CST (they may be equal).

   We return the simplified tree or 0 if no optimization is possible.  */

static tree
range_test (jcode, type, lo_code, hi_code, var, lo_cst, hi_cst)
     enum tree_code jcode, lo_code, hi_code;
     tree type, var, lo_cst, hi_cst;
{
  tree utype;
  enum tree_code rcode;

  /* See if this is a range test and normalize the constant terms.  */

  if (jcode == TRUTH_AND_EXPR)
    {
      switch (lo_code)
	{
	case NE_EXPR:
	  /* See if we have VAR != CST && VAR != CST+1.  */
	  if (! (hi_code == NE_EXPR
		 && TREE_INT_CST_LOW (hi_cst) - TREE_INT_CST_LOW (lo_cst) == 1
		 && tree_int_cst_equal (integer_one_node,
					const_binop (MINUS_EXPR,
						     hi_cst, lo_cst, 0))))
	    return 0;

	  rcode = GT_EXPR;
	  break;

	case GT_EXPR:
	case GE_EXPR:
	  if (hi_code == LT_EXPR)
	    hi_cst = const_binop (MINUS_EXPR, hi_cst, integer_one_node, 0);
	  else if (hi_code != LE_EXPR)
	    return 0;

	  if (lo_code == GT_EXPR)
	    lo_cst = const_binop (PLUS_EXPR, lo_cst, integer_one_node, 0);

	  /* We now have VAR >= LO_CST && VAR <= HI_CST.  */
	  rcode = LE_EXPR;
	  break;

	default:
	  return 0;
	}
    }
  else
    {
      switch (lo_code)
	{
	case EQ_EXPR:
	  /* See if we have VAR == CST || VAR == CST+1.  */
	  if (! (hi_code == EQ_EXPR
		 && TREE_INT_CST_LOW (hi_cst) - TREE_INT_CST_LOW (lo_cst) == 1
		 && tree_int_cst_equal (integer_one_node,
					const_binop (MINUS_EXPR,
						     hi_cst, lo_cst, 0))))
	    return 0;

	  rcode = LE_EXPR;
	  break;

	case LE_EXPR:
	case LT_EXPR:
	  if (hi_code == GE_EXPR)
	    hi_cst = const_binop (MINUS_EXPR, hi_cst, integer_one_node, 0);
	  else if (hi_code != GT_EXPR)
	    return 0;

	  if (lo_code == LE_EXPR)
	    lo_cst = const_binop (PLUS_EXPR, lo_cst, integer_one_node, 0);

	  /* We now have VAR < LO_CST || VAR > HI_CST.  */
	  rcode = GT_EXPR;
	  break;

	default:
	  return 0;
	}
    }

  /* When normalizing, it is possible to both increment the smaller constant
     and decrement the larger constant.  See if they are still ordered.  */
  if (tree_int_cst_lt (hi_cst, lo_cst))
    return 0;

  /* Fail if VAR isn't an integer.  */
  utype = TREE_TYPE (var);
  if (! INTEGRAL_TYPE_P (utype))
    return 0;

  /* The range test is invalid if subtracting the two constants results
     in overflow.  This can happen in traditional mode.  */
  if (! int_fits_type_p (hi_cst, TREE_TYPE (var))
      || ! int_fits_type_p (lo_cst, TREE_TYPE (var)))
    return 0;

  if (! TREE_UNSIGNED (utype))
    {
      utype = unsigned_type (utype);
      var = convert (utype, var);
      lo_cst = convert (utype, lo_cst);
      hi_cst = convert (utype, hi_cst);
    }

  return fold (convert (type,
			build (rcode, utype,
			       build (MINUS_EXPR, utype, var, lo_cst),
			       const_binop (MINUS_EXPR, hi_cst, lo_cst, 0))));
}

/* Subroutine for fold_truthop: C is an INTEGER_CST interpreted as a P
   bit value.  Arrange things so the extra bits will be set to zero if and
   only if C is signed-extended to its full width.  If MASK is nonzero,
   it is an INTEGER_CST that should be AND'ed with the extra bits.  */

static tree
unextend (c, p, unsignedp, mask)
     tree c;
     int p;
     int unsignedp;
     tree mask;
{
  tree type = TREE_TYPE (c);
  int modesize = GET_MODE_BITSIZE (TYPE_MODE (type));
  tree temp;

  if (p == modesize || unsignedp)
    return c;

  if (TREE_UNSIGNED (type))
    c = convert (signed_type (type), c);

  /* We work by getting just the sign bit into the low-order bit, then
     into the high-order bit, then sign-extend.  We then XOR that value
     with C.  */
  temp = const_binop (RSHIFT_EXPR, c, size_int (p - 1), 0);
  temp = const_binop (BIT_AND_EXPR, temp, size_int (1), 0);
  temp = const_binop (LSHIFT_EXPR, temp, size_int (modesize - 1), 0);
  temp = const_binop (RSHIFT_EXPR, temp, size_int (modesize - p - 1), 0);
  if (mask != 0)
    temp = const_binop (BIT_AND_EXPR, temp, convert (TREE_TYPE (c), mask), 0);

  return convert (type, const_binop (BIT_XOR_EXPR, c, temp, 0));
}

/* Find ways of folding logical expressions of LHS and RHS:
   Try to merge two comparisons to the same innermost item.
   Look for range tests like "ch >= '0' && ch <= '9'".
   Look for combinations of simple terms on machines with expensive branches
   and evaluate the RHS unconditionally.

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
fold_truthop (code, truth_type, lhs, rhs)
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

  enum tree_code wanted_code;
  enum tree_code lcode, rcode;
  tree ll_arg, lr_arg, rl_arg, rr_arg;
  tree ll_inner, lr_inner, rl_inner, rr_inner;
  int ll_bitsize, ll_bitpos, lr_bitsize, lr_bitpos;
  int rl_bitsize, rl_bitpos, rr_bitsize, rr_bitpos;
  int xll_bitpos, xlr_bitpos, xrl_bitpos, xrr_bitpos;
  int lnbitsize, lnbitpos, rnbitsize, rnbitpos;
  int ll_unsignedp, lr_unsignedp, rl_unsignedp, rr_unsignedp;
  enum machine_mode ll_mode, lr_mode, rl_mode, rr_mode;
  enum machine_mode lnmode, rnmode;
  tree ll_mask, lr_mask, rl_mask, rr_mask;
  tree ll_and_mask, lr_and_mask, rl_and_mask, rr_and_mask;
  tree l_const, r_const;
  tree type, result;
  int first_bit, end_bit;
  int volatilep;

  /* Start by getting the comparison codes and seeing if this looks like
     a range test.  Fail if anything is volatile.  If one operand is a
     BIT_AND_EXPR with the constant one, treat it as if it were surrounded
     with a NE_EXPR.  */

  if (TREE_SIDE_EFFECTS (lhs)
      || TREE_SIDE_EFFECTS (rhs))
    return 0;

  lcode = TREE_CODE (lhs);
  rcode = TREE_CODE (rhs);

  if (lcode == BIT_AND_EXPR && integer_onep (TREE_OPERAND (lhs, 1)))
    lcode = NE_EXPR, lhs = build (NE_EXPR, truth_type, lhs, integer_zero_node);

  if (rcode == BIT_AND_EXPR && integer_onep (TREE_OPERAND (rhs, 1)))
    rcode = NE_EXPR, rhs = build (NE_EXPR, truth_type, rhs, integer_zero_node);

  if (TREE_CODE_CLASS (lcode) != '<'
      || TREE_CODE_CLASS (rcode) != '<')
    return 0;

  code = ((code == TRUTH_AND_EXPR || code == TRUTH_ANDIF_EXPR)
	  ? TRUTH_AND_EXPR : TRUTH_OR_EXPR);

  ll_arg = TREE_OPERAND (lhs, 0);
  lr_arg = TREE_OPERAND (lhs, 1);
  rl_arg = TREE_OPERAND (rhs, 0);
  rr_arg = TREE_OPERAND (rhs, 1);
  
  if (TREE_CODE (lr_arg) == INTEGER_CST
      && TREE_CODE (rr_arg) == INTEGER_CST
      && operand_equal_p (ll_arg, rl_arg, 0))
    {
      if (tree_int_cst_lt (lr_arg, rr_arg))
	result = range_test (code, truth_type, lcode, rcode,
			     ll_arg, lr_arg, rr_arg);
      else
	result = range_test (code, truth_type, rcode, lcode,
			     ll_arg, rr_arg, lr_arg);

      /* If this isn't a range test, it also isn't a comparison that
	 can be merged.  However, it wins to evaluate the RHS unconditionally
	 on machines with expensive branches.   */

      if (result == 0 && BRANCH_COST >= 2)
	{
	  if (TREE_CODE (ll_arg) != VAR_DECL
	      && TREE_CODE (ll_arg) != PARM_DECL)
	    {
	      /* Avoid evaluating the variable part twice.  */
	      ll_arg = save_expr (ll_arg);
	      lhs = build (lcode, TREE_TYPE (lhs), ll_arg, lr_arg);
	      rhs = build (rcode, TREE_TYPE (rhs), ll_arg, rr_arg);
	    }
	  return build (code, truth_type, lhs, rhs);
	}
      return result;
    }

  /* If the RHS can be evaluated unconditionally and its operands are
     simple, it wins to evaluate the RHS unconditionally on machines
     with expensive branches.  In this case, this isn't a comparison
     that can be merged.  */

  /* @@ I'm not sure it wins on the m88110 to do this if the comparisons
     are with zero (tmw).  */

  if (BRANCH_COST >= 2
      && INTEGRAL_TYPE_P (TREE_TYPE (rhs))
      && simple_operand_p (rl_arg)
      && simple_operand_p (rr_arg))
    return build (code, truth_type, lhs, rhs);

  /* See if the comparisons can be merged.  Then get all the parameters for
     each side.  */

  if ((lcode != EQ_EXPR && lcode != NE_EXPR)
      || (rcode != EQ_EXPR && rcode != NE_EXPR))
    return 0;

  volatilep = 0;
  ll_inner = decode_field_reference (ll_arg,
				     &ll_bitsize, &ll_bitpos, &ll_mode,
				     &ll_unsignedp, &volatilep, &ll_mask,
				     &ll_and_mask);
  lr_inner = decode_field_reference (lr_arg,
				     &lr_bitsize, &lr_bitpos, &lr_mode,
				     &lr_unsignedp, &volatilep, &lr_mask,
				     &lr_and_mask);
  rl_inner = decode_field_reference (rl_arg,
				     &rl_bitsize, &rl_bitpos, &rl_mode,
				     &rl_unsignedp, &volatilep, &rl_mask,
				     &rl_and_mask);
  rr_inner = decode_field_reference (rr_arg,
				     &rr_bitsize, &rr_bitpos, &rr_mode,
				     &rr_unsignedp, &volatilep, &rr_mask,
				     &rr_and_mask);

  /* It must be true that the inner operation on the lhs of each
     comparison must be the same if we are to be able to do anything.
     Then see if we have constants.  If not, the same must be true for
     the rhs's.  */
  if (volatilep || ll_inner == 0 || rl_inner == 0
      || ! operand_equal_p (ll_inner, rl_inner, 0))
    return 0;

  if (TREE_CODE (lr_arg) == INTEGER_CST
      && TREE_CODE (rr_arg) == INTEGER_CST)
    l_const = lr_arg, r_const = rr_arg;
  else if (lr_inner == 0 || rr_inner == 0
	   || ! operand_equal_p (lr_inner, rr_inner, 0))
    return 0;
  else
    l_const = r_const = 0;

  /* If either comparison code is not correct for our logical operation,
     fail.  However, we can convert a one-bit comparison against zero into
     the opposite comparison against that bit being set in the field.  */

  wanted_code = (code == TRUTH_AND_EXPR ? EQ_EXPR : NE_EXPR);
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

  if (BYTES_BIG_ENDIAN)
    {
      xll_bitpos = lnbitsize - xll_bitpos - ll_bitsize;
      xrl_bitpos = lnbitsize - xrl_bitpos - rl_bitsize;
    }

  ll_mask = const_binop (LSHIFT_EXPR, convert (type, ll_mask),
			 size_int (xll_bitpos), 0);
  rl_mask = const_binop (LSHIFT_EXPR, convert (type, rl_mask),
			 size_int (xrl_bitpos), 0);

  if (l_const)
    {
      l_const = convert (type, l_const);
      l_const = unextend (l_const,  ll_bitsize, ll_unsignedp, ll_and_mask);
      l_const = const_binop (LSHIFT_EXPR, l_const, size_int (xll_bitpos), 0);
      if (! integer_zerop (const_binop (BIT_AND_EXPR, l_const,
					fold (build1 (BIT_NOT_EXPR,
						      type, ll_mask)),
					0)))
	{
	  warning ("comparison is always %s",
		   wanted_code == NE_EXPR ? "one" : "zero");
	  
	  return convert (truth_type,
			  wanted_code == NE_EXPR
			  ? integer_one_node : integer_zero_node);
	}
    }
  if (r_const)
    {
      r_const = convert (type, r_const);
      r_const = unextend (r_const, rl_bitsize, rl_unsignedp, rl_and_mask);
      r_const = const_binop (LSHIFT_EXPR, r_const, size_int (xrl_bitpos), 0);
      if (! integer_zerop (const_binop (BIT_AND_EXPR, r_const,
					fold (build1 (BIT_NOT_EXPR,
						      type, rl_mask)),
					0)))
	{
	  warning ("comparison is always %s",
		   wanted_code == NE_EXPR ? "one" : "zero");
	  
	  return convert (truth_type,
			  wanted_code == NE_EXPR
			  ? integer_one_node : integer_zero_node);
	}
    }

  /* If the right sides are not constant, do the same for it.  Also,
     disallow this optimization if a size or signedness mismatch occurs
     between the left and right sides.  */
  if (l_const == 0)
    {
      if (ll_bitsize != lr_bitsize || rl_bitsize != rr_bitsize
	  || ll_unsignedp != lr_unsignedp || rl_unsignedp != rr_unsignedp
	  /* Make sure the two fields on the right
	     correspond to the left without being swapped.  */
	  || ll_bitpos - rl_bitpos != lr_bitpos - rr_bitpos)
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

      if (BYTES_BIG_ENDIAN)
	{
	  xlr_bitpos = rnbitsize - xlr_bitpos - lr_bitsize;
	  xrr_bitpos = rnbitsize - xrr_bitpos - rr_bitsize;
	}

      lr_mask = const_binop (LSHIFT_EXPR, convert (type, lr_mask),
			     size_int (xlr_bitpos), 0);
      rr_mask = const_binop (LSHIFT_EXPR, convert (type, rr_mask),
			     size_int (xrr_bitpos), 0);

      /* Make a mask that corresponds to both fields being compared.
	 Do this for both items being compared.  If the masks agree,
	 we can do this by masking both and comparing the masked
	 results.  */
      ll_mask = const_binop (BIT_IOR_EXPR, ll_mask, rl_mask, 0);
      lr_mask = const_binop (BIT_IOR_EXPR, lr_mask, rr_mask, 0);
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
  result = const_binop (BIT_AND_EXPR, ll_mask, rl_mask, 0);
  if (! integer_zerop (result)
      && simple_cst_equal (const_binop (BIT_AND_EXPR, result, l_const, 0),
			   const_binop (BIT_AND_EXPR, result, r_const, 0)) != 1)
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

  ll_mask = const_binop (BIT_IOR_EXPR, ll_mask, rl_mask, 0);
  if (! all_ones_mask_p (ll_mask, lnbitsize))
    result = build (BIT_AND_EXPR, type, result, ll_mask);

  return build (wanted_code, truth_type, result,
		const_binop (BIT_IOR_EXPR, l_const, r_const, 0));
}

/* If T contains a COMPOUND_EXPR which was inserted merely to evaluate
   S, a SAVE_EXPR, return the expression actually being evaluated.   Note
   that we may sometimes modify the tree.  */

static tree
strip_compound_expr (t, s)
     tree t;
     tree s;
{
  tree type = TREE_TYPE (t);
  enum tree_code code = TREE_CODE (t);

  /* See if this is the COMPOUND_EXPR we want to eliminate.  */
  if (code == COMPOUND_EXPR && TREE_CODE (TREE_OPERAND (t, 0)) == CONVERT_EXPR
      && TREE_OPERAND (TREE_OPERAND (t, 0), 0) == s)
    return TREE_OPERAND (t, 1);

  /* See if this is a COND_EXPR or a simple arithmetic operator.   We
     don't bother handling any other types.  */
  else if (code == COND_EXPR)
    {
      TREE_OPERAND (t, 0) = strip_compound_expr (TREE_OPERAND (t, 0), s);
      TREE_OPERAND (t, 1) = strip_compound_expr (TREE_OPERAND (t, 1), s);
      TREE_OPERAND (t, 2) = strip_compound_expr (TREE_OPERAND (t, 2), s);
    }
  else if (TREE_CODE_CLASS (code) == '1')
    TREE_OPERAND (t, 0) = strip_compound_expr (TREE_OPERAND (t, 0), s);
  else if (TREE_CODE_CLASS (code) == '<'
	   || TREE_CODE_CLASS (code) == '2')
    {
      TREE_OPERAND (t, 0) = strip_compound_expr (TREE_OPERAND (t, 0), s);
      TREE_OPERAND (t, 1) = strip_compound_expr (TREE_OPERAND (t, 1), s);
    }

  return t;
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
  tree tem;
  tree type = TREE_TYPE (expr);
  register tree arg0, arg1;
  register enum tree_code code = TREE_CODE (t);
  register int kind;
  int invert;

  /* WINS will be nonzero when the switch is done
     if all operands are constant.  */

  int wins = 1;

  /* Don't try to process an RTL_EXPR since its operands aren't trees.  */
  if (code == RTL_EXPR)
    return t;

  /* Return right away if already constant.  */
  if (TREE_CONSTANT (t))
    {
      if (code == CONST_DECL)
	return DECL_INITIAL (t);
      return t;
    }
  
  kind = TREE_CODE_CLASS (code);
  if (code == NOP_EXPR || code == FLOAT_EXPR || code == CONVERT_EXPR)
    {
      tree subop;

      /* Special case for conversion ops that can have fixed point args.  */
      arg0 = TREE_OPERAND (t, 0);

      /* Don't use STRIP_NOPS, because signedness of argument type matters.  */
      if (arg0 != 0)
	STRIP_TYPE_NOPS (arg0);

      if (arg0 != 0 && TREE_CODE (arg0) == COMPLEX_CST)
	subop = TREE_REALPART (arg0);
      else
	subop = arg0;

      if (subop != 0 && TREE_CODE (subop) != INTEGER_CST
#if ! defined (REAL_IS_NOT_DOUBLE) || defined (REAL_ARITHMETIC)
	  && TREE_CODE (subop) != REAL_CST
#endif /* not REAL_IS_NOT_DOUBLE, or REAL_ARITHMETIC */
	  )
	/* Note that TREE_CONSTANT isn't enough:
	   static var addresses are constant but we can't
	   do arithmetic on them.  */
	wins = 0;
    }
  else if (kind == 'e' || kind == '<'
	   || kind == '1' || kind == '2' || kind == 'r')
    {
      register int len = tree_code_length[(int) code];
      register int i;
      for (i = 0; i < len; i++)
	{
	  tree op = TREE_OPERAND (t, i);
	  tree subop;

	  if (op == 0)
	    continue;		/* Valid for CALL_EXPR, at least.  */

	  if (kind == '<' || code == RSHIFT_EXPR)
	    {
	      /* Signedness matters here.  Perhaps we can refine this
		 later.  */
	      STRIP_TYPE_NOPS (op);
	    }
	  else
	    {
	      /* Strip any conversions that don't change the mode.  */
	      STRIP_NOPS (op);
	    }
	  
	  if (TREE_CODE (op) == COMPLEX_CST)
	    subop = TREE_REALPART (op);
	  else
	    subop = op;

	  if (TREE_CODE (subop) != INTEGER_CST
#if ! defined (REAL_IS_NOT_DOUBLE) || defined (REAL_ARITHMETIC)
	      && TREE_CODE (subop) != REAL_CST
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
      tem = arg0; arg0 = arg1; arg1 = tem;

      tem = TREE_OPERAND (t, 0); TREE_OPERAND (t, 0) = TREE_OPERAND (t, 1);
      TREE_OPERAND (t, 1) = tem;
    }

  /* Now WINS is set as described above,
     ARG0 is the first operand of EXPR,
     and ARG1 is the second operand (if it has more than one operand).

     First check for cases where an arithmetic operation is applied to a
     compound, conditional, or comparison operation.  Push the arithmetic
     operation inside the compound or conditional to see if any folding
     can then be done.  Convert comparison to conditional for this purpose.
     The also optimizes non-constant cases that used to be done in
     expand_expr.

     Before we do that, see if this is a BIT_AND_EXPR or a BIT_OR_EXPR,
     one of the operands is a comparison and the other is a comparison, a
     BIT_AND_EXPR with the constant 1, or a truth value.  In that case, the
     code below would make the expression more complex.  Change it to a
     TRUTH_{AND,OR}_EXPR.  Likewise, convert a similar NE_EXPR to 
     TRUTH_XOR_EXPR and an EQ_EXPR to the inversion of a TRUTH_XOR_EXPR.  */

  if ((code == BIT_AND_EXPR || code == BIT_IOR_EXPR
       || code == EQ_EXPR || code == NE_EXPR)
      && ((truth_value_p (TREE_CODE (arg0))
	   && (truth_value_p (TREE_CODE (arg1))
	       || (TREE_CODE (arg1) == BIT_AND_EXPR
		   && integer_onep (TREE_OPERAND (arg1, 1)))))
	  || (truth_value_p (TREE_CODE (arg1))
	      && (truth_value_p (TREE_CODE (arg0))
		  || (TREE_CODE (arg0) == BIT_AND_EXPR
		      && integer_onep (TREE_OPERAND (arg0, 1)))))))
    {
      t = fold (build (code == BIT_AND_EXPR ? TRUTH_AND_EXPR
		       : code == BIT_IOR_EXPR ? TRUTH_OR_EXPR
		       : TRUTH_XOR_EXPR,
		       type, arg0, arg1));

      if (code == EQ_EXPR)
	t = invert_truthvalue (t);

      return t;
    }

  if (TREE_CODE_CLASS (code) == '1')
    {
      if (TREE_CODE (arg0) == COMPOUND_EXPR)
	return build (COMPOUND_EXPR, type, TREE_OPERAND (arg0, 0),
		      fold (build1 (code, type, TREE_OPERAND (arg0, 1))));
      else if (TREE_CODE (arg0) == COND_EXPR)
	{
	  t = fold (build (COND_EXPR, type, TREE_OPERAND (arg0, 0),
			   fold (build1 (code, type, TREE_OPERAND (arg0, 1))),
			   fold (build1 (code, type, TREE_OPERAND (arg0, 2)))));

	  /* If this was a conversion, and all we did was to move into
	     inside the COND_EXPR, bring it back out.  But leave it if
	     it is a conversion from integer to integer and the
	     result precision is no wider than a word since such a
	     conversion is cheap and may be optimized away by combine,
	     while it couldn't if it were outside the COND_EXPR.  Then return
	     so we don't get into an infinite recursion loop taking the
	     conversion out and then back in.  */

	  if ((code == NOP_EXPR || code == CONVERT_EXPR
	       || code == NON_LVALUE_EXPR)
	      && TREE_CODE (t) == COND_EXPR
	      && TREE_CODE (TREE_OPERAND (t, 1)) == code
	      && TREE_CODE (TREE_OPERAND (t, 2)) == code
	      && (TREE_TYPE (TREE_OPERAND (TREE_OPERAND (t, 1), 0))
		  == TREE_TYPE (TREE_OPERAND (TREE_OPERAND (t, 2), 0)))
	      && ! (INTEGRAL_TYPE_P (TREE_TYPE (t))
		    && INTEGRAL_TYPE_P (TREE_TYPE (TREE_OPERAND (TREE_OPERAND (t, 1), 0)))
		    && TYPE_PRECISION (TREE_TYPE (t)) <= BITS_PER_WORD))
	    t = build1 (code, type,
			build (COND_EXPR,
			       TREE_TYPE (TREE_OPERAND (TREE_OPERAND (t, 1), 0)),
			       TREE_OPERAND (t, 0),
			       TREE_OPERAND (TREE_OPERAND (t, 1), 0),
			       TREE_OPERAND (TREE_OPERAND (t, 2), 0)));
	  return t;
	}
      else if (TREE_CODE_CLASS (TREE_CODE (arg0)) == '<') 
	return fold (build (COND_EXPR, type, arg0,
			    fold (build1 (code, type, integer_one_node)),
			    fold (build1 (code, type, integer_zero_node))));
   }
  else if (TREE_CODE_CLASS (code) == '2'
	   || TREE_CODE_CLASS (code) == '<')
    {
      if (TREE_CODE (arg1) == COMPOUND_EXPR)
	return build (COMPOUND_EXPR, type, TREE_OPERAND (arg1, 0),
		      fold (build (code, type,
				   arg0, TREE_OPERAND (arg1, 1))));
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
	      tree testtype = TREE_TYPE (arg1);
	      test = arg1;
	      true_value = convert (testtype, integer_one_node);
	      false_value = convert (testtype, integer_zero_node);
	    }

	  /* If ARG0 is complex we want to make sure we only evaluate
	     it once.  Though this is only required if it is volatile, it
	     might be more efficient even if it is not.  However, if we
	     succeed in folding one part to a constant, we do not need
	     to make this SAVE_EXPR.  Since we do this optimization
	     primarily to see if we do end up with constant and this
	     SAVE_EXPR interferes with later optimizations, suppressing
	     it when we can is important.  */

	  if (TREE_CODE (arg0) != SAVE_EXPR
	      && ((TREE_CODE (arg0) != VAR_DECL
		   && TREE_CODE (arg0) != PARM_DECL)
		  || TREE_SIDE_EFFECTS (arg0)))
	    {
	      tree lhs = fold (build (code, type, arg0, true_value));
	      tree rhs = fold (build (code, type, arg0, false_value));

	      if (TREE_CONSTANT (lhs) || TREE_CONSTANT (rhs))
		return fold (build (COND_EXPR, type, test, lhs, rhs));

	      arg0 = save_expr (arg0);
	    }

	  test = fold (build (COND_EXPR, type, test,
			      fold (build (code, type, arg0, true_value)),
			      fold (build (code, type, arg0, false_value))));
	  if (TREE_CODE (arg0) == SAVE_EXPR)
	    return build (COMPOUND_EXPR, type,
			  convert (void_type_node, arg0),
			  strip_compound_expr (test, arg0));
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
	      tree testtype = TREE_TYPE (arg0);
	      test = arg0;
	      true_value = convert (testtype, integer_one_node);
	      false_value = convert (testtype, integer_zero_node);
	    }

	  if (TREE_CODE (arg1) != SAVE_EXPR
	      && ((TREE_CODE (arg1) != VAR_DECL
		   && TREE_CODE (arg1) != PARM_DECL)
		  || TREE_SIDE_EFFECTS (arg1)))
	    {
	      tree lhs = fold (build (code, type, true_value, arg1));
	      tree rhs = fold (build (code, type, false_value, arg1));

	      if (TREE_CONSTANT (lhs) || TREE_CONSTANT (rhs)
		  || TREE_CONSTANT (arg1))
		return fold (build (COND_EXPR, type, test, lhs, rhs));

	      arg1 = save_expr (arg1);
	    }

	  test = fold (build (COND_EXPR, type, test,
			      fold (build (code, type, true_value, arg1)),
			      fold (build (code, type, false_value, arg1))));
	  if (TREE_CODE (arg1) == SAVE_EXPR)
	    return build (COMPOUND_EXPR, type,
			  convert (void_type_node, arg1),
			  strip_compound_expr (test, arg1));
	  else
	    return convert (type, test);
	}
    }
  else if (TREE_CODE_CLASS (code) == '<'
	   && TREE_CODE (arg0) == COMPOUND_EXPR)
    return build (COMPOUND_EXPR, type, TREE_OPERAND (arg0, 0),
		  fold (build (code, type, TREE_OPERAND (arg0, 1), arg1)));
  else if (TREE_CODE_CLASS (code) == '<'
	   && TREE_CODE (arg1) == COMPOUND_EXPR)
    return build (COMPOUND_EXPR, type, TREE_OPERAND (arg1, 0),
		  fold (build (code, type, arg0, TREE_OPERAND (arg1, 1))));
	  
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

      if (TREE_TYPE (TREE_OPERAND (t, 0)) == TREE_TYPE (t))
	return TREE_OPERAND (t, 0);

      /* Handle cases of two conversions in a row.  */
      if (TREE_CODE (TREE_OPERAND (t, 0)) == NOP_EXPR
	  || TREE_CODE (TREE_OPERAND (t, 0)) == CONVERT_EXPR)
	{
	  tree inside_type = TREE_TYPE (TREE_OPERAND (TREE_OPERAND (t, 0), 0));
	  tree inter_type = TREE_TYPE (TREE_OPERAND (t, 0));
	  tree final_type = TREE_TYPE (t);
	  int inside_int = INTEGRAL_TYPE_P (inside_type);
	  int inside_ptr = POINTER_TYPE_P (inside_type);
	  int inside_float = FLOAT_TYPE_P (inside_type);
	  int inside_prec = TYPE_PRECISION (inside_type);
	  int inside_unsignedp = TREE_UNSIGNED (inside_type);
	  int inter_int = INTEGRAL_TYPE_P (inter_type);
	  int inter_ptr = POINTER_TYPE_P (inter_type);
	  int inter_float = FLOAT_TYPE_P (inter_type);
	  int inter_prec = TYPE_PRECISION (inter_type);
	  int inter_unsignedp = TREE_UNSIGNED (inter_type);
	  int final_int = INTEGRAL_TYPE_P (final_type);
	  int final_ptr = POINTER_TYPE_P (final_type);
	  int final_float = FLOAT_TYPE_P (final_type);
	  int final_prec = TYPE_PRECISION (final_type);
	  int final_unsignedp = TREE_UNSIGNED (final_type);

	  /* In addition to the cases of two conversions in a row 
	     handled below, if we are converting something to its own
	     type via an object of identical or wider precision, neither
	     conversion is needed.  */
	  if (inside_type == final_type
	      && ((inter_int && final_int) || (inter_float && final_float))
	      && inter_prec >= final_prec)
	    return TREE_OPERAND (TREE_OPERAND (t, 0), 0);

	  /* Likewise, if the intermediate and final types are either both
	     float or both integer, we don't need the middle conversion if
	     it is wider than the final type and doesn't change the signedness
	     (for integers).  Avoid this if the final type is a pointer
	     since then we sometimes need the inner conversion.  Likewise if
	     the outer has a precision not equal to the size of its mode.  */
	  if ((((inter_int || inter_ptr) && (inside_int || inside_ptr))
	       || (inter_float && inside_float))
	      && inter_prec >= inside_prec
	      && (inter_float || inter_unsignedp == inside_unsignedp)
	      && ! (final_prec != GET_MODE_BITSIZE (TYPE_MODE (final_type))
		    && TYPE_MODE (final_type) == TYPE_MODE (inter_type))
	      && ! final_ptr)
	    return convert (final_type, TREE_OPERAND (TREE_OPERAND (t, 0), 0));

	  /* Two conversions in a row are not needed unless:
	     - some conversion is floating-point (overstrict for now), or
	     - the intermediate type is narrower than both initial and
	       final, or
	     - the intermediate type and innermost type differ in signedness,
	       and the outermost type is wider than the intermediate, or
	     - the initial type is a pointer type and the precisions of the
	       intermediate and final types differ, or
	     - the final type is a pointer type and the precisions of the 
	       initial and intermediate types differ.  */
	  if (! inside_float && ! inter_float && ! final_float
	      && (inter_prec > inside_prec || inter_prec > final_prec)
	      && ! (inside_int && inter_int
		    && inter_unsignedp != inside_unsignedp
		    && inter_prec < final_prec)
	      && ((inter_unsignedp && inter_prec > inside_prec)
		  == (final_unsignedp && final_prec > inter_prec))
	      && ! (inside_ptr && inter_prec != final_prec)
	      && ! (final_ptr && inside_prec != inter_prec)
	      && ! (final_prec != GET_MODE_BITSIZE (TYPE_MODE (final_type))
		    && TYPE_MODE (final_type) == TYPE_MODE (inter_type))
	      && ! final_ptr)
	    return convert (final_type, TREE_OPERAND (TREE_OPERAND (t, 0), 0));
	}

      if (TREE_CODE (TREE_OPERAND (t, 0)) == MODIFY_EXPR
	  && TREE_CONSTANT (TREE_OPERAND (TREE_OPERAND (t, 0), 1))
	  /* Detect assigning a bitfield.  */
	  && !(TREE_CODE (TREE_OPERAND (TREE_OPERAND (t, 0), 0)) == COMPONENT_REF
	       && DECL_BIT_FIELD (TREE_OPERAND (TREE_OPERAND (TREE_OPERAND (t, 0), 0), 1))))
	{
	  /* Don't leave an assignment inside a conversion
	     unless assigning a bitfield.  */
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
	      force_fit_type (t, 0);
	    }
	}
      return t;
#endif /* 0 */

    case COMPONENT_REF:
      if (TREE_CODE (arg0) == CONSTRUCTOR)
	{
	  tree m = purpose_member (arg1, CONSTRUCTOR_ELTS (arg0));
	  if (m)
	    t = TREE_VALUE (m);
	}
      return t;

    case RANGE_EXPR:
      TREE_CONSTANT (t) = wins;
      return t;

    case NEGATE_EXPR:
      if (wins)
	{
	  if (TREE_CODE (arg0) == INTEGER_CST)
	    {
	      HOST_WIDE_INT low, high;
	      int overflow = neg_double (TREE_INT_CST_LOW (arg0),
					 TREE_INT_CST_HIGH (arg0),
					 &low, &high);
	      t = build_int_2 (low, high);
	      TREE_TYPE (t) = type;
	      TREE_OVERFLOW (t)
		= (TREE_OVERFLOW (arg0)
		   | force_fit_type (t, overflow));
	      TREE_CONSTANT_OVERFLOW (t)
		= TREE_OVERFLOW (t) | TREE_CONSTANT_OVERFLOW (arg0);
	    }
	  else if (TREE_CODE (arg0) == REAL_CST)
	    t = build_real (type, REAL_VALUE_NEGATE (TREE_REAL_CST (arg0)));
	  TREE_TYPE (t) = type;
	}
      else if (TREE_CODE (arg0) == NEGATE_EXPR)
	return TREE_OPERAND (arg0, 0);

      /* Convert - (a - b) to (b - a) for non-floating-point.  */
      else if (TREE_CODE (arg0) == MINUS_EXPR && ! FLOAT_TYPE_P (type))
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
		  HOST_WIDE_INT low, high;
		  int overflow = neg_double (TREE_INT_CST_LOW (arg0),
					     TREE_INT_CST_HIGH (arg0),
					     &low, &high);
		  t = build_int_2 (low, high);
		  TREE_TYPE (t) = type;
		  TREE_OVERFLOW (t)
		    = (TREE_OVERFLOW (arg0)
		       | force_fit_type (t, overflow));
		  TREE_CONSTANT_OVERFLOW (t)
		    = TREE_OVERFLOW (t) | TREE_CONSTANT_OVERFLOW (arg0);
		}
	    }
	  else if (TREE_CODE (arg0) == REAL_CST)
	    {
	      if (REAL_VALUE_NEGATIVE (TREE_REAL_CST (arg0)))
		t = build_real (type,
				REAL_VALUE_NEGATE (TREE_REAL_CST (arg0)));
	    }
	  TREE_TYPE (t) = type;
	}
      else if (TREE_CODE (arg0) == ABS_EXPR || TREE_CODE (arg0) == NEGATE_EXPR)
	return build1 (ABS_EXPR, type, TREE_OPERAND (arg0, 0));
      return t;

    case CONJ_EXPR:
      if (TREE_CODE (TREE_TYPE (arg0)) != COMPLEX_TYPE)
	return arg0;
      else if (TREE_CODE (arg0) == COMPLEX_EXPR)
	return build (COMPLEX_EXPR, TREE_TYPE (arg0),
		      TREE_OPERAND (arg0, 0),
		      fold (build1 (NEGATE_EXPR,
				    TREE_TYPE (TREE_TYPE (arg0)),
				    TREE_OPERAND (arg0, 1))));
      else if (TREE_CODE (arg0) == COMPLEX_CST)
	return build_complex (TREE_OPERAND (arg0, 0),
			      fold (build1 (NEGATE_EXPR,
					    TREE_TYPE (TREE_TYPE (arg0)),
					    TREE_OPERAND (arg0, 1))));
      else if (TREE_CODE (arg0) == PLUS_EXPR || TREE_CODE (arg0) == MINUS_EXPR)
	return fold (build (TREE_CODE (arg0), type,
			    fold (build1 (CONJ_EXPR, type,
					  TREE_OPERAND (arg0, 0))),
			    fold (build1 (CONJ_EXPR,
					  type, TREE_OPERAND (arg0, 1)))));
      else if (TREE_CODE (arg0) == CONJ_EXPR)
	return TREE_OPERAND (arg0, 0);
      return t;

    case BIT_NOT_EXPR:
      if (wins)
	{
	  if (TREE_CODE (arg0) == INTEGER_CST)
	    t = build_int_2 (~ TREE_INT_CST_LOW (arg0),
			     ~ TREE_INT_CST_HIGH (arg0));
	  TREE_TYPE (t) = type;
	  force_fit_type (t, 0);
	  TREE_OVERFLOW (t) = TREE_OVERFLOW (arg0);
	  TREE_CONSTANT_OVERFLOW (t) = TREE_CONSTANT_OVERFLOW (arg0);
	}
      else if (TREE_CODE (arg0) == BIT_NOT_EXPR)
	return TREE_OPERAND (arg0, 0);
      return t;

    case PLUS_EXPR:
      /* A + (-B) -> A - B */
      if (TREE_CODE (arg1) == NEGATE_EXPR)
	return fold (build (MINUS_EXPR, type, arg0, TREE_OPERAND (arg1, 0)));
      else if (! FLOAT_TYPE_P (type))
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
					     TREE_OPERAND (arg1, 1), 0)))
	    {
	      code = BIT_IOR_EXPR;
	      goto bit_ior;
	    }

	  /* (A * C) + (B * C) -> (A+B) * C.  Since we are most concerned
	     about the case where C is a constant, just try one of the
	     four possibilities.  */

	  if (TREE_CODE (arg0) == MULT_EXPR && TREE_CODE (arg1) == MULT_EXPR
	      && operand_equal_p (TREE_OPERAND (arg0, 1),
				  TREE_OPERAND (arg1, 1), 0))
	    return fold (build (MULT_EXPR, type,
				fold (build (PLUS_EXPR, type,
					     TREE_OPERAND (arg0, 0),
					     TREE_OPERAND (arg1, 0))),
				TREE_OPERAND (arg0, 1)));
	}
      /* In IEEE floating point, x+0 may not equal x.  */
      else if ((TARGET_FLOAT_FORMAT != IEEE_FLOAT_FORMAT
		|| flag_fast_math)
	       && real_zerop (arg1))
	return non_lvalue (convert (type, arg0));
    associate:
      /* In most languages, can't associate operations on floats
	 through parentheses.  Rather than remember where the parentheses
	 were, we don't associate floats at all.  It shouldn't matter much.
	 However, associating multiplications is only very slightly
	 inaccurate, so do that if -ffast-math is specified.  */
      if (FLOAT_TYPE_P (type)
	  && ! (flag_fast_math && code == MULT_EXPR))
	goto binary;

      /* The varsign == -1 cases happen only for addition and subtraction.
	 It says that the arg that was split was really CON minus VAR.
	 The rest of the code applies to all associative operations.  */
      if (!wins)
	{
	  tree var, con;
	  int varsign;

	  if (split_tree (arg0, code, &var, &con, &varsign))
	    {
	      if (varsign == -1)
		{
		  /* EXPR is (CON-VAR) +- ARG1.  */
		  /* If it is + and VAR==ARG1, return just CONST.  */
		  if (code == PLUS_EXPR && operand_equal_p (var, arg1, 0))
		    return convert (TREE_TYPE (t), con);
		    
		  /* If ARG0 is a constant, don't change things around;
		     instead keep all the constant computations together.  */

		  if (TREE_CONSTANT (arg0))
		    return t;

		  /* Otherwise return (CON +- ARG1) - VAR.  */
		  t = build (MINUS_EXPR, type,
			     fold (build (code, type, con, arg1)), var);
		}
	      else
		{
		  /* EXPR is (VAR+CON) +- ARG1.  */
		  /* If it is - and VAR==ARG1, return just CONST.  */
		  if (code == MINUS_EXPR && operand_equal_p (var, arg1, 0))
		    return convert (TREE_TYPE (t), con);
		    
		  /* If ARG0 is a constant, don't change things around;
		     instead keep all the constant computations together.  */

		  if (TREE_CONSTANT (arg0))
		    return t;

		  /* Otherwise return VAR +- (ARG1 +- CON).  */
		  tem = fold (build (code, type, arg1, con));
		  t = build (code, type, var, tem);

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
	      if (TREE_CONSTANT (arg1))
		return t;

	      if (varsign == -1)
		TREE_SET_CODE (t,
			       (code == PLUS_EXPR ? MINUS_EXPR : PLUS_EXPR));

	      /* EXPR is ARG0 +- (CON +- VAR).  */
	      if (TREE_CODE (t) == MINUS_EXPR
		  && operand_equal_p (var, arg0, 0))
		{
		  /* If VAR and ARG0 cancel, return just CON or -CON.  */
		  if (code == PLUS_EXPR)
		    return convert (TREE_TYPE (t), con);
		  return fold (build1 (NEGATE_EXPR, TREE_TYPE (t),
				       convert (TREE_TYPE (t), con)));
		}

	      t = build (TREE_CODE (t), type,
			 fold (build (code, TREE_TYPE (t), arg0, con)), var);

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
	t1 = const_binop (code, arg0, arg1, 0);
      if (t1 != NULL_TREE)
	{
	  /* The return value should always have
	     the same type as the original expression.  */
	  TREE_TYPE (t1) = TREE_TYPE (t);
	  return t1;
	}
      return t;

    case MINUS_EXPR:
      if (! FLOAT_TYPE_P (type))
	{
	  if (! wins && integer_zerop (arg0))
	    return build1 (NEGATE_EXPR, type, arg1);
	  if (integer_zerop (arg1))
	    return non_lvalue (convert (type, arg0));

	  /* (A * C) - (B * C) -> (A-B) * C.  Since we are most concerned
	     about the case where C is a constant, just try one of the
	     four possibilities.  */

	  if (TREE_CODE (arg0) == MULT_EXPR && TREE_CODE (arg1) == MULT_EXPR
	      && operand_equal_p (TREE_OPERAND (arg0, 1),
				  TREE_OPERAND (arg1, 1), 0))
	    return fold (build (MULT_EXPR, type,
				fold (build (MINUS_EXPR, type,
					     TREE_OPERAND (arg0, 0),
					     TREE_OPERAND (arg1, 0))),
				TREE_OPERAND (arg0, 1)));
	}
      /* Convert A - (-B) to A + B.  */
      else if (TREE_CODE (arg1) == NEGATE_EXPR)
	return fold (build (PLUS_EXPR, type, arg0, TREE_OPERAND (arg1, 0)));

      else if (TARGET_FLOAT_FORMAT != IEEE_FLOAT_FORMAT
	       || flag_fast_math)
	{
	  /* Except with IEEE floating point, 0-x equals -x.  */
	  if (! wins && real_zerop (arg0))
	    return build1 (NEGATE_EXPR, type, arg1);
	  /* Except with IEEE floating point, x-0 equals x.  */
	  if (real_zerop (arg1))
	    return non_lvalue (convert (type, arg0));
	}

      /* Fold &x - &x.  This can happen from &x.foo - &x. 
	 This is unsafe for certain floats even in non-IEEE formats.
	 In IEEE, it is unsafe because it does wrong for NaNs.
	 Also note that operand_equal_p is always false if an operand
	 is volatile.  */

      if ((! FLOAT_TYPE_P (type) || flag_fast_math)
	  && operand_equal_p (arg0, arg1, 0))
	return convert (type, integer_zero_node);

      goto associate;

    case MULT_EXPR:
      if (! FLOAT_TYPE_P (type))
	{
	  if (integer_zerop (arg1))
	    return omit_one_operand (type, arg1, arg0);
	  if (integer_onep (arg1))
	    return non_lvalue (convert (type, arg0));

	  /* ((A / C) * C) is A if the division is an
	     EXACT_DIV_EXPR.   Since C is normally a constant,
	     just check for one of the four possibilities.  */

	  if (TREE_CODE (arg0) == EXACT_DIV_EXPR
	      && operand_equal_p (TREE_OPERAND (arg0, 1), arg1, 0))
	    return TREE_OPERAND (arg0, 0);

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
      else
	{
	  /* x*0 is 0, except for IEEE floating point.  */
	  if ((TARGET_FLOAT_FORMAT != IEEE_FLOAT_FORMAT
	       || flag_fast_math)
	      && real_zerop (arg1))
	    return omit_one_operand (type, arg1, arg0);
	  /* In IEEE floating point, x*1 is not equivalent to x for snans.
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

      /* (a << C1) | (a >> C2) if A is unsigned and C1+C2 is the size of A
	 is a rotate of A by C1 bits.  */

      if ((TREE_CODE (arg0) == RSHIFT_EXPR
	   || TREE_CODE (arg0) == LSHIFT_EXPR)
	  && (TREE_CODE (arg1) == RSHIFT_EXPR
	      || TREE_CODE (arg1) == LSHIFT_EXPR)
	  && TREE_CODE (arg0) != TREE_CODE (arg1)
	  && operand_equal_p (TREE_OPERAND (arg0, 0), TREE_OPERAND (arg1,0), 0)
	  && TREE_UNSIGNED (TREE_TYPE (TREE_OPERAND (arg0, 0)))
	  && TREE_CODE (TREE_OPERAND (arg0, 1)) == INTEGER_CST
	  && TREE_CODE (TREE_OPERAND (arg1, 1)) == INTEGER_CST
	  && TREE_INT_CST_HIGH (TREE_OPERAND (arg0, 1)) == 0
	  && TREE_INT_CST_HIGH (TREE_OPERAND (arg1, 1)) == 0
	  && ((TREE_INT_CST_LOW (TREE_OPERAND (arg0, 1))
	       + TREE_INT_CST_LOW (TREE_OPERAND (arg1, 1)))
	      == TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (arg0, 0)))))
	return build (LROTATE_EXPR, type, TREE_OPERAND (arg0, 0),
		      TREE_CODE (arg0) == LSHIFT_EXPR
		      ? TREE_OPERAND (arg0, 1) : TREE_OPERAND (arg1, 1));

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
	  if (prec < BITS_PER_WORD && prec < HOST_BITS_PER_WIDE_INT
	      && (~TREE_INT_CST_LOW (arg0)
		  & (((HOST_WIDE_INT) 1 << prec) - 1)) == 0)
	    return build1 (NOP_EXPR, type, TREE_OPERAND (arg1, 0));
	}
      if (TREE_CODE (arg1) == INTEGER_CST && TREE_CODE (arg0) == NOP_EXPR
	  && TREE_UNSIGNED (TREE_TYPE (TREE_OPERAND (arg0, 0))))
	{
	  int prec = TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (arg0, 0)));
	  if (prec < BITS_PER_WORD && prec < HOST_BITS_PER_WIDE_INT
	      && (~TREE_INT_CST_LOW (arg1)
		  & (((HOST_WIDE_INT) 1 << prec) - 1)) == 0)
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

    case RDIV_EXPR:
      /* In most cases, do nothing with a divide by zero.  */
#if !defined (REAL_IS_NOT_DOUBLE) || defined (REAL_ARITHMETIC)
#ifndef REAL_INFINITY
      if (TREE_CODE (arg1) == REAL_CST && real_zerop (arg1))
	return t;
#endif
#endif /* not REAL_IS_NOT_DOUBLE, or REAL_ARITHMETIC */

      /* In IEEE floating point, x/1 is not equivalent to x for snans.
	 However, ANSI says we can drop signals, so we can do this anyway.  */
      if (real_onep (arg1))
	return non_lvalue (convert (type, arg0));

      /* If ARG1 is a constant, we can convert this to a multiply by the
	 reciprocal.  This does not have the same rounding properties,
	 so only do this if -ffast-math.  We can actually always safely
	 do it if ARG1 is a power of two, but it's hard to tell if it is
	 or not in a portable manner.  */
      if (TREE_CODE (arg1) == REAL_CST && flag_fast_math
	  && 0 != (tem = const_binop (code, build_real (type, dconst1),
				      arg1, 0)))
	return fold (build (MULT_EXPR, type, arg0, tem));

      goto binary;

    case TRUNC_DIV_EXPR:
    case ROUND_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case CEIL_DIV_EXPR:
    case EXACT_DIV_EXPR:
      if (integer_onep (arg1))
	return non_lvalue (convert (type, arg0));
      if (integer_zerop (arg1))
	return t;

      /* If we have ((a / C1) / C2) where both division are the same type, try
	 to simplify.  First see if C1 * C2 overflows or not.  */
      if (TREE_CODE (arg0) == code && TREE_CODE (arg1) == INTEGER_CST
	  && TREE_CODE (TREE_OPERAND (arg0, 1)) == INTEGER_CST)
	{
	  tree new_divisor;

	  new_divisor = const_binop (MULT_EXPR, TREE_OPERAND (arg0, 1), arg1, 0);
	  tem = const_binop (FLOOR_DIV_EXPR, new_divisor, arg1, 0);

	  if (TREE_INT_CST_LOW (TREE_OPERAND (arg0, 1)) == TREE_INT_CST_LOW (tem)
	      && TREE_INT_CST_HIGH (TREE_OPERAND (arg0, 1)) == TREE_INT_CST_HIGH (tem))
	    {
	      /* If no overflow, divide by C1*C2.  */
	      return fold (build (code, type, TREE_OPERAND (arg0, 0), new_divisor));
	    }
	}

      /* Look for ((a * C1) / C3) or (((a * C1) + C2) / C3),
	 where C1 % C3 == 0 or C3 % C1 == 0.  We can simplify these
	 expressions, which often appear in the offsets or sizes of
	 objects with a varying size.  Only deal with positive divisors
	 and multiplicands.   If C2 is negative, we must have C2 % C3 == 0.

	 Look for NOPs and SAVE_EXPRs inside.  */

      if (TREE_CODE (arg1) == INTEGER_CST
	  && tree_int_cst_sgn (arg1) >= 0)
	{
	  int have_save_expr = 0;
	  tree c2 = integer_zero_node;
	  tree xarg0 = arg0;

	  if (TREE_CODE (xarg0) == SAVE_EXPR)
	    have_save_expr = 1, xarg0 = TREE_OPERAND (xarg0, 0);

	  STRIP_NOPS (xarg0);

	  if (TREE_CODE (xarg0) == PLUS_EXPR
	      && TREE_CODE (TREE_OPERAND (xarg0, 1)) == INTEGER_CST)
	    c2 = TREE_OPERAND (xarg0, 1), xarg0 = TREE_OPERAND (xarg0, 0);
	  else if (TREE_CODE (xarg0) == MINUS_EXPR
		   && TREE_CODE (TREE_OPERAND (xarg0, 1)) == INTEGER_CST
		   /* If we are doing this computation unsigned, the negate
		      is incorrect.  */
		   && ! TREE_UNSIGNED (type))
	    {
	      c2 = fold (build1 (NEGATE_EXPR, type, TREE_OPERAND (xarg0, 1)));
	      xarg0 = TREE_OPERAND (xarg0, 0);
	    }

	  if (TREE_CODE (xarg0) == SAVE_EXPR)
	    have_save_expr = 1, xarg0 = TREE_OPERAND (xarg0, 0);

	  STRIP_NOPS (xarg0);

	  if (TREE_CODE (xarg0) == MULT_EXPR
	      && TREE_CODE (TREE_OPERAND (xarg0, 1)) == INTEGER_CST
	      && tree_int_cst_sgn (TREE_OPERAND (xarg0, 1)) >= 0
	      && (integer_zerop (const_binop (TRUNC_MOD_EXPR,
					      TREE_OPERAND (xarg0, 1), arg1, 1))
		  || integer_zerop (const_binop (TRUNC_MOD_EXPR, arg1,
						 TREE_OPERAND (xarg0, 1), 1)))
	      && (tree_int_cst_sgn (c2) >= 0
		  || integer_zerop (const_binop (TRUNC_MOD_EXPR, c2,
						 arg1, 1))))
	    {
	      tree outer_div = integer_one_node;
	      tree c1 = TREE_OPERAND (xarg0, 1);
	      tree c3 = arg1;

	      /* If C3 > C1, set them equal and do a divide by
		 C3/C1 at the end of the operation.  */
	      if (tree_int_cst_lt (c1, c3))
		outer_div = const_binop (code, c3, c1, 0), c3 = c1;
		
	      /* The result is A * (C1/C3) + (C2/C3).  */
	      t = fold (build (PLUS_EXPR, type,
			       fold (build (MULT_EXPR, type,
					    TREE_OPERAND (xarg0, 0),
					    const_binop (code, c1, c3, 1))),
			       const_binop (code, c2, c3, 1)));

	      if (! integer_onep (outer_div))
		t = fold (build (code, type, t, convert (type, outer_div)));

	      if (have_save_expr)
		t = save_expr (t);

	      return t;
	    }
	}

      goto binary;

    case CEIL_MOD_EXPR:
    case FLOOR_MOD_EXPR:
    case ROUND_MOD_EXPR:
    case TRUNC_MOD_EXPR:
      if (integer_onep (arg1))
	return omit_one_operand (type, integer_zero_node, arg0);
      if (integer_zerop (arg1))
	return t;

      /* Look for ((a * C1) % C3) or (((a * C1) + C2) % C3),
	 where C1 % C3 == 0.  Handle similarly to the division case,
	 but don't bother with SAVE_EXPRs.  */

      if (TREE_CODE (arg1) == INTEGER_CST
	  && ! integer_zerop (arg1))
	{
	  tree c2 = integer_zero_node;
	  tree xarg0 = arg0;

	  if (TREE_CODE (xarg0) == PLUS_EXPR
	      && TREE_CODE (TREE_OPERAND (xarg0, 1)) == INTEGER_CST)
	    c2 = TREE_OPERAND (xarg0, 1), xarg0 = TREE_OPERAND (xarg0, 0);
	  else if (TREE_CODE (xarg0) == MINUS_EXPR
		   && TREE_CODE (TREE_OPERAND (xarg0, 1)) == INTEGER_CST
		   && ! TREE_UNSIGNED (type))
	    {
	      c2 = fold (build1 (NEGATE_EXPR, type, TREE_OPERAND (xarg0, 1)));
	      xarg0 = TREE_OPERAND (xarg0, 0);
	    }

	  STRIP_NOPS (xarg0);

	  if (TREE_CODE (xarg0) == MULT_EXPR
	      && TREE_CODE (TREE_OPERAND (xarg0, 1)) == INTEGER_CST
	      && integer_zerop (const_binop (TRUNC_MOD_EXPR,
					     TREE_OPERAND (xarg0, 1),
					     arg1, 1))
	      && tree_int_cst_sgn (c2) >= 0)
	    /* The result is (C2%C3).  */
	    return omit_one_operand (type, const_binop (code, c2, arg1, 1),
				     TREE_OPERAND (xarg0, 0));
	}

      goto binary;

    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
    case LROTATE_EXPR:
    case RROTATE_EXPR:
      if (integer_zerop (arg1))
	return non_lvalue (convert (type, arg0));
      /* Since negative shift count is not well-defined,
	 don't try to compute it in the compiler.  */
      if (TREE_CODE (arg1) == INTEGER_CST && tree_int_cst_sgn (arg1) < 0)
	return t;
      /* Rewrite an LROTATE_EXPR by a constant into an
	 RROTATE_EXPR by a new constant.  */
      if (code == LROTATE_EXPR && TREE_CODE (arg1) == INTEGER_CST)
	{
	  TREE_SET_CODE (t, RROTATE_EXPR);
	  code = RROTATE_EXPR;
	  TREE_OPERAND (t, 1) = arg1
	    = const_binop
	      (MINUS_EXPR,
	       convert (TREE_TYPE (arg1),
			build_int_2 (GET_MODE_BITSIZE (TYPE_MODE (type)), 0)),
	       arg1, 0);
	  if (tree_int_cst_sgn (arg1) < 0)
	    return t;
	}

      /* If we have a rotate of a bit operation with the rotate count and
	 the second operand of the bit operation both constant,
	 permute the two operations.  */
      if (code == RROTATE_EXPR && TREE_CODE (arg1) == INTEGER_CST
	  && (TREE_CODE (arg0) == BIT_AND_EXPR
	      || TREE_CODE (arg0) == BIT_ANDTC_EXPR
	      || TREE_CODE (arg0) == BIT_IOR_EXPR
	      || TREE_CODE (arg0) == BIT_XOR_EXPR)
	  && TREE_CODE (TREE_OPERAND (arg0, 1)) == INTEGER_CST)
	return fold (build (TREE_CODE (arg0), type,
			    fold (build (code, type,
					 TREE_OPERAND (arg0, 0), arg1)),
			    fold (build (code, type,
					 TREE_OPERAND (arg0, 1), arg1))));

      /* Two consecutive rotates adding up to the width of the mode can
	 be ignored.  */
      if (code == RROTATE_EXPR && TREE_CODE (arg1) == INTEGER_CST
	  && TREE_CODE (arg0) == RROTATE_EXPR
	  && TREE_CODE (TREE_OPERAND (arg0, 1)) == INTEGER_CST
	  && TREE_INT_CST_HIGH (arg1) == 0
	  && TREE_INT_CST_HIGH (TREE_OPERAND (arg0, 1)) == 0
	  && ((TREE_INT_CST_LOW (arg1)
	       + TREE_INT_CST_LOW (TREE_OPERAND (arg0, 1)))
	      == GET_MODE_BITSIZE (TYPE_MODE (type))))
	return TREE_OPERAND (arg0, 0);

      goto binary;

    case MIN_EXPR:
      if (operand_equal_p (arg0, arg1, 0))
	return arg0;
      if (INTEGRAL_TYPE_P (type)
	  && operand_equal_p (arg1, TYPE_MIN_VALUE (type), 1))
	return omit_one_operand (type, arg1, arg0);
      goto associate;

    case MAX_EXPR:
      if (operand_equal_p (arg0, arg1, 0))
	return arg0;
      if (INTEGRAL_TYPE_P (type)
	  && operand_equal_p (arg1, TYPE_MAX_VALUE (type), 1))
	return omit_one_operand (type, arg1, arg0);
      goto associate;

    case TRUTH_NOT_EXPR:
      /* Note that the operand of this must be an int
	 and its values must be 0 or 1.
	 ("true" is a fixed value perhaps depending on the language,
	 but we don't handle values other than 1 correctly yet.)  */
      tem = invert_truthvalue (arg0);
      /* Avoid infinite recursion.  */
      if (TREE_CODE (tem) == TRUTH_NOT_EXPR)
	return t;
      return convert (type, tem);

    case TRUTH_ANDIF_EXPR:
      /* Note that the operands of this must be ints
	 and their values must be 0 or 1.
	 ("true" is a fixed value perhaps depending on the language.)  */
      /* If first arg is constant zero, return it.  */
      if (integer_zerop (arg0))
	return arg0;
    case TRUTH_AND_EXPR:
      /* If either arg is constant true, drop it.  */
      if (TREE_CODE (arg0) == INTEGER_CST && ! integer_zerop (arg0))
	return non_lvalue (arg1);
      if (TREE_CODE (arg1) == INTEGER_CST && ! integer_zerop (arg1))
	return non_lvalue (arg0);
      /* If second arg is constant zero, result is zero, but first arg
	 must be evaluated.  */
      if (integer_zerop (arg1))
	return omit_one_operand (type, arg1, arg0);

    truth_andor:
      /* We only do these simplifications if we are optimizing.  */
      if (!optimize)
	return t;

      /* Check for things like (A || B) && (A || C).  We can convert this
	 to A || (B && C).  Note that either operator can be any of the four
	 truth and/or operations and the transformation will still be
	 valid.   Also note that we only care about order for the
	 ANDIF and ORIF operators.  */
      if (TREE_CODE (arg0) == TREE_CODE (arg1)
	  && (TREE_CODE (arg0) == TRUTH_ANDIF_EXPR
	      || TREE_CODE (arg0) == TRUTH_ORIF_EXPR
	      || TREE_CODE (arg0) == TRUTH_AND_EXPR
	      || TREE_CODE (arg0) == TRUTH_OR_EXPR))
	{
	  tree a00 = TREE_OPERAND (arg0, 0);
	  tree a01 = TREE_OPERAND (arg0, 1);
	  tree a10 = TREE_OPERAND (arg1, 0);
	  tree a11 = TREE_OPERAND (arg1, 1);
	  int commutative = ((TREE_CODE (arg0) == TRUTH_OR_EXPR
			      || TREE_CODE (arg0) == TRUTH_AND_EXPR)
			     && (code == TRUTH_AND_EXPR
				 || code == TRUTH_OR_EXPR));

	  if (operand_equal_p (a00, a10, 0))
	    return fold (build (TREE_CODE (arg0), type, a00,
				fold (build (code, type, a01, a11))));
	  else if (commutative && operand_equal_p (a00, a11, 0))
	    return fold (build (TREE_CODE (arg0), type, a00,
				fold (build (code, type, a01, a10))));
	  else if (commutative && operand_equal_p (a01, a10, 0))
	    return fold (build (TREE_CODE (arg0), type, a01,
				fold (build (code, type, a00, a11))));

	  /* This case if tricky because we must either have commutative
	     operators or else A10 must not have side-effects.  */

	  else if ((commutative || ! TREE_SIDE_EFFECTS (a10))
		   && operand_equal_p (a01, a11, 0))
	    return fold (build (TREE_CODE (arg0), type,
				fold (build (code, type, a00, a10)),
				a01));
	}

      /* Check for the possibility of merging component references.  If our
	 lhs is another similar operation, try to merge its rhs with our
	 rhs.  Then try to merge our lhs and rhs.  */
      if (TREE_CODE (arg0) == code
	  && 0 != (tem = fold_truthop (code, type,
				       TREE_OPERAND (arg0, 1), arg1)))
	return fold (build (code, type, TREE_OPERAND (arg0, 0), tem));

      if ((tem = fold_truthop (code, type, arg0, arg1)) != 0)
	return tem;

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
      /* If second arg is constant true, result is true, but we must
	 evaluate first arg.  */
      if (TREE_CODE (arg1) == INTEGER_CST && ! integer_zerop (arg1))
	return omit_one_operand (type, arg1, arg0);
      goto truth_andor;

    case TRUTH_XOR_EXPR:
      /* If either arg is constant zero, drop it.  */
      if (integer_zerop (arg0))
	return non_lvalue (arg1);
      if (integer_zerop (arg1))
	return non_lvalue (arg0);
      /* If either arg is constant true, this is a logical inversion.  */
      if (integer_onep (arg0))
	return non_lvalue (invert_truthvalue (arg1));
      if (integer_onep (arg1))
	return non_lvalue (invert_truthvalue (arg0));
      return t;

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
	  code = swap_tree_comparison (code);
	  TREE_SET_CODE (t, code);
	}

      /* Convert foo++ == CONST into ++foo == CONST + INCR.
	 First, see if one arg is constant; find the constant arg
	 and the other one.  */
      {
	tree constop = 0, varop;
	int constopnum = -1;

	if (TREE_CONSTANT (arg1))
	  constopnum = 1, constop = arg1, varop = arg0;
	if (TREE_CONSTANT (arg0))
	  constopnum = 0, constop = arg0, varop = arg1;

	if (constop && TREE_CODE (varop) == POSTINCREMENT_EXPR)
	  {
	    /* This optimization is invalid for ordered comparisons
	       if CONST+INCR overflows or if foo+incr might overflow.
	       This optimization is invalid for floating point due to rounding.
	       For pointer types we assume overflow doesn't happen.  */
	    if (TREE_CODE (TREE_TYPE (varop)) == POINTER_TYPE
		|| (! FLOAT_TYPE_P (TREE_TYPE (varop))
		    && (code == EQ_EXPR || code == NE_EXPR)))
	      {
		tree newconst
		  = fold (build (PLUS_EXPR, TREE_TYPE (varop),
				 constop, TREE_OPERAND (varop, 1)));
		TREE_SET_CODE (varop, PREINCREMENT_EXPR);

		t = build (code, type, TREE_OPERAND (t, 0),
			   TREE_OPERAND (t, 1));
		TREE_OPERAND (t, constopnum) = newconst;
		return t;
	      }
	  }
	else if (constop && TREE_CODE (varop) == POSTDECREMENT_EXPR)
	  {
	    if (TREE_CODE (TREE_TYPE (varop)) == POINTER_TYPE
		|| (! FLOAT_TYPE_P (TREE_TYPE (varop))
		    && (code == EQ_EXPR || code == NE_EXPR)))
	      {
		tree newconst
		  = fold (build (MINUS_EXPR, TREE_TYPE (varop),
				 constop, TREE_OPERAND (varop, 1)));
		TREE_SET_CODE (varop, PREDECREMENT_EXPR);
		t = build (code, type, TREE_OPERAND (t, 0),
			   TREE_OPERAND (t, 1));
		TREE_OPERAND (t, constopnum) = newconst;
		return t;
	      }
	  }
      }

      /* Change X >= CST to X > (CST - 1) if CST is positive.  */
      if (TREE_CODE (arg1) == INTEGER_CST
	  && TREE_CODE (arg0) != INTEGER_CST
	  && tree_int_cst_sgn (arg1) > 0)
	{
	  switch (TREE_CODE (t))
	    {
	    case GE_EXPR:
	      code = GT_EXPR;
	      arg1 = const_binop (MINUS_EXPR, arg1, integer_one_node, 0);
	      t = build (code, type, TREE_OPERAND (t, 0), arg1);
	      break;

	    case LT_EXPR:
	      code = LE_EXPR;
	      arg1 = const_binop (MINUS_EXPR, arg1, integer_one_node, 0);
	      t = build (code, type, TREE_OPERAND (t, 0), arg1);
	      break;
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

      /* If this is an NE or EQ comparison of zero against the result of a
	 signed MOD operation whose second operand is a power of 2, make
	 the MOD operation unsigned since it is simpler and equivalent.  */
      if ((code == NE_EXPR || code == EQ_EXPR)
	  && integer_zerop (arg1)
	  && ! TREE_UNSIGNED (TREE_TYPE (arg0))
	  && (TREE_CODE (arg0) == TRUNC_MOD_EXPR
	      || TREE_CODE (arg0) == CEIL_MOD_EXPR
	      || TREE_CODE (arg0) == FLOOR_MOD_EXPR
	      || TREE_CODE (arg0) == ROUND_MOD_EXPR)
	  && integer_pow2p (TREE_OPERAND (arg0, 1)))
	{
	  tree newtype = unsigned_type (TREE_TYPE (arg0));
	  tree newmod = build (TREE_CODE (arg0), newtype,
			       convert (newtype, TREE_OPERAND (arg0, 0)),
			       convert (newtype, TREE_OPERAND (arg0, 1)));

	  return build (code, type, newmod, convert (newtype, arg1));
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

      /* If X is unsigned, convert X < (1 << Y) into X >> Y == 0
	 and similarly for >= into !=.  */
      if ((code == LT_EXPR || code == GE_EXPR)
	  && TREE_UNSIGNED (TREE_TYPE (arg0))
	  && TREE_CODE (arg1) == LSHIFT_EXPR
	  && integer_onep (TREE_OPERAND (arg1, 0)))
	return build (code == LT_EXPR ? EQ_EXPR : NE_EXPR, type, 
		      build (RSHIFT_EXPR, TREE_TYPE (arg0), arg0,
			     TREE_OPERAND (arg1, 1)),
		      convert (TREE_TYPE (arg0), integer_zero_node));

      else if ((code == LT_EXPR || code == GE_EXPR)
	       && TREE_UNSIGNED (TREE_TYPE (arg0))
	       && (TREE_CODE (arg1) == NOP_EXPR
		   || TREE_CODE (arg1) == CONVERT_EXPR)
	       && TREE_CODE (TREE_OPERAND (arg1, 0)) == LSHIFT_EXPR
	       && integer_onep (TREE_OPERAND (TREE_OPERAND (arg1, 0), 0)))
	return
	  build (code == LT_EXPR ? EQ_EXPR : NE_EXPR, type,
		 convert (TREE_TYPE (arg0),
			  build (RSHIFT_EXPR, TREE_TYPE (arg0), arg0,
				 TREE_OPERAND (TREE_OPERAND (arg1, 0), 1))),
		 convert (TREE_TYPE (arg0), integer_zero_node));

      /* Simplify comparison of something with itself.  (For IEEE
	 floating-point, we can only do some of these simplifications.)  */
      if (operand_equal_p (arg0, arg1, 0))
	{
	  switch (code)
	    {
	    case EQ_EXPR:
	    case GE_EXPR:
	    case LE_EXPR:
	      if (INTEGRAL_TYPE_P (TREE_TYPE (arg0)))
		{
		  t = build_int_2 (1, 0);
		  TREE_TYPE (t) = type;
		  return t;
		}
	      code = EQ_EXPR;
	      TREE_SET_CODE (t, code);
	      break;

	    case NE_EXPR:
	      /* For NE, we can only do this simplification if integer.  */
	      if (! INTEGRAL_TYPE_P (TREE_TYPE (arg0)))
		break;
	      /* ... fall through ... */
	    case GT_EXPR:
	    case LT_EXPR:
	      t = build_int_2 (0, 0);
	      TREE_TYPE (t) = type;
	      return t;
	    }
	}

      /* An unsigned comparison against 0 can be simplified.  */
      if (integer_zerop (arg1)
	  && (INTEGRAL_TYPE_P (TREE_TYPE (arg1))
	      || TREE_CODE (TREE_TYPE (arg1)) == POINTER_TYPE)
	  && TREE_UNSIGNED (TREE_TYPE (arg1)))
	{
	  switch (TREE_CODE (t))
	    {
	    case GT_EXPR:
	      code = NE_EXPR;
	      TREE_SET_CODE (t, NE_EXPR);
	      break;
	    case LE_EXPR:
	      code = EQ_EXPR;
	      TREE_SET_CODE (t, EQ_EXPR);
	      break;
	    case GE_EXPR:
	      return omit_one_operand (type,
				       convert (type, integer_one_node),
				       arg0);
	    case LT_EXPR:
	      return omit_one_operand (type,
				       convert (type, integer_zero_node),
				       arg0);
	    }
	}

      /* If we are comparing an expression that just has comparisons
	 of two integer values, arithmetic expressions of those comparisons,
	 and constants, we can simplify it.  There are only three cases
	 to check: the two values can either be equal, the first can be
	 greater, or the second can be greater.  Fold the expression for
	 those three values.  Since each value must be 0 or 1, we have
	 eight possibilities, each of which corresponds to the constant 0
	 or 1 or one of the six possible comparisons.

	 This handles common cases like (a > b) == 0 but also handles
	 expressions like  ((x > y) - (y > x)) > 0, which supposedly
	 occur in macroized code.  */

      if (TREE_CODE (arg1) == INTEGER_CST && TREE_CODE (arg0) != INTEGER_CST)
	{
	  tree cval1 = 0, cval2 = 0;
	  int save_p = 0;

	  if (twoval_comparison_p (arg0, &cval1, &cval2, &save_p)
	      /* Don't handle degenerate cases here; they should already
		 have been handled anyway.  */
	      && cval1 != 0 && cval2 != 0
	      && ! (TREE_CONSTANT (cval1) && TREE_CONSTANT (cval2))
	      && TREE_TYPE (cval1) == TREE_TYPE (cval2)
	      && INTEGRAL_TYPE_P (TREE_TYPE (cval1))
	      && ! operand_equal_p (TYPE_MIN_VALUE (TREE_TYPE (cval1)),
				    TYPE_MAX_VALUE (TREE_TYPE (cval2)), 0))
	    {
	      tree maxval = TYPE_MAX_VALUE (TREE_TYPE (cval1));
	      tree minval = TYPE_MIN_VALUE (TREE_TYPE (cval1));

	      /* We can't just pass T to eval_subst in case cval1 or cval2
		 was the same as ARG1.  */

	      tree high_result
		= fold (build (code, type,
			       eval_subst (arg0, cval1, maxval, cval2, minval),
			       arg1));
	      tree equal_result
		= fold (build (code, type,
			       eval_subst (arg0, cval1, maxval, cval2, maxval),
			       arg1));
	      tree low_result
		= fold (build (code, type,
			       eval_subst (arg0, cval1, minval, cval2, maxval),
			       arg1));

	      /* All three of these results should be 0 or 1.  Confirm they
		 are.  Then use those values to select the proper code
		 to use.  */

	      if ((integer_zerop (high_result)
		   || integer_onep (high_result))
		  && (integer_zerop (equal_result)
		      || integer_onep (equal_result))
		  && (integer_zerop (low_result)
		      || integer_onep (low_result)))
		{
		  /* Make a 3-bit mask with the high-order bit being the
		     value for `>', the next for '=', and the low for '<'.  */
		  switch ((integer_onep (high_result) * 4)
			  + (integer_onep (equal_result) * 2)
			  + integer_onep (low_result))
		    {
		    case 0:
		      /* Always false.  */
		      return omit_one_operand (type, integer_zero_node, arg0);
		    case 1:
		      code = LT_EXPR;
		      break;
		    case 2:
		      code = EQ_EXPR;
		      break;
		    case 3:
		      code = LE_EXPR;
		      break;
		    case 4:
		      code = GT_EXPR;
		      break;
		    case 5:
		      code = NE_EXPR;
		      break;
		    case 6:
		      code = GE_EXPR;
		      break;
		    case 7:
		      /* Always true.  */
		      return omit_one_operand (type, integer_one_node, arg0);
		    }

		  t = build (code, type, cval1, cval2);
		  if (save_p)
		    return save_expr (t);
		  else
		    return fold (t);
		}
	    }
	}

      /* If this is a comparison of a field, we may be able to simplify it.  */
      if ((TREE_CODE (arg0) == COMPONENT_REF
		|| TREE_CODE (arg0) == BIT_FIELD_REF)
	       && (code == EQ_EXPR || code == NE_EXPR)
	       /* Handle the constant case even without -O
		  to make sure the warnings are given.  */
	       && (optimize || TREE_CODE (arg1) == INTEGER_CST))
	{
	  t1 = optimize_bit_field_compare (code, type, arg0, arg1);
	  return t1 ? t1 : t;
	}

      /* If this is a comparison of complex values and either or both
	 sizes are a COMPLEX_EXPR, it is best to split up the comparisons
	 and join them with a TRUTH_ANDIF_EXPR or TRUTH_ORIF_EXPR.  This
	 may prevent needless evaluations.  */
      if ((code == EQ_EXPR || code == NE_EXPR)
	  && TREE_CODE (TREE_TYPE (arg0)) == COMPLEX_TYPE
	  && (TREE_CODE (arg0) == COMPLEX_EXPR
	      || TREE_CODE (arg1) == COMPLEX_EXPR))
	{
	  tree subtype = TREE_TYPE (TREE_TYPE (arg0));
	  tree real0 = fold (build1 (REALPART_EXPR, subtype, arg0));
	  tree imag0 = fold (build1 (IMAGPART_EXPR, subtype, arg0));
	  tree real1 = fold (build1 (REALPART_EXPR, subtype, arg1));
	  tree imag1 = fold (build1 (IMAGPART_EXPR, subtype, arg1));

	  return fold (build ((code == EQ_EXPR ? TRUTH_ANDIF_EXPR
			       : TRUTH_ORIF_EXPR),
			      type,
			      fold (build (code, type, real0, real1)),
			      fold (build (code, type, imag0, imag1))));
	}

      /* From here on, the only cases we handle are when the result is
	 known to be a constant.

	 To compute GT, swap the arguments and do LT.
	 To compute GE, do LT and invert the result.
	 To compute LE, swap the arguments, do LT and invert the result.
	 To compute NE, do EQ and invert the result.

	 Therefore, the code below must handle only EQ and LT.  */

      if (code == LE_EXPR || code == GT_EXPR)
	{
	  tem = arg0, arg0 = arg1, arg1 = tem;
	  code = swap_tree_comparison (code);
	}

      /* Note that it is safe to invert for real values here because we
	 will check below in the one case that it matters.  */

      invert = 0;
      if (code == NE_EXPR || code == GE_EXPR)
	{
	  invert = 1;
	  code = invert_tree_comparison (code);
	}

      /* Compute a result for LT or EQ if args permit;
	 otherwise return T.  */
      if (TREE_CODE (arg0) == INTEGER_CST && TREE_CODE (arg1) == INTEGER_CST)
	{
	  if (code == EQ_EXPR)
	    t1 = build_int_2 ((TREE_INT_CST_LOW (arg0)
			       == TREE_INT_CST_LOW (arg1))
			      && (TREE_INT_CST_HIGH (arg0)
				  == TREE_INT_CST_HIGH (arg1)),
			      0);
	  else
	    t1 = build_int_2 ((TREE_UNSIGNED (TREE_TYPE (arg0))
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
	       && code == EQ_EXPR)
	t1 = build_int_2 (0, 0);

      /* Two real constants can be compared explicitly.  */
      else if (TREE_CODE (arg0) == REAL_CST && TREE_CODE (arg1) == REAL_CST)
	{
	  /* If either operand is a NaN, the result is false with two
	     exceptions: First, an NE_EXPR is true on NaNs, but that case
	     is already handled correctly since we will be inverting the
	     result for NE_EXPR.  Second, if we had inverted a LE_EXPR
	     or a GE_EXPR into a LT_EXPR, we must return true so that it
	     will be inverted into false.  */

	  if (REAL_VALUE_ISNAN (TREE_REAL_CST (arg0))
	      || REAL_VALUE_ISNAN (TREE_REAL_CST (arg1)))
	    t1 = build_int_2 (invert && code == LT_EXPR, 0);

	  else if (code == EQ_EXPR)
	    t1 = build_int_2 (REAL_VALUES_EQUAL (TREE_REAL_CST (arg0),
						 TREE_REAL_CST (arg1)),
			      0);
	  else
	    t1 = build_int_2 (REAL_VALUES_LESS (TREE_REAL_CST (arg0),
						TREE_REAL_CST (arg1)),
			      0);
	}

      if (t1 == NULL_TREE)
	return t;

      if (invert)
	TREE_INT_CST_LOW (t1) ^= 1;

      TREE_TYPE (t1) = type;
      return t1;

    case COND_EXPR:
      /* Pedantic ANSI C says that a conditional expression is never an lvalue,
	 so all simple results must be passed through pedantic_non_lvalue.  */
      if (TREE_CODE (arg0) == INTEGER_CST)
	return pedantic_non_lvalue
	  (TREE_OPERAND (t, (integer_zerop (arg0) ? 2 : 1)));
      else if (operand_equal_p (arg1, TREE_OPERAND (expr, 2), 0))
	return pedantic_omit_one_operand (type, arg1, arg0);

      /* If the second operand is zero, invert the comparison and swap
	 the second and third operands.  Likewise if the second operand
	 is constant and the third is not or if the third operand is
	 equivalent to the first operand of the comparison.  */

      if (integer_zerop (arg1)
	  || (TREE_CONSTANT (arg1) && ! TREE_CONSTANT (TREE_OPERAND (t, 2)))
	  || (TREE_CODE_CLASS (TREE_CODE (arg0)) == '<'
	      && operand_equal_for_comparison_p (TREE_OPERAND (arg0, 0),
						 TREE_OPERAND (t, 2),
						 TREE_OPERAND (arg0, 1))))
	{
	  /* See if this can be inverted.  If it can't, possibly because
	     it was a floating-point inequality comparison, don't do
	     anything.  */
	  tem = invert_truthvalue (arg0);

	  if (TREE_CODE (tem) != TRUTH_NOT_EXPR)
	    {
	      t = build (code, type, tem,
			 TREE_OPERAND (t, 2), TREE_OPERAND (t, 1));
	      arg0 = tem;
	      arg1 = TREE_OPERAND (t, 2);
	      STRIP_NOPS (arg1);
	    }
	}

      /* If we have A op B ? A : C, we may be able to convert this to a
	 simpler expression, depending on the operation and the values
	 of B and C.  IEEE floating point prevents this though,
	 because A or B might be -0.0 or a NaN.  */

      if (TREE_CODE_CLASS (TREE_CODE (arg0)) == '<'
	  && (TARGET_FLOAT_FORMAT != IEEE_FLOAT_FORMAT
	      || ! FLOAT_TYPE_P (TREE_TYPE (TREE_OPERAND (arg0, 0)))
	      || flag_fast_math)
	  && operand_equal_for_comparison_p (TREE_OPERAND (arg0, 0),
					     arg1, TREE_OPERAND (arg0, 1)))
	{
	  tree arg2 = TREE_OPERAND (t, 2);
	  enum tree_code comp_code = TREE_CODE (arg0);

	  STRIP_NOPS (arg2);

	  /* If we have A op 0 ? A : -A, this is A, -A, abs (A), or abs (-A),
	     depending on the comparison operation.  */
	  if ((FLOAT_TYPE_P (TREE_TYPE (TREE_OPERAND (arg0, 1)))
	       ? real_zerop (TREE_OPERAND (arg0, 1))
	       : integer_zerop (TREE_OPERAND (arg0, 1)))
	      && TREE_CODE (arg2) == NEGATE_EXPR
	      && operand_equal_p (TREE_OPERAND (arg2, 0), arg1, 0))
	    switch (comp_code)
	      {
	      case EQ_EXPR:
		return pedantic_non_lvalue
		  (fold (build1 (NEGATE_EXPR, type, arg1)));
	      case NE_EXPR:
		return pedantic_non_lvalue (convert (type, arg1));
	      case GE_EXPR:
	      case GT_EXPR:
		return pedantic_non_lvalue
		  (convert (type, fold (build1 (ABS_EXPR,
						TREE_TYPE (arg1), arg1))));
	      case LE_EXPR:
	      case LT_EXPR:
		return pedantic_non_lvalue
		  (fold (build1 (NEGATE_EXPR, type,
				 convert (type,
					  fold (build1 (ABS_EXPR,
							TREE_TYPE (arg1),
							arg1))))));
	      }

	  /* If this is A != 0 ? A : 0, this is simply A.  For ==, it is
	     always zero.  */

	  if (integer_zerop (TREE_OPERAND (arg0, 1)) && integer_zerop (arg2))
	    {
	      if (comp_code == NE_EXPR)
		return pedantic_non_lvalue (convert (type, arg1));
	      else if (comp_code == EQ_EXPR)
		return pedantic_non_lvalue (convert (type, integer_zero_node));
	    }

	  /* If this is A op B ? A : B, this is either A, B, min (A, B),
	     or max (A, B), depending on the operation.  */

	  if (operand_equal_for_comparison_p (TREE_OPERAND (arg0, 1),
					      arg2, TREE_OPERAND (arg0, 0)))
	    {
	      tree comp_op0 = TREE_OPERAND (arg0, 0);
	      tree comp_op1 = TREE_OPERAND (arg0, 1);
	      tree comp_type = TREE_TYPE (comp_op0);

	      switch (comp_code)
		{
		case EQ_EXPR:
		  return pedantic_non_lvalue (convert (type, arg2));
		case NE_EXPR:
		  return pedantic_non_lvalue (convert (type, arg1));
		case LE_EXPR:
		case LT_EXPR:
		  return pedantic_non_lvalue
		    (convert (type, (fold (build (MIN_EXPR, comp_type,
						  comp_op0, comp_op1)))));
		case GE_EXPR:
		case GT_EXPR:
		  return pedantic_non_lvalue
		    (convert (type, fold (build (MAX_EXPR, comp_type,
						 comp_op0, comp_op1))));
		}
	    }

	  /* If this is A op C1 ? A : C2 with C1 and C2 constant integers,
	     we might still be able to simplify this.  For example,
	     if C1 is one less or one more than C2, this might have started
	     out as a MIN or MAX and been transformed by this function.
	     Only good for INTEGER_TYPEs, because we need TYPE_MAX_VALUE.  */

	  if (INTEGRAL_TYPE_P (type)
	      && TREE_CODE (TREE_OPERAND (arg0, 1)) == INTEGER_CST
	      && TREE_CODE (arg2) == INTEGER_CST)
	    switch (comp_code)
	      {
	      case EQ_EXPR:
		/* We can replace A with C1 in this case.  */
		arg1 = convert (type, TREE_OPERAND (arg0, 1));
		t = build (code, type, TREE_OPERAND (t, 0), arg1,
			   TREE_OPERAND (t, 2));
		break;

	      case LT_EXPR:
		/* If C1 is C2 + 1, this is min(A, C2).  */
		if (! operand_equal_p (arg2, TYPE_MAX_VALUE (type), 1)
		    && operand_equal_p (TREE_OPERAND (arg0, 1),
					const_binop (PLUS_EXPR, arg2,
						     integer_one_node, 0), 1))
		  return pedantic_non_lvalue
		    (fold (build (MIN_EXPR, type, arg1, arg2)));
		break;

	      case LE_EXPR:
		/* If C1 is C2 - 1, this is min(A, C2).  */
		if (! operand_equal_p (arg2, TYPE_MIN_VALUE (type), 1)
		    && operand_equal_p (TREE_OPERAND (arg0, 1),
					const_binop (MINUS_EXPR, arg2,
						     integer_one_node, 0), 1))
		  return pedantic_non_lvalue
		    (fold (build (MIN_EXPR, type, arg1, arg2)));
		break;

	      case GT_EXPR:
		/* If C1 is C2 - 1, this is max(A, C2).  */
		if (! operand_equal_p (arg2, TYPE_MIN_VALUE (type), 1)
		    && operand_equal_p (TREE_OPERAND (arg0, 1),
					const_binop (MINUS_EXPR, arg2,
						     integer_one_node, 0), 1))
		  return pedantic_non_lvalue
		    (fold (build (MAX_EXPR, type, arg1, arg2)));
		break;

	      case GE_EXPR:
		/* If C1 is C2 + 1, this is max(A, C2).  */
		if (! operand_equal_p (arg2, TYPE_MAX_VALUE (type), 1)
		    && operand_equal_p (TREE_OPERAND (arg0, 1),
					const_binop (PLUS_EXPR, arg2,
						     integer_one_node, 0), 1))
		  return pedantic_non_lvalue
		    (fold (build (MAX_EXPR, type, arg1, arg2)));
		break;
	      }
	}

      /* If the second operand is simpler than the third, swap them
	 since that produces better jump optimization results.  */
      if ((TREE_CONSTANT (arg1) || TREE_CODE_CLASS (TREE_CODE (arg1)) == 'd'
	   || TREE_CODE (arg1) == SAVE_EXPR)
	  && ! (TREE_CONSTANT (TREE_OPERAND (t, 2))
		|| TREE_CODE_CLASS (TREE_CODE (TREE_OPERAND (t, 2))) == 'd'
		|| TREE_CODE (TREE_OPERAND (t, 2)) == SAVE_EXPR))
	{
	  /* See if this can be inverted.  If it can't, possibly because
	     it was a floating-point inequality comparison, don't do
	     anything.  */
	  tem = invert_truthvalue (arg0);

	  if (TREE_CODE (tem) != TRUTH_NOT_EXPR)
	    {
	      t = build (code, type, tem,
			 TREE_OPERAND (t, 2), TREE_OPERAND (t, 1));
	      arg0 = tem;
	      arg1 = TREE_OPERAND (t, 2);
	      STRIP_NOPS (arg1);
	    }
	}

      /* Convert A ? 1 : 0 to simply A.  */
      if (integer_onep (TREE_OPERAND (t, 1))
	  && integer_zerop (TREE_OPERAND (t, 2))
	  /* If we try to convert TREE_OPERAND (t, 0) to our type, the
	     call to fold will try to move the conversion inside 
	     a COND, which will recurse.  In that case, the COND_EXPR
	     is probably the best choice, so leave it alone.  */
	  && type == TREE_TYPE (arg0))
	return pedantic_non_lvalue (arg0);

      /* Look for expressions of the form A & 2 ? 2 : 0.  The result of this
	 operation is simply A & 2.  */

      if (integer_zerop (TREE_OPERAND (t, 2))
	  && TREE_CODE (arg0) == NE_EXPR
	  && integer_zerop (TREE_OPERAND (arg0, 1))
	  && integer_pow2p (arg1)
	  && TREE_CODE (TREE_OPERAND (arg0, 0)) == BIT_AND_EXPR
	  && operand_equal_p (TREE_OPERAND (TREE_OPERAND (arg0, 0), 1),
			      arg1, 1))
	return pedantic_non_lvalue (convert (type, TREE_OPERAND (arg0, 0)));

      return t;

    case COMPOUND_EXPR:
      /* When pedantic, a compound expression can be neither an lvalue
	 nor an integer constant expression.  */
      if (TREE_SIDE_EFFECTS (arg0) || pedantic)
	return t;
      /* Don't let (0, 0) be null pointer constant.  */
      if (integer_zerop (arg1))
	return non_lvalue (arg1);
      return arg1;

    case COMPLEX_EXPR:
      if (wins)
	return build_complex (arg0, arg1);
      return t;

    case REALPART_EXPR:
      if (TREE_CODE (TREE_TYPE (arg0)) != COMPLEX_TYPE)
	return t;
      else if (TREE_CODE (arg0) == COMPLEX_EXPR)
	return omit_one_operand (type, TREE_OPERAND (arg0, 0),
				 TREE_OPERAND (arg0, 1));
      else if (TREE_CODE (arg0) == COMPLEX_CST)
	return TREE_REALPART (arg0);
      else if (TREE_CODE (arg0) == PLUS_EXPR || TREE_CODE (arg0) == MINUS_EXPR)
	return fold (build (TREE_CODE (arg0), type,
			    fold (build1 (REALPART_EXPR, type,
					  TREE_OPERAND (arg0, 0))),
			    fold (build1 (REALPART_EXPR,
					  type, TREE_OPERAND (arg0, 1)))));
      return t;

    case IMAGPART_EXPR:
      if (TREE_CODE (TREE_TYPE (arg0)) != COMPLEX_TYPE)
	return convert (type, integer_zero_node);
      else if (TREE_CODE (arg0) == COMPLEX_EXPR)
	return omit_one_operand (type, TREE_OPERAND (arg0, 1),
				 TREE_OPERAND (arg0, 0));
      else if (TREE_CODE (arg0) == COMPLEX_CST)
	return TREE_IMAGPART (arg0);
      else if (TREE_CODE (arg0) == PLUS_EXPR || TREE_CODE (arg0) == MINUS_EXPR)
	return fold (build (TREE_CODE (arg0), type,
			    fold (build1 (IMAGPART_EXPR, type,
					  TREE_OPERAND (arg0, 0))),
			    fold (build1 (IMAGPART_EXPR, type,
					  TREE_OPERAND (arg0, 1)))));
      return t;

      /* Pull arithmetic ops out of the CLEANUP_POINT_EXPR where
         appropriate.  */
    case CLEANUP_POINT_EXPR:
      if (! TREE_SIDE_EFFECTS (arg0))
	return convert (type, arg0);

      {
	enum tree_code code0 = TREE_CODE (arg0);
	int kind0 = TREE_CODE_CLASS (code0);
	tree arg00 = TREE_OPERAND (arg0, 0);
	tree arg01;

	if (kind0 == '1' || code0 == TRUTH_NOT_EXPR)
	  return fold (build1 (code0, type, 
			       fold (build1 (CLEANUP_POINT_EXPR,
					     TREE_TYPE (arg00), arg00))));

	if (kind0 == '<' || kind0 == '2'
	    || code0 == TRUTH_ANDIF_EXPR || code0 == TRUTH_ORIF_EXPR
	    || code0 == TRUTH_AND_EXPR   || code0 == TRUTH_OR_EXPR
	    || code0 == TRUTH_XOR_EXPR)
	  {
	    arg01 = TREE_OPERAND (arg0, 1);

	    if (! TREE_SIDE_EFFECTS (arg00))
	      return fold (build (code0, type, arg00,
				  fold (build1 (CLEANUP_POINT_EXPR,
						TREE_TYPE (arg01), arg01))));

	    if (! TREE_SIDE_EFFECTS (arg01))
	      return fold (build (code0, type,
				  fold (build1 (CLEANUP_POINT_EXPR,
						TREE_TYPE (arg00), arg00)),
				  arg01));
	  }

	return t;
      }

    default:
      return t;
    } /* switch (code) */
}
