/* Fold a constant sub-tree into a single node for C-compiler
   Copyright (C) 1987, 1988, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000, 2001, 2002 Free Software Foundation, Inc.

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

/*@@ This file should be rewritten to use an arbitrary precision
  @@ representation for "struct tree_int_cst" and "struct tree_real_cst".
  @@ Perhaps the routines could also be used for bc/dc, and made a lib.
  @@ The routines that translate from the ap rep should
  @@ warn if precision et. al. is lost.
  @@ This would also make life easier when this technology is used
  @@ for cross-compilers.  */

/* The entry points in this file are fold, size_int_wide, size_binop
   and force_fit_type.

   fold takes a tree as argument and returns a simplified tree.

   size_binop takes a tree code for an arithmetic operation
   and two operands that are trees, and produces a tree for the
   result, assuming the type comes from `sizetype'.

   size_int takes an integer value, and creates a tree constant
   with type from `sizetype'.

   force_fit_type takes a constant and prior overflow indicator, and
   forces the value to fit the type.  It returns an overflow indicator.  */

#include "config.h"
#include "system.h"
#include "flags.h"
#include "tree.h"
#include "rtl.h"
#include "expr.h"
#include "tm_p.h"
#include "toplev.h"
#include "ggc.h"
#include "hashtab.h"

static void encode		PARAMS ((HOST_WIDE_INT *,
					 unsigned HOST_WIDE_INT,
					 HOST_WIDE_INT));
static void decode		PARAMS ((HOST_WIDE_INT *,
					 unsigned HOST_WIDE_INT *,
					 HOST_WIDE_INT *));
#ifndef REAL_ARITHMETIC
static void exact_real_inverse_1 PARAMS ((PTR));
#endif
static tree negate_expr		PARAMS ((tree));
static tree split_tree		PARAMS ((tree, enum tree_code, tree *, tree *,
					 tree *, int));
static tree associate_trees	PARAMS ((tree, tree, enum tree_code, tree));
static tree int_const_binop	PARAMS ((enum tree_code, tree, tree, int));
static void const_binop_1	PARAMS ((PTR));
static tree const_binop		PARAMS ((enum tree_code, tree, tree, int));
static hashval_t size_htab_hash	PARAMS ((const void *));
static int size_htab_eq		PARAMS ((const void *, const void *));
static void fold_convert_1	PARAMS ((PTR));
static tree fold_convert	PARAMS ((tree, tree));
static enum tree_code invert_tree_comparison PARAMS ((enum tree_code));
static enum tree_code swap_tree_comparison PARAMS ((enum tree_code));
static int truth_value_p	PARAMS ((enum tree_code));
static int operand_equal_for_comparison_p PARAMS ((tree, tree, tree));
static int twoval_comparison_p	PARAMS ((tree, tree *, tree *, int *));
static tree eval_subst		PARAMS ((tree, tree, tree, tree, tree));
static tree omit_one_operand	PARAMS ((tree, tree, tree));
static tree pedantic_omit_one_operand PARAMS ((tree, tree, tree));
static tree distribute_bit_expr PARAMS ((enum tree_code, tree, tree, tree));
static tree make_bit_field_ref	PARAMS ((tree, tree, int, int, int));
static tree optimize_bit_field_compare PARAMS ((enum tree_code, tree,
						tree, tree));
static tree decode_field_reference PARAMS ((tree, HOST_WIDE_INT *,
					    HOST_WIDE_INT *,
					    enum machine_mode *, int *,
					    int *, tree *, tree *));
static int all_ones_mask_p	PARAMS ((tree, int));
static int simple_operand_p	PARAMS ((tree));
static tree range_binop		PARAMS ((enum tree_code, tree, tree, int,
					 tree, int));
static tree make_range		PARAMS ((tree, int *, tree *, tree *));
static tree build_range_check	PARAMS ((tree, tree, int, tree, tree));
static int merge_ranges		PARAMS ((int *, tree *, tree *, int, tree, tree,
				       int, tree, tree));
static tree fold_range_test	PARAMS ((tree));
static tree unextend		PARAMS ((tree, int, int, tree));
static tree fold_truthop	PARAMS ((enum tree_code, tree, tree, tree));
static tree optimize_minmax_comparison PARAMS ((tree));
static tree extract_muldiv	PARAMS ((tree, tree, enum tree_code, tree));
static tree strip_compound_expr PARAMS ((tree, tree));
static int multiple_of_p	PARAMS ((tree, tree, tree));
static tree constant_boolean_node PARAMS ((int, tree));
static int count_cond		PARAMS ((tree, int));
static tree fold_binary_op_with_conditional_arg 
  PARAMS ((enum tree_code, tree, tree, tree, int));
							 
#if defined(HOST_EBCDIC)
/* bit 8 is significant in EBCDIC */
#define CHARMASK 0xff
#else
#define CHARMASK 0x7f
#endif

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
encode (words, low, hi)
     HOST_WIDE_INT *words;
     unsigned HOST_WIDE_INT low;
     HOST_WIDE_INT hi;
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
     unsigned HOST_WIDE_INT *low;
     HOST_WIDE_INT *hi;
{
  *low = words[0] + words[1] * BASE;
  *hi = words[2] + words[3] * BASE;
}

/* Make the integer constant T valid for its type by setting to 0 or 1 all
   the bits in the constant that don't belong in the type.

   Return 1 if a signed overflow occurs, 0 otherwise.  If OVERFLOW is
   nonzero, a signed overflow has already occurred in calculating T, so
   propagate it.

   Make the real constant T valid for its type by calling CHECK_FLOAT_VALUE,
   if it exists.  */

int
force_fit_type (t, overflow)
     tree t;
     int overflow;
{
  unsigned HOST_WIDE_INT low;
  HOST_WIDE_INT high;
  unsigned int prec;

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

  if (POINTER_TYPE_P (TREE_TYPE (t)))
    prec = POINTER_SIZE;
  else
    prec = TYPE_PRECISION (TREE_TYPE (t));

  /* First clear all bits that are beyond the type's precision.  */

  if (prec == 2 * HOST_BITS_PER_WIDE_INT)
    ;
  else if (prec > HOST_BITS_PER_WIDE_INT)
    TREE_INT_CST_HIGH (t)
      &= ~((HOST_WIDE_INT) (-1) << (prec - HOST_BITS_PER_WIDE_INT));
  else
    {
      TREE_INT_CST_HIGH (t) = 0;
      if (prec < HOST_BITS_PER_WIDE_INT)
	TREE_INT_CST_LOW (t) &= ~((unsigned HOST_WIDE_INT) (-1) << prec);
    }

  /* Unsigned types do not suffer sign extension or overflow unless they
     are a sizetype.  */
  if (TREE_UNSIGNED (TREE_TYPE (t))
      && ! (TREE_CODE (TREE_TYPE (t)) == INTEGER_TYPE
	    && TYPE_IS_SIZETYPE (TREE_TYPE (t))))
    return overflow;

  /* If the value's sign bit is set, extend the sign.  */
  if (prec != 2 * HOST_BITS_PER_WIDE_INT
      && (prec > HOST_BITS_PER_WIDE_INT
	  ? 0 != (TREE_INT_CST_HIGH (t)
		  & ((HOST_WIDE_INT) 1
		     << (prec - HOST_BITS_PER_WIDE_INT - 1)))
	  : 0 != (TREE_INT_CST_LOW (t)
		  & ((unsigned HOST_WIDE_INT) 1 << (prec - 1)))))
    {
      /* Value is negative:
	 set to 1 all the bits that are outside this type's precision.  */
      if (prec > HOST_BITS_PER_WIDE_INT)
	TREE_INT_CST_HIGH (t)
	  |= ((HOST_WIDE_INT) (-1) << (prec - HOST_BITS_PER_WIDE_INT));
      else
	{
	  TREE_INT_CST_HIGH (t) = -1;
	  if (prec < HOST_BITS_PER_WIDE_INT)
	    TREE_INT_CST_LOW (t) |= ((unsigned HOST_WIDE_INT) (-1) << prec);
	}
    }

  /* Return nonzero if signed overflow occurred.  */
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
     unsigned HOST_WIDE_INT l1, l2;
     HOST_WIDE_INT h1, h2;
     unsigned HOST_WIDE_INT *lv;
     HOST_WIDE_INT *hv;
{
  unsigned HOST_WIDE_INT l;
  HOST_WIDE_INT h;

  l = l1 + l2;
  h = h1 + h2 + (l < l1);

  *lv = l;
  *hv = h;
  return OVERFLOW_SUM_SIGN (h1, h2, h);
}

/* Negate a doubleword integer with doubleword result.
   Return nonzero if the operation overflows, assuming it's signed.
   The argument is given as two `HOST_WIDE_INT' pieces in L1 and H1.
   The value is stored as two `HOST_WIDE_INT' pieces in *LV and *HV.  */

int
neg_double (l1, h1, lv, hv)
     unsigned HOST_WIDE_INT l1;
     HOST_WIDE_INT h1;
     unsigned HOST_WIDE_INT *lv;
     HOST_WIDE_INT *hv;
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
   Return nonzero if the operation overflows, assuming it's signed.
   Each argument is given as two `HOST_WIDE_INT' pieces.
   One argument is L1 and H1; the other, L2 and H2.
   The value is stored as two `HOST_WIDE_INT' pieces in *LV and *HV.  */

int
mul_double (l1, h1, l2, h2, lv, hv)
     unsigned HOST_WIDE_INT l1, l2;
     HOST_WIDE_INT h1, h2;
     unsigned HOST_WIDE_INT *lv;
     HOST_WIDE_INT *hv;
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

  memset ((char *) prod, 0, sizeof prod);

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
  decode (prod + 4, &toplow, &tophigh);
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
     unsigned HOST_WIDE_INT l1;
     HOST_WIDE_INT h1, count;
     unsigned int prec;
     unsigned HOST_WIDE_INT *lv;
     HOST_WIDE_INT *hv;
     int arith;
{
  unsigned HOST_WIDE_INT signmask;

  if (count < 0)
    {
      rshift_double (l1, h1, -count, prec, lv, hv, arith);
      return;
    }

#ifdef SHIFT_COUNT_TRUNCATED
  if (SHIFT_COUNT_TRUNCATED)
    count %= prec;
#endif

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
		? (*hv >> (prec - HOST_BITS_PER_WIDE_INT - 1))
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

/* Shift the doubleword integer in L1, H1 right by COUNT places
   keeping only PREC bits of result.  COUNT must be positive.
   ARITH nonzero specifies arithmetic shifting; otherwise use logical shift.
   Store the value as two `HOST_WIDE_INT' pieces in *LV and *HV.  */

void
rshift_double (l1, h1, count, prec, lv, hv, arith)
     unsigned HOST_WIDE_INT l1;
     HOST_WIDE_INT h1, count;
     unsigned int prec;
     unsigned HOST_WIDE_INT *lv;
     HOST_WIDE_INT *hv;
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
	     | ((unsigned HOST_WIDE_INT) h1 << (HOST_BITS_PER_WIDE_INT - count - 1) << 1));
    }

  /* Zero / sign extend all bits that are beyond the precision.  */

  if (count >= (HOST_WIDE_INT)prec)
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

/* Rotate the doubleword integer in L1, H1 left by COUNT places
   keeping only PREC bits of result.
   Rotate right if COUNT is negative.
   Store the value as two `HOST_WIDE_INT' pieces in *LV and *HV.  */

void
lrotate_double (l1, h1, count, prec, lv, hv)
     unsigned HOST_WIDE_INT l1;
     HOST_WIDE_INT h1, count;
     unsigned int prec;
     unsigned HOST_WIDE_INT *lv;
     HOST_WIDE_INT *hv;
{
  unsigned HOST_WIDE_INT s1l, s2l;
  HOST_WIDE_INT s1h, s2h;

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
     unsigned HOST_WIDE_INT l1;
     HOST_WIDE_INT h1, count;
     unsigned int prec;
     unsigned HOST_WIDE_INT *lv;
     HOST_WIDE_INT *hv;
{
  unsigned HOST_WIDE_INT s1l, s2l;
  HOST_WIDE_INT s1h, s2h;

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
   It controls how the quotient is rounded to an integer.
   Return nonzero if the operation overflows.
   UNS nonzero says do unsigned division.  */

int
div_and_round_double (code, uns,
		      lnum_orig, hnum_orig, lden_orig, hden_orig,
		      lquo, hquo, lrem, hrem)
     enum tree_code code;
     int uns;
     unsigned HOST_WIDE_INT lnum_orig; /* num == numerator == dividend */
     HOST_WIDE_INT hnum_orig;
     unsigned HOST_WIDE_INT lden_orig; /* den == denominator == divisor */
     HOST_WIDE_INT hden_orig;
     unsigned HOST_WIDE_INT *lquo, *lrem;
     HOST_WIDE_INT *hquo, *hrem;
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

  /* calculate quotient sign and convert operands to unsigned.  */
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

  memset ((char *) quo, 0, sizeof quo);

  memset ((char *) num, 0, sizeof num);	/* to zero 9th element */
  memset ((char *) den, 0, sizeof den);

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

      /* Find the highest non-zero divisor digit.  */
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

	  /* Store the quotient digit.  */
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

	/* Get absolute values */
	if (*hrem < 0)
	  neg_double (*lrem, *hrem, &labs_rem, &habs_rem);
	if (hden < 0)
	  neg_double (lden, hden, &labs_den, &habs_den);

	/* If (2 * abs (lrem) >= abs (lden)) */
	mul_double ((HOST_WIDE_INT) 2, (HOST_WIDE_INT) 0,
		    labs_rem, habs_rem, &ltwice, &htwice);

	if (((unsigned HOST_WIDE_INT) habs_den
	     < (unsigned HOST_WIDE_INT) htwice)
	    || (((unsigned HOST_WIDE_INT) habs_den
		 == (unsigned HOST_WIDE_INT) htwice)
		&& (labs_den < ltwice)))
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
      unsigned mantissa2 : 32;
    } little_endian;
    struct {
      unsigned mantissa2 : 32;
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
      unsigned mantissa2 : 32;
    } little_endian;
    struct {
      unsigned mantissa2 : 32;
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
      unsigned mantissa2 : 32;
    } little_endian;
    struct {
      unsigned mantissa2 : 32;
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

int
target_isinf (x)
     REAL_VALUE_TYPE x ATTRIBUTE_UNUSED;
{
  return 0;
}

/* Let's assume other float formats don't have NaNs.
   (This can be overridden by redefining REAL_VALUE_ISNAN.)  */

int
target_isnan (x)
     REAL_VALUE_TYPE x ATTRIBUTE_UNUSED;
{
  return 0;
}

/* Let's assume other float formats don't have minus zero.
   (This can be overridden by redefining REAL_VALUE_NEGATIVE.)  */

int
target_negative (x)
     REAL_VALUE_TYPE x;
{
  return x < 0;
}
#endif /* Target not IEEE */

/* Try to change R into its exact multiplicative inverse in machine mode
   MODE.  Return nonzero function value if successful.  */
struct exact_real_inverse_args
{
  REAL_VALUE_TYPE *r;
  enum machine_mode mode;
  int success;
};

static void
exact_real_inverse_1 (p)
     PTR p;
{
  struct exact_real_inverse_args *args =
    (struct exact_real_inverse_args *) p;

  enum machine_mode mode = args->mode;
  REAL_VALUE_TYPE *r = args->r;

  union
  {
    double d;
    unsigned short i[4];
  }
  x, t, y;
#ifdef CHECK_FLOAT_VALUE
  int i;
#endif

  /* Set array index to the less significant bits in the unions, depending
     on the endian-ness of the host doubles.  */
#if HOST_FLOAT_FORMAT == VAX_FLOAT_FORMAT \
 || HOST_FLOAT_FORMAT == IBM_FLOAT_FORMAT
# define K 2
#else
# define K (2 * HOST_FLOAT_WORDS_BIG_ENDIAN)
#endif

  /* Domain check the argument.  */
  x.d = *r;
  if (x.d == 0.0)
    goto fail;

#ifdef REAL_INFINITY
  if (REAL_VALUE_ISINF (x.d) || REAL_VALUE_ISNAN (x.d))
    goto fail;
#endif

  /* Compute the reciprocal and check for numerical exactness.
     It is unnecessary to check all the significand bits to determine
     whether X is a power of 2.  If X is not, then it is impossible for
     the bottom half significand of both X and 1/X to be all zero bits.
     Hence we ignore the data structure of the top half and examine only
     the low order bits of the two significands.  */
  t.d = 1.0 / x.d;
  if (x.i[K] != 0 || x.i[K + 1] != 0 || t.i[K] != 0 || t.i[K + 1] != 0)
    goto fail;

  /* Truncate to the required mode and range-check the result.  */
  y.d = REAL_VALUE_TRUNCATE (mode, t.d);
#ifdef CHECK_FLOAT_VALUE
  i = 0;
  if (CHECK_FLOAT_VALUE (mode, y.d, i))
    goto fail;
#endif

  /* Fail if truncation changed the value.  */
  if (y.d != t.d || y.d == 0.0)
    goto fail;

#ifdef REAL_INFINITY
  if (REAL_VALUE_ISINF (y.d) || REAL_VALUE_ISNAN (y.d))
    goto fail;
#endif

  /* Output the reciprocal and return success flag.  */
  *r = y.d;
  args->success = 1;
  return;

 fail:
  args->success = 0;
  return;

#undef K
}


int
exact_real_inverse (mode, r)
     enum machine_mode mode;
     REAL_VALUE_TYPE *r;
{
  struct exact_real_inverse_args args;

  /* Disable if insufficient information on the data structure.  */
#if HOST_FLOAT_FORMAT == UNKNOWN_FLOAT_FORMAT
  return 0;
#endif

  /* Usually disable if bounds checks are not reliable.  */
  if ((HOST_FLOAT_FORMAT != TARGET_FLOAT_FORMAT) && !flag_pretend_float)
    return 0;

  args.mode = mode;
  args.r = r;

  if (do_float_handler (exact_real_inverse_1, (PTR) &args))
    return args.success;
  return 0;
}

/* Convert C99 hexadecimal floating point string constant S.  Return
   real value type in mode MODE.  This function uses the host computer's
   floating point arithmetic when there is no REAL_ARITHMETIC.  */

REAL_VALUE_TYPE
real_hex_to_f (s, mode)
   const char *s;
   enum machine_mode mode;
{
  REAL_VALUE_TYPE ip;
  const char *p = s;
  unsigned HOST_WIDE_INT low, high;
  int shcount, nrmcount, k;
  int sign, expsign, isfloat;
  int lost = 0;/* Nonzero low order bits shifted out and discarded.  */
  int frexpon = 0;  /* Bits after the decimal point.  */
  int expon = 0;  /* Value of exponent.  */
  int decpt = 0;  /* How many decimal points.  */
  int gotp = 0;  /* How many P's.  */
  char c;

  isfloat = 0;
  expsign = 1;
  ip = 0.0;

  while (*p == ' ' || *p == '\t')
    ++p;

  /* Sign, if any, comes first.  */
  sign = 1;
  if (*p == '-')
    {
      sign = -1;
      ++p;
    }

  /* The string is supposed to start with 0x or 0X .  */
  if (*p == '0')
    {
      ++p;
      if (*p == 'x' || *p == 'X')
	++p;
      else
	abort ();
    }
  else
    abort ();

  while (*p == '0')
    ++p;

  high = 0;
  low = 0;
  shcount = 0;
  while ((c = *p) != '\0')
    {
      if (ISXDIGIT (c))
	{
	  k = hex_value (c & CHARMASK);

	  if ((high & 0xf0000000) == 0)
	    {
	      high = (high << 4) + ((low >> 28) & 15);
	      low = (low << 4) + k;
	      shcount += 4;
	      if (decpt)
		frexpon += 4;
	    }
	  else
	    {
	      /* Record nonzero lost bits.  */
	      lost |= k;
	      if (! decpt)
		frexpon -= 4;
	    }
	  ++p;
	}
      else if (c == '.')
	{
	  ++decpt;
	  ++p;
	}

      else if (c == 'p' || c == 'P')
	{
	  ++gotp;
	  ++p;
	  /* Sign of exponent.  */
	  if (*p == '-')
	    {
	      expsign = -1;
	      ++p;
	    }

	  /* Value of exponent.
	     The exponent field is a decimal integer.  */
	  while (ISDIGIT (*p))
	    {
	      k = (*p++ & CHARMASK) - '0';
	      expon = 10 * expon + k;
	    }

	  expon *= expsign;
	  /* F suffix is ambiguous in the significand part
	     so it must appear after the decimal exponent field.  */
	  if (*p == 'f' || *p == 'F')
	    {
	      isfloat = 1;
	      ++p;
	      break;
	    }
	}

      else if (c == 'l' || c == 'L')
	{
	  ++p;
	  break;
	}
      else
	break;
    }

  /* Abort if last character read was not legitimate.  */
  c = *p;
  if ((c != '\0' && c != ' ' && c != '\n' && c != '\r') || (decpt > 1))
    abort ();

  /* There must be either one decimal point or one p.  */
  if (decpt == 0 && gotp == 0)
    abort ();

  shcount -= 4;
  if (high == 0 && low == 0)
    return dconst0;

  /* Normalize.  */
  nrmcount = 0;
  if (high == 0)
    {
      high = low;
      low = 0;
      nrmcount += 32;
    }

  /* Leave a high guard bit for carry-out.  */
  if ((high & 0x80000000) != 0)
    {
      lost |= low & 1;
      low = (low >> 1) | (high << 31);
      high = high >> 1;
      nrmcount -= 1;
    }

  if ((high & 0xffff8000) == 0)
    {
      high = (high << 16) + ((low >> 16) & 0xffff);
      low = low << 16;
      nrmcount += 16;
    }

  while ((high & 0xc0000000) == 0)
    {
      high = (high << 1) + ((low >> 31) & 1);
      low = low << 1;
      nrmcount += 1;
    }

  if (isfloat || GET_MODE_SIZE (mode) == UNITS_PER_WORD)
    {
      /* Keep 24 bits precision, bits 0x7fffff80.
	 Rounding bit is 0x40.  */
      lost = lost | low | (high & 0x3f);
      low = 0;
      if (high & 0x40)
	{
	  if ((high & 0x80) || lost)
	    high += 0x40;
	}
      high &= 0xffffff80;
    }
  else
    {
      /* We need real.c to do long double formats, so here default
	 to double precision.  */
#if HOST_FLOAT_FORMAT == IEEE_FLOAT_FORMAT
      /* IEEE double.
	 Keep 53 bits precision, bits 0x7fffffff fffffc00.
	 Rounding bit is low word 0x200.  */
      lost = lost | (low & 0x1ff);
      if (low & 0x200)
	{
	  if ((low & 0x400) || lost)
	    {
	      low = (low + 0x200) & 0xfffffc00;
	      if (low == 0)
		high += 1;
	    }
	}
      low &= 0xfffffc00;
#else
      /* Assume it's a VAX with 56-bit significand,
	 bits 0x7fffffff ffffff80.  */
      lost = lost | (low & 0x7f);
      if (low & 0x40)
	{
	  if ((low & 0x80) || lost)
	    {
	      low = (low + 0x40) & 0xffffff80;
	      if (low == 0)
		high += 1;
	    }
	}
      low &= 0xffffff80;
#endif
    }

  ip = (double) high;
  ip = REAL_VALUE_LDEXP (ip, 32) + (double) low;
  /* Apply shifts and exponent value as power of 2.  */
  ip = REAL_VALUE_LDEXP (ip, expon - (nrmcount + frexpon));

  if (sign < 0)
    ip = -ip;
  return ip;
}

#endif /* no REAL_ARITHMETIC */

/* Given T, an expression, return the negation of T.  Allow for T to be
   null, in which case return null.  */

static tree
negate_expr (t)
     tree t;
{
  tree type;
  tree tem;

  if (t == 0)
    return 0;

  type = TREE_TYPE (t);
  STRIP_SIGN_NOPS (t);

  switch (TREE_CODE (t))
    {
    case INTEGER_CST:
    case REAL_CST:
      if (! TREE_UNSIGNED (type)
	  && 0 != (tem = fold (build1 (NEGATE_EXPR, type, t)))
	  && ! TREE_OVERFLOW (tem))
	return tem;
      break;

    case NEGATE_EXPR:
      return convert (type, TREE_OPERAND (t, 0));

    case MINUS_EXPR:
      /* - (A - B) -> B - A  */
      if (! FLOAT_TYPE_P (type) || flag_unsafe_math_optimizations)
	return convert (type,
			fold (build (MINUS_EXPR, TREE_TYPE (t),
				     TREE_OPERAND (t, 1),
				     TREE_OPERAND (t, 0))));
      break;

    default:
      break;
    }

  return convert (type, fold (build1 (NEGATE_EXPR, TREE_TYPE (t), t)));
}

/* Split a tree IN into a constant, literal and variable parts that could be
   combined with CODE to make IN.  "constant" means an expression with
   TREE_CONSTANT but that isn't an actual constant.  CODE must be a
   commutative arithmetic operation.  Store the constant part into *CONP,
   the literal in *LITP and return the variable part.  If a part isn't
   present, set it to null.  If the tree does not decompose in this way,
   return the entire tree as the variable part and the other parts as null.

   If CODE is PLUS_EXPR we also split trees that use MINUS_EXPR.  In that
   case, we negate an operand that was subtracted.  Except if it is a
   literal for which we use *MINUS_LITP instead.

   If NEGATE_P is true, we are negating all of IN, again except a literal
   for which we use *MINUS_LITP instead.

   If IN is itself a literal or constant, return it as appropriate.

   Note that we do not guarantee that any of the three values will be the
   same type as IN, but they will have the same signedness and mode.  */

static tree
split_tree (in, code, conp, litp, minus_litp, negate_p)
     tree in;
     enum tree_code code;
     tree *conp, *litp, *minus_litp;
     int negate_p;
{
  tree var = 0;

  *conp = 0;
  *litp = 0;
  *minus_litp = 0;

  /* Strip any conversions that don't change the machine mode or signedness.  */
  STRIP_SIGN_NOPS (in);

  if (TREE_CODE (in) == INTEGER_CST || TREE_CODE (in) == REAL_CST)
    *litp = in;
  else if (TREE_CODE (in) == code
	   || (! FLOAT_TYPE_P (TREE_TYPE (in))
	       /* We can associate addition and subtraction together (even
		  though the C standard doesn't say so) for integers because
		  the value is not affected.  For reals, the value might be
		  affected, so we can't.  */
	       && ((code == PLUS_EXPR && TREE_CODE (in) == MINUS_EXPR)
		   || (code == MINUS_EXPR && TREE_CODE (in) == PLUS_EXPR))))
    {
      tree op0 = TREE_OPERAND (in, 0);
      tree op1 = TREE_OPERAND (in, 1);
      int neg1_p = TREE_CODE (in) == MINUS_EXPR;
      int neg_litp_p = 0, neg_conp_p = 0, neg_var_p = 0;

      /* First see if either of the operands is a literal, then a constant.  */
      if (TREE_CODE (op0) == INTEGER_CST || TREE_CODE (op0) == REAL_CST)
	*litp = op0, op0 = 0;
      else if (TREE_CODE (op1) == INTEGER_CST || TREE_CODE (op1) == REAL_CST)
	*litp = op1, neg_litp_p = neg1_p, op1 = 0;

      if (op0 != 0 && TREE_CONSTANT (op0))
	*conp = op0, op0 = 0;
      else if (op1 != 0 && TREE_CONSTANT (op1))
	*conp = op1, neg_conp_p = neg1_p, op1 = 0;

      /* If we haven't dealt with either operand, this is not a case we can
	 decompose.  Otherwise, VAR is either of the ones remaining, if any.  */
      if (op0 != 0 && op1 != 0)
	var = in;
      else if (op0 != 0)
	var = op0;
      else
	var = op1, neg_var_p = neg1_p;

      /* Now do any needed negations.  */
      if (neg_litp_p)
	*minus_litp = *litp, *litp = 0;
      if (neg_conp_p)
	*conp = negate_expr (*conp);
      if (neg_var_p)
	var = negate_expr (var);
    }
  else if (TREE_CONSTANT (in))
    *conp = in;
  else
    var = in;

  if (negate_p)
    {
      if (*litp)
	*minus_litp = *litp, *litp = 0;
      else if (*minus_litp)
	*litp = *minus_litp, *minus_litp = 0;
      *conp = negate_expr (*conp);
      var = negate_expr (var);
    }

  return var;
}

/* Re-associate trees split by the above function.  T1 and T2 are either
   expressions to associate or null.  Return the new expression, if any.  If
   we build an operation, do it in TYPE and with CODE.  */

static tree
associate_trees (t1, t2, code, type)
     tree t1, t2;
     enum tree_code code;
     tree type;
{
  if (t1 == 0)
    return t2;
  else if (t2 == 0)
    return t1;

  /* If either input is CODE, a PLUS_EXPR, or a MINUS_EXPR, don't
     try to fold this since we will have infinite recursion.  But do
     deal with any NEGATE_EXPRs.  */
  if (TREE_CODE (t1) == code || TREE_CODE (t2) == code
      || TREE_CODE (t1) == MINUS_EXPR || TREE_CODE (t2) == MINUS_EXPR)
    {
      if (TREE_CODE (t1) == NEGATE_EXPR)
	return build (MINUS_EXPR, type, convert (type, t2),
		      convert (type, TREE_OPERAND (t1, 0)));
      else if (TREE_CODE (t2) == NEGATE_EXPR)
	return build (MINUS_EXPR, type, convert (type, t1),
		      convert (type, TREE_OPERAND (t2, 0)));
      else
	return build (code, type, convert (type, t1), convert (type, t2));
    }

  return fold (build (code, type, convert (type, t1), convert (type, t2)));
}

/* Combine two integer constants ARG1 and ARG2 under operation CODE
   to produce a new constant.

   If NOTRUNC is nonzero, do not truncate the result to fit the data type.  */

static tree
int_const_binop (code, arg1, arg2, notrunc)
     enum tree_code code;
     tree arg1, arg2;
     int notrunc;
{
  unsigned HOST_WIDE_INT int1l, int2l;
  HOST_WIDE_INT int1h, int2h;
  unsigned HOST_WIDE_INT low;
  HOST_WIDE_INT hi;
  unsigned HOST_WIDE_INT garbagel;
  HOST_WIDE_INT garbageh;
  tree t;
  tree type = TREE_TYPE (arg1);
  int uns = TREE_UNSIGNED (type);
  int is_sizetype
    = (TREE_CODE (type) == INTEGER_TYPE && TYPE_IS_SIZETYPE (type));
  int overflow = 0;
  int no_overflow = 0;

  int1l = TREE_INT_CST_LOW (arg1);
  int1h = TREE_INT_CST_HIGH (arg1);
  int2l = TREE_INT_CST_LOW (arg2);
  int2h = TREE_INT_CST_HIGH (arg2);

  switch (code)
    {
    case BIT_IOR_EXPR:
      low = int1l | int2l, hi = int1h | int2h;
      break;

    case BIT_XOR_EXPR:
      low = int1l ^ int2l, hi = int1h ^ int2h;
      break;

    case BIT_AND_EXPR:
      low = int1l & int2l, hi = int1h & int2h;
      break;

    case BIT_ANDTC_EXPR:
      low = int1l & ~int2l, hi = int1h & ~int2h;
      break;

    case RSHIFT_EXPR:
      int2l = -int2l;
    case LSHIFT_EXPR:
      /* It's unclear from the C standard whether shifts can overflow.
	 The following code ignores overflow; perhaps a C standard
	 interpretation ruling is needed.  */
      lshift_double (int1l, int1h, int2l, TYPE_PRECISION (type),
		     &low, &hi, !uns);
      no_overflow = 1;
      break;

    case RROTATE_EXPR:
      int2l = - int2l;
    case LROTATE_EXPR:
      lrotate_double (int1l, int1h, int2l, TYPE_PRECISION (type),
		      &low, &hi);
      break;

    case PLUS_EXPR:
      overflow = add_double (int1l, int1h, int2l, int2h, &low, &hi);
      break;

    case MINUS_EXPR:
      neg_double (int2l, int2h, &low, &hi);
      add_double (int1l, int1h, low, hi, &low, &hi);
      overflow = OVERFLOW_SUM_SIGN (hi, int2h, int1h);
      break;

    case MULT_EXPR:
      overflow = mul_double (int1l, int1h, int2l, int2h, &low, &hi);
      break;

    case TRUNC_DIV_EXPR:
    case FLOOR_DIV_EXPR: case CEIL_DIV_EXPR:
    case EXACT_DIV_EXPR:
      /* This is a shortcut for a common special case.  */
      if (int2h == 0 && (HOST_WIDE_INT) int2l > 0
	  && ! TREE_CONSTANT_OVERFLOW (arg1)
	  && ! TREE_CONSTANT_OVERFLOW (arg2)
	  && int1h == 0 && (HOST_WIDE_INT) int1l >= 0)
	{
	  if (code == CEIL_DIV_EXPR)
	    int1l += int2l - 1;

	  low = int1l / int2l, hi = 0;
	  break;
	}

      /* ... fall through ...  */

    case ROUND_DIV_EXPR:
      if (int2h == 0 && int2l == 1)
	{
	  low = int1l, hi = int1h;
	  break;
	}
      if (int1l == int2l && int1h == int2h
	  && ! (int1l == 0 && int1h == 0))
	{
	  low = 1, hi = 0;
	  break;
	}
      overflow = div_and_round_double (code, uns, int1l, int1h, int2l, int2h,
				       &low, &hi, &garbagel, &garbageh);
      break;

    case TRUNC_MOD_EXPR:
    case FLOOR_MOD_EXPR: case CEIL_MOD_EXPR:
      /* This is a shortcut for a common special case.  */
      if (int2h == 0 && (HOST_WIDE_INT) int2l > 0
	  && ! TREE_CONSTANT_OVERFLOW (arg1)
	  && ! TREE_CONSTANT_OVERFLOW (arg2)
	  && int1h == 0 && (HOST_WIDE_INT) int1l >= 0)
	{
	  if (code == CEIL_MOD_EXPR)
	    int1l += int2l - 1;
	  low = int1l % int2l, hi = 0;
	  break;
	}

      /* ... fall through ...  */

    case ROUND_MOD_EXPR:
      overflow = div_and_round_double (code, uns,
				       int1l, int1h, int2l, int2h,
				       &garbagel, &garbageh, &low, &hi);
      break;

    case MIN_EXPR:
    case MAX_EXPR:
      if (uns)
	low = (((unsigned HOST_WIDE_INT) int1h
		< (unsigned HOST_WIDE_INT) int2h)
	       || (((unsigned HOST_WIDE_INT) int1h
		    == (unsigned HOST_WIDE_INT) int2h)
		   && int1l < int2l));
      else
	low = (int1h < int2h
	       || (int1h == int2h && int1l < int2l));

      if (low == (code == MIN_EXPR))
	low = int1l, hi = int1h;
      else
	low = int2l, hi = int2h;
      break;

    default:
      abort ();
    }

  /* If this is for a sizetype, can be represented as one (signed)
     HOST_WIDE_INT word, and doesn't overflow, use size_int since it caches
     constants.  */
  if (is_sizetype
      && ((hi == 0 && (HOST_WIDE_INT) low >= 0)
	  || (hi == -1 && (HOST_WIDE_INT) low < 0))
      && overflow == 0 && ! TREE_OVERFLOW (arg1) && ! TREE_OVERFLOW (arg2))
    return size_int_type_wide (low, type);
  else
    {
      t = build_int_2 (low, hi);
      TREE_TYPE (t) = TREE_TYPE (arg1);
    }

  TREE_OVERFLOW (t)
    = ((notrunc
	? (!uns || is_sizetype) && overflow
	: (force_fit_type (t, (!uns || is_sizetype) && overflow)
	   && ! no_overflow))
       | TREE_OVERFLOW (arg1)
       | TREE_OVERFLOW (arg2));

  /* If we're doing a size calculation, unsigned arithmetic does overflow.
     So check if force_fit_type truncated the value.  */
  if (is_sizetype
      && ! TREE_OVERFLOW (t)
      && (TREE_INT_CST_HIGH (t) != hi
	  || TREE_INT_CST_LOW (t) != low))
    TREE_OVERFLOW (t) = 1;

  TREE_CONSTANT_OVERFLOW (t) = (TREE_OVERFLOW (t)
				| TREE_CONSTANT_OVERFLOW (arg1)
				| TREE_CONSTANT_OVERFLOW (arg2));
  return t;
}

/* Define input and output argument for const_binop_1.  */
struct cb_args
{
  enum tree_code code;		/* Input: tree code for operation.  */
  tree type;			/* Input: tree type for operation.  */
  REAL_VALUE_TYPE d1, d2;	/* Input: floating point operands.  */
  tree t;			/* Output: constant for result.  */
};

/* Do the real arithmetic for const_binop while protected by a
   float overflow handler.  */

static void
const_binop_1 (data)
     PTR data;
{
  struct cb_args *args = (struct cb_args *) data;
  REAL_VALUE_TYPE value;

#ifdef REAL_ARITHMETIC
  REAL_ARITHMETIC (value, args->code, args->d1, args->d2);
#else
  switch (args->code)
    {
    case PLUS_EXPR:
      value = args->d1 + args->d2;
      break;

    case MINUS_EXPR:
      value = args->d1 - args->d2;
      break;

    case MULT_EXPR:
      value = args->d1 * args->d2;
      break;

    case RDIV_EXPR:
#ifndef REAL_INFINITY
      if (args->d2 == 0)
	abort ();
#endif

      value = args->d1 / args->d2;
      break;

    case MIN_EXPR:
      value = MIN (args->d1, args->d2);
      break;

    case MAX_EXPR:
      value = MAX (args->d1, args->d2);
      break;

    default:
      abort ();
    }
#endif /* no REAL_ARITHMETIC */

  args->t
    = build_real (args->type,
		  real_value_truncate (TYPE_MODE (args->type), value));
}

/* Combine two constants ARG1 and ARG2 under operation CODE to produce a new
   constant.  We assume ARG1 and ARG2 have the same data type, or at least
   are the same kind of constant and the same machine mode.

   If NOTRUNC is nonzero, do not truncate the result to fit the data type.  */

static tree
const_binop (code, arg1, arg2, notrunc)
     enum tree_code code;
     tree arg1, arg2;
     int notrunc;
{
  STRIP_NOPS (arg1);
  STRIP_NOPS (arg2);

  if (TREE_CODE (arg1) == INTEGER_CST)
    return int_const_binop (code, arg1, arg2, notrunc);

#if ! defined (REAL_IS_NOT_DOUBLE) || defined (REAL_ARITHMETIC)
  if (TREE_CODE (arg1) == REAL_CST)
    {
      REAL_VALUE_TYPE d1;
      REAL_VALUE_TYPE d2;
      int overflow = 0;
      tree t;
      struct cb_args args;

      d1 = TREE_REAL_CST (arg1);
      d2 = TREE_REAL_CST (arg2);

      /* If either operand is a NaN, just return it.  Otherwise, set up
	 for floating-point trap; we return an overflow.  */
      if (REAL_VALUE_ISNAN (d1))
	return arg1;
      else if (REAL_VALUE_ISNAN (d2))
	return arg2;

      /* Setup input for const_binop_1() */
      args.type = TREE_TYPE (arg1);
      args.d1 = d1;
      args.d2 = d2;
      args.code = code;

      if (do_float_handler (const_binop_1, (PTR) &args))
	/* Receive output from const_binop_1.  */
	t = args.t;
      else
	{
	  /* We got an exception from const_binop_1.  */
	  t = copy_node (arg1);
	  overflow = 1;
	}

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
      tree type = TREE_TYPE (arg1);
      tree r1 = TREE_REALPART (arg1);
      tree i1 = TREE_IMAGPART (arg1);
      tree r2 = TREE_REALPART (arg2);
      tree i2 = TREE_IMAGPART (arg2);
      tree t;

      switch (code)
	{
	case PLUS_EXPR:
	  t = build_complex (type,
			     const_binop (PLUS_EXPR, r1, r2, notrunc),
			     const_binop (PLUS_EXPR, i1, i2, notrunc));
	  break;

	case MINUS_EXPR:
	  t = build_complex (type,
			     const_binop (MINUS_EXPR, r1, r2, notrunc),
			     const_binop (MINUS_EXPR, i1, i2, notrunc));
	  break;

	case MULT_EXPR:
	  t = build_complex (type,
			     const_binop (MINUS_EXPR,
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
	    tree magsquared
	      = const_binop (PLUS_EXPR,
			     const_binop (MULT_EXPR, r2, r2, notrunc),
			     const_binop (MULT_EXPR, i2, i2, notrunc),
			     notrunc);

	    t = build_complex (type,
			       const_binop
			       (INTEGRAL_TYPE_P (TREE_TYPE (r1))
				? TRUNC_DIV_EXPR : RDIV_EXPR,
				const_binop (PLUS_EXPR,
					     const_binop (MULT_EXPR, r1, r2,
							  notrunc),
					     const_binop (MULT_EXPR, i1, i2,
							  notrunc),
					     notrunc),
				magsquared, notrunc),
			       const_binop
			       (INTEGRAL_TYPE_P (TREE_TYPE (r1))
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
      return t;
    }
  return 0;
}

/* These are the hash table functions for the hash table of INTEGER_CST
   nodes of a sizetype.  */

/* Return the hash code code X, an INTEGER_CST.  */

static hashval_t
size_htab_hash (x)
     const void *x;
{
  tree t = (tree) x;

  return (TREE_INT_CST_HIGH (t) ^ TREE_INT_CST_LOW (t)
	  ^ (hashval_t) ((long) TREE_TYPE (t) >> 3)
	  ^ (TREE_OVERFLOW (t) << 20));
}

/* Return non-zero if the value represented by *X (an INTEGER_CST tree node)
   is the same as that given by *Y, which is the same.  */

static int
size_htab_eq (x, y)
     const void *x;
     const void *y;
{
  tree xt = (tree) x;
  tree yt = (tree) y;

  return (TREE_INT_CST_HIGH (xt) == TREE_INT_CST_HIGH (yt)
	  && TREE_INT_CST_LOW (xt) == TREE_INT_CST_LOW (yt)
	  && TREE_TYPE (xt) == TREE_TYPE (yt)
	  && TREE_OVERFLOW (xt) == TREE_OVERFLOW (yt));
}

/* Return an INTEGER_CST with value whose low-order HOST_BITS_PER_WIDE_INT
   bits are given by NUMBER and of the sizetype represented by KIND.  */

tree
size_int_wide (number, kind)
     HOST_WIDE_INT number;
     enum size_type_kind kind;
{
  return size_int_type_wide (number, sizetype_tab[(int) kind]);
}

/* Likewise, but the desired type is specified explicitly.  */

tree
size_int_type_wide (number, type)
     HOST_WIDE_INT number;
     tree type;
{
  static htab_t size_htab = 0;
  static tree new_const = 0;
  PTR *slot;

  if (size_htab == 0)
    {
      size_htab = htab_create (1024, size_htab_hash, size_htab_eq, NULL);
      ggc_add_deletable_htab (size_htab, NULL, NULL);
      new_const = make_node (INTEGER_CST);
      ggc_add_tree_root (&new_const, 1);
    }

  /* Adjust NEW_CONST to be the constant we want.  If it's already in the
     hash table, we return the value from the hash table.  Otherwise, we
     place that in the hash table and make a new node for the next time.  */
  TREE_INT_CST_LOW (new_const) = number;
  TREE_INT_CST_HIGH (new_const) = number < 0 ? -1 : 0;
  TREE_TYPE (new_const) = type;
  TREE_OVERFLOW (new_const) = TREE_CONSTANT_OVERFLOW (new_const)
    = force_fit_type (new_const, 0);

  slot = htab_find_slot (size_htab, new_const, INSERT);
  if (*slot == 0)
    {
      tree t = new_const;

      *slot = (PTR) new_const;
      new_const = make_node (INTEGER_CST);
      return t;
    }
  else
    return (tree) *slot;
}

/* Combine operands OP1 and OP2 with arithmetic operation CODE.  CODE
   is a tree code.  The type of the result is taken from the operands.
   Both must be the same type integer type and it must be a size type.
   If the operands are constant, so is the result.  */

tree
size_binop (code, arg0, arg1)
     enum tree_code code;
     tree arg0, arg1;
{
  tree type = TREE_TYPE (arg0);

  if (TREE_CODE (type) != INTEGER_TYPE || ! TYPE_IS_SIZETYPE (type)
      || type != TREE_TYPE (arg1))
    abort ();

  /* Handle the special case of two integer constants faster.  */
  if (TREE_CODE (arg0) == INTEGER_CST && TREE_CODE (arg1) == INTEGER_CST)
    {
      /* And some specific cases even faster than that.  */
      if (code == PLUS_EXPR && integer_zerop (arg0))
	return arg1;
      else if ((code == MINUS_EXPR || code == PLUS_EXPR)
	       && integer_zerop (arg1))
	return arg0;
      else if (code == MULT_EXPR && integer_onep (arg0))
	return arg1;

      /* Handle general case of two integer constants.  */
      return int_const_binop (code, arg0, arg1, 0);
    }

  if (arg0 == error_mark_node || arg1 == error_mark_node)
    return error_mark_node;

  return fold (build (code, type, arg0, arg1));
}

/* Given two values, either both of sizetype or both of bitsizetype,
   compute the difference between the two values.  Return the value
   in signed type corresponding to the type of the operands.  */

tree
size_diffop (arg0, arg1)
     tree arg0, arg1;
{
  tree type = TREE_TYPE (arg0);
  tree ctype;

  if (TREE_CODE (type) != INTEGER_TYPE || ! TYPE_IS_SIZETYPE (type)
      || type != TREE_TYPE (arg1))
    abort ();

  /* If the type is already signed, just do the simple thing.  */
  if (! TREE_UNSIGNED (type))
    return size_binop (MINUS_EXPR, arg0, arg1);

  ctype = (type == bitsizetype || type == ubitsizetype
	   ? sbitsizetype : ssizetype);

  /* If either operand is not a constant, do the conversions to the signed
     type and subtract.  The hardware will do the right thing with any
     overflow in the subtraction.  */
  if (TREE_CODE (arg0) != INTEGER_CST || TREE_CODE (arg1) != INTEGER_CST)
    return size_binop (MINUS_EXPR, convert (ctype, arg0),
		       convert (ctype, arg1));

  /* If ARG0 is larger than ARG1, subtract and return the result in CTYPE.
     Otherwise, subtract the other way, convert to CTYPE (we know that can't
     overflow) and negate (which can't either).  Special-case a result
     of zero while we're here.  */
  if (tree_int_cst_equal (arg0, arg1))
    return convert (ctype, integer_zero_node);
  else if (tree_int_cst_lt (arg1, arg0))
    return convert (ctype, size_binop (MINUS_EXPR, arg0, arg1));
  else
    return size_binop (MINUS_EXPR, convert (ctype, integer_zero_node),
		       convert (ctype, size_binop (MINUS_EXPR, arg1, arg0)));
}

/* This structure is used to communicate arguments to fold_convert_1.  */
struct fc_args
{
  tree arg1;			/* Input: value to convert.  */
  tree type;			/* Input: type to convert value to.  */
  tree t;			/* Output: result of conversion.  */
};

/* Function to convert floating-point constants, protected by floating
   point exception handler.  */

static void
fold_convert_1 (data)
     PTR data;
{
  struct fc_args *args = (struct fc_args *) data;

  args->t = build_real (args->type,
			real_value_truncate (TYPE_MODE (args->type),
					     TREE_REAL_CST (args->arg1)));
}

/* Given T, a tree representing type conversion of ARG1, a constant,
   return a constant tree representing the result of conversion.  */

static tree
fold_convert (t, arg1)
     tree t;
     tree arg1;
{
  tree type = TREE_TYPE (t);
  int overflow = 0;

  if (POINTER_TYPE_P (type) || INTEGRAL_TYPE_P (type))
    {
      if (TREE_CODE (arg1) == INTEGER_CST)
	{
	  /* If we would build a constant wider than GCC supports,
	     leave the conversion unfolded.  */
	  if (TYPE_PRECISION (type) > 2 * HOST_BITS_PER_WIDE_INT)
	    return t;

	  /* If we are trying to make a sizetype for a small integer, use
	     size_int to pick up cached types to reduce duplicate nodes.  */
	  if (TREE_CODE (type) == INTEGER_TYPE && TYPE_IS_SIZETYPE (type)
	      && !TREE_CONSTANT_OVERFLOW (arg1)
	      && compare_tree_int (arg1, 10000) < 0)
	    return size_int_type_wide (TREE_INT_CST_LOW (arg1), type);

	  /* Given an integer constant, make new constant with new type,
	     appropriately sign-extended or truncated.  */
	  t = build_int_2 (TREE_INT_CST_LOW (arg1),
			   TREE_INT_CST_HIGH (arg1));
	  TREE_TYPE (t) = type;
	  /* Indicate an overflow if (1) ARG1 already overflowed,
	     or (2) force_fit_type indicates an overflow.
	     Tell force_fit_type that an overflow has already occurred
	     if ARG1 is a too-large unsigned value and T is signed.
	     But don't indicate an overflow if converting a pointer.  */
	  TREE_OVERFLOW (t)
	    = ((force_fit_type (t,
				(TREE_INT_CST_HIGH (arg1) < 0
				 && (TREE_UNSIGNED (type)
				    < TREE_UNSIGNED (TREE_TYPE (arg1)))))
		&& ! POINTER_TYPE_P (TREE_TYPE (arg1)))
	       || TREE_OVERFLOW (arg1));
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
	  tree type1 = TREE_TYPE (arg1);
	  int no_upper_bound;

	  x = TREE_REAL_CST (arg1);
	  l = real_value_from_int_cst (type1, TYPE_MIN_VALUE (type));

	  no_upper_bound = (TYPE_MAX_VALUE (type) == NULL);
	  if (!no_upper_bound)
	    u = real_value_from_int_cst (type1, TYPE_MAX_VALUE (type));

	  /* See if X will be in range after truncation towards 0.
	     To compensate for truncation, move the bounds away from 0,
	     but reject if X exactly equals the adjusted bounds.  */
#ifdef REAL_ARITHMETIC
	  REAL_ARITHMETIC (l, MINUS_EXPR, l, dconst1);
	  if (!no_upper_bound)
	    REAL_ARITHMETIC (u, PLUS_EXPR, u, dconst1);
#else
	  l--;
	  if (!no_upper_bound)
	    u++;
#endif
	  /* If X is a NaN, use zero instead and show we have an overflow.
	     Otherwise, range check.  */
	  if (REAL_VALUE_ISNAN (x))
	    overflow = 1, x = dconst0;
	  else if (! (REAL_VALUES_LESS (l, x)
		      && !no_upper_bound
		      && REAL_VALUES_LESS (x, u)))
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
	  struct fc_args args;

	  if (REAL_VALUE_ISNAN (TREE_REAL_CST (arg1)))
	    {
	      t = arg1;
	      TREE_TYPE (arg1) = type;
	      return t;
	    }

	  /* Setup input for fold_convert_1() */
	  args.arg1 = arg1;
	  args.type = type;

	  if (do_float_handler (fold_convert_1, (PTR) &args))
	    {
	      /* Receive output from fold_convert_1() */
	      t = args.t;
	    }
	  else
	    {
	      /* We got an exception from fold_convert_1() */
	      overflow = 1;
	      t = copy_node (arg1);
	    }

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

  if (TREE_CODE (arg0) != TREE_CODE (arg1)
      /* This is needed for conversions and for COMPONENT_REF.
	 Might as well play it safe and always test this.  */
      || TREE_CODE (TREE_TYPE (arg0)) == ERROR_MARK
      || TREE_CODE (TREE_TYPE (arg1)) == ERROR_MARK
      || TYPE_MODE (TREE_TYPE (arg0)) != TYPE_MODE (TREE_TYPE (arg1)))
    return 0;

  /* If ARG0 and ARG1 are the same SAVE_EXPR, they are necessarily equal.
     We don't care about side effects in that case because the SAVE_EXPR
     takes care of that for us. In all other cases, two expressions are
     equal if they have no side effects.  If we have two identical
     expressions with side effects that should be treated the same due
     to the only side effects being identical SAVE_EXPR's, that will
     be detected in the recursive calls below.  */
  if (arg0 == arg1 && ! only_const
      && (TREE_CODE (arg0) == SAVE_EXPR
	  || (! TREE_SIDE_EFFECTS (arg0) && ! TREE_SIDE_EFFECTS (arg1))))
    return 1;

  /* Next handle constant cases, those for which we can return 1 even
     if ONLY_CONST is set.  */
  if (TREE_CONSTANT (arg0) && TREE_CONSTANT (arg1))
    switch (TREE_CODE (arg0))
      {
      case INTEGER_CST:
	return (! TREE_CONSTANT_OVERFLOW (arg0)
		&& ! TREE_CONSTANT_OVERFLOW (arg1)
		&& tree_int_cst_equal (arg0, arg1));

      case REAL_CST:
	return (! TREE_CONSTANT_OVERFLOW (arg0)
		&& ! TREE_CONSTANT_OVERFLOW (arg1)
		&& REAL_VALUES_IDENTICAL (TREE_REAL_CST (arg0),
					  TREE_REAL_CST (arg1)));

      case VECTOR_CST:
	{
	  tree v1, v2;

	  if (TREE_CONSTANT_OVERFLOW (arg0)
	      || TREE_CONSTANT_OVERFLOW (arg1))
	    return 0;

	  v1 = TREE_VECTOR_CST_ELTS (arg0);
	  v2 = TREE_VECTOR_CST_ELTS (arg1);
	  while (v1 && v2)
	    {
	      if (!operand_equal_p (v1, v2, only_const))
		return 0;
	      v1 = TREE_CHAIN (v1);
	      v2 = TREE_CHAIN (v2);
	    }

	  return 1;
	}

      case COMPLEX_CST:
	return (operand_equal_p (TREE_REALPART (arg0), TREE_REALPART (arg1),
				 only_const)
		&& operand_equal_p (TREE_IMAGPART (arg0), TREE_IMAGPART (arg1),
				    only_const));

      case STRING_CST:
	return (TREE_STRING_LENGTH (arg0) == TREE_STRING_LENGTH (arg1)
		&& ! memcmp (TREE_STRING_POINTER (arg0),
			      TREE_STRING_POINTER (arg1),
			      TREE_STRING_LENGTH (arg0)));

      case ADDR_EXPR:
	return operand_equal_p (TREE_OPERAND (arg0, 0), TREE_OPERAND (arg1, 0),
				0);
      default:
	break;
      }

  if (only_const)
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
      if (operand_equal_p (TREE_OPERAND (arg0, 0), TREE_OPERAND (arg1, 0), 0)
	  && operand_equal_p (TREE_OPERAND (arg0, 1), TREE_OPERAND (arg1, 1),
			      0))
	return 1;

      /* For commutative ops, allow the other order.  */
      return ((TREE_CODE (arg0) == PLUS_EXPR || TREE_CODE (arg0) == MULT_EXPR
	       || TREE_CODE (arg0) == MIN_EXPR || TREE_CODE (arg0) == MAX_EXPR
	       || TREE_CODE (arg0) == BIT_IOR_EXPR
	       || TREE_CODE (arg0) == BIT_XOR_EXPR
	       || TREE_CODE (arg0) == BIT_AND_EXPR
	       || TREE_CODE (arg0) == NE_EXPR || TREE_CODE (arg0) == EQ_EXPR)
	      && operand_equal_p (TREE_OPERAND (arg0, 0),
				  TREE_OPERAND (arg1, 1), 0)
	      && operand_equal_p (TREE_OPERAND (arg0, 1),
				  TREE_OPERAND (arg1, 0), 0));

    case 'r':
      /* If either of the pointer (or reference) expressions we are dereferencing
	 contain a side effect, these cannot be equal.  */
      if (TREE_SIDE_EFFECTS (arg0)
	  || TREE_SIDE_EFFECTS (arg1))
	return 0;

      switch (TREE_CODE (arg0))
	{
	case INDIRECT_REF:
	  return operand_equal_p (TREE_OPERAND (arg0, 0),
				  TREE_OPERAND (arg1, 0), 0);

	case COMPONENT_REF:
	case ARRAY_REF:
	case ARRAY_RANGE_REF:
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
	default:
	  return 0;
	}

    case 'e':
      if (TREE_CODE (arg0) == RTL_EXPR)
	return rtx_equal_p (RTL_EXPR_RTL (arg0), RTL_EXPR_RTL (arg1));
      return 0;

    default:
      return 0;
    }
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
  tree primarg0, primarg1, primother;
  unsigned int correct_width;

  if (operand_equal_p (arg0, arg1, 0))
    return 1;

  if (! INTEGRAL_TYPE_P (TREE_TYPE (arg0))
      || ! INTEGRAL_TYPE_P (TREE_TYPE (arg1)))
    return 0;

  /* Discard any conversions that don't change the modes of ARG0 and ARG1
     and see if the inner values are the same.  This removes any
     signedness comparison, which doesn't matter here.  */
  primarg0 = arg0, primarg1 = arg1;
  STRIP_NOPS (primarg0);
  STRIP_NOPS (primarg1);
  if (operand_equal_p (primarg0, primarg1, 0))
    return 1;

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

  else if (class == 'e' && code == SAVE_EXPR && SAVE_EXPR_RTL (arg) == 0
	   && ! TREE_SIDE_EFFECTS (TREE_OPERAND (arg, 0)))
    {
      /* If we've already found a CVAL1 or CVAL2, this expression is
	 two complex to handle.  */
      if (*cval1 || *cval2)
	return 0;

      class = '1';
      *save_p = 1;
    }

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

    default:
      return 0;
    }
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
	default:
	  break;
	}
      /* fall through - ??? */

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

    default:
      return arg;
    }
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
	  && !flag_unsafe_math_optimizations
	  && code != NE_EXPR 
	  && code != EQ_EXPR)
	return build1 (TRUTH_NOT_EXPR, type, arg);
      else
	return build (invert_tree_comparison (code), type,
		      TREE_OPERAND (arg, 0), TREE_OPERAND (arg, 1));
    }

  switch (code)
    {
    case INTEGER_CST:
      return convert (type, build_int_2 (integer_zerop (arg), 0));

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

    case WITH_RECORD_EXPR:
      return build (WITH_RECORD_EXPR, type,
		    invert_truthvalue (TREE_OPERAND (arg, 0)),
		    TREE_OPERAND (arg, 1));

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

    default:
      break;
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
		       size_int (bitsize), bitsize_int (bitpos));

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
  HOST_WIDE_INT lbitpos, lbitsize, rbitpos, rbitsize, nbitpos, nbitsize;
  tree type = TREE_TYPE (lhs);
  tree signed_type, unsigned_type;
  int const_p = TREE_CODE (rhs) == INTEGER_CST;
  enum machine_mode lmode, rmode, nmode;
  int lunsignedp, runsignedp;
  int lvolatilep = 0, rvolatilep = 0;
  tree linner, rinner = NULL_TREE;
  tree mask;
  tree offset;

  /* Get all the information about the extractions being done.  If the bit size
     if the same as the size of the underlying object, we aren't doing an
     extraction at all and so can do nothing.  We also don't want to
     do anything if the inner expression is a PLACEHOLDER_EXPR since we
     then will no longer be able to replace it.  */
  linner = get_inner_reference (lhs, &lbitsize, &lbitpos, &offset, &lmode,
				&lunsignedp, &lvolatilep);
  if (linner == lhs || lbitsize == GET_MODE_BITSIZE (lmode) || lbitsize < 0
      || offset != 0 || TREE_CODE (linner) == PLACEHOLDER_EXPR)
    return 0;

 if (!const_p)
   {
     /* If this is not a constant, we can only do something if bit positions,
	sizes, and signedness are the same.  */
     rinner = get_inner_reference (rhs, &rbitsize, &rbitpos, &offset, &rmode,
				   &runsignedp, &rvolatilep);

     if (rinner == rhs || lbitpos != rbitpos || lbitsize != rbitsize
	 || lunsignedp != runsignedp || offset != 0
	 || TREE_CODE (rinner) == PLACEHOLDER_EXPR)
       return 0;
   }

  /* See if we can find a mode to refer to this field.  We should be able to,
     but fail if we can't.  */
  nmode = get_best_mode (lbitsize, lbitpos,
			 const_p ? TYPE_ALIGN (TREE_TYPE (linner))
			 : MIN (TYPE_ALIGN (TREE_TYPE (linner)),
				TYPE_ALIGN (TREE_TYPE (rinner))),
			 word_mode, lvolatilep || rvolatilep);
  if (nmode == VOIDmode)
    return 0;

  /* Set signed and unsigned types of the precision of this mode for the
     shifts below.  */
  signed_type = type_for_mode (nmode, 0);
  unsigned_type = type_for_mode (nmode, 1);

  /* Compute the bit position and size for the new reference and our offset
     within it. If the new reference is the same size as the original, we
     won't optimize anything, so return zero.  */
  nbitsize = GET_MODE_BITSIZE (nmode);
  nbitpos = lbitpos & ~ (nbitsize - 1);
  lbitpos -= nbitpos;
  if (nbitsize == lbitsize)
    return 0;

  if (BYTES_BIG_ENDIAN)
    lbitpos = nbitsize - lbitsize - lbitpos;

  /* Make the mask to be used against the extracted field.  */
  mask = build_int_2 (~0, ~0);
  TREE_TYPE (mask) = unsigned_type;
  force_fit_type (mask, 0);
  mask = convert (unsigned_type, mask);
  mask = const_binop (LSHIFT_EXPR, mask, size_int (nbitsize - lbitsize), 0);
  mask = const_binop (RSHIFT_EXPR, mask,
		      size_int (nbitsize - lbitsize - lbitpos), 0);

  if (! const_p)
    /* If not comparing with constant, just rework the comparison
       and return.  */
    return build (code, compare_type,
		  build (BIT_AND_EXPR, unsigned_type,
			 make_bit_field_ref (linner, unsigned_type,
					     nbitsize, nbitpos, 1),
			 mask),
		  build (BIT_AND_EXPR, unsigned_type,
			 make_bit_field_ref (rinner, unsigned_type,
					     nbitsize, nbitpos, 1),
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
	  warning ("comparison is always %d due to width of bit-field",
		   code == NE_EXPR);
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
	  warning ("comparison is always %d due to width of bit-field",
		   code == NE_EXPR);
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
  lhs = make_bit_field_ref (linner, unsigned_type, nbitsize, nbitpos, 1);
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

   *PAND_MASK is set to the mask found in a BIT_AND_EXPR, if any.

   Return 0 if this is not a component reference or is one that we can't
   do anything with.  */

static tree
decode_field_reference (exp, pbitsize, pbitpos, pmode, punsignedp,
			pvolatilep, pmask, pand_mask)
     tree exp;
     HOST_WIDE_INT *pbitsize, *pbitpos;
     enum machine_mode *pmode;
     int *punsignedp, *pvolatilep;
     tree *pmask;
     tree *pand_mask;
{
  tree and_mask = 0;
  tree mask, inner, offset;
  tree unsigned_type;
  unsigned int precision;

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
      || *pbitsize < 0 || offset != 0
      || TREE_CODE (inner) == PLACEHOLDER_EXPR)
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
  unsigned int precision = TYPE_PRECISION (type);
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
	  || (DECL_P (exp)
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

/* The following functions are subroutines to fold_range_test and allow it to
   try to change a logical combination of comparisons into a range test.

   For example, both
   	X == 2 || X == 3 || X == 4 || X == 5
   and
   	X >= 2 && X <= 5
   are converted to
	(unsigned) (X - 2) <= 3

   We describe each set of comparisons as being either inside or outside
   a range, using a variable named like IN_P, and then describe the
   range with a lower and upper bound.  If one of the bounds is omitted,
   it represents either the highest or lowest value of the type.

   In the comments below, we represent a range by two numbers in brackets
   preceded by a "+" to designate being inside that range, or a "-" to
   designate being outside that range, so the condition can be inverted by
   flipping the prefix.  An omitted bound is represented by a "-".  For
   example, "- [-, 10]" means being outside the range starting at the lowest
   possible value and ending at 10, in other words, being greater than 10.
   The range "+ [-, -]" is always true and hence the range "- [-, -]" is
   always false.

   We set up things so that the missing bounds are handled in a consistent
   manner so neither a missing bound nor "true" and "false" need to be
   handled using a special case.  */

/* Return the result of applying CODE to ARG0 and ARG1, but handle the case
   of ARG0 and/or ARG1 being omitted, meaning an unlimited range. UPPER0_P
   and UPPER1_P are nonzero if the respective argument is an upper bound
   and zero for a lower.  TYPE, if nonzero, is the type of the result; it
   must be specified for a comparison.  ARG1 will be converted to ARG0's
   type if both are specified.  */

static tree
range_binop (code, type, arg0, upper0_p, arg1, upper1_p)
     enum tree_code code;
     tree type;
     tree arg0, arg1;
     int upper0_p, upper1_p;
{
  tree tem;
  int result;
  int sgn0, sgn1;

  /* If neither arg represents infinity, do the normal operation.
     Else, if not a comparison, return infinity.  Else handle the special
     comparison rules. Note that most of the cases below won't occur, but
     are handled for consistency.  */

  if (arg0 != 0 && arg1 != 0)
    {
      tem = fold (build (code, type != 0 ? type : TREE_TYPE (arg0),
			 arg0, convert (TREE_TYPE (arg0), arg1)));
      STRIP_NOPS (tem);
      return TREE_CODE (tem) == INTEGER_CST ? tem : 0;
    }

  if (TREE_CODE_CLASS (code) != '<')
    return 0;

  /* Set SGN[01] to -1 if ARG[01] is a lower bound, 1 for upper, and 0
     for neither.  In real maths, we cannot assume open ended ranges are
     the same. But, this is computer arithmetic, where numbers are finite.
     We can therefore make the transformation of any unbounded range with
     the value Z, Z being greater than any representable number. This permits
     us to treat unbounded ranges as equal.  */
  sgn0 = arg0 != 0 ? 0 : (upper0_p ? 1 : -1);
  sgn1 = arg1 != 0 ? 0 : (upper1_p ? 1 : -1);
  switch (code)
    {
    case EQ_EXPR:
      result = sgn0 == sgn1;
      break;
    case NE_EXPR:
      result = sgn0 != sgn1;
      break;
    case LT_EXPR:
      result = sgn0 < sgn1;
      break;
    case LE_EXPR:
      result = sgn0 <= sgn1;
      break;
    case GT_EXPR:
      result = sgn0 > sgn1;
      break;
    case GE_EXPR:
      result = sgn0 >= sgn1;
      break;
    default:
      abort ();
    }

  return convert (type, result ? integer_one_node : integer_zero_node);
}

/* Given EXP, a logical expression, set the range it is testing into
   variables denoted by PIN_P, PLOW, and PHIGH.  Return the expression
   actually being tested.  *PLOW and *PHIGH will be made of the same type
   as the returned expression.  If EXP is not a comparison, we will most
   likely not be returning a useful value and range.  */

static tree
make_range (exp, pin_p, plow, phigh)
     tree exp;
     int *pin_p;
     tree *plow, *phigh;
{
  enum tree_code code;
  tree arg0 = NULL_TREE, arg1 = NULL_TREE, type = NULL_TREE;
  tree orig_type = NULL_TREE;
  int in_p, n_in_p;
  tree low, high, n_low, n_high;

  /* Start with simply saying "EXP != 0" and then look at the code of EXP
     and see if we can refine the range.  Some of the cases below may not
     happen, but it doesn't seem worth worrying about this.  We "continue"
     the outer loop when we've changed something; otherwise we "break"
     the switch, which will "break" the while.  */

  in_p = 0, low = high = convert (TREE_TYPE (exp), integer_zero_node);

  while (1)
    {
      code = TREE_CODE (exp);

      if (IS_EXPR_CODE_CLASS (TREE_CODE_CLASS (code)))
	{
	  arg0 = TREE_OPERAND (exp, 0);
	  if (TREE_CODE_CLASS (code) == '<'
	      || TREE_CODE_CLASS (code) == '1'
	      || TREE_CODE_CLASS (code) == '2')
	    type = TREE_TYPE (arg0);
	  if (TREE_CODE_CLASS (code) == '2'
	      || TREE_CODE_CLASS (code) == '<'
	      || (TREE_CODE_CLASS (code) == 'e'
		  && TREE_CODE_LENGTH (code) > 1))
	    arg1 = TREE_OPERAND (exp, 1);
	}

      /* Set ORIG_TYPE as soon as TYPE is non-null so that we do not
	 lose a cast by accident.  */
      if (type != NULL_TREE && orig_type == NULL_TREE)
	orig_type = type;

      switch (code)
	{
	case TRUTH_NOT_EXPR:
	  in_p = ! in_p, exp = arg0;
	  continue;

	case EQ_EXPR: case NE_EXPR:
	case LT_EXPR: case LE_EXPR: case GE_EXPR: case GT_EXPR:
	  /* We can only do something if the range is testing for zero
	     and if the second operand is an integer constant.  Note that
	     saying something is "in" the range we make is done by
	     complementing IN_P since it will set in the initial case of
	     being not equal to zero; "out" is leaving it alone.  */
	  if (low == 0 || high == 0
	      || ! integer_zerop (low) || ! integer_zerop (high)
	      || TREE_CODE (arg1) != INTEGER_CST)
	    break;

	  switch (code)
	    {
	    case NE_EXPR:  /* - [c, c]  */
	      low = high = arg1;
	      break;
	    case EQ_EXPR:  /* + [c, c]  */
	      in_p = ! in_p, low = high = arg1;
	      break;
	    case GT_EXPR:  /* - [-, c] */
	      low = 0, high = arg1;
	      break;
	    case GE_EXPR:  /* + [c, -] */
	      in_p = ! in_p, low = arg1, high = 0;
	      break;
	    case LT_EXPR:  /* - [c, -] */
	      low = arg1, high = 0;
	      break;
	    case LE_EXPR:  /* + [-, c] */
	      in_p = ! in_p, low = 0, high = arg1;
	      break;
	    default:
	      abort ();
	    }

	  exp = arg0;

	  /* If this is an unsigned comparison, we also know that EXP is
	     greater than or equal to zero.  We base the range tests we make
	     on that fact, so we record it here so we can parse existing
	     range tests.  */
	  if (TREE_UNSIGNED (type) && (low == 0 || high == 0))
	    {
	      if (! merge_ranges (&n_in_p, &n_low, &n_high, in_p, low, high,
				  1, convert (type, integer_zero_node),
				  NULL_TREE))
		break;

	      in_p = n_in_p, low = n_low, high = n_high;

	      /* If the high bound is missing, but we
		 have a low bound, reverse the range so
		 it goes from zero to the low bound minus 1.  */
	      if (high == 0 && low)
		{
		  in_p = ! in_p;
		  high = range_binop (MINUS_EXPR, NULL_TREE, low, 0,
				      integer_one_node, 0);
		  low = convert (type, integer_zero_node);
		}
	    }
	  continue;

	case NEGATE_EXPR:
	  /* (-x) IN [a,b] -> x in [-b, -a]  */
	  n_low = range_binop (MINUS_EXPR, type,
			       convert (type, integer_zero_node), 0, high, 1);
	  n_high = range_binop (MINUS_EXPR, type,
				convert (type, integer_zero_node), 0, low, 0);
	  low = n_low, high = n_high;
	  exp = arg0;
	  continue;

	case BIT_NOT_EXPR:
	  /* ~ X -> -X - 1  */
	  exp = build (MINUS_EXPR, type, negate_expr (arg0),
		       convert (type, integer_one_node));
	  continue;

	case PLUS_EXPR:  case MINUS_EXPR:
	  if (TREE_CODE (arg1) != INTEGER_CST)
	    break;

	  /* If EXP is signed, any overflow in the computation is undefined,
	     so we don't worry about it so long as our computations on
	     the bounds don't overflow.  For unsigned, overflow is defined
	     and this is exactly the right thing.  */
	  n_low = range_binop (code == MINUS_EXPR ? PLUS_EXPR : MINUS_EXPR,
			       type, low, 0, arg1, 0);
	  n_high = range_binop (code == MINUS_EXPR ? PLUS_EXPR : MINUS_EXPR,
				type, high, 1, arg1, 0);
	  if ((n_low != 0 && TREE_OVERFLOW (n_low))
	      || (n_high != 0 && TREE_OVERFLOW (n_high)))
	    break;

	  /* Check for an unsigned range which has wrapped around the maximum
	     value thus making n_high < n_low, and normalize it.  */
	  if (n_low && n_high && tree_int_cst_lt (n_high, n_low))
	    {
	      low = range_binop (PLUS_EXPR, type, n_high, 0,
				 integer_one_node, 0);
	      high = range_binop (MINUS_EXPR, type, n_low, 0,
				  integer_one_node, 0);

	      /* If the range is of the form +/- [ x+1, x ], we won't
		 be able to normalize it.  But then, it represents the
		 whole range or the empty set, so make it
		 +/- [ -, - ].  */
	      if (tree_int_cst_equal (n_low, low)
		  && tree_int_cst_equal (n_high, high))
		low = high = 0;
	      else
		in_p = ! in_p;
	    }
	  else
	    low = n_low, high = n_high;

	  exp = arg0;
	  continue;

	case NOP_EXPR:  case NON_LVALUE_EXPR:  case CONVERT_EXPR:
	  if (TYPE_PRECISION (type) > TYPE_PRECISION (orig_type))
	    break;

	  if (! INTEGRAL_TYPE_P (type)
	      || (low != 0 && ! int_fits_type_p (low, type))
	      || (high != 0 && ! int_fits_type_p (high, type)))
	    break;

	  n_low = low, n_high = high;

	  if (n_low != 0)
	    n_low = convert (type, n_low);

	  if (n_high != 0)
	    n_high = convert (type, n_high);

	  /* If we're converting from an unsigned to a signed type,
	     we will be doing the comparison as unsigned.  The tests above
	     have already verified that LOW and HIGH are both positive.

	     So we have to make sure that the original unsigned value will
	     be interpreted as positive.  */
	  if (TREE_UNSIGNED (type) && ! TREE_UNSIGNED (TREE_TYPE (exp)))
	    {
	      tree equiv_type = type_for_mode (TYPE_MODE (type), 1);
	      tree high_positive;

	      /* A range without an upper bound is, naturally, unbounded.
		 Since convert would have cropped a very large value, use
		 the max value for the destination type.  */
	      high_positive
		= TYPE_MAX_VALUE (equiv_type) ? TYPE_MAX_VALUE (equiv_type)
		  : TYPE_MAX_VALUE (type);

	      high_positive = fold (build (RSHIFT_EXPR, type,
					   convert (type, high_positive),
					   convert (type, integer_one_node)));

	      /* If the low bound is specified, "and" the range with the
		 range for which the original unsigned value will be
		 positive.  */
	      if (low != 0)
		{
		  if (! merge_ranges (&n_in_p, &n_low, &n_high,
				      1, n_low, n_high,
				      1, convert (type, integer_zero_node),
				      high_positive))
		    break;

		  in_p = (n_in_p == in_p);
		}
	      else
		{
		  /* Otherwise, "or" the range with the range of the input
		     that will be interpreted as negative.  */
		  if (! merge_ranges (&n_in_p, &n_low, &n_high,
				      0, n_low, n_high,
				      1, convert (type, integer_zero_node),
				      high_positive))
		    break;

		  in_p = (in_p != n_in_p);
		}
	    }

	  exp = arg0;
	  low = n_low, high = n_high;
	  continue;

	default:
	  break;
	}

      break;
    }

  /* If EXP is a constant, we can evaluate whether this is true or false.  */
  if (TREE_CODE (exp) == INTEGER_CST)
    {
      in_p = in_p == (integer_onep (range_binop (GE_EXPR, integer_type_node,
						 exp, 0, low, 0))
		      && integer_onep (range_binop (LE_EXPR, integer_type_node,
						    exp, 1, high, 1)));
      low = high = 0;
      exp = 0;
    }

  *pin_p = in_p, *plow = low, *phigh = high;
  return exp;
}

/* Given a range, LOW, HIGH, and IN_P, an expression, EXP, and a result
   type, TYPE, return an expression to test if EXP is in (or out of, depending
   on IN_P) the range.  */

static tree
build_range_check (type, exp, in_p, low, high)
     tree type;
     tree exp;
     int in_p;
     tree low, high;
{
  tree etype = TREE_TYPE (exp);
  tree utype, value;

  if (! in_p
      && (0 != (value = build_range_check (type, exp, 1, low, high))))
    return invert_truthvalue (value);

  else if (low == 0 && high == 0)
    return convert (type, integer_one_node);

  else if (low == 0)
    return fold (build (LE_EXPR, type, exp, high));

  else if (high == 0)
    return fold (build (GE_EXPR, type, exp, low));

  else if (operand_equal_p (low, high, 0))
    return fold (build (EQ_EXPR, type, exp, low));

  else if (TREE_UNSIGNED (etype) && integer_zerop (low))
    return build_range_check (type, exp, 1, 0, high);

  else if (integer_zerop (low))
    {
      utype = unsigned_type (etype);
      return build_range_check (type, convert (utype, exp), 1, 0,
				convert (utype, high));
    }

  else if (0 != (value = const_binop (MINUS_EXPR, high, low, 0))
	   && ! TREE_OVERFLOW (value))
    return build_range_check (type,
			      fold (build (MINUS_EXPR, etype, exp, low)),
			      1, convert (etype, integer_zero_node), value);
  else
    return 0;
}

/* Given two ranges, see if we can merge them into one.  Return 1 if we
   can, 0 if we can't.  Set the output range into the specified parameters.  */

static int
merge_ranges (pin_p, plow, phigh, in0_p, low0, high0, in1_p, low1, high1)
     int *pin_p;
     tree *plow, *phigh;
     int in0_p, in1_p;
     tree low0, high0, low1, high1;
{
  int no_overlap;
  int subset;
  int temp;
  tree tem;
  int in_p;
  tree low, high;
  int lowequal = ((low0 == 0 && low1 == 0)
		  || integer_onep (range_binop (EQ_EXPR, integer_type_node,
						low0, 0, low1, 0)));
  int highequal = ((high0 == 0 && high1 == 0)
		   || integer_onep (range_binop (EQ_EXPR, integer_type_node,
						 high0, 1, high1, 1)));

  /* Make range 0 be the range that starts first, or ends last if they
     start at the same value.  Swap them if it isn't.  */
  if (integer_onep (range_binop (GT_EXPR, integer_type_node,
				 low0, 0, low1, 0))
      || (lowequal
	  && integer_onep (range_binop (GT_EXPR, integer_type_node,
					high1, 1, high0, 1))))
    {
      temp = in0_p, in0_p = in1_p, in1_p = temp;
      tem = low0, low0 = low1, low1 = tem;
      tem = high0, high0 = high1, high1 = tem;
    }

  /* Now flag two cases, whether the ranges are disjoint or whether the
     second range is totally subsumed in the first.  Note that the tests
     below are simplified by the ones above.  */
  no_overlap = integer_onep (range_binop (LT_EXPR, integer_type_node,
					  high0, 1, low1, 0));
  subset = integer_onep (range_binop (LE_EXPR, integer_type_node,
				      high1, 1, high0, 1));

  /* We now have four cases, depending on whether we are including or
     excluding the two ranges.  */
  if (in0_p && in1_p)
    {
      /* If they don't overlap, the result is false.  If the second range
	 is a subset it is the result.  Otherwise, the range is from the start
	 of the second to the end of the first.  */
      if (no_overlap)
	in_p = 0, low = high = 0;
      else if (subset)
	in_p = 1, low = low1, high = high1;
      else
	in_p = 1, low = low1, high = high0;
    }

  else if (in0_p && ! in1_p)
    {
      /* If they don't overlap, the result is the first range.  If they are
	 equal, the result is false.  If the second range is a subset of the
	 first, and the ranges begin at the same place, we go from just after
	 the end of the first range to the end of the second.  If the second
	 range is not a subset of the first, or if it is a subset and both
	 ranges end at the same place, the range starts at the start of the
	 first range and ends just before the second range.
	 Otherwise, we can't describe this as a single range.  */
      if (no_overlap)
	in_p = 1, low = low0, high = high0;
      else if (lowequal && highequal)
	in_p = 0, low = high = 0;
      else if (subset && lowequal)
	{
	  in_p = 1, high = high0;
	  low = range_binop (PLUS_EXPR, NULL_TREE, high1, 0,
			     integer_one_node, 0);
	}
      else if (! subset || highequal)
	{
	  in_p = 1, low = low0;
	  high = range_binop (MINUS_EXPR, NULL_TREE, low1, 0,
			      integer_one_node, 0);
	}
      else
	return 0;
    }

  else if (! in0_p && in1_p)
    {
      /* If they don't overlap, the result is the second range.  If the second
	 is a subset of the first, the result is false.  Otherwise,
	 the range starts just after the first range and ends at the
	 end of the second.  */
      if (no_overlap)
	in_p = 1, low = low1, high = high1;
      else if (subset || highequal)
	in_p = 0, low = high = 0;
      else
	{
	  in_p = 1, high = high1;
	  low = range_binop (PLUS_EXPR, NULL_TREE, high0, 1,
			     integer_one_node, 0);
	}
    }

  else
    {
      /* The case where we are excluding both ranges.  Here the complex case
	 is if they don't overlap.  In that case, the only time we have a
	 range is if they are adjacent.  If the second is a subset of the
	 first, the result is the first.  Otherwise, the range to exclude
	 starts at the beginning of the first range and ends at the end of the
	 second.  */
      if (no_overlap)
	{
	  if (integer_onep (range_binop (EQ_EXPR, integer_type_node,
					 range_binop (PLUS_EXPR, NULL_TREE,
						      high0, 1,
						      integer_one_node, 1),
					 1, low1, 0)))
	    in_p = 0, low = low0, high = high1;
	  else
	    return 0;
	}
      else if (subset)
	in_p = 0, low = low0, high = high0;
      else
	in_p = 0, low = low0, high = high1;
    }

  *pin_p = in_p, *plow = low, *phigh = high;
  return 1;
}

/* EXP is some logical combination of boolean tests.  See if we can
   merge it into some range test.  Return the new tree if so.  */

static tree
fold_range_test (exp)
     tree exp;
{
  int or_op = (TREE_CODE (exp) == TRUTH_ORIF_EXPR
	       || TREE_CODE (exp) == TRUTH_OR_EXPR);
  int in0_p, in1_p, in_p;
  tree low0, low1, low, high0, high1, high;
  tree lhs = make_range (TREE_OPERAND (exp, 0), &in0_p, &low0, &high0);
  tree rhs = make_range (TREE_OPERAND (exp, 1), &in1_p, &low1, &high1);
  tree tem;

  /* If this is an OR operation, invert both sides; we will invert
     again at the end.  */
  if (or_op)
    in0_p = ! in0_p, in1_p = ! in1_p;

  /* If both expressions are the same, if we can merge the ranges, and we
     can build the range test, return it or it inverted.  If one of the
     ranges is always true or always false, consider it to be the same
     expression as the other.  */
  if ((lhs == 0 || rhs == 0 || operand_equal_p (lhs, rhs, 0))
      && merge_ranges (&in_p, &low, &high, in0_p, low0, high0,
		       in1_p, low1, high1)
      && 0 != (tem = (build_range_check (TREE_TYPE (exp),
					 lhs != 0 ? lhs
					 : rhs != 0 ? rhs : integer_zero_node,
					 in_p, low, high))))
    return or_op ? invert_truthvalue (tem) : tem;

  /* On machines where the branch cost is expensive, if this is a
     short-circuited branch and the underlying object on both sides
     is the same, make a non-short-circuit operation.  */
  else if (BRANCH_COST >= 2
	   && lhs != 0 && rhs != 0
	   && (TREE_CODE (exp) == TRUTH_ANDIF_EXPR
	       || TREE_CODE (exp) == TRUTH_ORIF_EXPR)
	   && operand_equal_p (lhs, rhs, 0))
    {
      /* If simple enough, just rewrite.  Otherwise, make a SAVE_EXPR
	 unless we are at top level or LHS contains a PLACEHOLDER_EXPR, in
	 which cases we can't do this.  */
      if (simple_operand_p (lhs))
	return build (TREE_CODE (exp) == TRUTH_ANDIF_EXPR
		      ? TRUTH_AND_EXPR : TRUTH_OR_EXPR,
		      TREE_TYPE (exp), TREE_OPERAND (exp, 0),
		      TREE_OPERAND (exp, 1));

      else if (global_bindings_p () == 0
	       && ! contains_placeholder_p (lhs))
	{
	  tree common = save_expr (lhs);

	  if (0 != (lhs = build_range_check (TREE_TYPE (exp), common,
					     or_op ? ! in0_p : in0_p,
					     low0, high0))
	      && (0 != (rhs = build_range_check (TREE_TYPE (exp), common,
						 or_op ? ! in1_p : in1_p,
						 low1, high1))))
	    return build (TREE_CODE (exp) == TRUTH_ANDIF_EXPR
			  ? TRUTH_AND_EXPR : TRUTH_OR_EXPR,
			  TREE_TYPE (exp), lhs, rhs);
	}
    }

  return 0;
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

  /* We work by getting just the sign bit into the low-order bit, then
     into the high-order bit, then sign-extend.  We then XOR that value
     with C.  */
  temp = const_binop (RSHIFT_EXPR, c, size_int (p - 1), 0);
  temp = const_binop (BIT_AND_EXPR, temp, size_int (1), 0);

  /* We must use a signed type in order to get an arithmetic right shift.
     However, we must also avoid introducing accidental overflows, so that
     a subsequent call to integer_zerop will work.  Hence we must
     do the type conversion here.  At this point, the constant is either
     zero or one, and the conversion to a signed type can never overflow.
     We could get an overflow if this conversion is done anywhere else.  */
  if (TREE_UNSIGNED (type))
    temp = convert (signed_type (type), temp);

  temp = const_binop (LSHIFT_EXPR, temp, size_int (modesize - 1), 0);
  temp = const_binop (RSHIFT_EXPR, temp, size_int (modesize - p - 1), 0);
  if (mask != 0)
    temp = const_binop (BIT_AND_EXPR, temp, convert (TREE_TYPE (c), mask), 0);
  /* If necessary, convert the type back to match the type of C.  */
  if (TREE_UNSIGNED (type))
    temp = convert (type, temp);

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
  /* If this is the "or" of two comparisons, we can do something if
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
  HOST_WIDE_INT ll_bitsize, ll_bitpos, lr_bitsize, lr_bitpos;
  HOST_WIDE_INT rl_bitsize, rl_bitpos, rr_bitsize, rr_bitpos;
  HOST_WIDE_INT xll_bitpos, xlr_bitpos, xrl_bitpos, xrr_bitpos;
  HOST_WIDE_INT lnbitsize, lnbitpos, rnbitsize, rnbitpos;
  int ll_unsignedp, lr_unsignedp, rl_unsignedp, rr_unsignedp;
  enum machine_mode ll_mode, lr_mode, rl_mode, rr_mode;
  enum machine_mode lnmode, rnmode;
  tree ll_mask, lr_mask, rl_mask, rr_mask;
  tree ll_and_mask, lr_and_mask, rl_and_mask, rr_and_mask;
  tree l_const, r_const;
  tree lntype, rntype, result;
  int first_bit, end_bit;
  int volatilep;

  /* Start by getting the comparison codes.  Fail if anything is volatile.
     If one operand is a BIT_AND_EXPR with the constant one, treat it as if
     it were surrounded with a NE_EXPR.  */

  if (TREE_SIDE_EFFECTS (lhs) || TREE_SIDE_EFFECTS (rhs))
    return 0;

  lcode = TREE_CODE (lhs);
  rcode = TREE_CODE (rhs);

  if (lcode == BIT_AND_EXPR && integer_onep (TREE_OPERAND (lhs, 1)))
    lcode = NE_EXPR, lhs = build (NE_EXPR, truth_type, lhs, integer_zero_node);

  if (rcode == BIT_AND_EXPR && integer_onep (TREE_OPERAND (rhs, 1)))
    rcode = NE_EXPR, rhs = build (NE_EXPR, truth_type, rhs, integer_zero_node);

  if (TREE_CODE_CLASS (lcode) != '<' || TREE_CODE_CLASS (rcode) != '<')
    return 0;

  code = ((code == TRUTH_AND_EXPR || code == TRUTH_ANDIF_EXPR)
	  ? TRUTH_AND_EXPR : TRUTH_OR_EXPR);

  ll_arg = TREE_OPERAND (lhs, 0);
  lr_arg = TREE_OPERAND (lhs, 1);
  rl_arg = TREE_OPERAND (rhs, 0);
  rr_arg = TREE_OPERAND (rhs, 1);

  /* If the RHS can be evaluated unconditionally and its operands are
     simple, it wins to evaluate the RHS unconditionally on machines
     with expensive branches.  In this case, this isn't a comparison
     that can be merged.  Avoid doing this if the RHS is a floating-point
     comparison since those can trap.  */

  if (BRANCH_COST >= 2
      && ! FLOAT_TYPE_P (TREE_TYPE (rl_arg))
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
	{
	  /* Make the left operand unsigned, since we are only interested
	     in the value of one bit.  Otherwise we are doing the wrong
	     thing below.  */
	  ll_unsignedp = 1;
	  l_const = ll_mask;
	}
      else
	return 0;
    }

  /* This is analogous to the code for l_const above.  */
  if (rcode != wanted_code)
    {
      if (r_const && integer_zerop (r_const) && integer_pow2p (rl_mask))
	{
	  rl_unsignedp = 1;
	  r_const = rl_mask;
	}
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
  lntype = type_for_size (lnbitsize, 1);
  xll_bitpos = ll_bitpos - lnbitpos, xrl_bitpos = rl_bitpos - lnbitpos;

  if (BYTES_BIG_ENDIAN)
    {
      xll_bitpos = lnbitsize - xll_bitpos - ll_bitsize;
      xrl_bitpos = lnbitsize - xrl_bitpos - rl_bitsize;
    }

  ll_mask = const_binop (LSHIFT_EXPR, convert (lntype, ll_mask),
			 size_int (xll_bitpos), 0);
  rl_mask = const_binop (LSHIFT_EXPR, convert (lntype, rl_mask),
			 size_int (xrl_bitpos), 0);

  if (l_const)
    {
      l_const = convert (lntype, l_const);
      l_const = unextend (l_const, ll_bitsize, ll_unsignedp, ll_and_mask);
      l_const = const_binop (LSHIFT_EXPR, l_const, size_int (xll_bitpos), 0);
      if (! integer_zerop (const_binop (BIT_AND_EXPR, l_const,
					fold (build1 (BIT_NOT_EXPR,
						      lntype, ll_mask)),
					0)))
	{
	  warning ("comparison is always %d", wanted_code == NE_EXPR);

	  return convert (truth_type,
			  wanted_code == NE_EXPR
			  ? integer_one_node : integer_zero_node);
	}
    }
  if (r_const)
    {
      r_const = convert (lntype, r_const);
      r_const = unextend (r_const, rl_bitsize, rl_unsignedp, rl_and_mask);
      r_const = const_binop (LSHIFT_EXPR, r_const, size_int (xrl_bitpos), 0);
      if (! integer_zerop (const_binop (BIT_AND_EXPR, r_const,
					fold (build1 (BIT_NOT_EXPR,
						      lntype, rl_mask)),
					0)))
	{
	  warning ("comparison is always %d", wanted_code == NE_EXPR);

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
      rntype = type_for_size (rnbitsize, 1);
      xlr_bitpos = lr_bitpos - rnbitpos, xrr_bitpos = rr_bitpos - rnbitpos;

      if (BYTES_BIG_ENDIAN)
	{
	  xlr_bitpos = rnbitsize - xlr_bitpos - lr_bitsize;
	  xrr_bitpos = rnbitsize - xrr_bitpos - rr_bitsize;
	}

      lr_mask = const_binop (LSHIFT_EXPR, convert (rntype, lr_mask),
			     size_int (xlr_bitpos), 0);
      rr_mask = const_binop (LSHIFT_EXPR, convert (rntype, rr_mask),
			     size_int (xrr_bitpos), 0);

      /* Make a mask that corresponds to both fields being compared.
	 Do this for both items being compared.  If the operands are the
	 same size and the bits being compared are in the same position
	 then we can do this by masking both and comparing the masked
	 results.  */
      ll_mask = const_binop (BIT_IOR_EXPR, ll_mask, rl_mask, 0);
      lr_mask = const_binop (BIT_IOR_EXPR, lr_mask, rr_mask, 0);
      if (lnbitsize == rnbitsize && xll_bitpos == xlr_bitpos)
	{
	  lhs = make_bit_field_ref (ll_inner, lntype, lnbitsize, lnbitpos,
				    ll_unsignedp || rl_unsignedp);
	  if (! all_ones_mask_p (ll_mask, lnbitsize))
	    lhs = build (BIT_AND_EXPR, lntype, lhs, ll_mask);

	  rhs = make_bit_field_ref (lr_inner, rntype, rnbitsize, rnbitpos,
				    lr_unsignedp || rr_unsignedp);
	  if (! all_ones_mask_p (lr_mask, rnbitsize))
	    rhs = build (BIT_AND_EXPR, rntype, rhs, lr_mask);

	  return build (wanted_code, truth_type, lhs, rhs);
	}

      /* There is still another way we can do something:  If both pairs of
	 fields being compared are adjacent, we may be able to make a wider
	 field containing them both.

	 Note that we still must mask the lhs/rhs expressions.  Furthermore,
	 the mask must be shifted to account for the shift done by
	 make_bit_field_ref.  */
      if ((ll_bitsize + ll_bitpos == rl_bitpos
	   && lr_bitsize + lr_bitpos == rr_bitpos)
	  || (ll_bitpos == rl_bitpos + rl_bitsize
	      && lr_bitpos == rr_bitpos + rr_bitsize))
	{
	  tree type;

	  lhs = make_bit_field_ref (ll_inner, lntype, ll_bitsize + rl_bitsize,
				    MIN (ll_bitpos, rl_bitpos), ll_unsignedp);
	  rhs = make_bit_field_ref (lr_inner, rntype, lr_bitsize + rr_bitsize,
				    MIN (lr_bitpos, rr_bitpos), lr_unsignedp);

	  ll_mask = const_binop (RSHIFT_EXPR, ll_mask,
				 size_int (MIN (xll_bitpos, xrl_bitpos)), 0);
	  lr_mask = const_binop (RSHIFT_EXPR, lr_mask,
				 size_int (MIN (xlr_bitpos, xrr_bitpos)), 0);

	  /* Convert to the smaller type before masking out unwanted bits.  */
	  type = lntype;
	  if (lntype != rntype)
	    {
	      if (lnbitsize > rnbitsize)
		{
		  lhs = convert (rntype, lhs);
		  ll_mask = convert (rntype, ll_mask);
		  type = rntype;
		}
	      else if (lnbitsize < rnbitsize)
		{
		  rhs = convert (lntype, rhs);
		  lr_mask = convert (lntype, lr_mask);
		  type = lntype;
		}
	    }

	  if (! all_ones_mask_p (ll_mask, ll_bitsize + rl_bitsize))
	    lhs = build (BIT_AND_EXPR, type, lhs, ll_mask);

	  if (! all_ones_mask_p (lr_mask, lr_bitsize + rr_bitsize))
	    rhs = build (BIT_AND_EXPR, type, rhs, lr_mask);

	  return build (wanted_code, truth_type, lhs, rhs);
	}

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
	  warning ("`and' of mutually exclusive equal-tests is always 0");
	  return convert (truth_type, integer_zero_node);
	}
    }

  /* Construct the expression we will return.  First get the component
     reference we will make.  Unless the mask is all ones the width of
     that field, perform the mask operation.  Then compare with the
     merged constant.  */
  result = make_bit_field_ref (ll_inner, lntype, lnbitsize, lnbitpos,
			       ll_unsignedp || rl_unsignedp);

  ll_mask = const_binop (BIT_IOR_EXPR, ll_mask, rl_mask, 0);
  if (! all_ones_mask_p (ll_mask, lnbitsize))
    result = build (BIT_AND_EXPR, lntype, result, ll_mask);

  return build (wanted_code, truth_type, result,
		const_binop (BIT_IOR_EXPR, l_const, r_const, 0));
}

/* Optimize T, which is a comparison of a MIN_EXPR or MAX_EXPR with a
   constant.  */

static tree
optimize_minmax_comparison (t)
     tree t;
{
  tree type = TREE_TYPE (t);
  tree arg0 = TREE_OPERAND (t, 0);
  enum tree_code op_code;
  tree comp_const = TREE_OPERAND (t, 1);
  tree minmax_const;
  int consts_equal, consts_lt;
  tree inner;

  STRIP_SIGN_NOPS (arg0);

  op_code = TREE_CODE (arg0);
  minmax_const = TREE_OPERAND (arg0, 1);
  consts_equal = tree_int_cst_equal (minmax_const, comp_const);
  consts_lt = tree_int_cst_lt (minmax_const, comp_const);
  inner = TREE_OPERAND (arg0, 0);

  /* If something does not permit us to optimize, return the original tree.  */
  if ((op_code != MIN_EXPR && op_code != MAX_EXPR)
      || TREE_CODE (comp_const) != INTEGER_CST
      || TREE_CONSTANT_OVERFLOW (comp_const)
      || TREE_CODE (minmax_const) != INTEGER_CST
      || TREE_CONSTANT_OVERFLOW (minmax_const))
    return t;

  /* Now handle all the various comparison codes.  We only handle EQ_EXPR
     and GT_EXPR, doing the rest with recursive calls using logical
     simplifications.  */
  switch (TREE_CODE (t))
    {
    case NE_EXPR:  case LT_EXPR:  case LE_EXPR:
      return
	invert_truthvalue (optimize_minmax_comparison (invert_truthvalue (t)));

    case GE_EXPR:
      return
	fold (build (TRUTH_ORIF_EXPR, type,
		     optimize_minmax_comparison
		     (build (EQ_EXPR, type, arg0, comp_const)),
		     optimize_minmax_comparison
		     (build (GT_EXPR, type, arg0, comp_const))));

    case EQ_EXPR:
      if (op_code == MAX_EXPR && consts_equal)
	/* MAX (X, 0) == 0  ->  X <= 0  */
	return fold (build (LE_EXPR, type, inner, comp_const));

      else if (op_code == MAX_EXPR && consts_lt)
	/* MAX (X, 0) == 5  ->  X == 5   */
	return fold (build (EQ_EXPR, type, inner, comp_const));

      else if (op_code == MAX_EXPR)
	/* MAX (X, 0) == -1  ->  false  */
	return omit_one_operand (type, integer_zero_node, inner);

      else if (consts_equal)
	/* MIN (X, 0) == 0  ->  X >= 0  */
	return fold (build (GE_EXPR, type, inner, comp_const));

      else if (consts_lt)
	/* MIN (X, 0) == 5  ->  false  */
	return omit_one_operand (type, integer_zero_node, inner);

      else
	/* MIN (X, 0) == -1  ->  X == -1  */
	return fold (build (EQ_EXPR, type, inner, comp_const));

    case GT_EXPR:
      if (op_code == MAX_EXPR && (consts_equal || consts_lt))
	/* MAX (X, 0) > 0  ->  X > 0
	   MAX (X, 0) > 5  ->  X > 5  */
	return fold (build (GT_EXPR, type, inner, comp_const));

      else if (op_code == MAX_EXPR)
	/* MAX (X, 0) > -1  ->  true  */
	return omit_one_operand (type, integer_one_node, inner);

      else if (op_code == MIN_EXPR && (consts_equal || consts_lt))
	/* MIN (X, 0) > 0  ->  false
	   MIN (X, 0) > 5  ->  false  */
	return omit_one_operand (type, integer_zero_node, inner);

      else
	/* MIN (X, 0) > -1  ->  X > -1  */
	return fold (build (GT_EXPR, type, inner, comp_const));

    default:
      return t;
    }
}

/* T is an integer expression that is being multiplied, divided, or taken a
   modulus (CODE says which and what kind of divide or modulus) by a
   constant C.  See if we can eliminate that operation by folding it with
   other operations already in T.  WIDE_TYPE, if non-null, is a type that
   should be used for the computation if wider than our type.

   For example, if we are dividing (X * 8) + (Y * 16) by 4, we can return
   (X * 2) + (Y * 4).  We must, however, be assured that either the original
   expression would not overflow or that overflow is undefined for the type
   in the language in question.

   We also canonicalize (X + 7) * 4 into X * 4 + 28 in the hope that either
   the machine has a multiply-accumulate insn or that this is part of an
   addressing calculation.

   If we return a non-null expression, it is an equivalent form of the
   original computation, but need not be in the original type.  */

static tree
extract_muldiv (t, c, code, wide_type)
     tree t;
     tree c;
     enum tree_code code;
     tree wide_type;
{
  tree type = TREE_TYPE (t);
  enum tree_code tcode = TREE_CODE (t);
  tree ctype = (wide_type != 0 && (GET_MODE_SIZE (TYPE_MODE (wide_type))
				   > GET_MODE_SIZE (TYPE_MODE (type)))
		? wide_type : type);
  tree t1, t2;
  int same_p = tcode == code;
  tree op0 = NULL_TREE, op1 = NULL_TREE;

  /* Don't deal with constants of zero here; they confuse the code below.  */
  if (integer_zerop (c))
    return NULL_TREE;

  if (TREE_CODE_CLASS (tcode) == '1')
    op0 = TREE_OPERAND (t, 0);

  if (TREE_CODE_CLASS (tcode) == '2')
    op0 = TREE_OPERAND (t, 0), op1 = TREE_OPERAND (t, 1);

  /* Note that we need not handle conditional operations here since fold
     already handles those cases.  So just do arithmetic here.  */
  switch (tcode)
    {
    case INTEGER_CST:
      /* For a constant, we can always simplify if we are a multiply
	 or (for divide and modulus) if it is a multiple of our constant.  */
      if (code == MULT_EXPR
	  || integer_zerop (const_binop (TRUNC_MOD_EXPR, t, c, 0)))
	return const_binop (code, convert (ctype, t), convert (ctype, c), 0);
      break;

    case CONVERT_EXPR:  case NON_LVALUE_EXPR:  case NOP_EXPR:
      /* If op0 is an expression... */
      if ((TREE_CODE_CLASS (TREE_CODE (op0)) == '<'
	   || TREE_CODE_CLASS (TREE_CODE (op0)) == '1'
	   || TREE_CODE_CLASS (TREE_CODE (op0)) == '2'
	   || TREE_CODE_CLASS (TREE_CODE (op0)) == 'e')
	  /* ...and is unsigned, and its type is smaller than ctype,
	     then we cannot pass through this widening.  */
	  && ((TREE_UNSIGNED (TREE_TYPE (op0))
	       && ! (TREE_CODE (TREE_TYPE (op0)) == INTEGER_TYPE
		     && TYPE_IS_SIZETYPE (TREE_TYPE (op0)))
	       && (GET_MODE_SIZE (TYPE_MODE (ctype))
		   > GET_MODE_SIZE (TYPE_MODE (TREE_TYPE (op0)))))
	      /* ...and its type is larger than ctype,
		 then we cannot pass through this truncation.  */
	      || (GET_MODE_SIZE (TYPE_MODE (ctype))
		  < GET_MODE_SIZE (TYPE_MODE (TREE_TYPE (op0))))))
	break;

      /* Pass the constant down and see if we can make a simplification.  If
	 we can, replace this expression with the inner simplification for
	 possible later conversion to our or some other type.  */
      if (0 != (t1 = extract_muldiv (op0, convert (TREE_TYPE (op0), c), code,
				     code == MULT_EXPR ? ctype : NULL_TREE)))
	return t1;
      break;

    case NEGATE_EXPR:  case ABS_EXPR:
      if ((t1 = extract_muldiv (op0, c, code, wide_type)) != 0)
	return fold (build1 (tcode, ctype, convert (ctype, t1)));
      break;

    case MIN_EXPR:  case MAX_EXPR:
      /* If widening the type changes the signedness, then we can't perform
	 this optimization as that changes the result.  */
      if (TREE_UNSIGNED (ctype) != TREE_UNSIGNED (type))
	break;

      /* MIN (a, b) / 5 -> MIN (a / 5, b / 5)  */
      if ((t1 = extract_muldiv (op0, c, code, wide_type)) != 0
	  && (t2 = extract_muldiv (op1, c, code, wide_type)) != 0)
	{
	  if (tree_int_cst_sgn (c) < 0)
	    tcode = (tcode == MIN_EXPR ? MAX_EXPR : MIN_EXPR);

	  return fold (build (tcode, ctype, convert (ctype, t1),
			      convert (ctype, t2)));
	}
      break;

    case WITH_RECORD_EXPR:
      if ((t1 = extract_muldiv (TREE_OPERAND (t, 0), c, code, wide_type)) != 0)
	return build (WITH_RECORD_EXPR, TREE_TYPE (t1), t1,
		      TREE_OPERAND (t, 1));
      break;

    case SAVE_EXPR:
      /* If this has not been evaluated and the operand has no side effects,
	 we can see if we can do something inside it and make a new one.
	 Note that this test is overly conservative since we can do this
	 if the only reason it had side effects is that it was another
	 similar SAVE_EXPR, but that isn't worth bothering with.  */
      if (SAVE_EXPR_RTL (t) == 0 && ! TREE_SIDE_EFFECTS (TREE_OPERAND (t, 0))
	  && 0 != (t1 = extract_muldiv (TREE_OPERAND (t, 0), c, code,
					wide_type)))
	{
	  t1 = save_expr (t1);
	  if (SAVE_EXPR_PERSISTENT_P (t) && TREE_CODE (t1) == SAVE_EXPR)
	    SAVE_EXPR_PERSISTENT_P (t1) = 1;
	  if (is_pending_size (t))
	    put_pending_size (t1);
	  return t1;
	}
      break;

    case LSHIFT_EXPR:  case RSHIFT_EXPR:
      /* If the second operand is constant, this is a multiplication
	 or floor division, by a power of two, so we can treat it that
	 way unless the multiplier or divisor overflows.  */
      if (TREE_CODE (op1) == INTEGER_CST
	  /* const_binop may not detect overflow correctly,
	     so check for it explicitly here.  */
	  && TYPE_PRECISION (TREE_TYPE (size_one_node)) > TREE_INT_CST_LOW (op1)
	  && TREE_INT_CST_HIGH (op1) == 0
	  && 0 != (t1 = convert (ctype,
				 const_binop (LSHIFT_EXPR, size_one_node,
					      op1, 0)))
	  && ! TREE_OVERFLOW (t1))
	return extract_muldiv (build (tcode == LSHIFT_EXPR
				      ? MULT_EXPR : FLOOR_DIV_EXPR,
				      ctype, convert (ctype, op0), t1),
			       c, code, wide_type);
      break;

    case PLUS_EXPR:  case MINUS_EXPR:
      /* See if we can eliminate the operation on both sides.  If we can, we
	 can return a new PLUS or MINUS.  If we can't, the only remaining
	 cases where we can do anything are if the second operand is a
	 constant.  */
      t1 = extract_muldiv (op0, c, code, wide_type);
      t2 = extract_muldiv (op1, c, code, wide_type);
      if (t1 != 0 && t2 != 0
	  && (code == MULT_EXPR
	      /* If not multiplication, we can only do this if either operand
		 is divisible by c.  */
	      || multiple_of_p (ctype, op0, c)
	      || multiple_of_p (ctype, op1, c)))
	return fold (build (tcode, ctype, convert (ctype, t1),
			    convert (ctype, t2)));

      /* If this was a subtraction, negate OP1 and set it to be an addition.
	 This simplifies the logic below.  */
      if (tcode == MINUS_EXPR)
	tcode = PLUS_EXPR, op1 = negate_expr (op1);

      if (TREE_CODE (op1) != INTEGER_CST)
	break;

      /* If either OP1 or C are negative, this optimization is not safe for
	 some of the division and remainder types while for others we need
	 to change the code.  */
      if (tree_int_cst_sgn (op1) < 0 || tree_int_cst_sgn (c) < 0)
	{
	  if (code == CEIL_DIV_EXPR)
	    code = FLOOR_DIV_EXPR;
	  else if (code == FLOOR_DIV_EXPR)
	    code = CEIL_DIV_EXPR;
	  else if (code != MULT_EXPR
		   && code != CEIL_MOD_EXPR && code != FLOOR_MOD_EXPR)
	    break;
	}

      /* If it's a multiply or a division/modulus operation of a multiple
         of our constant, do the operation and verify it doesn't overflow.  */
      if (code == MULT_EXPR
	  || integer_zerop (const_binop (TRUNC_MOD_EXPR, op1, c, 0)))
        {
          op1 = const_binop (code, convert (ctype, op1), convert (ctype, c), 0);
          if (op1 == 0 || TREE_OVERFLOW (op1))
            break;
        }
      else
        break;

      /* If we have an unsigned type is not a sizetype, we cannot widen
	 the operation since it will change the result if the original
	 computation overflowed.  */
      if (TREE_UNSIGNED (ctype)
	  && ! (TREE_CODE (ctype) == INTEGER_TYPE && TYPE_IS_SIZETYPE (ctype))
	  && ctype != type)
	break;

      /* If we were able to eliminate our operation from the first side,
	 apply our operation to the second side and reform the PLUS.  */
      if (t1 != 0 && (TREE_CODE (t1) != code || code == MULT_EXPR))
	return fold (build (tcode, ctype, convert (ctype, t1), op1));

      /* The last case is if we are a multiply.  In that case, we can
	 apply the distributive law to commute the multiply and addition
	 if the multiplication of the constants doesn't overflow.  */
      if (code == MULT_EXPR)
	return fold (build (tcode, ctype, fold (build (code, ctype,
						       convert (ctype, op0),
						       convert (ctype, c))),
			    op1));

      break;

    case MULT_EXPR:
      /* We have a special case here if we are doing something like
	 (C * 8) % 4 since we know that's zero.  */
      if ((code == TRUNC_MOD_EXPR || code == CEIL_MOD_EXPR
	   || code == FLOOR_MOD_EXPR || code == ROUND_MOD_EXPR)
	  && TREE_CODE (TREE_OPERAND (t, 1)) == INTEGER_CST
	  && integer_zerop (const_binop (TRUNC_MOD_EXPR, op1, c, 0)))
	return omit_one_operand (type, integer_zero_node, op0);

      /* ... fall through ...  */

    case TRUNC_DIV_EXPR:  case CEIL_DIV_EXPR:  case FLOOR_DIV_EXPR:
    case ROUND_DIV_EXPR:  case EXACT_DIV_EXPR:
      /* If we can extract our operation from the LHS, do so and return a
	 new operation.  Likewise for the RHS from a MULT_EXPR.  Otherwise,
	 do something only if the second operand is a constant.  */
      if (same_p
	  && (t1 = extract_muldiv (op0, c, code, wide_type)) != 0)
	return fold (build (tcode, ctype, convert (ctype, t1),
			    convert (ctype, op1)));
      else if (tcode == MULT_EXPR && code == MULT_EXPR
	       && (t1 = extract_muldiv (op1, c, code, wide_type)) != 0)
	return fold (build (tcode, ctype, convert (ctype, op0),
			    convert (ctype, t1)));
      else if (TREE_CODE (op1) != INTEGER_CST)
	return 0;

      /* If these are the same operation types, we can associate them
	 assuming no overflow.  */
      if (tcode == code
	  && 0 != (t1 = const_binop (MULT_EXPR, convert (ctype, op1),
				     convert (ctype, c), 0))
	  && ! TREE_OVERFLOW (t1))
	return fold (build (tcode, ctype, convert (ctype, op0), t1));

      /* If these operations "cancel" each other, we have the main
	 optimizations of this pass, which occur when either constant is a
	 multiple of the other, in which case we replace this with either an
	 operation or CODE or TCODE.

	 If we have an unsigned type that is not a sizetype, we cannot do
	 this since it will change the result if the original computation
	 overflowed.  */
      if ((! TREE_UNSIGNED (ctype)
	   || (TREE_CODE (ctype) == INTEGER_TYPE && TYPE_IS_SIZETYPE (ctype)))
	  && ((code == MULT_EXPR && tcode == EXACT_DIV_EXPR)
	      || (tcode == MULT_EXPR
		  && code != TRUNC_MOD_EXPR && code != CEIL_MOD_EXPR
		  && code != FLOOR_MOD_EXPR && code != ROUND_MOD_EXPR)))
	{
	  if (integer_zerop (const_binop (TRUNC_MOD_EXPR, op1, c, 0)))
	    return fold (build (tcode, ctype, convert (ctype, op0),
				convert (ctype,
					 const_binop (TRUNC_DIV_EXPR,
						      op1, c, 0))));
	  else if (integer_zerop (const_binop (TRUNC_MOD_EXPR, c, op1, 0)))
	    return fold (build (code, ctype, convert (ctype, op0),
				convert (ctype,
					 const_binop (TRUNC_DIV_EXPR,
						      c, op1, 0))));
	}
      break;

    default:
      break;
    }

  return 0;
}

/* If T contains a COMPOUND_EXPR which was inserted merely to evaluate
   S, a SAVE_EXPR, return the expression actually being evaluated.   Note
   that we may sometimes modify the tree.  */

static tree
strip_compound_expr (t, s)
     tree t;
     tree s;
{
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

/* Return a node which has the indicated constant VALUE (either 0 or
   1), and is of the indicated TYPE.  */

static tree
constant_boolean_node (value, type)
     int value;
     tree type;
{
  if (type == integer_type_node)
    return value ? integer_one_node : integer_zero_node;
  else if (TREE_CODE (type) == BOOLEAN_TYPE)
    return truthvalue_conversion (value ? integer_one_node :
				  integer_zero_node);
  else
    {
      tree t = build_int_2 (value, 0);

      TREE_TYPE (t) = type;
      return t;
    }
}

/* Utility function for the following routine, to see how complex a nesting of
   COND_EXPRs can be.  EXPR is the expression and LIMIT is a count beyond which
   we don't care (to avoid spending too much time on complex expressions.).  */

static int
count_cond (expr, lim)
     tree expr;
     int lim;
{
  int ctrue, cfalse;

  if (TREE_CODE (expr) != COND_EXPR)
    return 0;
  else if (lim <= 0)
    return 0;

  ctrue = count_cond (TREE_OPERAND (expr, 1), lim - 1);
  cfalse = count_cond (TREE_OPERAND (expr, 2), lim - 1 - ctrue);
  return MIN (lim, 1 + ctrue + cfalse);
}

/* Transform `a + (b ? x : y)' into `x ? (a + b) : (a + y)'.
   Transform, `a + (x < y)' into `(x < y) ? (a + 1) : (a + 0)'.  Here
   CODE corresponds to the `+', COND to the `(b ? x : y)' or `(x < y)'
   expression, and ARG to `a'.  If COND_FIRST_P is non-zero, then the
   COND is the first argument to CODE; otherwise (as in the example
   given here), it is the second argument.  TYPE is the type of the
   original expression.  */

static tree
fold_binary_op_with_conditional_arg (code, type, cond, arg, cond_first_p)
     enum tree_code code;
     tree type;
     tree cond;
     tree arg;
     int cond_first_p;
{
  tree test, true_value, false_value;
  tree lhs = NULL_TREE;
  tree rhs = NULL_TREE;
  /* In the end, we'll produce a COND_EXPR.  Both arms of the
     conditional expression will be binary operations.  The left-hand
     side of the expression to be executed if the condition is true
     will be pointed to by TRUE_LHS.  Similarly, the right-hand side
     of the expression to be executed if the condition is true will be
     pointed to by TRUE_RHS.  FALSE_LHS and FALSE_RHS are analogous --
     but apply to the expression to be executed if the conditional is
     false.  */
  tree *true_lhs;
  tree *true_rhs;
  tree *false_lhs;
  tree *false_rhs;
  /* These are the codes to use for the left-hand side and right-hand
     side of the COND_EXPR.  Normally, they are the same as CODE.  */
  enum tree_code lhs_code = code;
  enum tree_code rhs_code = code;
  /* And these are the types of the expressions.  */
  tree lhs_type = type;
  tree rhs_type = type;

  if (cond_first_p)
    {
      true_rhs = false_rhs = &arg;
      true_lhs = &true_value;
      false_lhs = &false_value;
    }
  else
    {
      true_lhs = false_lhs = &arg;
      true_rhs = &true_value;
      false_rhs = &false_value;
    }

  if (TREE_CODE (cond) == COND_EXPR)
    {
      test = TREE_OPERAND (cond, 0);
      true_value = TREE_OPERAND (cond, 1);
      false_value = TREE_OPERAND (cond, 2);
      /* If this operand throws an expression, then it does not make
	 sense to try to perform a logical or arithmetic operation
	 involving it.  Instead of building `a + throw 3' for example,
	 we simply build `a, throw 3'.  */
      if (VOID_TYPE_P (TREE_TYPE (true_value)))
	{
	  lhs_code = COMPOUND_EXPR;
	  if (!cond_first_p)
	    lhs_type = void_type_node;
	}
      if (VOID_TYPE_P (TREE_TYPE (false_value)))
	{
	  rhs_code = COMPOUND_EXPR;
	  if (!cond_first_p)
	    rhs_type = void_type_node;
	}
    }
  else
    {
      tree testtype = TREE_TYPE (cond);
      test = cond;
      true_value = convert (testtype, integer_one_node);
      false_value = convert (testtype, integer_zero_node);
    }
  
  /* If ARG is complex we want to make sure we only evaluate
     it once.  Though this is only required if it is volatile, it
     might be more efficient even if it is not.  However, if we
     succeed in folding one part to a constant, we do not need
     to make this SAVE_EXPR.  Since we do this optimization
     primarily to see if we do end up with constant and this
     SAVE_EXPR interferes with later optimizations, suppressing
     it when we can is important.
     
     If we are not in a function, we can't make a SAVE_EXPR, so don't
     try to do so.  Don't try to see if the result is a constant
     if an arm is a COND_EXPR since we get exponential behavior
     in that case.  */
  
  if (TREE_CODE (arg) != SAVE_EXPR && ! TREE_CONSTANT (arg)
      && global_bindings_p () == 0
      && ((TREE_CODE (arg) != VAR_DECL
	   && TREE_CODE (arg) != PARM_DECL)
	  || TREE_SIDE_EFFECTS (arg)))
    {
      if (TREE_CODE (true_value) != COND_EXPR)
	lhs = fold (build (lhs_code, lhs_type, *true_lhs, *true_rhs));
      
      if (TREE_CODE (false_value) != COND_EXPR)
	rhs = fold (build (rhs_code, rhs_type, *false_lhs, *false_rhs));
      
      if ((lhs == 0 || ! TREE_CONSTANT (lhs))
	  && (rhs == 0 || !TREE_CONSTANT (rhs)))
	arg = save_expr (arg), lhs = rhs = 0;
    }
  
  if (lhs == 0)
    lhs = fold (build (lhs_code, lhs_type, *true_lhs, *true_rhs));
  if (rhs == 0)
    rhs = fold (build (rhs_code, rhs_type, *false_lhs, *false_rhs));
  
  test = fold (build (COND_EXPR, type, test, lhs, rhs));
  
  if (TREE_CODE (arg) == SAVE_EXPR)
    return build (COMPOUND_EXPR, type,
		  convert (void_type_node, arg),
		  strip_compound_expr (test, arg));
  else
    return convert (type, test);
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
  tree t = expr;
  tree t1 = NULL_TREE;
  tree tem;
  tree type = TREE_TYPE (expr);
  tree arg0 = NULL_TREE, arg1 = NULL_TREE;
  enum tree_code code = TREE_CODE (t);
  int kind = TREE_CODE_CLASS (code);
  int invert;
  /* WINS will be nonzero when the switch is done
     if all operands are constant.  */
  int wins = 1;

  /* Don't try to process an RTL_EXPR since its operands aren't trees.
     Likewise for a SAVE_EXPR that's already been evaluated.  */
  if (code == RTL_EXPR || (code == SAVE_EXPR && SAVE_EXPR_RTL (t) != 0))
    return t;

  /* Return right away if a constant.  */
  if (kind == 'c')
    return t;

#ifdef MAX_INTEGER_COMPUTATION_MODE
  check_max_integer_computation_mode (expr);
#endif

  if (code == NOP_EXPR || code == FLOAT_EXPR || code == CONVERT_EXPR)
    {
      tree subop;

      /* Special case for conversion ops that can have fixed point args.  */
      arg0 = TREE_OPERAND (t, 0);

      /* Don't use STRIP_NOPS, because signedness of argument type matters.  */
      if (arg0 != 0)
	STRIP_SIGN_NOPS (arg0);

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
  else if (IS_EXPR_CODE_CLASS (kind) || kind == 'r')
    {
      int len = first_rtl_op (code);
      int i;
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
	      STRIP_SIGN_NOPS (op);
	    }
	  else
	    /* Strip any conversions that don't change the mode.  */
	    STRIP_NOPS (op);

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

     Before we do that, see if this is a BIT_AND_EXPR or a BIT_IOR_EXPR,
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
		    && (INTEGRAL_TYPE_P
			(TREE_TYPE (TREE_OPERAND (TREE_OPERAND (t, 1), 0))))
		    && TYPE_PRECISION (TREE_TYPE (t)) <= BITS_PER_WORD))
	    t = build1 (code, type,
			build (COND_EXPR,
			       TREE_TYPE (TREE_OPERAND
					  (TREE_OPERAND (t, 1), 0)),
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
      else if ((TREE_CODE (arg1) == COND_EXPR
		|| (TREE_CODE_CLASS (TREE_CODE (arg1)) == '<'
		    && TREE_CODE_CLASS (code) != '<'))
	       && (TREE_CODE (arg0) != COND_EXPR
		   || count_cond (arg0, 25) + count_cond (arg1, 25) <= 25)
	       && (! TREE_SIDE_EFFECTS (arg0)
		   || (global_bindings_p () == 0
		       && ! contains_placeholder_p (arg0))))
	return 
	  fold_binary_op_with_conditional_arg (code, type, arg1, arg0,
					       /*cond_first_p=*/0);
      else if (TREE_CODE (arg0) == COMPOUND_EXPR)
	return build (COMPOUND_EXPR, type, TREE_OPERAND (arg0, 0),
		      fold (build (code, type, TREE_OPERAND (arg0, 1), arg1)));
      else if ((TREE_CODE (arg0) == COND_EXPR
		|| (TREE_CODE_CLASS (TREE_CODE (arg0)) == '<'
		    && TREE_CODE_CLASS (code) != '<'))
	       && (TREE_CODE (arg1) != COND_EXPR
		   || count_cond (arg0, 25) + count_cond (arg1, 25) <= 25)
	       && (! TREE_SIDE_EFFECTS (arg1)
		   || (global_bindings_p () == 0
		       && ! contains_placeholder_p (arg1))))
	return 
	  fold_binary_op_with_conditional_arg (code, type, arg0, arg1,
					       /*cond_first_p=*/1);
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
    case VECTOR_CST:
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
	  unsigned int inside_prec = TYPE_PRECISION (inside_type);
	  int inside_unsignedp = TREE_UNSIGNED (inside_type);
	  int inter_int = INTEGRAL_TYPE_P (inter_type);
	  int inter_ptr = POINTER_TYPE_P (inter_type);
	  int inter_float = FLOAT_TYPE_P (inter_type);
	  unsigned int inter_prec = TYPE_PRECISION (inter_type);
	  int inter_unsignedp = TREE_UNSIGNED (inter_type);
	  int final_int = INTEGRAL_TYPE_P (final_type);
	  int final_ptr = POINTER_TYPE_P (final_type);
	  int final_float = FLOAT_TYPE_P (final_type);
	  unsigned int final_prec = TYPE_PRECISION (final_type);
	  int final_unsignedp = TREE_UNSIGNED (final_type);

	  /* In addition to the cases of two conversions in a row
	     handled below, if we are converting something to its own
	     type via an object of identical or wider precision, neither
	     conversion is needed.  */
	  if (TYPE_MAIN_VARIANT (inside_type) == TYPE_MAIN_VARIANT (final_type)
	      && ((inter_int && final_int) || (inter_float && final_float))
	      && inter_prec >= final_prec)
	    return convert (final_type, TREE_OPERAND (TREE_OPERAND (t, 0), 0));

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

	  /* If we have a sign-extension of a zero-extended value, we can
	     replace that by a single zero-extension.  */
	  if (inside_int && inter_int && final_int
	      && inside_prec < inter_prec && inter_prec < final_prec
	      && inside_unsignedp && !inter_unsignedp)
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

    case VIEW_CONVERT_EXPR:
      if (TREE_CODE (TREE_OPERAND (t, 0)) == VIEW_CONVERT_EXPR)
	return build1 (VIEW_CONVERT_EXPR, type,
		       TREE_OPERAND (TREE_OPERAND (t, 0), 0));
      return t;

#if 0  /* This loses on &"foo"[0].  */
    case ARRAY_REF:
	{
	  int i;

	  /* Fold an expression like: "foo"[2] */
	  if (TREE_CODE (arg0) == STRING_CST
	      && TREE_CODE (arg1) == INTEGER_CST
	      && compare_tree_int (arg1, TREE_STRING_LENGTH (arg0)) < 0)
	    {
	      t = build_int_2 (TREE_STRING_POINTER (arg0)[TREE_INT_CST_LOW (arg))], 0);
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
	      unsigned HOST_WIDE_INT low;
	      HOST_WIDE_INT high;
	      int overflow = neg_double (TREE_INT_CST_LOW (arg0),
					 TREE_INT_CST_HIGH (arg0),
					 &low, &high);
	      t = build_int_2 (low, high);
	      TREE_TYPE (t) = type;
	      TREE_OVERFLOW (t)
		= (TREE_OVERFLOW (arg0)
		   | force_fit_type (t, overflow && !TREE_UNSIGNED (type)));
	      TREE_CONSTANT_OVERFLOW (t)
		= TREE_OVERFLOW (t) | TREE_CONSTANT_OVERFLOW (arg0);
	    }
	  else if (TREE_CODE (arg0) == REAL_CST)
	    t = build_real (type, REAL_VALUE_NEGATE (TREE_REAL_CST (arg0)));
	}
      else if (TREE_CODE (arg0) == NEGATE_EXPR)
	return TREE_OPERAND (arg0, 0);

      /* Convert - (a - b) to (b - a) for non-floating-point.  */
      else if (TREE_CODE (arg0) == MINUS_EXPR
	       && (! FLOAT_TYPE_P (type) || flag_unsafe_math_optimizations))
	return build (MINUS_EXPR, type, TREE_OPERAND (arg0, 1),
		      TREE_OPERAND (arg0, 0));

      return t;

    case ABS_EXPR:
      if (wins)
	{
	  if (TREE_CODE (arg0) == INTEGER_CST)
	    {
	      /* If the value is unsigned, then the absolute value is
		 the same as the ordinary value.  */
	      if (TREE_UNSIGNED (type))
		return arg0;
	      /* Similarly, if the value is non-negative.  */
	      else if (INT_CST_LT (integer_minus_one_node, arg0))
		return arg0;
	      /* If the value is negative, then the absolute value is
		 its negation.  */
	      else
		{
		  unsigned HOST_WIDE_INT low;
		  HOST_WIDE_INT high;
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
	}
      else if (TREE_CODE (arg0) == ABS_EXPR || TREE_CODE (arg0) == NEGATE_EXPR)
	return build1 (ABS_EXPR, type, TREE_OPERAND (arg0, 0));
      return t;

    case CONJ_EXPR:
      if (TREE_CODE (TREE_TYPE (arg0)) != COMPLEX_TYPE)
	return convert (type, arg0);
      else if (TREE_CODE (arg0) == COMPLEX_EXPR)
	return build (COMPLEX_EXPR, type,
		      TREE_OPERAND (arg0, 0),
		      negate_expr (TREE_OPERAND (arg0, 1)));
      else if (TREE_CODE (arg0) == COMPLEX_CST)
	return build_complex (type, TREE_REALPART (arg0),
			      negate_expr (TREE_IMAGPART (arg0)));
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
      /* (-A) + B -> B - A */
      if (TREE_CODE (arg0) == NEGATE_EXPR)
	return fold (build (MINUS_EXPR, type, arg1, TREE_OPERAND (arg0, 0)));
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

	  /* Reassociate (plus (plus (mult) (foo)) (mult)) as
	     (plus (plus (mult) (mult)) (foo)) so that we can
	     take advantage of the factoring cases below.  */
	  if ((TREE_CODE (arg0) == PLUS_EXPR
	       && TREE_CODE (arg1) == MULT_EXPR)
	      || (TREE_CODE (arg1) == PLUS_EXPR
		  && TREE_CODE (arg0) == MULT_EXPR))
	    {
	      tree parg0, parg1, parg, marg;

	      if (TREE_CODE (arg0) == PLUS_EXPR)
		parg = arg0, marg = arg1;
	      else
		parg = arg1, marg = arg0;
	      parg0 = TREE_OPERAND (parg, 0);
	      parg1 = TREE_OPERAND (parg, 1);
	      STRIP_NOPS (parg0);
	      STRIP_NOPS (parg1);

	      if (TREE_CODE (parg0) == MULT_EXPR
		  && TREE_CODE (parg1) != MULT_EXPR)
		return fold (build (PLUS_EXPR, type,
				    fold (build (PLUS_EXPR, type, parg0, marg)),
				    parg1));
	      if (TREE_CODE (parg0) != MULT_EXPR
		  && TREE_CODE (parg1) == MULT_EXPR)
		return fold (build (PLUS_EXPR, type,
				    fold (build (PLUS_EXPR, type, parg1, marg)),
				    parg0));
	    }

	  if (TREE_CODE (arg0) == MULT_EXPR && TREE_CODE (arg1) == MULT_EXPR)
	    {
	      tree arg00, arg01, arg10, arg11;
	      tree alt0 = NULL_TREE, alt1 = NULL_TREE, same;

	      /* (A * C) + (B * C) -> (A+B) * C.
		 We are most concerned about the case where C is a constant,
		 but other combinations show up during loop reduction.  Since
		 it is not difficult, try all four possibilities.  */

	      arg00 = TREE_OPERAND (arg0, 0);
	      arg01 = TREE_OPERAND (arg0, 1);
	      arg10 = TREE_OPERAND (arg1, 0);
	      arg11 = TREE_OPERAND (arg1, 1);
	      same = NULL_TREE;

	      if (operand_equal_p (arg01, arg11, 0))
		same = arg01, alt0 = arg00, alt1 = arg10;
	      else if (operand_equal_p (arg00, arg10, 0))
		same = arg00, alt0 = arg01, alt1 = arg11;
	      else if (operand_equal_p (arg00, arg11, 0))
		same = arg00, alt0 = arg01, alt1 = arg10;
	      else if (operand_equal_p (arg01, arg10, 0))
		same = arg01, alt0 = arg00, alt1 = arg11;

	      /* No identical multiplicands; see if we can find a common
		 power-of-two factor in non-power-of-two multiplies.  This
		 can help in multi-dimensional array access.  */
	      else if (TREE_CODE (arg01) == INTEGER_CST
		       && TREE_CODE (arg11) == INTEGER_CST
		       && TREE_INT_CST_HIGH (arg01) == 0
		       && TREE_INT_CST_HIGH (arg11) == 0)
		{
		  HOST_WIDE_INT int01, int11, tmp;
		  int01 = TREE_INT_CST_LOW (arg01);
		  int11 = TREE_INT_CST_LOW (arg11);

		  /* Move min of absolute values to int11.  */
		  if ((int01 >= 0 ? int01 : -int01)
		      < (int11 >= 0 ? int11 : -int11))
		    {
		      tmp = int01, int01 = int11, int11 = tmp;
		      alt0 = arg00, arg00 = arg10, arg10 = alt0;
		      alt0 = arg01, arg01 = arg11, arg11 = alt0;
		    }

		  if (exact_log2 (int11) > 0 && int01 % int11 == 0)
		    {
		      alt0 = fold (build (MULT_EXPR, type, arg00,
					  build_int_2 (int01 / int11, 0)));
		      alt1 = arg10;
		      same = arg11;
		    }
		}

	      if (same)
		return fold (build (MULT_EXPR, type,
				    fold (build (PLUS_EXPR, type, alt0, alt1)),
				    same));
	    }
	}
      /* In IEEE floating point, x+0 may not equal x.  */
      else if ((TARGET_FLOAT_FORMAT != IEEE_FLOAT_FORMAT
		|| flag_unsafe_math_optimizations)
	       && real_zerop (arg1))
	return non_lvalue (convert (type, arg0));
      /* x+(-0) equals x, even for IEEE.  */
      else if (TREE_CODE (arg1) == REAL_CST
	       && REAL_VALUE_MINUS_ZERO (TREE_REAL_CST (arg1)))
	return non_lvalue (convert (type, arg0));

     bit_rotate:
      /* (A << C1) + (A >> C2) if A is unsigned and C1+C2 is the size of A
	 is a rotate of A by C1 bits.  */
      /* (A << B) + (A >> (Z - B)) if A is unsigned and Z is the size of A
	 is a rotate of A by B bits.  */
      {
	enum tree_code code0, code1;
	code0 = TREE_CODE (arg0);
	code1 = TREE_CODE (arg1);
	if (((code0 == RSHIFT_EXPR && code1 == LSHIFT_EXPR)
	     || (code1 == RSHIFT_EXPR && code0 == LSHIFT_EXPR))
	    && operand_equal_p (TREE_OPERAND (arg0, 0),
			        TREE_OPERAND (arg1, 0), 0)
	    && TREE_UNSIGNED (TREE_TYPE (TREE_OPERAND (arg0, 0))))
	  {
	    tree tree01, tree11;
	    enum tree_code code01, code11;

	    tree01 = TREE_OPERAND (arg0, 1);
	    tree11 = TREE_OPERAND (arg1, 1);
	    STRIP_NOPS (tree01);
	    STRIP_NOPS (tree11);
	    code01 = TREE_CODE (tree01);
	    code11 = TREE_CODE (tree11);
	    if (code01 == INTEGER_CST
		&& code11 == INTEGER_CST
		&& TREE_INT_CST_HIGH (tree01) == 0
		&& TREE_INT_CST_HIGH (tree11) == 0
		&& ((TREE_INT_CST_LOW (tree01) + TREE_INT_CST_LOW (tree11))
		    == TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (arg0, 0)))))
	      return build (LROTATE_EXPR, type, TREE_OPERAND (arg0, 0),
			    code0 == LSHIFT_EXPR ? tree01 : tree11);
	    else if (code11 == MINUS_EXPR)
	      {
		tree tree110, tree111;
		tree110 = TREE_OPERAND (tree11, 0);
		tree111 = TREE_OPERAND (tree11, 1);
		STRIP_NOPS (tree110);
		STRIP_NOPS (tree111);
		if (TREE_CODE (tree110) == INTEGER_CST
		    && 0 == compare_tree_int (tree110,
					      TYPE_PRECISION
					      (TREE_TYPE (TREE_OPERAND
							  (arg0, 0))))
		    && operand_equal_p (tree01, tree111, 0))
		  return build ((code0 == LSHIFT_EXPR
				 ? LROTATE_EXPR
				 : RROTATE_EXPR),
				type, TREE_OPERAND (arg0, 0), tree01);
	      }
	    else if (code01 == MINUS_EXPR)
	      {
		tree tree010, tree011;
		tree010 = TREE_OPERAND (tree01, 0);
		tree011 = TREE_OPERAND (tree01, 1);
		STRIP_NOPS (tree010);
		STRIP_NOPS (tree011);
		if (TREE_CODE (tree010) == INTEGER_CST
		    && 0 == compare_tree_int (tree010,
					      TYPE_PRECISION
					      (TREE_TYPE (TREE_OPERAND
							  (arg0, 0))))
		    && operand_equal_p (tree11, tree011, 0))
		  return build ((code0 != LSHIFT_EXPR
				 ? LROTATE_EXPR
				 : RROTATE_EXPR),
				type, TREE_OPERAND (arg0, 0), tree11);
	      }
	  }
      }

    associate:
      /* In most languages, can't associate operations on floats through
	 parentheses.  Rather than remember where the parentheses were, we
	 don't associate floats at all.  It shouldn't matter much.  However,
	 associating multiplications is only very slightly inaccurate, so do
	 that if -funsafe-math-optimizations is specified.  */

      if (! wins
	  && (! FLOAT_TYPE_P (type)
	      || (flag_unsafe_math_optimizations && code == MULT_EXPR)))
	{
	  tree var0, con0, lit0, minus_lit0;
	  tree var1, con1, lit1, minus_lit1;

	  /* Split both trees into variables, constants, and literals.  Then
	     associate each group together, the constants with literals,
	     then the result with variables.  This increases the chances of
	     literals being recombined later and of generating relocatable
	     expressions for the sum of a constant and literal.  */
	  var0 = split_tree (arg0, code, &con0, &lit0, &minus_lit0, 0);
	  var1 = split_tree (arg1, code, &con1, &lit1, &minus_lit1,
			     code == MINUS_EXPR);

	  /* Only do something if we found more than two objects.  Otherwise,
	     nothing has changed and we risk infinite recursion.  */
	  if (2 < ((var0 != 0) + (var1 != 0)
		   + (con0 != 0) + (con1 != 0)
		   + (lit0 != 0) + (lit1 != 0)
		   + (minus_lit0 != 0) + (minus_lit1 != 0)))
	    {
	      /* Recombine MINUS_EXPR operands by using PLUS_EXPR.  */
	      if (code == MINUS_EXPR)
		code = PLUS_EXPR;

	      var0 = associate_trees (var0, var1, code, type);
	      con0 = associate_trees (con0, con1, code, type);
	      lit0 = associate_trees (lit0, lit1, code, type);
	      minus_lit0 = associate_trees (minus_lit0, minus_lit1, code, type);

	      /* Preserve the MINUS_EXPR if the negative part of the literal is
		 greater than the positive part.  Otherwise, the multiplicative
		 folding code (i.e extract_muldiv) may be fooled in case
		 unsigned constants are substracted, like in the following
		 example: ((X*2 + 4) - 8U)/2.  */
	      if (minus_lit0 && lit0)
		{
		  if (tree_int_cst_lt (lit0, minus_lit0))
		    {
		      minus_lit0 = associate_trees (minus_lit0, lit0,
						    MINUS_EXPR, type);
		      lit0 = 0;
		    }
		  else
		    {
		      lit0 = associate_trees (lit0, minus_lit0,
					      MINUS_EXPR, type);
		      minus_lit0 = 0;
		    }
		}
	      if (minus_lit0)
		{
		  if (con0 == 0)
		    return convert (type, associate_trees (var0, minus_lit0,
							   MINUS_EXPR, type));
		  else
		    {
		      con0 = associate_trees (con0, minus_lit0,
					      MINUS_EXPR, type);
		      return convert (type, associate_trees (var0, con0,
							     PLUS_EXPR, type));
		    }
		}

	      con0 = associate_trees (con0, lit0, code, type);
	      return convert (type, associate_trees (var0, con0, code, type));
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
	  if (TREE_TYPE (t1) != TREE_TYPE (t))
	    t1 = convert (TREE_TYPE (t), t1);

	  return t1;
	}
      return t;

    case MINUS_EXPR:
      /* A - (-B) -> A + B */
      if (TREE_CODE (arg1) == NEGATE_EXPR)
	return fold (build (PLUS_EXPR, type, arg0, TREE_OPERAND (arg1, 0)));
      /* (-A) - CST -> (-CST) - A   for floating point (what about ints ?)  */
      if (TREE_CODE (arg0) == NEGATE_EXPR && TREE_CODE (arg1) == REAL_CST)
	return
	  fold (build (MINUS_EXPR, type,
		       build_real (TREE_TYPE (arg1),
				   REAL_VALUE_NEGATE (TREE_REAL_CST (arg1))),
		       TREE_OPERAND (arg0, 0)));

      if (! FLOAT_TYPE_P (type))
	{
	  if (! wins && integer_zerop (arg0))
	    return negate_expr (convert (type, arg1));
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

      else if (TARGET_FLOAT_FORMAT != IEEE_FLOAT_FORMAT
	       || flag_unsafe_math_optimizations)
	{
	  /* Except with IEEE floating point, 0-x equals -x.  */
	  if (! wins && real_zerop (arg0))
	    return negate_expr (convert (type, arg1));
	  /* Except with IEEE floating point, x-0 equals x.  */
	  if (real_zerop (arg1))
	    return non_lvalue (convert (type, arg0));
	}

      /* Fold &x - &x.  This can happen from &x.foo - &x.
	 This is unsafe for certain floats even in non-IEEE formats.
	 In IEEE, it is unsafe because it does wrong for NaNs.
	 Also note that operand_equal_p is always false if an operand
	 is volatile.  */

      if ((! FLOAT_TYPE_P (type) || flag_unsafe_math_optimizations)
	  && operand_equal_p (arg0, arg1, 0))
	return convert (type, integer_zero_node);

      goto associate;

    case MULT_EXPR:
      /* (-A) * (-B) -> A * B  */
      if (TREE_CODE (arg0) == NEGATE_EXPR && TREE_CODE (arg1) == NEGATE_EXPR)
	return fold (build (MULT_EXPR, type, TREE_OPERAND (arg0, 0),
			    TREE_OPERAND (arg1, 0)));

      if (! FLOAT_TYPE_P (type))
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

	  if (TREE_CODE (arg1) == INTEGER_CST
	      && 0 != (tem = extract_muldiv (TREE_OPERAND (t, 0), arg1,
					     code, NULL_TREE)))
	    return convert (type, tem);

	}
      else
	{
	  /* x*0 is 0, except for IEEE floating point.  */
	  if ((TARGET_FLOAT_FORMAT != IEEE_FLOAT_FORMAT
	       || flag_unsafe_math_optimizations)
	      && real_zerop (arg1))
	    return omit_one_operand (type, arg1, arg0);
	  /* In IEEE floating point, x*1 is not equivalent to x for snans.
	     However, ANSI says we can drop signals,
	     so we can do this anyway.  */
	  if (real_onep (arg1))
	    return non_lvalue (convert (type, arg0));
	  /* x*2 is x+x */
	  if (! wins && real_twop (arg1) && global_bindings_p () == 0
	      && ! contains_placeholder_p (arg0))
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

      /* Convert (or (not arg0) (not arg1)) to (not (and (arg0) (arg1))).

	 This results in more efficient code for machines without a NAND
	 instruction.  Combine will canonicalize to the first form
	 which will allow use of NAND instructions provided by the
	 backend if they exist.  */
      if (TREE_CODE (arg0) == BIT_NOT_EXPR
	  && TREE_CODE (arg1) == BIT_NOT_EXPR)
	{
	  return fold (build1 (BIT_NOT_EXPR, type,
			       build (BIT_AND_EXPR, type,
				      TREE_OPERAND (arg0, 0),
				      TREE_OPERAND (arg1, 0))));
	}

      /* See if this can be simplified into a rotate first.  If that
	 is unsuccessful continue in the association code.  */
      goto bit_rotate;

    case BIT_XOR_EXPR:
      if (integer_zerop (arg1))
	return non_lvalue (convert (type, arg0));
      if (integer_all_onesp (arg1))
	return fold (build1 (BIT_NOT_EXPR, type, arg0));

      /* If we are XORing two BIT_AND_EXPR's, both of which are and'ing
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

      /* See if this can be simplified into a rotate first.  If that
	 is unsuccessful continue in the association code.  */
      goto bit_rotate;

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
	  unsigned int prec
	    = TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (arg1, 0)));

	  if (prec < BITS_PER_WORD && prec < HOST_BITS_PER_WIDE_INT
	      && (~TREE_INT_CST_LOW (arg0)
		  & (((HOST_WIDE_INT) 1 << prec) - 1)) == 0)
	    return build1 (NOP_EXPR, type, TREE_OPERAND (arg1, 0));
	}
      if (TREE_CODE (arg1) == INTEGER_CST && TREE_CODE (arg0) == NOP_EXPR
	  && TREE_UNSIGNED (TREE_TYPE (TREE_OPERAND (arg0, 0))))
	{
	  unsigned int prec
	    = TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (arg0, 0)));

	  if (prec < BITS_PER_WORD && prec < HOST_BITS_PER_WIDE_INT
	      && (~TREE_INT_CST_LOW (arg1)
		  & (((HOST_WIDE_INT) 1 << prec) - 1)) == 0)
	    return build1 (NOP_EXPR, type, TREE_OPERAND (arg0, 0));
	}

      /* Convert (and (not arg0) (not arg1)) to (not (or (arg0) (arg1))).

	 This results in more efficient code for machines without a NOR
	 instruction.  Combine will canonicalize to the first form
	 which will allow use of NOR instructions provided by the
	 backend if they exist.  */
      if (TREE_CODE (arg0) == BIT_NOT_EXPR
	  && TREE_CODE (arg1) == BIT_NOT_EXPR)
	{
	  return fold (build1 (BIT_NOT_EXPR, type,
			       build (BIT_IOR_EXPR, type,
				      TREE_OPERAND (arg0, 0),
				      TREE_OPERAND (arg1, 0))));
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

      /* (-A) / (-B) -> A / B  */
      if (TREE_CODE (arg0) == NEGATE_EXPR && TREE_CODE (arg1) == NEGATE_EXPR)
	return fold (build (RDIV_EXPR, type, TREE_OPERAND (arg0, 0),
			    TREE_OPERAND (arg1, 0)));

      /* In IEEE floating point, x/1 is not equivalent to x for snans.
	 However, ANSI says we can drop signals, so we can do this anyway.  */
      if (real_onep (arg1))
	return non_lvalue (convert (type, arg0));

      /* If ARG1 is a constant, we can convert this to a multiply by the
	 reciprocal.  This does not have the same rounding properties,
	 so only do this if -funsafe-math-optimizations.  We can actually
	 always safely do it if ARG1 is a power of two, but it's hard to
	 tell if it is or not in a portable manner.  */
      if (TREE_CODE (arg1) == REAL_CST)
	{
	  if (flag_unsafe_math_optimizations
	      && 0 != (tem = const_binop (code, build_real (type, dconst1),
					  arg1, 0)))
	    return fold (build (MULT_EXPR, type, arg0, tem));
	  /* Find the reciprocal if optimizing and the result is exact.  */
	  else if (optimize)
	    {
	      REAL_VALUE_TYPE r;
	      r = TREE_REAL_CST (arg1);
	      if (exact_real_inverse (TYPE_MODE(TREE_TYPE(arg0)), &r))
		{
		  tem = build_real (type, r);
		  return fold (build (MULT_EXPR, type, arg0, tem));
		}
	    }
	}
      /* Convert A/B/C to A/(B*C).  */
      if (flag_unsafe_math_optimizations
	  && TREE_CODE (arg0) == RDIV_EXPR)
	{
	  return fold (build (RDIV_EXPR, type, TREE_OPERAND (arg0, 0),
			      build (MULT_EXPR, type, TREE_OPERAND (arg0, 1),
				     arg1)));
	}
      /* Convert A/(B/C) to (A/B)*C.  */
      if (flag_unsafe_math_optimizations
	  && TREE_CODE (arg1) == RDIV_EXPR)
	{
	  return fold (build (MULT_EXPR, type,
			      build (RDIV_EXPR, type, arg0,
			     	     TREE_OPERAND (arg1, 0)),
	 		      TREE_OPERAND (arg1, 1)));
	}
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

      /* If arg0 is a multiple of arg1, then rewrite to the fastest div
	 operation, EXACT_DIV_EXPR.

	 Note that only CEIL_DIV_EXPR and FLOOR_DIV_EXPR are rewritten now.
	 At one time others generated faster code, it's not clear if they do
	 after the last round to changes to the DIV code in expmed.c.  */
      if ((code == CEIL_DIV_EXPR || code == FLOOR_DIV_EXPR)
	  && multiple_of_p (type, arg0, arg1))
	return fold (build (EXACT_DIV_EXPR, type, arg0, arg1));

      if (TREE_CODE (arg1) == INTEGER_CST
	  && 0 != (tem = extract_muldiv (TREE_OPERAND (t, 0), arg1,
					 code, NULL_TREE)))
	return convert (type, tem);

      goto binary;

    case CEIL_MOD_EXPR:
    case FLOOR_MOD_EXPR:
    case ROUND_MOD_EXPR:
    case TRUNC_MOD_EXPR:
      if (integer_onep (arg1))
	return omit_one_operand (type, integer_zero_node, arg0);
      if (integer_zerop (arg1))
	return t;

      if (TREE_CODE (arg1) == INTEGER_CST
	  && 0 != (tem = extract_muldiv (TREE_OPERAND (t, 0), arg1,
					 code, NULL_TREE)))
	return convert (type, tem);

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
	      == (unsigned int) GET_MODE_BITSIZE (TYPE_MODE (type))))
	return TREE_OPERAND (arg0, 0);

      goto binary;

    case MIN_EXPR:
      if (operand_equal_p (arg0, arg1, 0))
	return omit_one_operand (type, arg0, arg1);
      if (INTEGRAL_TYPE_P (type)
	  && operand_equal_p (arg1, TYPE_MIN_VALUE (type), 1))
	return omit_one_operand (type, arg1, arg0);
      goto associate;

    case MAX_EXPR:
      if (operand_equal_p (arg0, arg1, 0))
	return omit_one_operand (type, arg0, arg1);
      if (INTEGRAL_TYPE_P (type)
	  && TYPE_MAX_VALUE (type)
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
	return convert (type, arg0);
    case TRUTH_AND_EXPR:
      /* If either arg is constant true, drop it.  */
      if (TREE_CODE (arg0) == INTEGER_CST && ! integer_zerop (arg0))
	return non_lvalue (convert (type, arg1));
      if (TREE_CODE (arg1) == INTEGER_CST && ! integer_zerop (arg1)
	  /* Preserve sequence points.  */
	  && (code != TRUTH_ANDIF_EXPR || ! TREE_SIDE_EFFECTS (arg0)))
	return non_lvalue (convert (type, arg0));
      /* If second arg is constant zero, result is zero, but first arg
	 must be evaluated.  */
      if (integer_zerop (arg1))
	return omit_one_operand (type, arg1, arg0);
      /* Likewise for first arg, but note that only the TRUTH_AND_EXPR
	 case will be handled here.  */
      if (integer_zerop (arg0))
	return omit_one_operand (type, arg0, arg1);

    truth_andor:
      /* We only do these simplifications if we are optimizing.  */
      if (!optimize)
	return t;

      /* Check for things like (A || B) && (A || C).  We can convert this
	 to A || (B && C).  Note that either operator can be any of the four
	 truth and/or operations and the transformation will still be
	 valid.   Also note that we only care about order for the
	 ANDIF and ORIF operators.  If B contains side effects, this
	 might change the truth-value of A.  */
      if (TREE_CODE (arg0) == TREE_CODE (arg1)
	  && (TREE_CODE (arg0) == TRUTH_ANDIF_EXPR
	      || TREE_CODE (arg0) == TRUTH_ORIF_EXPR
	      || TREE_CODE (arg0) == TRUTH_AND_EXPR
	      || TREE_CODE (arg0) == TRUTH_OR_EXPR)
	  && ! TREE_SIDE_EFFECTS (TREE_OPERAND (arg0, 1)))
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

      /* See if we can build a range comparison.  */
      if (0 != (tem = fold_range_test (t)))
	return tem;

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
	return convert (type, arg0);
    case TRUTH_OR_EXPR:
      /* If either arg is constant zero, drop it.  */
      if (TREE_CODE (arg0) == INTEGER_CST && integer_zerop (arg0))
	return non_lvalue (convert (type, arg1));
      if (TREE_CODE (arg1) == INTEGER_CST && integer_zerop (arg1)
	  /* Preserve sequence points.  */
	  && (code != TRUTH_ORIF_EXPR || ! TREE_SIDE_EFFECTS (arg0)))
	return non_lvalue (convert (type, arg0));
      /* If second arg is constant true, result is true, but we must
	 evaluate first arg.  */
      if (TREE_CODE (arg1) == INTEGER_CST && ! integer_zerop (arg1))
	return omit_one_operand (type, arg1, arg0);
      /* Likewise for first arg, but note this only occurs here for
	 TRUTH_OR_EXPR.  */
      if (TREE_CODE (arg0) == INTEGER_CST && ! integer_zerop (arg0))
	return omit_one_operand (type, arg0, arg1);
      goto truth_andor;

    case TRUTH_XOR_EXPR:
      /* If either arg is constant zero, drop it.  */
      if (integer_zerop (arg0))
	return non_lvalue (convert (type, arg1));
      if (integer_zerop (arg1))
	return non_lvalue (convert (type, arg0));
      /* If either arg is constant true, this is a logical inversion.  */
      if (integer_onep (arg0))
	return non_lvalue (convert (type, invert_truthvalue (arg1)));
      if (integer_onep (arg1))
	return non_lvalue (convert (type, invert_truthvalue (arg0)));
      return t;

    case EQ_EXPR:
    case NE_EXPR:
    case LT_EXPR:
    case GT_EXPR:
    case LE_EXPR:
    case GE_EXPR:
      if (FLOAT_TYPE_P (TREE_TYPE (arg0)))
	{
	  /* (-a) CMP (-b) -> b CMP a  */
	  if (TREE_CODE (arg0) == NEGATE_EXPR
	      && TREE_CODE (arg1) == NEGATE_EXPR)
	    return fold (build (code, type, TREE_OPERAND (arg1, 0),
				TREE_OPERAND (arg0, 0)));
	  /* (-a) CMP CST -> a swap(CMP) (-CST)  */
	  if (TREE_CODE (arg0) == NEGATE_EXPR && TREE_CODE (arg1) == REAL_CST)
	    return
	      fold (build
		    (swap_tree_comparison (code), type,
		     TREE_OPERAND (arg0, 0),
		     build_real (TREE_TYPE (arg1),
				 REAL_VALUE_NEGATE (TREE_REAL_CST (arg1)))));
	  /* IEEE doesn't distinguish +0 and -0 in comparisons.  */
	  /* a CMP (-0) -> a CMP 0  */
	  if (TREE_CODE (arg1) == REAL_CST
	      && REAL_VALUE_MINUS_ZERO (TREE_REAL_CST (arg1)))
	    return fold (build (code, type, arg0,
				build_real (TREE_TYPE (arg1), dconst0)));
	}

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
	tree constop = 0, varop = NULL_TREE;
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
	    if (POINTER_TYPE_P (TREE_TYPE (varop))
		|| (! FLOAT_TYPE_P (TREE_TYPE (varop))
		    && (code == EQ_EXPR || code == NE_EXPR)))
	      {
		tree newconst
		  = fold (build (PLUS_EXPR, TREE_TYPE (varop),
				 constop, TREE_OPERAND (varop, 1)));

		/* Do not overwrite the current varop to be a preincrement,
		   create a new node so that we won't confuse our caller who
		   might create trees and throw them away, reusing the
		   arguments that they passed to build.  This shows up in
		   the THEN or ELSE parts of ?: being postincrements.  */
		varop = build (PREINCREMENT_EXPR, TREE_TYPE (varop),
			       TREE_OPERAND (varop, 0),
			       TREE_OPERAND (varop, 1));

		/* If VAROP is a reference to a bitfield, we must mask
		   the constant by the width of the field.  */
		if (TREE_CODE (TREE_OPERAND (varop, 0)) == COMPONENT_REF
		    && DECL_BIT_FIELD(TREE_OPERAND
				      (TREE_OPERAND (varop, 0), 1)))
		  {
		    int size
		      = TREE_INT_CST_LOW (DECL_SIZE
					  (TREE_OPERAND
					   (TREE_OPERAND (varop, 0), 1)));
		    tree mask, unsigned_type;
		    unsigned int precision;
		    tree folded_compare;

		    /* First check whether the comparison would come out
		       always the same.  If we don't do that we would
		       change the meaning with the masking.  */
		    if (constopnum == 0)
		      folded_compare = fold (build (code, type, constop,
						    TREE_OPERAND (varop, 0)));
		    else
		      folded_compare = fold (build (code, type,
						    TREE_OPERAND (varop, 0),
						    constop));
		    if (integer_zerop (folded_compare)
			|| integer_onep (folded_compare))
		      return omit_one_operand (type, folded_compare, varop);

		    unsigned_type = type_for_size (size, 1);
		    precision = TYPE_PRECISION (unsigned_type);
		    mask = build_int_2 (~0, ~0);
		    TREE_TYPE (mask) = unsigned_type;
		    force_fit_type (mask, 0);
		    mask = const_binop (RSHIFT_EXPR, mask,
					size_int (precision - size), 0);
		    newconst = fold (build (BIT_AND_EXPR,
					    TREE_TYPE (varop), newconst,
					    convert (TREE_TYPE (varop),
						     mask)));
		  }

		t = build (code, type,
			   (constopnum == 0) ? newconst : varop,
			   (constopnum == 1) ? newconst : varop);
		return t;
	      }
	  }
	else if (constop && TREE_CODE (varop) == POSTDECREMENT_EXPR)
	  {
	    if (POINTER_TYPE_P (TREE_TYPE (varop))
		|| (! FLOAT_TYPE_P (TREE_TYPE (varop))
		    && (code == EQ_EXPR || code == NE_EXPR)))
	      {
		tree newconst
		  = fold (build (MINUS_EXPR, TREE_TYPE (varop),
				 constop, TREE_OPERAND (varop, 1)));

		/* Do not overwrite the current varop to be a predecrement,
		   create a new node so that we won't confuse our caller who
		   might create trees and throw them away, reusing the
		   arguments that they passed to build.  This shows up in
		   the THEN or ELSE parts of ?: being postdecrements.  */
		varop = build (PREDECREMENT_EXPR, TREE_TYPE (varop),
			       TREE_OPERAND (varop, 0),
			       TREE_OPERAND (varop, 1));

		if (TREE_CODE (TREE_OPERAND (varop, 0)) == COMPONENT_REF
		    && DECL_BIT_FIELD(TREE_OPERAND
				      (TREE_OPERAND (varop, 0), 1)))
		  {
		    int size
		      = TREE_INT_CST_LOW (DECL_SIZE
					  (TREE_OPERAND
					   (TREE_OPERAND (varop, 0), 1)));
		    tree mask, unsigned_type;
		    unsigned int precision;
		    tree folded_compare;

		    if (constopnum == 0)
		      folded_compare = fold (build (code, type, constop,
						    TREE_OPERAND (varop, 0)));
		    else
		      folded_compare = fold (build (code, type,
						    TREE_OPERAND (varop, 0),
						    constop));
		    if (integer_zerop (folded_compare)
			|| integer_onep (folded_compare))
		      return omit_one_operand (type, folded_compare, varop);

		    unsigned_type = type_for_size (size, 1);
		    precision = TYPE_PRECISION (unsigned_type);
		    mask = build_int_2 (~0, ~0);
		    TREE_TYPE (mask) = TREE_TYPE (varop);
		    force_fit_type (mask, 0);
		    mask = const_binop (RSHIFT_EXPR, mask,
					size_int (precision - size), 0);
		    newconst = fold (build (BIT_AND_EXPR,
					    TREE_TYPE (varop), newconst,
					    convert (TREE_TYPE (varop),
						     mask)));
		  }

		t = build (code, type,
			   (constopnum == 0) ? newconst : varop,
			   (constopnum == 1) ? newconst : varop);
		return t;
	      }
	  }
      }

      /* Comparisons with the highest or lowest possible integer of
	 the specified size will have known values and an unsigned
	 <= 0x7fffffff can be simplified.  */
      {
	int width = GET_MODE_BITSIZE (TYPE_MODE (TREE_TYPE (arg1)));

	if (TREE_CODE (arg1) == INTEGER_CST
	    && ! TREE_CONSTANT_OVERFLOW (arg1)
	    && width <= HOST_BITS_PER_WIDE_INT
	    && (INTEGRAL_TYPE_P (TREE_TYPE (arg1))
		|| POINTER_TYPE_P (TREE_TYPE (arg1))))
	  {
	    if (TREE_INT_CST_HIGH (arg1) == 0
		&& (TREE_INT_CST_LOW (arg1)
		    == ((unsigned HOST_WIDE_INT) 1 << (width - 1)) - 1)
		&& ! TREE_UNSIGNED (TREE_TYPE (arg1)))
	      switch (TREE_CODE (t))
		{
		case GT_EXPR:
		  return omit_one_operand (type,
					   convert (type, integer_zero_node),
					   arg0);
		case GE_EXPR:
		  TREE_SET_CODE (t, EQ_EXPR);
		  break;

		case LE_EXPR:
		  return omit_one_operand (type,
					   convert (type, integer_one_node),
					   arg0);
		case LT_EXPR:
		  TREE_SET_CODE (t, NE_EXPR);
		  break;

		default:
		  break;
		}

	    else if (TREE_INT_CST_HIGH (arg1) == -1
		     && (TREE_INT_CST_LOW (arg1)
			 == ((unsigned HOST_WIDE_INT) 1 << (width - 1)))
		     && ! TREE_UNSIGNED (TREE_TYPE (arg1)))
	      switch (TREE_CODE (t))
		{
		case LT_EXPR:
		  return omit_one_operand (type,
					   convert (type, integer_zero_node),
					   arg0);
		case LE_EXPR:
		  TREE_SET_CODE (t, EQ_EXPR);
		  break;

		case GE_EXPR:
		  return omit_one_operand (type,
					   convert (type, integer_one_node),
					   arg0);
		case GT_EXPR:
		  TREE_SET_CODE (t, NE_EXPR);
		  break;

		default:
		  break;
		}

	    else if (TREE_INT_CST_HIGH (arg1) == 0
		     && (TREE_INT_CST_LOW (arg1)
			 == ((unsigned HOST_WIDE_INT) 1 << (width - 1)) - 1)
		     && TREE_UNSIGNED (TREE_TYPE (arg1))
		     /* signed_type does not work on pointer types.  */
		     && INTEGRAL_TYPE_P (TREE_TYPE (arg1)))
	      switch (TREE_CODE (t))
		{
		case LE_EXPR:
		  return fold (build (GE_EXPR, type,
				      convert (signed_type (TREE_TYPE (arg0)),
					       arg0),
				      convert (signed_type (TREE_TYPE (arg1)),
					       integer_zero_node)));
		case GT_EXPR:
		  return fold (build (LT_EXPR, type,
				      convert (signed_type (TREE_TYPE (arg0)),
					       arg0),
				      convert (signed_type (TREE_TYPE (arg1)),
					       integer_zero_node)));

		default:
		  break;
		}

            else if (TREE_INT_CST_HIGH (arg1) == 0
		     && (TREE_INT_CST_LOW (arg1)
			 == ((unsigned HOST_WIDE_INT) 2 << (width - 1)) - 1)
		     && TREE_UNSIGNED (TREE_TYPE (arg1)))
              switch (TREE_CODE (t))
                {
                case GT_EXPR:
                  return omit_one_operand (type,
                                           convert (type, integer_zero_node),
                                           arg0);
                case GE_EXPR:
                  TREE_SET_CODE (t, EQ_EXPR);
                  break;

                case LE_EXPR:
                  return omit_one_operand (type,
                                           convert (type, integer_one_node),
                                           arg0);
                case LT_EXPR:
                  TREE_SET_CODE (t, NE_EXPR);
                  break;

                default:
                  break;
                }
	  }
      }

      /* Change X >= CST to X > (CST - 1) and X < CST to X <= (CST - 1)
	 if CST is positive.  */
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

	    default:
	      break;
	    }
	}

      /* An unsigned comparison against 0 can be simplified.  */
      if (integer_zerop (arg1)
	  && (INTEGRAL_TYPE_P (TREE_TYPE (arg1))
	      || POINTER_TYPE_P (TREE_TYPE (arg1)))
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
	    default:
	      break;
	    }
	}

      /* If this is an EQ or NE comparison of a constant with a PLUS_EXPR or
	 a MINUS_EXPR of a constant, we can convert it into a comparison with
	 a revised constant as long as no overflow occurs.  */
      if ((code == EQ_EXPR || code == NE_EXPR)
	  && TREE_CODE (arg1) == INTEGER_CST
	  && (TREE_CODE (arg0) == PLUS_EXPR
	      || TREE_CODE (arg0) == MINUS_EXPR)
	  && TREE_CODE (TREE_OPERAND (arg0, 1)) == INTEGER_CST
	  && 0 != (tem = const_binop (TREE_CODE (arg0) == PLUS_EXPR
				      ? MINUS_EXPR : PLUS_EXPR,
				      arg1, TREE_OPERAND (arg0, 1), 0))
	  && ! TREE_CONSTANT_OVERFLOW (tem))
	return fold (build (code, type, TREE_OPERAND (arg0, 0), tem));

      /* Similarly for a NEGATE_EXPR.  */
      else if ((code == EQ_EXPR || code == NE_EXPR)
	       && TREE_CODE (arg0) == NEGATE_EXPR
	       && TREE_CODE (arg1) == INTEGER_CST
	       && 0 != (tem = negate_expr (arg1))
	       && TREE_CODE (tem) == INTEGER_CST
	       && ! TREE_CONSTANT_OVERFLOW (tem))
	return fold (build (code, type, TREE_OPERAND (arg0, 0), tem));

      /* If we have X - Y == 0, we can convert that to X == Y and similarly
	 for !=.  Don't do this for ordered comparisons due to overflow.  */
      else if ((code == NE_EXPR || code == EQ_EXPR)
	       && integer_zerop (arg1) && TREE_CODE (arg0) == MINUS_EXPR)
	return fold (build (code, type,
			    TREE_OPERAND (arg0, 0), TREE_OPERAND (arg0, 1)));

      /* If we are widening one operand of an integer comparison,
	 see if the other operand is similarly being widened.  Perhaps we
	 can do the comparison in the narrower type.  */
      else if (TREE_CODE (TREE_TYPE (arg0)) == INTEGER_TYPE
	       && TREE_CODE (arg0) == NOP_EXPR
	       && (tem = get_unwidened (arg0, NULL_TREE)) != arg0
	       && (t1 = get_unwidened (arg1, TREE_TYPE (tem))) != 0
	       && (TREE_TYPE (t1) == TREE_TYPE (tem)
		   || (TREE_CODE (t1) == INTEGER_CST
		       && int_fits_type_p (t1, TREE_TYPE (tem)))))
	return fold (build (code, type, tem, convert (TREE_TYPE (tem), t1)));

      /* If this is comparing a constant with a MIN_EXPR or a MAX_EXPR of a
	 constant, we can simplify it.  */
      else if (TREE_CODE (arg1) == INTEGER_CST
	       && (TREE_CODE (arg0) == MIN_EXPR
		   || TREE_CODE (arg0) == MAX_EXPR)
	       && TREE_CODE (TREE_OPERAND (arg0, 1)) == INTEGER_CST)
	return optimize_minmax_comparison (t);

      /* If we are comparing an ABS_EXPR with a constant, we can
	 convert all the cases into explicit comparisons, but they may
	 well not be faster than doing the ABS and one comparison.
	 But ABS (X) <= C is a range comparison, which becomes a subtraction
	 and a comparison, and is probably faster.  */
      else if (code == LE_EXPR && TREE_CODE (arg1) == INTEGER_CST
	       && TREE_CODE (arg0) == ABS_EXPR
	       && ! TREE_SIDE_EFFECTS (arg0)
	       && (0 != (tem = negate_expr (arg1)))
	       && TREE_CODE (tem) == INTEGER_CST
	       && ! TREE_CONSTANT_OVERFLOW (tem))
	return fold (build (TRUTH_ANDIF_EXPR, type,
			    build (GE_EXPR, type, TREE_OPERAND (arg0, 0), tem),
			    build (LE_EXPR, type,
				   TREE_OPERAND (arg0, 0), arg1)));

      /* If this is an EQ or NE comparison with zero and ARG0 is
	 (1 << foo) & bar, convert it to (bar >> foo) & 1.  Both require
	 two operations, but the latter can be done in one less insn
	 on machines that have only two-operand insns or on which a
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
	      if (! FLOAT_TYPE_P (TREE_TYPE (arg0)))
		return constant_boolean_node (1, type);
	      code = EQ_EXPR;
	      TREE_SET_CODE (t, code);
	      break;

	    case NE_EXPR:
	      /* For NE, we can only do this simplification if integer.  */
	      if (FLOAT_TYPE_P (TREE_TYPE (arg0)))
		break;
	      /* ... fall through ...  */
	    case GT_EXPR:
	    case LT_EXPR:
	      return constant_boolean_node (0, type);
	    default:
	      abort ();
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
	      && TYPE_MAX_VALUE (TREE_TYPE (cval1))
	      && TYPE_MAX_VALUE (TREE_TYPE (cval2))
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

      /* If this is a comparison of complex values and either or both sides
	 are a COMPLEX_EXPR or COMPLEX_CST, it is best to split up the
	 comparisons and join them with a TRUTH_ANDIF_EXPR or TRUTH_ORIF_EXPR.
	 This may prevent needless evaluations.  */
      if ((code == EQ_EXPR || code == NE_EXPR)
	  && TREE_CODE (TREE_TYPE (arg0)) == COMPLEX_TYPE
	  && (TREE_CODE (arg0) == COMPLEX_EXPR
	      || TREE_CODE (arg1) == COMPLEX_EXPR
	      || TREE_CODE (arg0) == COMPLEX_CST
	      || TREE_CODE (arg1) == COMPLEX_CST))
	{
	  tree subtype = TREE_TYPE (TREE_TYPE (arg0));
	  tree real0, imag0, real1, imag1;

	  arg0 = save_expr (arg0);
	  arg1 = save_expr (arg1);
	  real0 = fold (build1 (REALPART_EXPR, subtype, arg0));
	  imag0 = fold (build1 (IMAGPART_EXPR, subtype, arg0));
	  real1 = fold (build1 (REALPART_EXPR, subtype, arg1));
	  imag1 = fold (build1 (IMAGPART_EXPR, subtype, arg1));

	  return fold (build ((code == EQ_EXPR ? TRUTH_ANDIF_EXPR
			       : TRUTH_ORIF_EXPR),
			      type,
			      fold (build (code, type, real0, real1)),
			      fold (build (code, type, imag0, imag1))));
	}

      /* Optimize comparisons of strlen vs zero to a compare of the
	 first character of the string vs zero.  To wit, 
	 	strlen(ptr) == 0   =>  *ptr == 0
		strlen(ptr) != 0   =>  *ptr != 0
	 Other cases should reduce to one of these two (or a constant)
	 due to the return value of strlen being unsigned.  */
      if ((code == EQ_EXPR || code == NE_EXPR)
	  && integer_zerop (arg1)
	  && TREE_CODE (arg0) == CALL_EXPR
	  && TREE_CODE (TREE_OPERAND (arg0, 0)) == ADDR_EXPR)
	{
	  tree fndecl = TREE_OPERAND (TREE_OPERAND (arg0, 0), 0);
	  tree arglist;

	  if (TREE_CODE (fndecl) == FUNCTION_DECL
	      && DECL_BUILT_IN (fndecl)
	      && DECL_BUILT_IN_CLASS (fndecl) != BUILT_IN_MD
	      && DECL_FUNCTION_CODE (fndecl) == BUILT_IN_STRLEN
	      && (arglist = TREE_OPERAND (arg0, 1))
	      && TREE_CODE (TREE_TYPE (TREE_VALUE (arglist))) == POINTER_TYPE
	      && ! TREE_CHAIN (arglist))
	    return fold (build (code, type,
				build1 (INDIRECT_REF, char_type_node,
					TREE_VALUE(arglist)),
				integer_zero_node));
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

      t1 = NULL_TREE;
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
	    t1 = build_int_2 (tree_int_cst_equal (arg0, arg1), 0);
	  else
	    t1 = build_int_2 ((TREE_UNSIGNED (TREE_TYPE (arg0))
			       ? INT_CST_LT_UNSIGNED (arg0, arg1)
			       : INT_CST_LT (arg0, arg1)),
			      0);
	}

#if 0 /* This is no longer useful, but breaks some real code.  */
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
#endif
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
      if (TREE_CODE (type) == BOOLEAN_TYPE)
	return truthvalue_conversion (t1);
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
	      /* arg1 should be the first argument of the new T.  */
	      arg1 = TREE_OPERAND (t, 1);
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
	      || flag_unsafe_math_optimizations)
	  && operand_equal_for_comparison_p (TREE_OPERAND (arg0, 0),
					     arg1, TREE_OPERAND (arg0, 1)))
	{
	  tree arg2 = TREE_OPERAND (t, 2);
	  enum tree_code comp_code = TREE_CODE (arg0);

	  STRIP_NOPS (arg2);

	  /* If we have A op 0 ? A : -A, this is A, -A, abs (A), or -abs (A),
	     depending on the comparison operation.  */
	  if ((FLOAT_TYPE_P (TREE_TYPE (TREE_OPERAND (arg0, 1)))
	       ? real_zerop (TREE_OPERAND (arg0, 1))
	       : integer_zerop (TREE_OPERAND (arg0, 1)))
	      && TREE_CODE (arg2) == NEGATE_EXPR
	      && operand_equal_p (TREE_OPERAND (arg2, 0), arg1, 0))
	    switch (comp_code)
	      {
	      case EQ_EXPR:
		return
		  pedantic_non_lvalue
		    (convert (type,
			      negate_expr
			      (convert (TREE_TYPE (TREE_OPERAND (t, 1)),
					arg1))));

	      case NE_EXPR:
		return pedantic_non_lvalue (convert (type, arg1));
	      case GE_EXPR:
	      case GT_EXPR:
		if (TREE_UNSIGNED (TREE_TYPE (arg1)))
		  arg1 = convert (signed_type (TREE_TYPE (arg1)), arg1);
		return pedantic_non_lvalue
		  (convert (type, fold (build1 (ABS_EXPR,
						TREE_TYPE (arg1), arg1))));
	      case LE_EXPR:
	      case LT_EXPR:
		if (TREE_UNSIGNED (TREE_TYPE (arg1)))
		  arg1 = convert (signed_type (TREE_TYPE (arg1)), arg1);
		return pedantic_non_lvalue
		  (negate_expr (convert (type,
					 fold (build1 (ABS_EXPR,
						       TREE_TYPE (arg1),
						       arg1)))));
	      default:
		abort ();
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

	      /* Avoid adding NOP_EXPRs in case this is an lvalue.  */
	      if (TYPE_MAIN_VARIANT (comp_type) == TYPE_MAIN_VARIANT (type))
		comp_type = type;

	      switch (comp_code)
		{
		case EQ_EXPR:
		  return pedantic_non_lvalue (convert (type, arg2));
		case NE_EXPR:
		  return pedantic_non_lvalue (convert (type, arg1));
		case LE_EXPR:
		case LT_EXPR:
		  /* In C++ a ?: expression can be an lvalue, so put the
		     operand which will be used if they are equal first
		     so that we can convert this back to the
		     corresponding COND_EXPR.  */
		  return pedantic_non_lvalue
		    (convert (type, fold (build (MIN_EXPR, comp_type,
						 (comp_code == LE_EXPR
						  ? comp_op0 : comp_op1),
						 (comp_code == LE_EXPR
						  ? comp_op1 : comp_op0)))));
		  break;
		case GE_EXPR:
		case GT_EXPR:
		  return pedantic_non_lvalue
		    (convert (type, fold (build (MAX_EXPR, comp_type,
						 (comp_code == GE_EXPR
						  ? comp_op0 : comp_op1),
						 (comp_code == GE_EXPR
						  ? comp_op1 : comp_op0)))));
		  break;
		default:
		  abort ();
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
	      case NE_EXPR:
		break;
	      default:
		abort ();
	      }
	}

      /* If the second operand is simpler than the third, swap them
	 since that produces better jump optimization results.  */
      if ((TREE_CONSTANT (arg1) || DECL_P (arg1)
	   || TREE_CODE (arg1) == SAVE_EXPR)
	  && ! (TREE_CONSTANT (TREE_OPERAND (t, 2))
		|| DECL_P (TREE_OPERAND (t, 2))
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
	      /* arg1 should be the first argument of the new T.  */
	      arg1 = TREE_OPERAND (t, 1);
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
	return build1 (NOP_EXPR, type, arg1);
      return convert (type, arg1);

    case COMPLEX_EXPR:
      if (wins)
	return build_complex (type, arg0, arg1);
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
      if (! has_cleanups (arg0))
	return TREE_OPERAND (t, 0);

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

	    if (TREE_CONSTANT (arg00)
		|| ((code0 == TRUTH_ANDIF_EXPR || code0 == TRUTH_ORIF_EXPR)
		    && ! has_cleanups (arg00)))
	      return fold (build (code0, type, arg00,
				  fold (build1 (CLEANUP_POINT_EXPR,
						TREE_TYPE (arg01), arg01))));

	    if (TREE_CONSTANT (arg01))
	      return fold (build (code0, type,
				  fold (build1 (CLEANUP_POINT_EXPR,
						TREE_TYPE (arg00), arg00)),
				  arg01));
	  }

	return t;
      }

    case CALL_EXPR:
      /* Check for a built-in function.  */
      if (TREE_CODE (TREE_OPERAND (expr, 0)) == ADDR_EXPR
	  && (TREE_CODE (TREE_OPERAND (TREE_OPERAND (expr, 0), 0))
	      == FUNCTION_DECL)
	  && DECL_BUILT_IN (TREE_OPERAND (TREE_OPERAND (expr, 0), 0)))
	{
	  tree tmp = fold_builtin (expr);
	  if (tmp)
	    return tmp;
	}
      return t;

    default:
      return t;
    } /* switch (code) */
}

/* Determine if first argument is a multiple of second argument.  Return 0 if
   it is not, or we cannot easily determined it to be.

   An example of the sort of thing we care about (at this point; this routine
   could surely be made more general, and expanded to do what the *_DIV_EXPR's
   fold cases do now) is discovering that

     SAVE_EXPR (I) * SAVE_EXPR (J * 8)

   is a multiple of

     SAVE_EXPR (J * 8)

   when we know that the two SAVE_EXPR (J * 8) nodes are the same node.

   This code also handles discovering that

     SAVE_EXPR (I) * SAVE_EXPR (J * 8)

   is a multiple of 8 so we don't have to worry about dealing with a
   possible remainder.

   Note that we *look* inside a SAVE_EXPR only to determine how it was
   calculated; it is not safe for fold to do much of anything else with the
   internals of a SAVE_EXPR, since it cannot know when it will be evaluated
   at run time.  For example, the latter example above *cannot* be implemented
   as SAVE_EXPR (I) * J or any variant thereof, since the value of J at
   evaluation time of the original SAVE_EXPR is not necessarily the same at
   the time the new expression is evaluated.  The only optimization of this
   sort that would be valid is changing

     SAVE_EXPR (I) * SAVE_EXPR (SAVE_EXPR (J) * 8)

   divided by 8 to

     SAVE_EXPR (I) * SAVE_EXPR (J)

   (where the same SAVE_EXPR (J) is used in the original and the
   transformed version).  */

static int
multiple_of_p (type, top, bottom)
     tree type;
     tree top;
     tree bottom;
{
  if (operand_equal_p (top, bottom, 0))
    return 1;

  if (TREE_CODE (type) != INTEGER_TYPE)
    return 0;

  switch (TREE_CODE (top))
    {
    case MULT_EXPR:
      return (multiple_of_p (type, TREE_OPERAND (top, 0), bottom)
	      || multiple_of_p (type, TREE_OPERAND (top, 1), bottom));

    case PLUS_EXPR:
    case MINUS_EXPR:
      return (multiple_of_p (type, TREE_OPERAND (top, 0), bottom)
	      && multiple_of_p (type, TREE_OPERAND (top, 1), bottom));

    case LSHIFT_EXPR:
      if (TREE_CODE (TREE_OPERAND (top, 1)) == INTEGER_CST)
	{
	  tree op1, t1;

	  op1 = TREE_OPERAND (top, 1);
	  /* const_binop may not detect overflow correctly,
	     so check for it explicitly here.  */
	  if (TYPE_PRECISION (TREE_TYPE (size_one_node))
	      > TREE_INT_CST_LOW (op1)
	      && TREE_INT_CST_HIGH (op1) == 0
	      && 0 != (t1 = convert (type,
				     const_binop (LSHIFT_EXPR, size_one_node,
						  op1, 0)))
	      && ! TREE_OVERFLOW (t1))
	    return multiple_of_p (type, t1, bottom);
	}
      return 0;

    case NOP_EXPR:
      /* Can't handle conversions from non-integral or wider integral type.  */
      if ((TREE_CODE (TREE_TYPE (TREE_OPERAND (top, 0))) != INTEGER_TYPE)
	  || (TYPE_PRECISION (type)
	      < TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (top, 0)))))
	return 0;

      /* .. fall through ...  */

    case SAVE_EXPR:
      return multiple_of_p (type, TREE_OPERAND (top, 0), bottom);

    case INTEGER_CST:
      if (TREE_CODE (bottom) != INTEGER_CST
	  || (TREE_UNSIGNED (type)
	      && (tree_int_cst_sgn (top) < 0
		  || tree_int_cst_sgn (bottom) < 0)))
	return 0;
      return integer_zerop (const_binop (TRUNC_MOD_EXPR,
					 top, bottom, 0));

    default:
      return 0;
    }
}

/* Return true if `t' is known to be non-negative.  */

int
tree_expr_nonnegative_p (t)
     tree t;
{
  switch (TREE_CODE (t))
    {
    case ABS_EXPR:
    case FFS_EXPR:
      return 1;
    case INTEGER_CST:
      return tree_int_cst_sgn (t) >= 0;
    case TRUNC_DIV_EXPR:
    case CEIL_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case ROUND_DIV_EXPR:
      return tree_expr_nonnegative_p (TREE_OPERAND (t, 0))
        && tree_expr_nonnegative_p (TREE_OPERAND (t, 1));
    case TRUNC_MOD_EXPR:
    case CEIL_MOD_EXPR:
    case FLOOR_MOD_EXPR:
    case ROUND_MOD_EXPR:
      return tree_expr_nonnegative_p (TREE_OPERAND (t, 0));
    case COND_EXPR:
      return tree_expr_nonnegative_p (TREE_OPERAND (t, 1))
	&& tree_expr_nonnegative_p (TREE_OPERAND (t, 2));
    case COMPOUND_EXPR:
      return tree_expr_nonnegative_p (TREE_OPERAND (t, 1));
    case MIN_EXPR:
      return tree_expr_nonnegative_p (TREE_OPERAND (t, 0))
        && tree_expr_nonnegative_p (TREE_OPERAND (t, 1));
    case MAX_EXPR:
      return tree_expr_nonnegative_p (TREE_OPERAND (t, 0))
        || tree_expr_nonnegative_p (TREE_OPERAND (t, 1));
    case MODIFY_EXPR:
      return tree_expr_nonnegative_p (TREE_OPERAND (t, 1));
    case BIND_EXPR:
      return tree_expr_nonnegative_p (TREE_OPERAND (t, 1));
    case SAVE_EXPR:
      return tree_expr_nonnegative_p (TREE_OPERAND (t, 0));
    case NON_LVALUE_EXPR:
      return tree_expr_nonnegative_p (TREE_OPERAND (t, 0));
    case RTL_EXPR:
      return rtl_expr_nonnegative_p (RTL_EXPR_RTL (t));
      
    default:
      if (truth_value_p (TREE_CODE (t)))
	/* Truth values evaluate to 0 or 1, which is nonnegative.  */
	return 1;
      else
	/* We don't know sign of `t', so be conservative and return false.  */
	return 0;
    }
}

/* Return true if `r' is known to be non-negative.
   Only handles constants at the moment.  */

int
rtl_expr_nonnegative_p (r)
     rtx r;
{
  switch (GET_CODE (r))
    {
    case CONST_INT:
      return INTVAL (r) >= 0;

    case CONST_DOUBLE:
      if (GET_MODE (r) == VOIDmode)
	return CONST_DOUBLE_HIGH (r) >= 0;
      return 0;

    case CONST_VECTOR:
      {
	int units, i;
	rtx elt;

	units = CONST_VECTOR_NUNITS (r);

	for (i = 0; i < units; ++i)
	  {
	    elt = CONST_VECTOR_ELT (r, i);
	    if (!rtl_expr_nonnegative_p (elt))
	      return 0;
	  }

	return 1;
      }

    case SYMBOL_REF:
    case LABEL_REF:
      /* These are always nonnegative.  */
      return 1;

    default:
      return 0;
    }
}
