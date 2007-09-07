/* Fixed-point arithmetic support.
   Copyright (C) 2006, 2007 Free Software Foundation, Inc.

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
#include "tm.h"
#include "tree.h"
#include "toplev.h"
#include "fixed-value.h"

/* Compare two fixed objects for bitwise identity.  */

bool
fixed_identical (const FIXED_VALUE_TYPE *a, const FIXED_VALUE_TYPE *b)
{
  return (a->mode == b->mode
	  && a->data.high == b->data.high
	  && a->data.low == b->data.low);
}

/* Calculate a hash value.  */

unsigned int
fixed_hash (const FIXED_VALUE_TYPE *f)
{
  return (unsigned int) (f->data.low ^ f->data.high);
}

/* Define the enum code for the range of the fixed-point value.  */
enum fixed_value_range_code {
  FIXED_OK,		/* The value is within the range.  */
  FIXED_UNDERFLOW,	/* The value is less than the minimum.  */
  FIXED_GT_MAX_EPS,	/* The value is greater than the maximum, but not equal
			   to the maximum plus the epsilon.  */
  FIXED_MAX_EPS		/* The value equals the maximum plus the epsilon.  */
};

/* Check REAL_VALUE against the range of the fixed-point mode.
   Return FIXED_OK, if it is within the range.
          FIXED_UNDERFLOW, if it is less than the minimum.
          FIXED_GT_MAX_EPS, if it is greater than the maximum, but not equal to
	    the maximum plus the epsilon.
          FIXED_MAX_EPS, if it is equal to the maximum plus the epsilon.  */

static enum fixed_value_range_code
check_real_for_fixed_mode (REAL_VALUE_TYPE *real_value, enum machine_mode mode)
{
  REAL_VALUE_TYPE max_value, min_value, epsilon_value;

  real_2expN (&max_value, GET_MODE_IBIT (mode), mode);
  real_2expN (&epsilon_value, -GET_MODE_FBIT (mode), mode);

  if (SIGNED_FIXED_POINT_MODE_P (mode))
    min_value = REAL_VALUE_NEGATE (max_value);
  else
    real_from_string (&min_value, "0.0");

  if (real_compare (LT_EXPR, real_value, &min_value))
    return FIXED_UNDERFLOW;
  if (real_compare (EQ_EXPR, real_value, &max_value))
    return FIXED_MAX_EPS;
  real_arithmetic (&max_value, MINUS_EXPR, &max_value, &epsilon_value);
  if (real_compare (GT_EXPR, real_value, &max_value))
    return FIXED_GT_MAX_EPS;
  return FIXED_OK;
}

/* Initialize from a decimal or hexadecimal string.  */

void
fixed_from_string (FIXED_VALUE_TYPE *f, const char *str, enum machine_mode mode)
{
  REAL_VALUE_TYPE real_value, fixed_value, base_value;
  unsigned int fbit;
  enum fixed_value_range_code temp;

  f->mode = mode;
  fbit = GET_MODE_FBIT (mode);

  real_from_string (&real_value, str);
  temp = check_real_for_fixed_mode (&real_value, f->mode);
  /* We don't want to warn the case when the _Fract value is 1.0.  */
  if (temp == FIXED_UNDERFLOW
      || temp == FIXED_GT_MAX_EPS
      || (temp == FIXED_MAX_EPS && ALL_ACCUM_MODE_P (f->mode)))
    warning (OPT_Woverflow,
	     "large fixed-point constant implicitly truncated to fixed-point type");
  real_2expN (&base_value, fbit, mode);
  real_arithmetic (&fixed_value, MULT_EXPR, &real_value, &base_value);
  real_to_integer2 ((HOST_WIDE_INT *)&f->data.low, &f->data.high,
		    &fixed_value);

  if (temp == FIXED_MAX_EPS && ALL_FRACT_MODE_P (f->mode))
    {
      /* From the spec, we need to evaluate 1 to the maximal value.  */
      f->data.low = -1;
      f->data.high = -1;
      f->data = double_int_ext (f->data,
				GET_MODE_FBIT (f->mode)
				+ GET_MODE_IBIT (f->mode), 1);
    }
  else
    f->data = double_int_ext (f->data,
			      SIGNED_FIXED_POINT_MODE_P (f->mode)
			      + GET_MODE_FBIT (f->mode)
			      + GET_MODE_IBIT (f->mode),
			      UNSIGNED_FIXED_POINT_MODE_P (f->mode));
}

/* Render F as a decimal floating point constant.  */

void
fixed_to_decimal (char *str, const FIXED_VALUE_TYPE *f_orig,
		  size_t buf_size)
{
  REAL_VALUE_TYPE real_value, base_value, fixed_value;

  real_2expN (&base_value, GET_MODE_FBIT (f_orig->mode), f_orig->mode);
  real_from_integer (&real_value, VOIDmode, f_orig->data.low, f_orig->data.high,
		     UNSIGNED_FIXED_POINT_MODE_P (f_orig->mode));
  real_arithmetic (&fixed_value, RDIV_EXPR, &real_value, &base_value);
  real_to_decimal (str, &fixed_value, buf_size, 0, 1);
}

/* If SAT_P, saturate A to the maximum or the minimum, and save to *F based on
   the machine mode MODE.
   Do not modify *F otherwise.
   This function assumes the width of double_int is greater than the width
   of the fixed-point value (the sum of a possible sign bit, possible ibits,
   and fbits).
   Return true, if !SAT_P and overflow.  */

static bool
fixed_saturate1 (enum machine_mode mode, double_int a, double_int *f,
		 bool sat_p)
{
  bool overflow_p = false;
  bool unsigned_p = UNSIGNED_FIXED_POINT_MODE_P (mode);
  int i_f_bits = GET_MODE_IBIT (mode) + GET_MODE_FBIT (mode);

  if (unsigned_p) /* Unsigned type.  */
    {
      double_int max;
      max.low = -1;
      max.high = -1;
      max = double_int_ext (max, i_f_bits, 1);
      if (double_int_cmp (a, max, 1) == 1)
	{
	  if (sat_p)
	    *f = max;
	  else
	    overflow_p = true;
	}
    }
  else /* Signed type.  */
    {
      double_int max, min;
      max.high = -1;
      max.low = -1;
      max = double_int_ext (max, i_f_bits, 1);
      min.high = 0;
      min.low = 1;
      lshift_double (min.low, min.high, i_f_bits,
		     2 * HOST_BITS_PER_WIDE_INT,
		     &min.low, &min.high, 1);
      min = double_int_ext (min, 1 + i_f_bits, 0);
      if (double_int_cmp (a, max, 0) == 1)
	{
	  if (sat_p)
	    *f = max;
	  else
	    overflow_p = true;
	}
      else if (double_int_cmp (a, min, 0) == -1)
	{
	  if (sat_p)
	    *f = min;
	  else
	    overflow_p = true;
	}
    }
  return overflow_p;
}

/* If SAT_P, saturate {A_HIGH, A_LOW} to the maximum or the minimum, and
   save to *F based on the machine mode MODE.
   Do not modify *F otherwise.
   This function assumes the width of two double_int is greater than the width
   of the fixed-point value (the sum of a possible sign bit, possible ibits,
   and fbits).
   Return true, if !SAT_P and overflow.  */

static bool
fixed_saturate2 (enum machine_mode mode, double_int a_high, double_int a_low,
		 double_int *f, bool sat_p)
{
  bool overflow_p = false;
  bool unsigned_p = UNSIGNED_FIXED_POINT_MODE_P (mode);
  int i_f_bits = GET_MODE_IBIT (mode) + GET_MODE_FBIT (mode);

  if (unsigned_p) /* Unsigned type.  */
    {
      double_int max_r, max_s;
      max_r.high = 0;
      max_r.low = 0;
      max_s.high = -1;
      max_s.low = -1;
      max_s = double_int_ext (max_s, i_f_bits, 1);
      if (double_int_cmp (a_high, max_r, 1) == 1
	  || (double_int_equal_p (a_high, max_r) &&
	      double_int_cmp (a_low, max_s, 1) == 1))
	{
	  if (sat_p)
	    *f = max_s;
	  else
	    overflow_p = true;
	}
    }
  else /* Signed type.  */
    {
      double_int max_r, max_s, min_r, min_s;
      max_r.high = 0;
      max_r.low = 0;
      max_s.high = -1;
      max_s.low = -1;
      max_s = double_int_ext (max_s, i_f_bits, 1);
      min_r.high = -1;
      min_r.low = -1;
      min_s.high = 0;
      min_s.low = 1;
      lshift_double (min_s.low, min_s.high, i_f_bits,
		     2 * HOST_BITS_PER_WIDE_INT,
		     &min_s.low, &min_s.high, 1);
      min_s = double_int_ext (min_s, 1 + i_f_bits, 0);
      if (double_int_cmp (a_high, max_r, 0) == 1
	  || (double_int_equal_p (a_high, max_r) &&
	      double_int_cmp (a_low, max_s, 1) == 1))
	{
	  if (sat_p)
	    *f = max_s;
	  else
	    overflow_p = true;
	}
      else if (double_int_cmp (a_high, min_r, 0) == -1
	       || (double_int_equal_p (a_high, min_r) &&
		   double_int_cmp (a_low, min_s, 1) == -1))
	{
	  if (sat_p)
	    *f = min_s;
	  else
	    overflow_p = true;
	}
    }
  return overflow_p;
}

/* Return the sign bit based on I_F_BITS.  */

static inline int
get_fixed_sign_bit (double_int a, int i_f_bits)
{
  if (i_f_bits < HOST_BITS_PER_WIDE_INT)
    return (a.low >> i_f_bits) & 1;
  else
    return (a.high >> (i_f_bits - HOST_BITS_PER_WIDE_INT)) & 1;
}

/* Calculate F = A + (SUBTRACT_P ? -B : B).
   If SAT_P, saturate the result to the max or the min.
   Return true, if !SAT_P and overflow.  */

static bool
do_fixed_add (FIXED_VALUE_TYPE *f, const FIXED_VALUE_TYPE *a,
	      const FIXED_VALUE_TYPE *b, bool subtract_p, bool sat_p)
{
  bool overflow_p = false;
  double_int temp = subtract_p ? double_int_neg (b->data) : b->data;
  bool unsigned_p = UNSIGNED_FIXED_POINT_MODE_P (a->mode);
  int i_f_bits = GET_MODE_IBIT (a->mode) + GET_MODE_FBIT (a->mode);
  f->mode = a->mode;
  f->data = double_int_add (a->data, temp);
  if (unsigned_p) /* Unsigned type.  */
    {
      if (subtract_p) /* Unsigned subtraction.  */
	{
	  if (double_int_cmp (a->data, b->data, 1) == -1)
	    {
	      if (sat_p)
		{
		  f->data.high = 0;
		  f->data.low = 0;
		 }
	      else
		overflow_p = true;
	    }
	}
      else /* Unsigned addition.  */
	{
	  f->data = double_int_ext (f->data, i_f_bits, 1);
	  if (double_int_cmp (f->data, a->data, 1) == -1
	      || double_int_cmp (f->data, b->data, 1) == -1)
	    {
	      if (sat_p)
		{
		  f->data.high = -1;
		  f->data.low = -1;
		}
	      else
		overflow_p = true;
	    }
	}
    }
  else /* Signed type.  */
    {
      if ((!subtract_p
	   && (get_fixed_sign_bit (a->data, i_f_bits)
	       == get_fixed_sign_bit (b->data, i_f_bits))
	   && (get_fixed_sign_bit (a->data, i_f_bits)
	       != get_fixed_sign_bit (f->data, i_f_bits)))
	  || (subtract_p
	      && (get_fixed_sign_bit (a->data, i_f_bits)
		  != get_fixed_sign_bit (b->data, i_f_bits))
	      && (get_fixed_sign_bit (a->data, i_f_bits)
		  != get_fixed_sign_bit (f->data, i_f_bits))))
	{
	  if (sat_p)
	    {
	      f->data.low = 1;
	      f->data.high = 0;
	      lshift_double (f->data.low, f->data.high, i_f_bits,
			     2 * HOST_BITS_PER_WIDE_INT,
			     &f->data.low, &f->data.high, 1);
	      if (get_fixed_sign_bit (a->data, i_f_bits) == 0)
		{
		  double_int one;
		  one.low = 1;
		  one.high = 0;
		  f->data = double_int_add (f->data, double_int_neg (one));
		}
	    }
	  else
	    overflow_p = true;
	}
    }
  f->data = double_int_ext (f->data, (!unsigned_p) + i_f_bits, unsigned_p);
  return overflow_p;
}

/* Calculate F = A * B.
   If SAT_P, saturate the result to the max or the min.
   Return true, if !SAT_P and overflow.  */

static bool
do_fixed_multiply (FIXED_VALUE_TYPE *f, const FIXED_VALUE_TYPE *a,
		   const FIXED_VALUE_TYPE *b, bool sat_p)
{
  bool overflow_p = false;
  bool unsigned_p = UNSIGNED_FIXED_POINT_MODE_P (a->mode);
  int i_f_bits = GET_MODE_IBIT (a->mode) + GET_MODE_FBIT (a->mode);
  f->mode = a->mode;
  if (GET_MODE_PRECISION (f->mode) <= HOST_BITS_PER_WIDE_INT)
    {
      f->data = double_int_mul (a->data, b->data);
      lshift_double (f->data.low, f->data.high,
		     (-GET_MODE_FBIT (f->mode)),
		     2 * HOST_BITS_PER_WIDE_INT,
		     &f->data.low, &f->data.high, !unsigned_p);
      overflow_p = fixed_saturate1 (f->mode, f->data, &f->data, sat_p);
    }
  else
    {
      /* The result of multiplication expands to two double_int.  */
      double_int a_high, a_low, b_high, b_low;
      double_int high_high, high_low, low_high, low_low;
      double_int r, s, temp1, temp2;
      int carry = 0;

      /* Decompose a and b to four double_int.  */
      a_high.low = a->data.high;
      a_high.high = 0;
      a_low.low = a->data.low;
      a_low.high = 0;
      b_high.low = b->data.high;
      b_high.high = 0;
      b_low.low = b->data.low;
      b_low.high = 0;

      /* Perform four multiplications.  */
      low_low = double_int_mul (a_low, b_low);
      low_high = double_int_mul (a_low, b_high);
      high_low = double_int_mul (a_high, b_low);
      high_high = double_int_mul (a_high, b_high);

      /* Accumulate four results to {r, s}.  */
      temp1.high = high_low.low;
      temp1.low = 0;
      s = double_int_add (low_low, temp1);
      if (double_int_cmp (s, low_low, 1) == -1
	  || double_int_cmp (s, temp1, 1) == -1)
	carry ++; /* Carry */
      temp1.high = s.high;
      temp1.low = s.low;
      temp2.high = low_high.low;
      temp2.low = 0;
      s = double_int_add (temp1, temp2);
      if (double_int_cmp (s, temp1, 1) == -1
	  || double_int_cmp (s, temp2, 1) == -1)
	carry ++; /* Carry */

      temp1.low = high_low.high;
      temp1.high = 0;
      r = double_int_add (high_high, temp1);
      temp1.low = low_high.high;
      temp1.high = 0;
      r = double_int_add (r, temp1);
      temp1.low = carry;
      temp1.high = 0;
      r = double_int_add (r, temp1);

      /* We need to add neg(b) to r, if a < 0.  */
      if (!unsigned_p && a->data.high < 0)
	r = double_int_add (r, double_int_neg (b->data));
      /* We need to add neg(a) to r, if b < 0.  */
      if (!unsigned_p && b->data.high < 0)
	r = double_int_add (r, double_int_neg (a->data));

      /* Shift right the result by FBIT.  */
      if (GET_MODE_FBIT (f->mode) == 2 * HOST_BITS_PER_WIDE_INT)
	{
	  s.low = r.low;
	  s.high = r.high;
	  if (unsigned_p)
	    {
	      r.low = 0;
	      r.high = 0;
	    }
	  else
	    {
	      r.low = -1;
	      r.high = -1;
	    }
	  f->data.low = s.low;
	  f->data.high = s.high;
	}
      else
	{
	  lshift_double (s.low, s.high,
			 (-GET_MODE_FBIT (f->mode)),
			 2 * HOST_BITS_PER_WIDE_INT,
			 &s.low, &s.high, 0);
	  lshift_double (r.low, r.high,
			 (2 * HOST_BITS_PER_WIDE_INT
			  - GET_MODE_FBIT (f->mode)),
			 2 * HOST_BITS_PER_WIDE_INT,
			 &f->data.low, &f->data.high, 0);
	  f->data.low = f->data.low | s.low;
	  f->data.high = f->data.high | s.high;
	  s.low = f->data.low;
	  s.high = f->data.high;
	  lshift_double (r.low, r.high,
			 (-GET_MODE_FBIT (f->mode)),
			 2 * HOST_BITS_PER_WIDE_INT,
			 &r.low, &r.high, !unsigned_p);
	}

      overflow_p = fixed_saturate2 (f->mode, r, s, &f->data, sat_p);
    }

  f->data = double_int_ext (f->data, (!unsigned_p) + i_f_bits, unsigned_p);
  return overflow_p;
}

/* Calculate F = A / B.
   If SAT_P, saturate the result to the max or the min.
   Return true, if !SAT_P and overflow.  */

static bool
do_fixed_divide (FIXED_VALUE_TYPE *f, const FIXED_VALUE_TYPE *a,
		 const FIXED_VALUE_TYPE *b, bool sat_p)
{
  bool overflow_p = false;
  bool unsigned_p = UNSIGNED_FIXED_POINT_MODE_P (a->mode);
  int i_f_bits = GET_MODE_IBIT (a->mode) + GET_MODE_FBIT (a->mode);
  f->mode = a->mode;
  if (GET_MODE_PRECISION (f->mode) <= HOST_BITS_PER_WIDE_INT)
    {
      lshift_double (a->data.low, a->data.high,
		     GET_MODE_FBIT (f->mode),
		     2 * HOST_BITS_PER_WIDE_INT,
		     &f->data.low, &f->data.high, !unsigned_p);
      f->data = double_int_div (f->data, b->data, unsigned_p, TRUNC_DIV_EXPR);
      overflow_p = fixed_saturate1 (f->mode, f->data, &f->data, sat_p);
    }
  else
    {
      double_int pos_a, pos_b, r, s;
      double_int quo_r, quo_s, mod, temp;
      int num_of_neg = 0;
      int i;

      /* If a < 0, negate a.  */
      if (!unsigned_p && a->data.high < 0)
	{
	  pos_a = double_int_neg (a->data);
	  num_of_neg ++;
	}
      else
	pos_a = a->data;

      /* If b < 0, negate b.  */
      if (!unsigned_p && b->data.high < 0)
	{
	  pos_b = double_int_neg (b->data);
	  num_of_neg ++;
	}
      else
	pos_b = b->data;

      /* Left shift pos_a to {r, s} by FBIT.  */
      if (GET_MODE_FBIT (f->mode) == 2 * HOST_BITS_PER_WIDE_INT)
	{
	  r = pos_a;
	  s.high = 0;
	  s.low = 0;
	}
      else
 	{
	  lshift_double (pos_a.low, pos_a.high,
			 GET_MODE_FBIT (f->mode),
			 2 * HOST_BITS_PER_WIDE_INT,
			 &s.low, &s.high, 0);
	  lshift_double (pos_a.low, pos_a.high,
			 - (2 * HOST_BITS_PER_WIDE_INT
			    - GET_MODE_FBIT (f->mode)),
			 2 * HOST_BITS_PER_WIDE_INT,
			 &r.low, &r.high, 0);
 	}

      /* Divide r by pos_b to quo_r.  The remainder is in mod.  */
      div_and_round_double (TRUNC_DIV_EXPR, 1, r.low, r.high, pos_b.low,
			    pos_b.high, &quo_r.low, &quo_r.high, &mod.low,
			    &mod.high);

      quo_s.high = 0;
      quo_s.low = 0;

      for (i = 0; i < 2 * HOST_BITS_PER_WIDE_INT; i++)
	{
	  /* Record the leftmost bit of mod.  */
	  int leftmost_mod = (mod.high < 0);

	  /* Shift left mod by 1 bit.  */
	  lshift_double (mod.low, mod.high, 1, 2 * HOST_BITS_PER_WIDE_INT,
			 &mod.low, &mod.high, 0);

	  /* Test the leftmost bit of s to add to mod.  */
	  if (s.high < 0)
	    mod.low += 1;

	  /* Shift left quo_s by 1 bit.  */
	  lshift_double (quo_s.low, quo_s.high, 1, 2 * HOST_BITS_PER_WIDE_INT,
			 &quo_s.low, &quo_s.high, 0);

	  /* Try to calculate (mod - pos_b).  */
	  temp = double_int_add (mod, double_int_neg (pos_b));

	  if (leftmost_mod == 1 || double_int_cmp (mod, pos_b, 1) != -1)
	    {
	      quo_s.low += 1;
	      mod = temp;
	    }

	  /* Shift left s by 1 bit.  */
	  lshift_double (s.low, s.high, 1, 2 * HOST_BITS_PER_WIDE_INT,
			 &s.low, &s.high, 0);

	}

      if (num_of_neg == 1)
	{
	  quo_s = double_int_neg (quo_s);
	  if (quo_s.high == 0 && quo_s.low == 0)
	    quo_r = double_int_neg (quo_r);
	  else
	    {
	      quo_r.low = ~quo_r.low;
	      quo_r.high = ~quo_r.high;
	    }
	}

      f->data = quo_s;
      overflow_p = fixed_saturate2 (f->mode, quo_r, quo_s, &f->data, sat_p);
    }

  f->data = double_int_ext (f->data, (!unsigned_p) + i_f_bits, unsigned_p);
  return overflow_p;
}

/* Calculate F = A << B if LEFT_P.  Otherwise, F = A >> B.
   If SAT_P, saturate the result to the max or the min.
   Return true, if !SAT_P and overflow.  */

static bool
do_fixed_shift (FIXED_VALUE_TYPE *f, const FIXED_VALUE_TYPE *a,
	      const FIXED_VALUE_TYPE *b, bool left_p, bool sat_p)
{
  bool overflow_p = false;
  bool unsigned_p = UNSIGNED_FIXED_POINT_MODE_P (a->mode);
  int i_f_bits = GET_MODE_IBIT (a->mode) + GET_MODE_FBIT (a->mode);
  f->mode = a->mode;

  if (b->data.low == 0)
    {
      f->data = a->data;
      return overflow_p;
    }

  if (GET_MODE_PRECISION (f->mode) <= HOST_BITS_PER_WIDE_INT || (!left_p))
    {
      lshift_double (a->data.low, a->data.high,
		     left_p ? b->data.low : (-b->data.low),
		     2 * HOST_BITS_PER_WIDE_INT,
		     &f->data.low, &f->data.high, !unsigned_p);
      if (left_p) /* Only left shift saturates.  */
	overflow_p = fixed_saturate1 (f->mode, f->data, &f->data, sat_p);
    }
  else /* We need two double_int to store the left-shift result.  */
    {
      double_int temp_high, temp_low;
      if (b->data.low == 2 * HOST_BITS_PER_WIDE_INT)
	{
	  temp_high = a->data;
	  temp_low.high = 0;
	  temp_low.low = 0;
	}
      else
	{
	  lshift_double (a->data.low, a->data.high,
			 b->data.low,
			 2 * HOST_BITS_PER_WIDE_INT,
			 &temp_low.low, &temp_low.high, !unsigned_p);
	  /* Logical shift right to temp_high.  */
	  lshift_double (a->data.low, a->data.high,
			 b->data.low - 2 * HOST_BITS_PER_WIDE_INT,
			 2 * HOST_BITS_PER_WIDE_INT,
			 &temp_high.low, &temp_high.high, 0);
	}
      if (!unsigned_p && a->data.high < 0) /* Signed-extend temp_high.  */
	temp_high = double_int_ext (temp_high, b->data.low, unsigned_p);
      f->data = temp_low;
      overflow_p = fixed_saturate2 (f->mode, temp_high, temp_low, &f->data,
				    sat_p);
    }
  f->data = double_int_ext (f->data, (!unsigned_p) + i_f_bits, unsigned_p);
  return overflow_p;
}

/* Calculate F = -A.
   If SAT_P, saturate the result to the max or the min.
   Return true, if !SAT_P and overflow.  */

static bool
do_fixed_neg (FIXED_VALUE_TYPE *f, const FIXED_VALUE_TYPE *a, bool sat_p)
{
  bool overflow_p = false;
  bool unsigned_p = UNSIGNED_FIXED_POINT_MODE_P (a->mode);
  int i_f_bits = GET_MODE_IBIT (a->mode) + GET_MODE_FBIT (a->mode);
  f->mode = a->mode;
  f->data = double_int_neg (a->data);
  f->data = double_int_ext (f->data, (!unsigned_p) + i_f_bits, unsigned_p);

  if (unsigned_p) /* Unsigned type.  */
    {
      if (f->data.low != 0 || f->data.high != 0)
	{
	  if (sat_p)
	    {
	      f->data.low = 0;
	      f->data.high = 0;
	    }
	  else
	    overflow_p = true;
	}
    }
  else /* Signed type.  */
    {
      if (!(f->data.high == 0 && f->data.low == 0)
	  && f->data.high == a->data.high && f->data.low == a->data.low )
	{
	  if (sat_p)
	    {
	      /* Saturate to the maximum by subtracting f->data by one.  */
	      f->data.low = -1;
	      f->data.high = -1;
	      f->data = double_int_ext (f->data, i_f_bits, 1);
	    }
	  else
	    overflow_p = true;
	}
    }
  return overflow_p;
}

/* Perform the binary or unary operation described by CODE.
   Note that OP0 and OP1 must have the same mode for binary operators.
   For a unary operation, leave OP1 NULL.
   Return true, if !SAT_P and overflow.  */

bool
fixed_arithmetic (FIXED_VALUE_TYPE *f, int icode, const FIXED_VALUE_TYPE *op0,
		  const FIXED_VALUE_TYPE *op1, bool sat_p)
{
  switch (icode)
    {
    case NEGATE_EXPR:
      return do_fixed_neg (f, op0, sat_p);
      break;

    case PLUS_EXPR:
      gcc_assert (op0->mode == op1->mode);
      return do_fixed_add (f, op0, op1, false, sat_p);
      break;

    case MINUS_EXPR:
      gcc_assert (op0->mode == op1->mode);
      return do_fixed_add (f, op0, op1, true, sat_p);
      break;

    case MULT_EXPR:
      gcc_assert (op0->mode == op1->mode);
      return do_fixed_multiply (f, op0, op1, sat_p);
      break;

    case TRUNC_DIV_EXPR:
      gcc_assert (op0->mode == op1->mode);
      return do_fixed_divide (f, op0, op1, sat_p);
      break;

    case LSHIFT_EXPR:
      return do_fixed_shift (f, op0, op1, true, sat_p);
      break;

    case RSHIFT_EXPR:
      return do_fixed_shift (f, op0, op1, false, sat_p);
      break;

    default:
      gcc_unreachable ();
    }
  return false;
}

/* Compare fixed-point values by tree_code.
   Note that OP0 and OP1 must have the same mode.  */

bool
fixed_compare (int icode, const FIXED_VALUE_TYPE *op0,
	       const FIXED_VALUE_TYPE *op1)
{
  enum tree_code code = icode;
  gcc_assert (op0->mode == op1->mode);

  switch (code)
    {
    case NE_EXPR:
      return !double_int_equal_p (op0->data, op1->data);

    case EQ_EXPR:
      return double_int_equal_p (op0->data, op1->data);

    case LT_EXPR:
      return double_int_cmp (op0->data, op1->data,
			     UNSIGNED_FIXED_POINT_MODE_P (op0->mode)) == -1;

    case LE_EXPR:
      return double_int_cmp (op0->data, op1->data,
			     UNSIGNED_FIXED_POINT_MODE_P (op0->mode)) != 1;

    case GT_EXPR:
      return double_int_cmp (op0->data, op1->data,
			     UNSIGNED_FIXED_POINT_MODE_P (op0->mode)) == 1;

    case GE_EXPR:
      return double_int_cmp (op0->data, op1->data,
			     UNSIGNED_FIXED_POINT_MODE_P (op0->mode)) != -1;

    default:
      gcc_unreachable ();
    }
}

/* Extend or truncate to a new mode.
   If SAT_P, saturate the result to the max or the min.
   Return true, if !SAT_P and overflow.  */

bool
fixed_convert (FIXED_VALUE_TYPE *f, enum machine_mode mode,
               const FIXED_VALUE_TYPE *a, bool sat_p)
{
  bool overflow_p = false;
  if (mode == a->mode)
    {
      *f = *a;
      return overflow_p;
    }

  if (GET_MODE_FBIT (mode) > GET_MODE_FBIT (a->mode))
    {
      /* Left shift a to temp_high, temp_low based on a->mode.  */
      double_int temp_high, temp_low;
      int amount = GET_MODE_FBIT (mode) - GET_MODE_FBIT (a->mode);
      lshift_double (a->data.low, a->data.high,
		     amount,
		     2 * HOST_BITS_PER_WIDE_INT,
		     &temp_low.low, &temp_low.high,
		     SIGNED_FIXED_POINT_MODE_P (a->mode));
      /* Logical shift right to temp_high.  */
      lshift_double (a->data.low, a->data.high,
		     amount - 2 * HOST_BITS_PER_WIDE_INT,
		     2 * HOST_BITS_PER_WIDE_INT,
		     &temp_high.low, &temp_high.high, 0);
      if (SIGNED_FIXED_POINT_MODE_P (a->mode)
	  && a->data.high < 0) /* Signed-extend temp_high.  */
	temp_high = double_int_ext (temp_high, amount, 0);
      f->mode = mode;
      f->data = temp_low;
      if (SIGNED_FIXED_POINT_MODE_P (a->mode) ==
	  SIGNED_FIXED_POINT_MODE_P (f->mode))
	overflow_p = fixed_saturate2 (f->mode, temp_high, temp_low, &f->data,
				      sat_p);
      else
	{
	  /* Take care of the cases when converting between signed and
	     unsigned.  */
	  if (SIGNED_FIXED_POINT_MODE_P (a->mode))
	    {
	      /* Signed -> Unsigned.  */
	      if (a->data.high < 0)
		{
		  if (sat_p)
		    {
		      f->data.low = 0;  /* Set to zero.  */
		      f->data.high = 0;  /* Set to zero.  */
		    }
		  else
		    overflow_p = true;
		}
	      else
		overflow_p = fixed_saturate2 (f->mode, temp_high, temp_low,
					      &f->data, sat_p);
	    }
	  else
	    {
	      /* Unsigned -> Signed.  */
	      if (temp_high.high < 0)
		{
		  if (sat_p)
		    {
		      /* Set to maximum.  */
		      f->data.low = -1;  /* Set to all ones.  */
		      f->data.high = -1;  /* Set to all ones.  */
		      f->data = double_int_ext (f->data,
						GET_MODE_FBIT (f->mode)
						+ GET_MODE_IBIT (f->mode),
						1); /* Clear the sign.  */
		    }
		  else
		    overflow_p = true;
		}
	      else
		overflow_p = fixed_saturate2 (f->mode, temp_high, temp_low,
					      &f->data, sat_p);
	    }
	}
    }
  else
    {
      /* Right shift a to temp based on a->mode.  */
      double_int temp;
      lshift_double (a->data.low, a->data.high,
		     GET_MODE_FBIT (mode) - GET_MODE_FBIT (a->mode),
		     2 * HOST_BITS_PER_WIDE_INT,
		     &temp.low, &temp.high,
		     SIGNED_FIXED_POINT_MODE_P (a->mode));
      f->mode = mode;
      f->data = temp;
      if (SIGNED_FIXED_POINT_MODE_P (a->mode) ==
	  SIGNED_FIXED_POINT_MODE_P (f->mode))
	overflow_p = fixed_saturate1 (f->mode, f->data, &f->data, sat_p);
      else
	{
	  /* Take care of the cases when converting between signed and
	     unsigned.  */
	  if (SIGNED_FIXED_POINT_MODE_P (a->mode))
	    {
	      /* Signed -> Unsigned.  */
	      if (a->data.high < 0)
		{
		  if (sat_p)
		    {
		      f->data.low = 0;  /* Set to zero.  */
		      f->data.high = 0;  /* Set to zero.  */
		    }
		  else
		    overflow_p = true;
		}
	      else
		overflow_p = fixed_saturate1 (f->mode, f->data, &f->data,
					      sat_p);
	    }
	  else
	    {
	      /* Unsigned -> Signed.  */
	      if (temp.high < 0)
		{
		  if (sat_p)
		    {
		      /* Set to maximum.  */
		      f->data.low = -1;  /* Set to all ones.  */
		      f->data.high = -1;  /* Set to all ones.  */
		      f->data = double_int_ext (f->data,
						GET_MODE_FBIT (f->mode)
						+ GET_MODE_IBIT (f->mode),
						1); /* Clear the sign.  */
		    }
		  else
		    overflow_p = true;
		}
	      else
		overflow_p = fixed_saturate1 (f->mode, f->data, &f->data,
					      sat_p);
	    }
	}
    }

  f->data = double_int_ext (f->data,
			    SIGNED_FIXED_POINT_MODE_P (f->mode)
			    + GET_MODE_FBIT (f->mode)
			    + GET_MODE_IBIT (f->mode),
			    UNSIGNED_FIXED_POINT_MODE_P (f->mode));
  return overflow_p;
}

/* Convert to a new fixed-point mode from an integer.
   If UNSIGNED_P, this integer is unsigned.
   If SAT_P, saturate the result to the max or the min.
   Return true, if !SAT_P and overflow.  */

bool
fixed_convert_from_int (FIXED_VALUE_TYPE *f, enum machine_mode mode,
			double_int a, bool unsigned_p, bool sat_p)
{
  bool overflow_p = false;
  /* Left shift a to temp_high, temp_low.  */
  double_int temp_high, temp_low;
  int amount = GET_MODE_FBIT (mode);
  if (amount == 2 * HOST_BITS_PER_WIDE_INT)
    {
       temp_high = a;
       temp_low.low = 0;
       temp_low.high = 0;
    }
  else
    {
      lshift_double (a.low, a.high,
		     amount,
		     2 * HOST_BITS_PER_WIDE_INT,
		     &temp_low.low, &temp_low.high, 0);

      /* Logical shift right to temp_high.  */
      lshift_double (a.low, a.high,
		     amount - 2 * HOST_BITS_PER_WIDE_INT,
		     2 * HOST_BITS_PER_WIDE_INT,
		     &temp_high.low, &temp_high.high, 0);
    }
  if (!unsigned_p && a.high < 0) /* Signed-extend temp_high.  */
    temp_high = double_int_ext (temp_high, amount, 0);

  f->mode = mode;
  f->data = temp_low;

  if (unsigned_p == UNSIGNED_FIXED_POINT_MODE_P (f->mode))
    overflow_p = fixed_saturate2 (f->mode, temp_high, temp_low, &f->data,
				  sat_p);
  else
    {
      /* Take care of the cases when converting between signed and unsigned.  */
      if (!unsigned_p)
	{
	  /* Signed -> Unsigned.  */
	  if (a.high < 0)
	    {
	      if (sat_p)
		{
		  f->data.low = 0;  /* Set to zero.  */
		  f->data.high = 0;  /* Set to zero.  */
		}
	      else
		overflow_p = true;
	    }
	  else
	    overflow_p = fixed_saturate2 (f->mode, temp_high, temp_low,
					  &f->data, sat_p);
	}
      else
	{
	  /* Unsigned -> Signed.  */
	  if (temp_high.high < 0)
	    {
	      if (sat_p)
		{
		  /* Set to maximum.  */
		  f->data.low = -1;  /* Set to all ones.  */
		  f->data.high = -1;  /* Set to all ones.  */
		  f->data = double_int_ext (f->data,
					    GET_MODE_FBIT (f->mode)
					    + GET_MODE_IBIT (f->mode),
					    1); /* Clear the sign.  */
		}
	      else
		overflow_p = true;
	    }
	  else
	    overflow_p = fixed_saturate2 (f->mode, temp_high, temp_low,
					  &f->data, sat_p);
	}
    }
  f->data = double_int_ext (f->data,
			    SIGNED_FIXED_POINT_MODE_P (f->mode)
			    + GET_MODE_FBIT (f->mode)
			    + GET_MODE_IBIT (f->mode),
			    UNSIGNED_FIXED_POINT_MODE_P (f->mode));
  return overflow_p;
}

/* Convert to a new fixed-point mode from a real.
   If SAT_P, saturate the result to the max or the min.
   Return true, if !SAT_P and overflow.  */

bool
fixed_convert_from_real (FIXED_VALUE_TYPE *f, enum machine_mode mode,
			 const REAL_VALUE_TYPE *a, bool sat_p)
{
  bool overflow_p = false;
  REAL_VALUE_TYPE real_value, fixed_value, base_value;
  bool unsigned_p = UNSIGNED_FIXED_POINT_MODE_P (mode);
  int i_f_bits = GET_MODE_IBIT (mode) + GET_MODE_FBIT (mode);
  unsigned int fbit = GET_MODE_FBIT (mode);
  enum fixed_value_range_code temp;

  real_value = *a;
  f->mode = mode;
  real_2expN (&base_value, fbit, mode);
  real_arithmetic (&fixed_value, MULT_EXPR, &real_value, &base_value);
  real_to_integer2 ((HOST_WIDE_INT *)&f->data.low, &f->data.high, &fixed_value);
  temp = check_real_for_fixed_mode (&real_value, mode);
  if (temp == FIXED_UNDERFLOW) /* Minimum.  */
    {
      if (sat_p)
	{
	  if (unsigned_p)
	    {
	      f->data.low = 0;
	      f->data.high = 0;
	    }
	  else
	    {
	      f->data.low = 1;
	      f->data.high = 0;
	      lshift_double (f->data.low, f->data.high, i_f_bits,
			     2 * HOST_BITS_PER_WIDE_INT,
			     &f->data.low, &f->data.high, 1);
	      f->data = double_int_ext (f->data, 1 + i_f_bits, 0);
	    }
	}
      else
	overflow_p = true;
    }
  else if (temp == FIXED_GT_MAX_EPS || temp == FIXED_MAX_EPS) /* Maximum.  */
    {
      if (sat_p)
	{
	  f->data.low = -1;
	  f->data.high = -1;
	  f->data = double_int_ext (f->data, i_f_bits, 1);
	}
      else
	overflow_p = true;
    }
  f->data = double_int_ext (f->data, (!unsigned_p) + i_f_bits, unsigned_p);
  return overflow_p;
}

/* Convert to a new real mode from a fixed-point.  */

void
real_convert_from_fixed (REAL_VALUE_TYPE *r, enum machine_mode mode,
			 const FIXED_VALUE_TYPE *f)
{
  REAL_VALUE_TYPE base_value, fixed_value, real_value;

  real_2expN (&base_value, GET_MODE_FBIT (f->mode), f->mode);
  real_from_integer (&fixed_value, VOIDmode, f->data.low, f->data.high,
		     UNSIGNED_FIXED_POINT_MODE_P (f->mode));
  real_arithmetic (&real_value, RDIV_EXPR, &fixed_value, &base_value);
  real_convert (r, mode, &real_value);
}

/* Determine whether a fixed-point value F is negative.  */

bool
fixed_isneg (const FIXED_VALUE_TYPE *f)
{
  if (SIGNED_FIXED_POINT_MODE_P (f->mode))
    {
      int i_f_bits = GET_MODE_IBIT (f->mode) + GET_MODE_FBIT (f->mode);
      int sign_bit = get_fixed_sign_bit (f->data, i_f_bits);
      if (sign_bit == 1)
	return true;
    }

  return false;
}
