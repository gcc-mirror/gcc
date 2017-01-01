/* Simple data type for real numbers for the GNU compiler.
   Copyright (C) 2002-2017 Free Software Foundation, Inc.

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

/* This library supports real numbers;
   inf and nan are NOT supported.
   It is written to be simple and fast.

   Value of sreal is
	x = sig * 2 ^ exp
   where
	sig = significant
	  (for < 64-bit machines sig = sig_lo + sig_hi * 2 ^ SREAL_PART_BITS)
	exp = exponent

   One uint64_t is used for the significant.
   Only a half of significant bits is used (in normalized sreals) so that we do
   not have problems with overflow, for example when c->sig = a->sig * b->sig.
   So the precision is 32-bit.

   Invariant: The numbers are normalized before and after each call of sreal_*.

   Normalized sreals:
   All numbers (except zero) meet following conditions:
	 SREAL_MIN_SIG <= sig && sig <= SREAL_MAX_SIG
	-SREAL_MAX_EXP <= exp && exp <= SREAL_MAX_EXP

   If the number would be too large, it is set to upper bounds of these
   conditions.

   If the number is zero or would be too small it meets following conditions:
	sig == 0 && exp == -SREAL_MAX_EXP
*/

#include "config.h"
#include "system.h"
#include <math.h>
#include "coretypes.h"
#include "sreal.h"
#include "selftest.h"

/* Print the content of struct sreal.  */

void
sreal::dump (FILE *file) const
{
  fprintf (file, "(%" PRIi64 " * 2^%d)", m_sig, m_exp);
}

DEBUG_FUNCTION void
debug (const sreal &ref)
{
  ref.dump (stderr);
}

DEBUG_FUNCTION void
debug (const sreal *ptr)
{
  if (ptr)
    debug (*ptr);
  else
    fprintf (stderr, "<nil>\n");
}

/* Shift this right by S bits.  Needed: 0 < S <= SREAL_BITS.
   When the most significant bit shifted out is 1, add 1 to this (rounding).
   */

void
sreal::shift_right (int s)
{
  gcc_checking_assert (s > 0);
  gcc_checking_assert (s <= SREAL_BITS);
  /* Exponent should never be so large because shift_right is used only by
     sreal_add and sreal_sub ant thus the number cannot be shifted out from
     exponent range.  */
  gcc_checking_assert (m_exp + s <= SREAL_MAX_EXP);

  m_exp += s;

  m_sig += (int64_t) 1 << (s - 1);
  m_sig >>= s;
}

/* Return integer value of *this.  */

int64_t
sreal::to_int () const
{
  int64_t sign = SREAL_SIGN (m_sig);

  if (m_exp <= -SREAL_BITS)
    return 0;
  if (m_exp >= SREAL_PART_BITS)
    return sign * INTTYPE_MAXIMUM (int64_t);
  if (m_exp > 0)
    return sign * (SREAL_ABS (m_sig) << m_exp);
  if (m_exp < 0)
    return m_sig >> -m_exp;
  return m_sig;
}

/* Return value of *this as double.
   This should be used for debug output only.  */

double
sreal::to_double () const
{
  double val = m_sig;
  if (m_exp)
    val = ldexp (val, m_exp);
  return val;
}

/* Return *this + other.  */

sreal
sreal::operator+ (const sreal &other) const
{
  int dexp;
  sreal tmp, r;

  const sreal *a_p = this, *b_p = &other, *bb;

  if (a_p->m_exp < b_p->m_exp)
    std::swap (a_p, b_p);

  dexp = a_p->m_exp - b_p->m_exp;
  r.m_exp = a_p->m_exp;
  if (dexp > SREAL_BITS)
    {
      r.m_sig = a_p->m_sig;
      return r;
    }

  if (dexp == 0)
    bb = b_p;
  else
    {
      tmp = *b_p;
      tmp.shift_right (dexp);
      bb = &tmp;
    }

  r.m_sig = a_p->m_sig + bb->m_sig;
  r.normalize ();
  return r;
}


/* Return *this - other.  */

sreal
sreal::operator- (const sreal &other) const
{
  int dexp;
  sreal tmp, r;
  const sreal *bb;
  const sreal *a_p = this, *b_p = &other;

  int64_t sign = 1;
  if (a_p->m_exp < b_p->m_exp)
    {
      sign = -1;
      std::swap (a_p, b_p);
    }

  dexp = a_p->m_exp - b_p->m_exp;
  r.m_exp = a_p->m_exp;
  if (dexp > SREAL_BITS)
    {
      r.m_sig = sign * a_p->m_sig;
      return r;
    }
  if (dexp == 0)
    bb = b_p;
  else
    {
      tmp = *b_p;
      tmp.shift_right (dexp);
      bb = &tmp;
    }

  r.m_sig = sign * (a_p->m_sig - bb->m_sig);
  r.normalize ();
  return r;
}

/* Return *this * other.  */

sreal
sreal::operator* (const sreal &other) const
{
  sreal r;
  if (absu_hwi (m_sig) < SREAL_MIN_SIG || absu_hwi (other.m_sig) < SREAL_MIN_SIG)
    {
      r.m_sig = 0;
      r.m_exp = -SREAL_MAX_EXP;
    }
  else
    {
      r.m_sig = m_sig * other.m_sig;
      r.m_exp = m_exp + other.m_exp;
      r.normalize ();
    }

  return r;
}

/* Return *this / other.  */

sreal
sreal::operator/ (const sreal &other) const
{
  gcc_checking_assert (other.m_sig != 0);
  sreal r;
  r.m_sig
    = SREAL_SIGN (m_sig) * (SREAL_ABS (m_sig) << SREAL_PART_BITS) / other.m_sig;
  r.m_exp = m_exp - other.m_exp - SREAL_PART_BITS;
  r.normalize ();
  return r;
}

#if CHECKING_P

namespace selftest {

/* Selftests for sreals.  */

/* Verify basic sreal operations.  */

static void
sreal_verify_basics (void)
{
  sreal minimum = INT_MIN;
  sreal maximum = INT_MAX;

  sreal seven = 7;
  sreal minus_two = -2;
  sreal minus_nine = -9;

  ASSERT_EQ (INT_MIN, minimum.to_int ());
  ASSERT_EQ (INT_MAX, maximum.to_int ());

  ASSERT_FALSE (minus_two < minus_two);
  ASSERT_FALSE (seven < seven);
  ASSERT_TRUE (seven > minus_two);
  ASSERT_TRUE (minus_two < seven);
  ASSERT_TRUE (minus_two != seven);
  ASSERT_EQ (minus_two, -2);
  ASSERT_EQ (seven, 7);
  ASSERT_EQ ((seven << 10) >> 10, 7);
  ASSERT_EQ (seven + minus_nine, -2);
}

/* Helper function that performs basic arithmetics and comparison
   of given arguments A and B.  */

static void
verify_aritmetics (int64_t a, int64_t b)
{
  ASSERT_EQ (a, -(-(sreal (a))).to_int ());
  ASSERT_EQ (a < b, sreal (a) < sreal (b));
  ASSERT_EQ (a <= b, sreal (a) <= sreal (b));
  ASSERT_EQ (a == b, sreal (a) == sreal (b));
  ASSERT_EQ (a != b, sreal (a) != sreal (b));
  ASSERT_EQ (a > b, sreal (a) > sreal (b));
  ASSERT_EQ (a >= b, sreal (a) >= sreal (b));
  ASSERT_EQ (a + b, (sreal (a) + sreal (b)).to_int ());
  ASSERT_EQ (a - b, (sreal (a) - sreal (b)).to_int ());
  ASSERT_EQ (b + a, (sreal (b) + sreal (a)).to_int ());
  ASSERT_EQ (b - a, (sreal (b) - sreal (a)).to_int ());
}

/* Verify arithmetics for interesting numbers.  */

static void
sreal_verify_arithmetics (void)
{
  int values[] = {-14123413, -7777, -17, -10, -2, 0, 17, 139, 1234123};
  unsigned c = sizeof (values) / sizeof (int);

  for (unsigned i = 0; i < c; i++)
    for (unsigned j = 0; j < c; j++)
      {
	int a = values[i];
	int b = values[j];

	verify_aritmetics (a, b);
      }
}

/* Helper function that performs various shifting test of a given
   argument A.  */

static void
verify_shifting (int64_t a)
{
  sreal v = a;

  for (unsigned i = 0; i < 16; i++)
    ASSERT_EQ (a << i, (v << i).to_int());

  a = a << 16;
  v = v << 16;

  for (unsigned i = 0; i < 16; i++)
    ASSERT_EQ (a >> i, (v >> i).to_int());
}

/* Verify shifting for interesting numbers.  */

static void
sreal_verify_shifting (void)
{
  int values[] = {0, 17, 32, 139, 1024, 55555, 1234123};
  unsigned c = sizeof (values) / sizeof (int);

  for (unsigned i = 0; i < c; i++)
    verify_shifting (values[i]);
}

/* Verify division by (of) a negative value.  */

static void
sreal_verify_negative_division (void)
{
  ASSERT_EQ (sreal (1) / sreal (1), sreal (1));
  ASSERT_EQ (sreal (-1) / sreal (-1), sreal (1));
  ASSERT_EQ (sreal (-1234567) / sreal (-1234567), sreal (1));
  ASSERT_EQ (sreal (-1234567) / sreal (1234567), sreal (-1));
  ASSERT_EQ (sreal (1234567) / sreal (-1234567), sreal (-1));
}

/* Run all of the selftests within this file.  */

void sreal_c_tests ()
{
  sreal_verify_basics ();
  sreal_verify_arithmetics ();
  sreal_verify_shifting ();
  sreal_verify_negative_division ();
}

} // namespace selftest
#endif /* CHECKING_P */
