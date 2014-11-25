/* Simple data type for real numbers for the GNU compiler.
   Copyright (C) 2002-2014 Free Software Foundation, Inc.

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
#include "coretypes.h"
#include "sreal.h"

/* Print the content of struct sreal.  */

void
sreal::dump (FILE *file) const
{
  fprintf (file, "(%" PRIu64 " * 2^%d)", m_sig, m_exp);
}

DEBUG_FUNCTION void
debug (sreal &ref)
{
  ref.dump (stderr);
}

DEBUG_FUNCTION void
debug (sreal *ptr)
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

  m_sig += (uint64_t) 1 << (s - 1);
  m_sig >>= s;
}

/* Normalize *this.  */

void
sreal::normalize ()
{
  if (m_sig == 0)
    {
      m_negative = 0;
      m_exp = -SREAL_MAX_EXP;
    }
  else if (m_sig < SREAL_MIN_SIG)
    {
      do
	{
	  m_sig <<= 1;
	  m_exp--;
	}
      while (m_sig < SREAL_MIN_SIG);

      /* Check underflow.  */
      if (m_exp < -SREAL_MAX_EXP)
	{
	  m_exp = -SREAL_MAX_EXP;
	  m_sig = 0;
	}
    }
  else if (m_sig > SREAL_MAX_SIG)
    {
      int last_bit;
      do
	{
	  last_bit = m_sig & 1;
	  m_sig >>= 1;
	  m_exp++;
	}
      while (m_sig > SREAL_MAX_SIG);

      /* Round the number.  */
      m_sig += last_bit;
      if (m_sig > SREAL_MAX_SIG)
	{
	  m_sig >>= 1;
	  m_exp++;
	}

      /* Check overflow.  */
      if (m_exp > SREAL_MAX_EXP)
	{
	  m_exp = SREAL_MAX_EXP;
	  m_sig = SREAL_MAX_SIG;
	}
    }
}

/* Return integer value of *this.  */

int64_t
sreal::to_int () const
{
  int64_t sign = m_negative ? -1 : 1;

  if (m_exp <= -SREAL_BITS)
    return 0;
  if (m_exp >= SREAL_PART_BITS)
    return sign * INTTYPE_MAXIMUM (int64_t);
  if (m_exp > 0)
    return sign * (m_sig << m_exp);
  if (m_exp < 0)
    return sign * (m_sig >> -m_exp);
  return sign * m_sig;
}

/* Return *this + other.  */

sreal
sreal::operator+ (const sreal &other) const
{
  const sreal *a_p = this, *b_p = &other;

  if (a_p->m_negative && !b_p->m_negative)
    std::swap (a_p, b_p);

  /* a + -b => a - b.  */
  if (!a_p->m_negative && b_p->m_negative)
    {
      sreal tmp = -(*b_p);
      if (*a_p < tmp)
	return signedless_minus (tmp, *a_p, true);
      else
	return signedless_minus (*a_p, tmp, false);
    }

  gcc_checking_assert (a_p->m_negative == b_p->m_negative);

  sreal r = signedless_plus (*a_p, *b_p, a_p->m_negative);

  return r;
}

sreal
sreal::signedless_plus (const sreal &a, const sreal &b, bool negative)
{
  const sreal *bb;
  sreal r, tmp;
  int dexp;
  const sreal *a_p = &a;
  const sreal *b_p = &b;

  if (a_p->m_exp < b_p->m_exp)
    std::swap (a_p, b_p);

  dexp = a_p->m_exp - b_p->m_exp;
  r.m_exp = a_p->m_exp;
  if (dexp > SREAL_BITS)
    {
      r.m_sig = a_p->m_sig;
      r.m_negative = negative;
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

  r.m_negative = negative;
  return r;
}

/* Return *this - other.  */

sreal
sreal::operator- (const sreal &other) const
{
  /* -a - b => -a + (-b).  */
  if (m_negative && !other.m_negative)
    return signedless_plus (*this, -other, true);

  /* a - (-b) => a + b.  */
  if (!m_negative && other.m_negative)
    return signedless_plus (*this, -other, false);

  gcc_checking_assert (m_negative == other.m_negative);

  /* We want to substract a smaller number from bigger
    for nonegative numbers.  */
  if (!m_negative && *this < other)
    return signedless_minus (other, *this, true);

  /* Example: -2 - (-3) => 3 - 2 */
  if (m_negative && *this > other)
    return signedless_minus (-other, -(*this), false);

  sreal r = signedless_minus (*this, other, m_negative);

  return r;
}

sreal
sreal::signedless_minus (const sreal &a, const sreal &b, bool negative)
{
  int dexp;
  sreal tmp, r;
  const sreal *bb;
  const sreal *a_p = &a;
  const sreal *b_p = &b;

  dexp = a_p->m_exp - b_p->m_exp;

  r.m_exp = a_p->m_exp;
  if (dexp > SREAL_BITS)
    {
      r.m_sig = a_p->m_sig;
      r.m_negative = negative;
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

  r.m_sig = a_p->m_sig - bb->m_sig;
  r.normalize ();

  r.m_negative = negative;
  return r;
}

/* Return *this * other.  */

sreal
sreal::operator* (const sreal &other) const
{
  sreal r;
  if (m_sig < SREAL_MIN_SIG || other.m_sig < SREAL_MIN_SIG)
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

  r.m_negative = m_negative ^ other.m_negative;
  return r;
}

/* Return *this / other.  */

sreal
sreal::operator/ (const sreal &other) const
{
  gcc_checking_assert (other.m_sig != 0);
  sreal r;
  r.m_sig = (m_sig << SREAL_PART_BITS) / other.m_sig;
  r.m_exp = m_exp - other.m_exp - SREAL_PART_BITS;
  r.m_negative = m_negative ^ other.m_negative;
  r.normalize ();
  return r;
}
