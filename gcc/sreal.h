/* Definitions for simple data type for positive real numbers.
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

#ifndef GCC_SREAL_H
#define GCC_SREAL_H

/* SREAL_PART_BITS has to be an even number.  */
#define SREAL_PART_BITS 32

#define SREAL_MIN_SIG ((uint64_t) 1 << (SREAL_PART_BITS - 1))
#define SREAL_MAX_SIG (((uint64_t) 1 << SREAL_PART_BITS) - 1)
#define SREAL_MAX_EXP (INT_MAX / 4)

#define SREAL_BITS SREAL_PART_BITS

/* Structure for holding a simple real number.  */
class sreal
{
public:
  /* Construct an uninitialized sreal.  */
  sreal () : m_sig (-1), m_exp (-1) {}

  /* Construct a sreal.  */
  sreal (uint64_t sig, int exp) : m_sig (sig), m_exp (exp) { normalize (); }

  void dump (FILE *) const;
  int64_t to_int () const;

  sreal operator+ (const sreal &other) const;
  sreal operator- (const sreal &other) const;
  sreal operator* (const sreal &other) const;
  sreal operator/ (const sreal &other) const;

  bool operator< (const sreal &other) const
  {
    return m_exp < other.m_exp
      || (m_exp == other.m_exp && m_sig < other.m_sig);
  }

  bool operator== (const sreal &other) const
  {
    return m_exp == other.m_exp && m_sig == other.m_sig;
  }

private:
  void normalize ();
  void shift_right (int amount);

  uint64_t m_sig;		/* Significant.  */
  signed int m_exp;			/* Exponent.  */
};

extern void debug (sreal &ref);
extern void debug (sreal *ptr);

inline sreal &operator+= (sreal &a, const sreal &b)
{
  return a = a + b;
}

inline sreal &operator-= (sreal &a, const sreal &b)
{
return a = a - b;
}

inline sreal &operator/= (sreal &a, const sreal &b)
{
return a = a / b;
}

inline sreal &operator*= (sreal &a, const sreal &b)
{
  return a = a  * b;
}

inline bool operator!= (const sreal &a, const sreal &b)
{
  return !(a == b);
}

inline bool operator> (const sreal &a, const sreal &b)
{
  return !(a == b || a < b);
}

inline bool operator<= (const sreal &a, const sreal &b)
{
  return a < b || a == b;
}

inline bool operator>= (const sreal &a, const sreal &b)
{
  return a == b || a > b;
}

#endif
