// Simple fixed-point representation of fractional costs
// Copyright (C) 2021-2025 Free Software Foundation, Inc.
//
// This file is part of GCC.
//
// GCC is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free
// Software Foundation; either version 3, or (at your option) any later
// version.
//
// GCC is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
// for more details.
//
// You should have received a copy of the GNU General Public License
// along with GCC; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// A simple saturating fixed-point type for representing fractional
// intermediate results in cost calculations.  The input and result
// costs are assumed to be uint32_ts.  Unlike sreal, the class can
// represent most values that we care about exactly (without rounding).
// See the comment above the SCALE field for the current set of
// exactly-representable reciprocals.
class fractional_cost
{
public:
  // Construct an object equal to INT_VALUE.
  constexpr fractional_cost (uint32_t int_value = 0)
    : m_value (uint64_t (int_value) * SCALE) {}

  fractional_cost (uint32_t a, uint32_t b);

  fractional_cost operator+ (const fractional_cost &) const;
  fractional_cost operator- (const fractional_cost &) const;
  fractional_cost operator* (uint32_t) const;

  fractional_cost &operator+= (const fractional_cost &);
  fractional_cost &operator-= (const fractional_cost &);
  fractional_cost &operator*= (uint32_t);

  bool operator== (const fractional_cost &) const;
  bool operator!= (const fractional_cost &) const;
  bool operator< (const fractional_cost &) const;
  bool operator<= (const fractional_cost &) const;
  bool operator>= (const fractional_cost &) const;
  bool operator> (const fractional_cost &) const;

  uint32_t ceil () const;

  static uint32_t scale (uint32_t, fractional_cost, fractional_cost);

  explicit operator bool () const { return m_value != 0; }

  // Convert the value to a double.
  double as_double () const { return double (m_value) / SCALE; }

private:
  enum raw { RAW };
  constexpr fractional_cost (uint64_t value, raw) : m_value (value) {}

  // A multiple of [1, 16] * 16.  This ensures that 1/N is representable
  // for every possible SVE element count N, or for any "X per cycle"
  // value N in the range [1, 16].
  static const uint32_t SCALE = 11531520;

  // The value multiplied by BIAS.
  uint64_t m_value;
};

// Construct a representation of A / B, rounding up if (contrary to
// expectations) we can't represent the value exactly.  For now we
// treat inexact values as a bug, since all values of B should come
// from a small set of values that are known at compile time.
inline fractional_cost::fractional_cost (uint32_t a, uint32_t b)
  : m_value (CEIL (uint64_t (a) * SCALE, uint64_t (b)))
{
  gcc_checking_assert (SCALE % b == 0);
}

inline fractional_cost
fractional_cost::operator+ (const fractional_cost &other) const
{
  uint64_t sum = m_value + other.m_value;
  return { sum >= m_value ? sum : ~uint64_t (0), RAW };
}

inline fractional_cost &
fractional_cost::operator+= (const fractional_cost &other)
{
  *this = *this + other;
  return *this;
}

inline fractional_cost
fractional_cost::operator- (const fractional_cost &other) const
{
  uint64_t diff = m_value - other.m_value;
  return { diff <= m_value ? diff : 0, RAW };
}

inline fractional_cost &
fractional_cost::operator-= (const fractional_cost &other)
{
  *this = *this - other;
  return *this;
}

inline fractional_cost
fractional_cost::operator* (uint32_t other) const
{
  if (other == 0)
    return 0;

  uint64_t max = ~uint64_t (0);
  return { m_value <= max / other ? m_value * other : max, RAW };
}

inline fractional_cost &
fractional_cost::operator*= (uint32_t other)
{
  *this = *this * other;
  return *this;
}

inline bool
fractional_cost::operator== (const fractional_cost &other) const
{
  return m_value == other.m_value;
}

inline bool
fractional_cost::operator!= (const fractional_cost &other) const
{
  return m_value != other.m_value;
}

inline bool
fractional_cost::operator< (const fractional_cost &other) const
{
  return m_value < other.m_value;
}

inline bool
fractional_cost::operator<= (const fractional_cost &other) const
{
  return m_value <= other.m_value;
}

inline bool
fractional_cost::operator>= (const fractional_cost &other) const
{
  return m_value >= other.m_value;
}

inline bool
fractional_cost::operator> (const fractional_cost &other) const
{
  return m_value > other.m_value;
}

// Round the value up to the nearest integer and saturate to a uint32_t.
inline uint32_t
fractional_cost::ceil () const
{
  uint32_t max = ~uint32_t (0);
  if (m_value <= uint64_t (max - 1) * SCALE)
    return (m_value + SCALE - 1) / SCALE;
  return max;
}

// Round (COST * A) / B up to the nearest integer and saturate to a uint32_t.
inline uint32_t
fractional_cost::scale (uint32_t cost, fractional_cost a, fractional_cost b)
{
  widest_int result = wi::div_ceil (widest_int (cost) * a.m_value,
				    b.m_value, SIGNED);
  if (result < ~uint32_t (0))
    return result.to_shwi ();
  return ~uint32_t (0);
}

inline fractional_cost
operator+ (uint32_t a, const fractional_cost &b)
{
  return b.operator+ (a);
}

inline fractional_cost
operator- (uint32_t a, const fractional_cost &b)
{
  return fractional_cost (a).operator- (b);
}

inline fractional_cost
operator* (uint32_t a, const fractional_cost &b)
{
  return b.operator* (a);
}

inline bool
operator== (uint32_t a, const fractional_cost &b)
{
  return b.operator== (a);
}

inline bool
operator!= (uint32_t a, const fractional_cost &b)
{
  return b.operator!= (a);
}

inline bool
operator< (uint32_t a, const fractional_cost &b)
{
  return b.operator> (a);
}

inline bool
operator<= (uint32_t a, const fractional_cost &b)
{
  return b.operator>= (a);
}

inline bool
operator>= (uint32_t a, const fractional_cost &b)
{
  return b.operator<= (a);
}

inline bool
operator> (uint32_t a, const fractional_cost &b)
{
  return b.operator< (a);
}
