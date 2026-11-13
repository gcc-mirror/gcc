/* Unit-safe arbitrary precision integers (e.g. "32 bytes").
   Copyright (C) 2026 Free Software Foundation, Inc.
   Contributed by David Malcolm <dmalcolm@redhat.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_ANALYZER_INTEGER_WITH_UNIT_H
#define GCC_ANALYZER_INTEGER_WITH_UNIT_H

#include "analyzer/integer.h"
#include "analyzer/units.h"

namespace ana {

/* A unit-safe arbitrary precision integer e.g. "32_bytes" or "5_bits".  */

template<typename Unit>
class integer_with_unit
{
public:
  static constexpr const char *dump_type_name = Unit::dump_type_name;

  integer_with_unit () : m_integer () {}

  explicit integer_with_unit (int i) : m_integer (i) {}

  explicit integer_with_unit (integer i) : m_integer (std::move (i)) {}

  const integer &get_integer () const { return m_integer; }

  void
  dump_to_pp (pretty_printer *pp,
	      units::role role = units::role::generic) const;
  void
  dump () const;

  std::unique_ptr<json::object>
  to_json () const;

  std::unique_ptr<diagnostics::markup::node>
  make_markup_node () const;

  bool
  is_zero_p () const { return m_integer.is_zero_p (); }

  bool
  is_one_p () const { return m_integer.is_one_p (); }

  static int
  cmp (const integer_with_unit<Unit> &lhs,
       const integer_with_unit<Unit> &rhs)
  {
    return integer::cmp (lhs.m_integer, rhs.m_integer);
  }

  integer_with_unit<Unit>&
  operator+= (const integer_with_unit<Unit> &rhs)
  {
    m_integer += rhs.m_integer;
    return *this;
  }

  integer_with_unit<Unit>&
  operator-= (const integer_with_unit<Unit> &rhs)
  {
    m_integer -= rhs.m_integer;
    return *this;
  }

  integer_with_unit<Unit>&
  operator++ ()
  {
    m_integer += 1;
    return *this;
  }

  bool
  operator< (const integer_with_unit<Unit> &rhs) const
  {
    return m_integer < rhs.m_integer;
  }

  bool
  operator<= (const integer_with_unit<Unit> &rhs) const
  {
    return m_integer <= rhs.m_integer;
  }

  bool
  operator> (const integer_with_unit<Unit> &rhs) const
  {
    return m_integer > rhs.m_integer;
  }

  bool
  operator>= (const integer_with_unit<Unit> &rhs) const
  {
    return m_integer >= rhs.m_integer;
  }

  integer_with_unit<Unit>
  operator- () const
  {
    return integer_with_unit<Unit> (-m_integer);
  }

  /* Division.  */

  integer
  floor_div (const integer_with_unit<Unit> &other) const
  {
    return get_integer ().floor_div (other.get_integer ());
  }

  /* Modulus.  */

  integer_with_unit<Unit>
  floor_mod (const integer_with_unit<Unit> &other) const
  {
    return integer_with_unit<Unit>
      (get_integer ().floor_mod (other.get_integer ()));
  }

  long
  as_long () const
  {
    return m_integer.as_long ();
  }

  unsigned long
  as_unsigned_long () const
  {
    return m_integer.as_unsigned_long ();
  }

private:
  integer m_integer;
};

/* User-defined literal operators.  */

inline integer_with_unit<units::bit>
operator ""_bit (unsigned long long v)
{
  gcc_checking_assert (v == 1);
  return integer_with_unit<units::bit> (v);
}

inline integer_with_unit<units::bit>
operator ""_bits (unsigned long long v)
{
  gcc_checking_assert (v != 1);
  return integer_with_unit<units::bit> (v);
}

inline integer_with_unit<units::byte>
operator ""_byte (unsigned long long v)
{
  gcc_checking_assert (v == 1);
  return integer_with_unit<units::byte> (v);
}

inline integer_with_unit<units::byte>
operator ""_bytes (unsigned long long v)
{
  gcc_checking_assert (v != 1);
  return integer_with_unit<units::byte> (v);
}

/* Explicit conversions.  */

inline integer_with_unit<units::bit>
bits_from_bytes (integer_with_unit<units::byte> bytes)
{
  return integer_with_unit<units::bit> (bytes.get_integer () * BITS_PER_UNIT);
}

bool
maybe_bytes_from_bits (integer_with_unit<units::bit> bits,
		       integer_with_unit<units::byte> *out);

/* Overloaded operators.  */

/* Comparisons of like units.  */

template<typename Unit>
inline bool
operator== (const integer_with_unit<Unit> &lhs,
	    const integer_with_unit<Unit> &rhs)
{
  return lhs.get_integer () == rhs.get_integer ();
}

template<typename Unit>
inline bool
operator!= (const integer_with_unit<Unit> &lhs,
	    const integer_with_unit<Unit> &rhs)
{
  return lhs.get_integer () != rhs.get_integer ();
}

/* Comparisons of compatible units, implicitly converting to bits.  */

inline bool
operator== (const integer_with_unit<units::bit> &lhs,
	    const integer_with_unit<units::byte> &rhs)
{
  return lhs == bits_from_bytes (rhs);
}

inline bool
operator== (const integer_with_unit<units::byte> &lhs,
	    const integer_with_unit<units::bit> &rhs)
{
  return bits_from_bytes (lhs) == rhs;
}

inline bool
operator!= (const integer_with_unit<units::bit> &lhs,
	    const integer_with_unit<units::byte> &rhs)
{
  return lhs != bits_from_bytes (rhs);
}

inline bool
operator!= (const integer_with_unit<units::byte> &lhs,
	    const integer_with_unit<units::bit> &rhs)
{
  return bits_from_bytes (lhs) != rhs;
}

/* Addition of integer_with_unit of same unit.  */

template<typename Unit>
inline integer_with_unit<Unit>
operator+ (const integer_with_unit<Unit> &lhs,
	   const integer_with_unit<Unit> &rhs)
{
  return integer_with_unit<Unit> (lhs.get_integer () + rhs.get_integer ());
}

/* Addition of integer_with_unit of compatible types, implicitly converting to bits.  */

inline integer_with_unit<units::bit>
operator+ (const integer_with_unit<units::bit> &lhs,
	   const integer_with_unit<units::byte> &rhs)
{
  return lhs + bits_from_bytes (rhs);
}

inline integer_with_unit<units::bit>
operator+ (const integer_with_unit<units::byte> &lhs,
	   const integer_with_unit<units::bit> &rhs)
{
  return bits_from_bytes (lhs) + rhs;
}

/* Negation.  */

template<typename Unit>
inline integer_with_unit<Unit>
operator- (const integer_with_unit<Unit> &i)
{
  return integer_with_unit<Unit> (-(i.get_integer ()));
}

/* Subtraction of integer_with_unit of same kind.  */

template<typename Unit>
inline integer_with_unit<Unit>
operator- (const integer_with_unit<Unit> &lhs,
	   const integer_with_unit<Unit> &rhs)
{
  return integer_with_unit<Unit> (lhs.get_integer () - rhs.get_integer ());
}

/* Subtraction of integer_with_unit of compatible types,
   implicitly converting to bits.  */

integer_with_unit<units::bit>
inline operator- (const integer_with_unit<units::bit> &lhs,
		  const integer_with_unit<units::byte> &rhs)
{
  return lhs - bits_from_bytes (rhs);
}

inline integer_with_unit<units::bit>
operator- (const integer_with_unit<units::byte> &lhs,
	   const integer_with_unit<units::bit> &rhs)
{
  return bits_from_bytes (lhs) - rhs;
}

/* Multiplication of integer_with_unit by a unitless value.  */

template<typename Unit>
inline integer_with_unit<Unit>
operator* (const integer_with_unit<Unit> &lhs,
	   const integer &rhs)
{
  return integer_with_unit<Unit> (lhs.get_integer () * rhs);
}

/* Multiplication of unitless value by a integer_with_unit.  */

template<typename Unit>
inline integer_with_unit<Unit>
operator* (const integer &lhs,
	   const integer_with_unit<Unit> &rhs)
{
  return integer_with_unit<Unit> (lhs * rhs.get_integer ());
}

using concrete_bit_size = integer_with_unit<units::bit>;
using concrete_bit_offset = integer_with_unit<units::bit>;
using concrete_byte_size = integer_with_unit<units::byte>;
using concrete_byte_offset = integer_with_unit<units::byte>;

inline concrete_byte_offset
round_bit_to_lowest_byte (concrete_bit_offset bit_offset)
{
  return concrete_byte_offset
    (bit_offset.get_integer ().floor_div (BITS_PER_UNIT));
}

inline concrete_bit_offset
bit_within_byte (concrete_bit_offset bit_offset)
{
  return concrete_bit_offset
    (bit_offset.get_integer ().floor_mod (BITS_PER_UNIT));
}

extern bool
get_type_size (const_tree type, concrete_bit_size *out);

/* Functions for converting from older representation.

   Ultimately we'll want {bit,byte}_{offset,size}_t to be
   integer_with_unit<units::{bit,byte}>
   but for now these should help migrate the code.  */

concrete_bit_offset
convert_from_bit_offset_t (bit_offset_t);

concrete_bit_size
convert_from_bit_size_t (bit_size_t);

integer_with_unit<units::byte>
convert_from_byte_offset_t (byte_offset_t);

integer_with_unit<units::byte>
convert_from_byte_size_t (byte_size_t);

/* Functions for converting to older representation.  */

bit_offset_t
convert_to_bit_offset_t (concrete_bit_offset);

bit_size_t
convert_to_bit_size_t (concrete_bit_size);

byte_offset_t
convert_to_byte_offset_t (concrete_byte_offset);

byte_size_t
convert_to_byte_size_t (concrete_byte_size);

template <typename ConcreteValueType>
inline bool
eval_comparison_p (ConcreteValueType a,
		   enum tree_code op,
		   ConcreteValueType b)
{
  switch (op)
    {
    default:
      gcc_unreachable ();
    case EQ_EXPR: return a == b;
    case NE_EXPR: return a != b;
    case GE_EXPR: return a >= b;
    case LE_EXPR: return a <= b;
    case GT_EXPR: return a > b;
    case LT_EXPR: return a < b;
    }
}

} // namespace ana

#endif /* GCC_ANALYZER_INTEGER_WITH_UNIT_H */
