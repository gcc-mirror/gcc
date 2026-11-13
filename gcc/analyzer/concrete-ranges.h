/* Unit-safe concrete ranges of values (e.g. "bits 16-31", "bytes 0-1023").
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

#ifndef GCC_ANALYZER_CONCRETE_RANGES_H
#define GCC_ANALYZER_CONCRETE_RANGES_H

#include "analyzer/integer-with-unit.h"

namespace ana {
namespace concrete {

/* Concrete ranges of bits or bytes (or unitless).  */

template<typename ConcreteValueType>
class range
{
public:
  range () = default;

  range (ConcreteValueType start_offset,
	 ConcreteValueType size)
  : m_start_offset (start_offset),
    m_size (size)
  {
  }

  void
  dump () const;

  void
  dump_to_pp (pretty_printer *pp) const;

  void
  dump_to_pp_pretty (pretty_printer *pp) const;

  ConcreteValueType
  get_start_offset () const { return m_start_offset; };

  ConcreteValueType
  get_size () const { return m_size; }

  ConcreteValueType
  get_last_offset () const
  {
    return get_next_offset () - ConcreteValueType (1);
  }

  ConcreteValueType
  get_next_offset () const { return m_start_offset + m_size; }

  bool
  contains_p (ConcreteValueType offset) const
  {
    return (offset >= get_start_offset ()
	    && offset < get_next_offset ());
  }

  bool
  intersects_p (const range &other, range *out = nullptr) const;

  bool
  exceeds_p (ConcreteValueType offset,
	     range<ConcreteValueType> *out) const;

  bool
  falls_short_of_p (ConcreteValueType offset,
		    range<ConcreteValueType> *out) const;

  bool
  operator== (const range<ConcreteValueType> &other) const
  {
    return (m_start_offset == other.m_start_offset
	    && m_size == other.m_size);
  }

  static int
  cmp (const range<ConcreteValueType> &lhs,
       const range<ConcreteValueType> &rhs);

  bool
  operator< (const range<ConcreteValueType> &other) const
  {
    return cmp (*this, other) < 0;
  }

  std::unique_ptr<json::object>
  to_json () const;


  std::unique_ptr<diagnostics::markup::node>
  make_markup_node () const;

private:
  ConcreteValueType m_start_offset;
  ConcreteValueType m_size;
};

} // namespace ana::concrete

using concrete_bit_range = concrete::range<integer_with_unit<units::bit>>;
using concrete_byte_range = concrete::range<integer_with_unit<units::byte>>;

inline concrete_bit_range
bits_from_bytes (concrete_byte_range bytes)
{
  return concrete_bit_range (bits_from_bytes (bytes.get_start_offset ()),
			     bits_from_bytes (bytes.get_size ()));
}

bool
maybe_bytes_from_bits (concrete_bit_range bits,
		       concrete_byte_range *out);

bool
concrete_bit_range_from_mask (unsigned HOST_WIDE_INT mask,
			      concrete_bit_range *out);

void
get_smallest_enclosing_byte_range (const concrete_bit_range &in_bit_range,
				   concrete_byte_range *out_byte_range,
				   concrete_bit_offset *out_bit_offset);

} // namespace ana

#endif /* GCC_ANALYZER_CONCRETE_RANGES_H */
