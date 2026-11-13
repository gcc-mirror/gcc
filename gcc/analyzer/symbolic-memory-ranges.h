/* A (base_region, symbolic bit range) pair.
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

#ifndef GCC_ANALYZER_SYMBOLIC_MEMORY_RANGES_H
#define GCC_ANALYZER_SYMBOLIC_MEMORY_RANGES_H

#include "analyzer/symbolic-ranges.h"
#include "analyzer/symbolic-pointers.h"

namespace ana {
namespace symbolic {

/* A (base_region, symbolic bit range) pair.
   Note that the range has a granularity of *bits*.  */

class memory_range
{
public:
  memory_range (const region &base_reg,
		symbolic_bit_range bit_range)
  : m_base_reg (base_reg),
    m_bit_range (bit_range)
  {
  }

  memory_range (pointer sym_ptr,
		const symbolic_byte_size &num_bytes);

  void dump () const;

  bool
  operator== (const memory_range &other) const
  {
    return (&m_base_reg == &other.m_base_reg
	    && m_bit_range == other.m_bit_range);
  }

  std::unique_ptr<text_art::tree_widget>
  make_dump_widget (node_dump_state &nds) const;

  std::unique_ptr<json::object>
  to_json () const;

  const region &
  get_base_region () const { return m_base_reg; }

  symbolic_bit_range
  get_bit_range () const { return m_bit_range; }

  const symbolic_bit_offset &
  get_start_offset () const { return m_bit_range.get_start_offset (); }

  const symbolic_bit_offset &
  get_next_offset () const { return m_bit_range.get_next_offset (); }

  const symbolic_bit_size &
  get_size () const { return m_bit_range.get_size (); }

private:
  const region &m_base_reg;
  symbolic_bit_range m_bit_range;
};

} // namespace ana::symbolic

} // namespace ana

#endif /* GCC_ANALYZER_SYMBOLIC_MEMORY_RANGES_H */
