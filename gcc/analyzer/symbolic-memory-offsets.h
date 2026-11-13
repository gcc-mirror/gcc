/* A (base_region, symbolic bit offset) pair for representing
   where a region starts in memory.
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

#ifndef GCC_ANALYZER_SYMBOLIC_MEMORY_OFFSETS_H
#define GCC_ANALYZER_SYMBOLIC_MEMORY_OFFSETS_H

#include "analyzer/symbolic-ranges.h"

namespace ana {
namespace symbolic {

/* A (base_region, symbolic bit offset) pair for representing
   where a region starts in memory.
   Note that these have a granularity of *bits*.  */

class memory_offset
{
public:
  using symbolic_bit_offset = symbolic::scalar<integer_with_unit<units::bit>>;

  memory_offset (const region &base_reg,
		 const symbolic_bit_offset &bit_offset)
  : m_base_reg (base_reg),
    m_bit_offset (bit_offset)
  {
  }

  void dump () const;

  bool
  operator== (const memory_offset &other) const
  {
    return (&m_base_reg == &other.m_base_reg
	    && &m_bit_offset == &other.m_bit_offset);
  }

  bool
  operator!= (const memory_offset &other) const
  {
    return !(*this == other);
  }

  int
  cmp (const memory_offset &other) const;

  bool
  zero_offset_p () const
  {
    return m_bit_offset.is_zero_p ();
  }

  bool non_null_p () const;

  std::unique_ptr<text_art::tree_widget>
  make_dump_widget (node_dump_state &nds) const;

  void
  accept_node_visitor (symbolic::node_visitor &v) const;

  std::unique_ptr<json::object>
  to_json () const;

  const region &
  get_base_region () const { return m_base_reg; }

  const symbolic_bit_offset &
  get_bit_offset () const { return m_bit_offset; }

private:
  const region &m_base_reg;
  const symbolic_bit_offset &m_bit_offset;
};

} // namespace ana::symbolic

} // namespace ana

#endif /* GCC_ANALYZER_SYMBOLIC_MEMORY_OFFSETS_H */
