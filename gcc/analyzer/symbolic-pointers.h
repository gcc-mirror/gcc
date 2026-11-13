/* A (base_region, symbolic byte offset) pair.
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

#ifndef GCC_ANALYZER_SYMBOLIC_POINTERS_H
#define GCC_ANALYZER_SYMBOLIC_POINTERS_H

#include "analyzer/symbolic-ranges.h"

namespace ana {
namespace symbolic {

/* A (base_region, symbolic byte offset) pair.
   Note that these have a granularity of *bytes*.  */

class pointer
{
public:
  using symbolic_byte_offset = symbolic::scalar<integer_with_unit<units::byte>>;

  pointer (const region &base_reg,
	   const symbolic_byte_offset &byte_offset)
  : m_base_reg (base_reg),
    m_byte_offset (byte_offset)
  {
  }

  void dump () const;

  bool
  operator== (const pointer &other) const
  {
    return (&m_base_reg == &other.m_base_reg
	    && &m_byte_offset == &other.m_byte_offset);
  }

  bool
  operator!= (const pointer &other) const
  {
    return !(*this == other);
  }

  int
  cmp (const pointer &other) const;

  bool
  zero_offset_p () const
  {
    return m_byte_offset.is_zero_p ();
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

  const symbolic_byte_offset &
  get_byte_offset () const { return m_byte_offset; }

  static tristate
  eval_condition (const pointer &lhs_ptr,
		  enum tree_code op,
		  const pointer &rhs_ptr,
		  const region_model &model);

private:
  const region &m_base_reg;
  const symbolic_byte_offset &m_byte_offset;
};

} // namespace ana::symbolic

} // namespace ana

#endif /* GCC_ANALYZER_SYMBOLIC_POINTERS_H */
