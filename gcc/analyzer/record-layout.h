/* Declaration of class record_layout.
   Copyright (C) 2022-2024 Free Software Foundation, Inc.
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

#ifndef GCC_ANALYZER_RECORD_LAYOUT_H
#define GCC_ANALYZER_RECORD_LAYOUT_H

#include "analyzer/store.h"

namespace ana {

/* Information of the layout of a RECORD_TYPE, capturing it as a vector
   of items, where each item is either a field or padding.  */

class record_layout
{
public:
  /* An item within a record; either a field, or padding after a field.  */
  struct item
  {
  public:
    item (const bit_range &br,
	  tree field,
	  bool is_padding)
    : m_bit_range (br),
      m_field (field),
      m_is_padding (is_padding)
    {
    }

    bit_offset_t get_start_bit_offset () const
    {
      return m_bit_range.get_start_bit_offset ();
    }
    bit_offset_t get_next_bit_offset () const
    {
      return m_bit_range.get_next_bit_offset ();
    }

    bool contains_p (bit_offset_t offset) const
    {
      return m_bit_range.contains_p (offset);
    }

    void dump_to_pp (pretty_printer *pp) const
    {
      if (m_is_padding)
	pp_printf (pp, "padding after %qD", m_field);
      else
	pp_printf (pp, "%qD", m_field);
      pp_string (pp, ", ");
      m_bit_range.dump_to_pp (pp);
    }

    bit_range m_bit_range;
    tree m_field;
    bool m_is_padding;
  };

  record_layout (tree record_type);

  void dump_to_pp (pretty_printer *pp) const;
  DEBUG_FUNCTION void dump () const;

  const record_layout::item *get_item_at (bit_offset_t offset) const;

private:
  void maybe_pad_to (bit_offset_t next_offset);

  auto_vec<item> m_items;
};

} // namespace ana

#endif /* GCC_ANALYZER_RECORD_LAYOUT_H */
