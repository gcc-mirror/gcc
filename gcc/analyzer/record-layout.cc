/* Implementation of class record_layout.
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

#include "config.h"
#define INCLUDE_VECTOR
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "function.h"
#include "basic-block.h"
#include "gimple.h"
#include "diagnostic-core.h"
#include "diagnostic.h"
#include "tree-diagnostic.h"
#include "analyzer/analyzer.h"
#include "analyzer/record-layout.h"

#if ENABLE_ANALYZER

namespace ana {

/* class record_layout.  */

record_layout::record_layout (tree record_type)
{
  gcc_assert (TREE_CODE (record_type) == RECORD_TYPE);

  for (tree iter = TYPE_FIELDS (record_type); iter != NULL_TREE;
       iter = DECL_CHAIN (iter))
    {
      if (TREE_CODE (iter) == FIELD_DECL)
	{
	  int iter_field_offset = int_bit_position (iter);
	  bit_size_t size_in_bits;
	  if (!int_size_in_bits (TREE_TYPE (iter), &size_in_bits))
	    size_in_bits = 0;

	  maybe_pad_to (iter_field_offset);

	  /* Add field.  */
	  m_items.safe_push (item (bit_range (iter_field_offset,
					      size_in_bits),
				   iter, false));
	}
    }

  /* Add any trailing padding.  */
  bit_size_t size_in_bits;
  if (int_size_in_bits (record_type, &size_in_bits))
    maybe_pad_to (size_in_bits);
}

void
record_layout::dump_to_pp (pretty_printer *pp) const
{
  unsigned i;
  item *it;
  FOR_EACH_VEC_ELT (m_items, i, it)
    {
      it->dump_to_pp (pp);
      pp_newline (pp);
    }
}

void
record_layout::dump () const
{
  tree_dump_pretty_printer pp (stderr);
  dump_to_pp (&pp);
}

const record_layout::item *
record_layout::get_item_at (bit_offset_t offset) const
{
  unsigned i;
  item *it;
  FOR_EACH_VEC_ELT (m_items, i, it)
    if (it->contains_p (offset))
      return it;
  return NULL;
}

/* Subroutine of ctor.  Add padding item to NEXT_OFFSET if necessary.  */

void
record_layout::maybe_pad_to (bit_offset_t next_offset)
{
  if (m_items.length () > 0)
    {
      const item &last_item = m_items[m_items.length () - 1];
      bit_offset_t offset_after_last_item
	= last_item.get_next_bit_offset ();
      if (next_offset > offset_after_last_item)
	{
	  bit_size_t padding_size
	    = next_offset - offset_after_last_item;
	  m_items.safe_push (item (bit_range (offset_after_last_item,
					      padding_size),
				   last_item.m_field, true));
	}
    }
}

} // namespace ana

#endif /* #if ENABLE_ANALYZER  */
