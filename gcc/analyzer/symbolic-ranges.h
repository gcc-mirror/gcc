/* Typesafe symbolic ranges of values (e.g. "bytes m-n").
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

#ifndef GCC_ANALYZER_SYMBOLIC_RANGES_H
#define GCC_ANALYZER_SYMBOLIC_RANGES_H

#include "analyzer/symbolic-scalars.h"
#include "analyzer/concrete-ranges.h"

namespace ana {
namespace symbolic {

template<typename ConcreteValueType>
class range
{
public:
  range ()
 : m_start_offset (nullptr),
   m_size (nullptr)
  {
    /* Placeholder default ctor for the few places where we need
       to conditionally initialize these.  */
  }

  range (const scalar<ConcreteValueType> &start_offset,
	 const scalar<ConcreteValueType> &size)
  : m_start_offset (&start_offset),
    m_size (&size)
  {
  }

  range (const concrete::range<ConcreteValueType> &conc_range,
	 scalar_manager<ConcreteValueType> &mgr)
  : m_start_offset (&mgr.get_concrete (conc_range.get_start_offset ())),
    m_size (&mgr.get_concrete (conc_range.get_size ()))
  {
  }

  static range
  from_endpoints (const scalar<ConcreteValueType> &start_offset,
		  const scalar<ConcreteValueType> &next_offset)
  {
    return range (start_offset, next_offset - start_offset);
  }

  complexity
  get_children_complexity () const
  {
    return (m_start_offset->get_complexity ()
	    + m_size->get_complexity ());
  }

  const scalar<ConcreteValueType> &
  get_start_offset () const { return *m_start_offset; };

  const scalar<ConcreteValueType> &
  get_size () const { return *m_size; }

  const scalar<ConcreteValueType> &
  get_last_offset () const
  {
    ConcreteValueType concrete_one (1);
    auto &symbolic_one = m_start_offset->get_mgr ().get_concrete (concrete_one);
    return (get_next_offset () - symbolic_one);
  }

  const scalar<ConcreteValueType> &
  get_next_offset () const { return *m_start_offset + *m_size; }

  std::unique_ptr<text_art::tree_widget>
  make_dump_widget (node_dump_state &nds,
		    const char *prefix = nullptr) const;

  void dump () const;

  void log (const char *label, logger &log) const;

  bool
  operator== (const range<ConcreteValueType> &other) const
  {
    return (m_start_offset == other.m_start_offset
	    && m_size == other.m_size);
  }

  static int
  cmp (const range<ConcreteValueType> &lhs,
       const range<ConcreteValueType> &rhs)
  {
    if (int c = node::cmp (*lhs.m_start_offset, *rhs.m_start_offset))
      return c;
    return node::cmp (*lhs.m_size, *rhs.m_size);
  }


  bool
  operator< (const range<ConcreteValueType> &other) const
  {
    return cmp (*this, other) < 0;
  }

  complexity
  get_complexity () const
  {
    return (m_start_offset->get_complexity ()
	    + m_size->get_complexity ());
  }

  void
  accept_node_visitor (node_visitor &v) const;

  std::unique_ptr<json::object>
  to_json () const;

  void
  dump_inline_to_pp (pretty_printer *pp) const;

  tristate
  intersects (const range<ConcreteValueType> &other,
	      const region_model &model) const;

  bool
  access_range_contains_p (const range<ConcreteValueType> &other) const;

  bool
  empty_p () const
  {
    return m_size->is_zero_p ();
  }

private:
  const scalar<ConcreteValueType> *m_start_offset;
  const scalar<ConcreteValueType> *m_size;
};

template <typename ConcreteValueType>
inline bool
concrete_from_symbolic (const symbolic::range<ConcreteValueType> &in,
			concrete::range<ConcreteValueType> *out)
{
  if (auto *concrete_start_offset
	= in.get_start_offset ().dyn_cast_concrete_scalar ())
    if (auto *concrete_size = in.get_size ().dyn_cast_concrete_scalar ())
      {
	*out = concrete::range<ConcreteValueType>
		 (concrete_start_offset->get_concrete (),
		  concrete_size->get_concrete ());
	return true;
      }
  return false;
}

template <typename ConcreteValueType>
inline symbolic::range<ConcreteValueType>
symbolic_from_concrete (const concrete::range<ConcreteValueType> &conc_range,
			scalar_manager<concrete_bit_size> &bit_mgr)
{
  return symbolic::range<ConcreteValueType>
    (symbolic_from_concrete (conc_range.get_start_offset (), bit_mgr),
     symbolic_from_concrete (conc_range.get_size (), bit_mgr));
}

} // namespace ana::symbolic

symbolic_bit_range
bits_from_bytes (const symbolic_byte_range &byte_range);

bool
maybe_bytes_from_bits (const symbolic_bit_range &bit_range,
		       symbolic_byte_range *out);

} // namespace ana

#endif /* GCC_ANALYZER_SYMBOLIC_RANGES_H */
