/* Symbolic offsets and ranges.
   Copyright (C) 2023-2025 Free Software Foundation, Inc.
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

#ifndef GCC_ANALYZER_RANGES_H
#define GCC_ANALYZER_RANGES_H

namespace ana {

/* Wrapper around an svalue for a value measured in bytes.  */

class symbolic_byte_offset
{
public:
  explicit symbolic_byte_offset (int i, region_model_manager &mgr);
  symbolic_byte_offset (const svalue *num_bytes_sval);
  explicit symbolic_byte_offset (region_offset offset,
				 region_model_manager &mgr);

  const svalue *get_svalue () const { return m_num_bytes_sval; }
  tree maybe_get_constant () const;

  void dump_to_pp (pretty_printer *pp, bool) const;
  void dump (bool) const;

  std::unique_ptr<json::value> to_json () const;

  bool operator== (const symbolic_byte_offset &other) const
  {
   return m_num_bytes_sval == other.m_num_bytes_sval;
  }

private:
  const svalue *m_num_bytes_sval;
};

/* A range of byte offsets, where both the start and size of the
   range can be symbolic.  */

class symbolic_byte_range
{
public:
  symbolic_byte_range (symbolic_byte_offset start,
		       symbolic_byte_offset size)
  : m_start (start),
    m_size (size)
  {
  }

  symbolic_byte_range (region_offset start,
		       const svalue *num_bytes,
		       region_model_manager &mgr);

  void dump_to_pp (pretty_printer *pp,
		   bool simple,
		   region_model_manager &mgr) const;
  void dump (bool, region_model_manager &mgr) const;

  std::unique_ptr<json::value> to_json () const;

  bool empty_p () const;

  symbolic_byte_offset get_start_byte_offset () const
  {
    return m_start;
  }
  symbolic_byte_offset get_last_byte_offset (region_model_manager &mgr) const;
  symbolic_byte_offset get_size_in_bytes () const
  {
    return m_size;
  }
  symbolic_byte_offset get_next_byte_offset (region_model_manager &mgr) const;

  tristate intersection (const symbolic_byte_range &other,
			 const region_model &model) const;

private:
  symbolic_byte_offset m_start;
  symbolic_byte_offset m_size;
};

} // namespace ana

#endif /* GCC_ANALYZER_RANGES_H */
