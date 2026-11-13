/* Support for scanning buffers for a NUL terminator (for C-style strings).
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

#ifndef GCC_ANALYZER_STRING_SCAN_H
#define GCC_ANALYZER_STRING_SCAN_H

#include "analyzer/concrete-ranges.h"

namespace ana {

/* A class representing the result of a scan through a bit_vector,
   bit_contents, or constant at some start offset looking for the first of:
   - NUL terminator (if any), or
   - an access that will trigger a diagnostic (if any), e.g. uninitialized, or
   - exhausting the domain of the input

   Records what happened, and the offset at which it happened.  The
   offset is relative to the start of the buffer, rather than the given
   start offset, i.e. they are both in the same "coordinate system".
   For exhausted data, this will be the index of one past the last
   valid element, i.e. the size of the bit_vector/constant.  */

struct string_scan
{
  using byte_offset_type = concrete_byte_offset;

  /* A enum to discriminate the different kinds of result.  */
  enum class state
  {
    /* Found a NUL terminator.
       Result offset is that of the NUL (which could be that
       of the start_byte_offset).
      . +-----+-----+-----+-----+-----+-----+
      . | 'a' | 'b' | 'c' | 'd' | NUL | NUL |
      . +-----+-----+-----+-----+-----+-----+
      .                ^           ^
      .                |           result offset
      .                start_byte_offset.  */
    found_nul,

    /* Found a poisoned byte (e.g. uninitialized)
       Result offset is that of the problematic byte.
      . +-----+-----+-----+-----+------+-----+
      . | 'a' | 'b' | 'c' | 'd' |uninit| NUL |
      . +-----+-----+-----+-----+------+-----+
      .                ^           ^
      .                |           result offset
      .                start_byte_offset.  */
    found_poison,

    /* Didn't find any terminators or problems.
       Result offset is one past the end of the domain of the bit_vector
       or constant, which is its size in bytes.
      . +-----+-----+-----+-----+-----+-----+......+
      . |     |     |     |     |     |     |     ..
      . +-0---+-----+-----+-----+-----+-----+.size.+
      .                ^                       ^
      .                start_byte_offset       result offset.  */
    beyond_domain,

    /* This object can't be scanned for some reason.
       The result offset is where the problem occurred
       (often the start byte offset).  */
    cannot_scan
  };

  string_scan (byte_offset_type byte_offset,
	       enum state state)
  : m_byte_offset (byte_offset),
    m_state (state),
    m_poison_kind (poison_kind::other)
  {
  }

  string_scan (byte_offset_type byte_offset,
	       enum poison_kind poison_kind_)
  : m_byte_offset (byte_offset),
    m_state (state::found_poison),
    m_poison_kind (poison_kind_)
  {
  }

  static string_scan
  found_nul (byte_offset_type byte_offset)
  {
    return string_scan (byte_offset, state::found_nul);
  }
  static string_scan
  found_poison (byte_offset_type byte_offset,
		enum poison_kind poison_kind_)
  {
    return string_scan (byte_offset, poison_kind_);
  }
  static string_scan
  beyond_domain (byte_offset_type byte_offset)
  {
    // byte offset == offset of 1 past the end of the buffer == buffer size
    return string_scan (byte_offset, state::beyond_domain);
  }
  static string_scan
  cannot_scan (byte_offset_type byte_offset)
  {
    return string_scan (byte_offset, state::cannot_scan);
  }

  bool
  operator== (const string_scan &other) const
  {
    return (m_byte_offset == other.m_byte_offset
	    && m_state == other.m_state);
  }

  void dump_to_pp (pretty_printer *pp) const;
  void dump () const;

  static const char *
  state_to_string (enum state s);

  byte_offset_type m_byte_offset;
  enum state m_state;
  enum poison_kind m_poison_kind;
};

} // namespace ana

#endif /* GCC_ANALYZER_STRING_SCAN_H */
