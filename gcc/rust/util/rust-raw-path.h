// Copyright (C) 2026 Free Software Foundation, Inc.

// This file is part of GCC.

// GCC is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free
// Software Foundation; either version 3, or (at your option) any later
// version.

// GCC is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
// for more details.

// You should have received a copy of the GNU General Public License
// along with GCC; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include "rust-system.h"

#ifndef RUST_RAW_PATH_H
#define RUST_RAW_PATH_H

namespace Rust {

template <typename Segment> class RawPath
{
protected:
  std::vector<Segment> segments;
  bool opening_scope_resolution;

public:
  RawPath (std::vector<Segment> segments, bool opening_scope_resolution)
    : segments (std::move (segments)),
      opening_scope_resolution (opening_scope_resolution)
  {}

  /* Returns whether the RawPath has an opening scope resolution operator
   * (i.e. is global path or crate-relative path, not module-relative) */
  bool has_opening_scope_resolution () const
  {
    return opening_scope_resolution;
  }

  const std::vector<Segment> &get_segments () const { return segments; }
  std::vector<Segment> &get_segments () { return segments; }

  bool is_empty () const { return segments.empty (); }

  Segment &get_final_segment ()
  {
    rust_assert (!is_empty ());
    return get_segments ().back ();
  }
  const Segment &get_final_segment () const
  {
    rust_assert (!is_empty ());
    return get_segments ().back ();
  }
};

} // namespace Rust

#endif
