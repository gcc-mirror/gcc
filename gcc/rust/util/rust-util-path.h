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

#ifndef RUST_UTIL_PATH_H
#define RUST_UTIL_PATH_H

// Simple path container class to keep segments and opening scope flag together
// whilst keeping the segment type generic.
//
// Only meant as a tuple for path elements with a name and a proper type.
template <typename Segment> struct UtilPath
{
  UtilPath (const std::vector<Segment> &segments, bool opening_scope)
    : segments (segments), opening_scope (opening_scope)
  {}

  const std::vector<Segment> &segments;
  bool opening_scope;

  bool has_opening_scope () const { return opening_scope; }
  const std::vector<Segment> &get_segments () const { return segments; }
};

#endif
