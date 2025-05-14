// Copyright (C) 2020-2024 Free Software Foundation, Inc.

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

#ifndef RUST_HIR_SIMPLE_PATH_H
#define RUST_HIR_SIMPLE_PATH_H

#include "rust-hir-map.h"

namespace Rust {
namespace HIR {

class SimplePathSegment
{
  Analysis::NodeMapping mappings;

public:
  SimplePathSegment (Analysis::NodeMapping mappings) : mappings (mappings) {}

  const Analysis::NodeMapping &get_mappings () const { return mappings; }
};

class SimplePath
{
  std::vector<SimplePathSegment> segments;
  Analysis::NodeMapping mappings;
  location_t locus;

public:
  SimplePath (std::vector<SimplePathSegment> segments,
	      Analysis::NodeMapping mappings, location_t locus)
    : segments (std::move (segments)), mappings (mappings), locus (locus)
  {}

  static HIR::SimplePath create_empty ()
  {
    return HIR::SimplePath ({}, Analysis::NodeMapping::get_error (),
			    UNDEF_LOCATION);
  }

  bool is_error () const { return segments.empty (); }

  const Analysis::NodeMapping &get_mappings () const { return mappings; }
  location_t get_locus () const { return locus; }
};

} // namespace HIR
} // namespace Rust

#endif
