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

#ifndef RUST_HIR_VISIBILITY_H
#define RUST_HIR_VISIBILITY_H

#include "rust-hir-simple-path.h"

namespace Rust {
namespace HIR {
// Visibility of an item
struct Visibility
{
public:
  enum VisType
  {
    PRIVATE,
    PUBLIC,
    RESTRICTED,
    ERROR,
  };

private:
  VisType vis_type;
  HIR::SimplePath path;
  location_t locus;

  // should this store location info?

public:
  Visibility (VisType vis_type,
	      HIR::SimplePath path = HIR::SimplePath::create_empty (),
	      location_t locus = UNDEF_LOCATION)
    : vis_type (vis_type), path (std::move (path)), locus (locus)
  {}

  // Returns whether visibility is in an error state.
  bool is_error () const { return vis_type == ERROR; }

  // Does the current visibility refer to a simple `pub <item>` entirely public
  bool is_public () const { return vis_type == PUBLIC; }

  // Is the current visibility public restricted to a certain path
  bool is_restricted () const { return vis_type == RESTRICTED; }

  // Creates an error visibility.
  static Visibility create_error ()
  {
    return Visibility (ERROR, HIR::SimplePath::create_empty ());
  }

  VisType get_vis_type () const { return vis_type; }

  const HIR::SimplePath &get_path () const
  {
    rust_assert (!is_error ());
    return path;
  }

  std::string as_string () const;
};
} // namespace HIR
} // namespace Rust

#endif
