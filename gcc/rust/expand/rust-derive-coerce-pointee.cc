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

#include "rust-derive-coerce-pointee.h"
#include "rust-session-manager.h"
#include "rust-feature.h"
#include "rust-feature-store.h"

namespace Rust {
namespace AST {

DeriveCoercePointee::DeriveCoercePointee (location_t loc,
					  Builder::Source item_source)
  : DeriveVisitor (loc, item_source)
{}

std::unique_ptr<AST::Item>
DeriveCoercePointee::go (Item &item)
{
  Features::EarlyFeatureGateStore::get ().add (
    Feature::Name::DERIVE_COERCE_POINTEE,
    Error (loc, "use of unstable library feature %<derive_coerce_pointee%>"));

  // NOTE: This is technically a library feature, so I don't think this is
  // how it should be gated. But as far as I can tell at the moment we have
  // no mechanisms for handling these, and furthermore this will go through
  // our compatibility layer.
  //
  // What this means is that the `CoercePointee` macro will *not* be defined
  // in the `core` that we will be using to compile the kernel at first.
  //
  // The basic example for `CoercePointee` is something like this:
  //
  // ```rust
  // use std::marker::CoercePointee;
  //
  // #[derive(CoercePointee)]
  // struct Flip<Flop>(*const Flop);
  // ```
  //
  // but the marker will not be present in the `core` we will be using at
  // first, so we will need to fake its existence with
  // -frust-compat-version, and then the compiler can assume that it is a
  // built-in derive. At least that's my expectation.

  if (!Session::get_instance ().should_support_coerce_pointee ())
    {
      rust_error_at (loc, "derive(CoercePointee) requires a compatibility mode "
			  "greater or equal to 1.84");
      return {};
    }

  rust_warning_at (
    loc, 0,
    "derive(CoercePointee) is currently unimplemented and has no effect");

  return {};
}

} // namespace AST
} // namespace Rust
