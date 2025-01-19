// Copyright (C) 2020-2025 Free Software Foundation, Inc.

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

#ifndef RUST_PRIVACY_CHECK_H
#define RUST_PRIVACY_CHECK_H

#include "rust-hir.h"
#include "rust-hir-expr.h"
#include "rust-hir-stmt.h"
#include "rust-hir-item.h"
#include "rust-hir-type-check.h"

namespace Rust {
namespace Privacy {
class Resolver
{
public:
  /**
   * Perform the full privacy resolving pass on a crate.
   *
   * This resolver first computes the reachability of all items in a crate,
   * before checking for privacy violations.
   */
  static void resolve (HIR::Crate &crate);
};
} // namespace Privacy
} // namespace Rust

#endif // !RUST_PRIVACY_CHECK_H
