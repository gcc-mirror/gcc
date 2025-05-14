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

#ifndef RUST_AST_RESOLVE_H
#define RUST_AST_RESOLVE_H

#include "rust-name-resolver.h"
#include "rust-hir-map.h"

namespace Rust {
namespace Resolver {

class NameResolution
{
public:
  static void Resolve (AST::Crate &crate);

  static NameResolution *get ();

  ~NameResolution () {}

private:
  void go (AST::Crate &crate);

  NameResolution ();

  Resolver *resolver;
  Analysis::Mappings &mappings;
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_AST_RESOLVE_H
