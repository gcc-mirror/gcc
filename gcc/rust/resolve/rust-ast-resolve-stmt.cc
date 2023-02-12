// Copyright (C) 2020-2023 Free Software Foundation, Inc.

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

#include "rust-ast-resolve-item.h"
#include "rust-ast-resolve-stmt.h"

namespace Rust {
namespace Resolver {

void
ResolveStmt::visit (AST::ExternBlock &extern_block)
{
  resolve_visibility (extern_block.get_visibility ());
  for (auto &item : extern_block.get_extern_items ())
    {
      ResolveToplevelExternItem::go (item.get (),
				     CanonicalPath::create_empty ());
      ResolveExternItem::go (item.get (), prefix, canonical_prefix);
    }
}

} // namespace Resolver
} // namespace Rust
