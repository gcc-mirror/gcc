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

#include "rust-compile-drop-builder.h"
#include "rust-compile-context.h"

namespace Rust {
namespace Compile {

DropBuilder::DropBuilder (Context &ctx) : ctx (ctx) {}

void
DropBuilder::note_simple_drop_candidate (HirId hirid, location_t locus)
{
  rust_assert (!ctx.block_drop_candidates.empty ());
  ctx.block_drop_candidates.back ().emplace_back (hirid, locus);
}

std::vector<DropCandidate> &
DropBuilder::peek_block_drop_candidates ()
{
  rust_assert (!ctx.block_drop_candidates.empty ());
  return ctx.block_drop_candidates.back ();
}

} // namespace Compile
} // namespace Rust