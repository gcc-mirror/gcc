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
#include "rust-bir-drop-analysis.h"

namespace Rust {
namespace Compile {

DropBuilder::DropBuilder (Context &ctx) : ctx (ctx) {}

void
DropBuilder::note_simple_drop_candidate (HirId hirid, location_t locus)
{
  rust_assert (!ctx.block_drop_candidates.empty ());
  ctx.block_drop_candidates.back ().emplace_back (hirid, locus);
}

void
DropBuilder::maybe_create_drop_flag (HirId hirid, location_t locus,
				     bool initialized)
{
  if (!BIR::DropAnalysis::get ().needs_drop_flag (hirid))
    return;

  Bvariable *existing = nullptr;
  if (ctx.lookup_drop_flag (hirid, &existing))
    return;

  tree declaration = nullptr;
  Bvariable *flag = Backend::temporary_variable (
    ctx.peek_fn ().fndecl, nullptr, boolean_type_node,
    Backend::boolean_constant_expression (initialized), false, locus,
    &declaration);
  ctx.add_statement (declaration);
  ctx.insert_drop_flag (hirid, flag);
}

tree
DropBuilder::drop_flag_assignment (HirId hirid, bool value, location_t locus)
{
  Bvariable *flag = nullptr;
  if (!ctx.lookup_drop_flag (hirid, &flag))
    return nullptr;

  return Backend::assignment_statement (Backend::var_expression (flag, locus),
					Backend::boolean_constant_expression (
					  value),
					locus);
}

std::vector<DropCandidate> &
DropBuilder::peek_block_drop_candidates ()
{
  rust_assert (!ctx.block_drop_candidates.empty ());
  return ctx.block_drop_candidates.back ();
}

} // namespace Compile
} // namespace Rust
