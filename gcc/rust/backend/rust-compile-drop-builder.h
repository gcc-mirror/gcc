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

#ifndef RUST_COMPILE_DROP_BUILDER_H
#define RUST_COMPILE_DROP_BUILDER_H

#include "rust-compile-drop-candidate.h"

namespace Rust {
namespace Compile {

class Context;

class DropBuilder
{
public:
  DropBuilder (Context &ctx);

  void note_simple_drop_candidate (HirId hirid, location_t locus);
  std::vector<DropCandidate> &peek_block_drop_candidates ();

private:
  Context &ctx;
};

} // namespace Compile
} // namespace Rust

#endif // RUST_COMPILE_DROP_BUILDER_H