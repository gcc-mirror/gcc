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

#ifndef RUST_COMPILE_DROP_H
#define RUST_COMPILE_DROP_H

#include "rust-compile-context.h"

namespace Rust {
namespace Compile {

class CompileDrop
{
public:
  CompileDrop (Context *ctx);

  bool type_has_drop_impl (TyTy::BaseType *ty);

  tree build_current_scope_drop_cleanup ();

private:
  tree compile_drop_call (Bvariable *var, TyTy::BaseType *ty, location_t locus);

  void
  emit_drop_candidate_calls (const std::vector<DropCandidate> &drop_candidates);

  Context *ctx;
};

} // namespace Compile
} // namespace Rust

#endif // RUST_COMPILE_DROP_H