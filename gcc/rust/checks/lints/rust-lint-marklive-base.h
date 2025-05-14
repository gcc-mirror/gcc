// Copyright (C) 2021-2025 Free Software Foundation, Inc.

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

#ifndef RUST_HIR_LIVENESS_BASE
#define RUST_HIR_LIVENESS_BASE

#include "rust-diagnostics.h"
#include "rust-lint-marklive.h"
#include "rust-lint-marklive-base.h"
#include "rust-hir-visitor.h"
#include "rust-hir-map.h"

namespace Rust {
namespace Analysis {

class MarkLiveBase : public HIR::HIRFullVisitorBase
{
public:
  virtual ~MarkLiveBase () {}

protected:
  MarkLiveBase () : mappings (Analysis::Mappings::get ()) {}

  Analysis::Mappings &mappings;
};

} // namespace Analysis
} // namespace Rust

#endif
