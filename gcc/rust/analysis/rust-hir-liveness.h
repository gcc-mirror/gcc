// Copyright (C) 2021 Free Software Foundation, Inc.

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

#ifndef RUST_HIR_LIVENESS
#define RUST_HIR_LIVENESS

#include "rust-hir-full-decls.h"
#include "rust-hir-map.h"
#include "rust-hir-liveness-base.h"
#include "rust-name-resolver.h"
#include <set>

namespace Rust {
namespace Analysis {

class Liveness : public LivenessBase
{
  using Rust::Analysis::LivenessBase::visit;

public:
  static std::set<HirId> Analysis (HIR::Crate &crate);
  void go (HIR::Crate &crate);

  void visit (HIR::ExprStmtWithoutBlock &stmt) override;
  void visit (HIR::CallExpr &expr) override;
  void visit (HIR::Function &function) override;
  void visit (HIR::BlockExpr &expr) override;
  void visit (HIR::PathInExpression &expr) override;

private:
  std::vector<HirId> worklist;
  std::set<HirId> liveSymbols;
  std::set<HirId> scannedSymbols;
  Analysis::Mappings *mappings;
  Resolver::Resolver *resolver;
  Liveness (std::vector<HirId> worklist)
    : worklist (worklist), mappings (Analysis::Mappings::get ()),
      resolver (Resolver::Resolver::get ()){};
};

} // namespace Analysis
} // namespace Rust

#endif