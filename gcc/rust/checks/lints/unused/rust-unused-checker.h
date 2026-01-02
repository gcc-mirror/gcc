// Copyright (C) 2025-2026 Free Software Foundation, Inc.

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

#include "rust-hir-expr.h"
#include "rust-hir-item.h"
#include "rust-hir-pattern.h"
#include "rust-hir-visitor.h"
#include "rust-unused-collector.h"
#include "rust-immutable-name-resolution-context.h"

namespace Rust {
namespace Analysis {
class UnusedChecker : public HIR::DefaultHIRVisitor
{
public:
  UnusedChecker ();
  void go (HIR::Crate &crate);

private:
  const Resolver2_0::NameResolutionContext &nr_context;
  Analysis::Mappings &mappings;
  UnusedContext unused_context;

  using HIR::DefaultHIRVisitor::visit;
  virtual void visit (HIR::TraitItemFunc &decl) override;
  virtual void visit (HIR::ConstantItem &item) override;
  virtual void visit (HIR::StaticItem &item) override;
  virtual void visit (HIR::IdentifierPattern &identifier) override;
  virtual void visit (HIR::AssignmentExpr &identifier) override;
  virtual void visit (HIR::StructPatternFieldIdent &identifier) override;
};
} // namespace Analysis
} // namespace Rust
