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
#include "rust-hir-generic-param.h"
#include "rust-hir-item.h"
#include "rust-hir-pattern.h"
#include "rust-hir-visitor.h"
#include "rust-unused-collector.h"
#include "rust-finalized-name-resolution-context.h"

namespace Rust {
namespace Analysis {
class UnusedChecker : public HIR::DefaultHIRVisitor
{
public:
  UnusedChecker ();
  void go (HIR::Crate &crate);

private:
  const Resolver2_0::FinalizedNameResolutionContext &nr_context;
  Analysis::Mappings &mappings;
  UnusedContext unused_context;

  using HIR::DefaultHIRVisitor::visit;
  virtual void visit (HIR::TraitItemFunc &decl) override;
  virtual void visit (HIR::ConstantItem &item) override;
  virtual void visit (HIR::StaticItem &item) override;
  virtual void visit (HIR::IdentifierPattern &identifier) override;
  virtual void visit (HIR::AssignmentExpr &identifier) override;
  virtual void visit (HIR::StructPatternFieldIdent &identifier) override;
  virtual void visit (HIR::EmptyStmt &stmt) override;
  virtual void visit (HIR::Function &fct) override;
  virtual void visit (HIR::Module &mod) override;
  virtual void visit (HIR::LifetimeParam &lft) override;
  virtual void visit (HIR::StructPatternFieldIdentPat &field) override;
  virtual void visit (HIR::MatchExpr &expr) override;
  virtual void visit (HIR::ExternBlock &block) override;
  virtual void visit (HIR::LetStmt &stmt) override;
  virtual void visit (HIR::BorrowExpr &expr) override;
  virtual void visit (HIR::NegationExpr &expr) override;
  virtual void visit_loop_label (HIR::LoopLabel &label) override;
};
} // namespace Analysis
} // namespace Rust
