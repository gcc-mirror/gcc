// Copyright (C) 2025 Free Software Foundation, Inc.

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

#include "rust-hir-item.h"
#include "rust-hir-pattern.h"
#include "rust-hir-visitor.h"
#include "rust-immutable-name-resolution-context.h"
#include "rust-unused-var-collector.h"

namespace Rust {
namespace Analysis {
class UnusedVarChecker : public HIR::DefaultHIRVisitor
{
public:
  UnusedVarChecker ();
  void go (HIR::Crate &crate);

private:
  const Resolver2_0::NameResolutionContext &nr_context;
  Analysis::Mappings &mappings;
  std::unique_ptr<UnusedVarContext> unused_var_context;

  using HIR::DefaultHIRVisitor::visit;
  virtual void visit (HIR::TraitItemFunc &decl) override;
  virtual void visit (HIR::ConstantItem &item) override;
  virtual void visit (HIR::StaticItem &item) override;
  virtual void visit (HIR::IdentifierPattern &identifier) override;
};
} // namespace Analysis
} // namespace Rust