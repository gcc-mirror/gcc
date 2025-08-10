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

#include "rust-hir-expr.h"
#include "rust-hir-item.h"
#include "rust-hir-path.h"
#include "rust-hir-pattern.h"
#include "rust-hir-visitor.h"
#include "rust-mapping-common.h"
#include "rust-name-resolution-context.h"
#include "rust-unused-var-context.h"
#include "rust-name-resolver.h"

namespace Rust {
namespace Analysis {
class UnusedVarCollector : public HIR::DefaultHIRVisitor
{
public:
  UnusedVarCollector (UnusedVarContext &context);
  void go (HIR::Crate &crate);

private:
  const Resolver2_0::NameResolutionContext &nr_context;
  Analysis::Mappings &mappings;
  UnusedVarContext &unused_var_context;

  using HIR::DefaultHIRVisitor::visit;
  virtual void visit (HIR::PathInExpression &expr) override;
  virtual void visit (HIR::StructExprFieldIdentifier &ident) override;
  virtual void visit (HIR::ConstantItem &item) override;
  virtual void visit (HIR::StaticItem &item) override;
  virtual void visit (HIR::IdentifierPattern &pattern) override;
  virtual void visit (HIR::QualifiedPathInExpression &expr) override;

  template <typename T> void mark_path_used (T &path_expr)
  {
    NodeId ast_node_id = path_expr.get_mappings ().get_nodeid ();
    NodeId def_id = nr_context.lookup (ast_node_id).value ();
    HirId hir_id = mappings.lookup_node_to_hir (def_id).value ();
    unused_var_context.mark_used (hir_id);
  }
};
} // namespace Analysis
} // namespace Rust