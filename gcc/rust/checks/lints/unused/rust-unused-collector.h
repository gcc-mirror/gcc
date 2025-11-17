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
#include "rust-hir-path.h"
#include "rust-hir-pattern.h"
#include "rust-hir-visitor.h"
#include "rust-mapping-common.h"
#include "rust-name-resolution-context.h"
#include "rust-unused-context.h"

namespace Rust {
namespace Analysis {
class UnusedCollector : public HIR::DefaultHIRVisitor
{
public:
  UnusedCollector (UnusedContext &context);
  void go (HIR::Crate &crate);

private:
  const Resolver2_0::NameResolutionContext &nr_context;
  Analysis::Mappings &mappings;
  UnusedContext &unused_context;

  using HIR::DefaultHIRVisitor::visit;

  // Unused var
  virtual void visit (HIR::PathInExpression &expr) override;
  virtual void visit (HIR::StructExprFieldIdentifier &ident) override;
  virtual void visit (HIR::QualifiedPathInExpression &expr) override;

  // Unused assignments
  virtual void visit (HIR::AssignmentExpr &expr) override;

  // Unused mut
  virtual void visit (HIR::IdentifierPattern &pattern) override;
  virtual void visit (HIR::StructPatternFieldIdent &pattern) override;

  template <typename T> HirId get_def_id (T &path_expr)
  {
    NodeId ast_node_id = path_expr.get_mappings ().get_nodeid ();
    NodeId id = nr_context.lookup (ast_node_id).value ();
    HirId def_id = mappings.lookup_node_to_hir (id).value ();
    return def_id;
  }

  template <typename T> void mark_path_used (T &path_expr)
  {
    auto def_id = get_def_id (path_expr);
    unused_context.add_variable (def_id);
    unused_context.remove_assign (def_id);
  }
};
} // namespace Analysis
} // namespace Rust
