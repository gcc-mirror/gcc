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

#include "rust-hir-visitor.h"
#include "rust-name-resolver.h"
#include "rust-stacked-contexts.h"
#include "rust-hir-type-check.h"

namespace Rust {
namespace HIR {
class ReadonlyChecker : public DefaultHIRVisitor
{
public:
  ReadonlyChecker ();

  void go (HIR::Crate &crate);

private:
  enum class lvalue_use
  {
    assign,
    increment,
    decrement,
  };

  Resolver::Resolver &resolver;
  Analysis::Mappings &mappings;
  Resolver::TypeCheckContext &context;
  StackedContexts<HirId> mutable_context;

  using DefaultHIRVisitor::visit;

  virtual void visit (AssignmentExpr &expr) override;
  virtual void visit (PathInExpression &expr) override;
  virtual void visit (FieldAccessExpr &expr) override;
  virtual void visit (ArrayIndexExpr &expr) override;
  virtual void visit (TupleExpr &expr) override;
  virtual void visit (TupleIndexExpr &expr) override;
  virtual void visit (LetStmt &stmt) override;
  virtual void visit (LiteralExpr &expr) override;
  virtual void visit (DereferenceExpr &expr) override;

  void collect_assignment (Pattern &pattern, bool has_init_expr);
  void collect_assignment_identifier (IdentifierPattern &pattern,
				      bool has_init_expr);
  void collect_assignment_tuple (TuplePattern &pattern, bool has_init_expr);

  void check_variable (IdentifierPattern *pattern, location_t assigned_loc);

  bool is_mutable_type (TyTy::BaseType *type);
};

} // namespace HIR
} // namespace Rust