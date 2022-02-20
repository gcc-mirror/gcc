// Copyright (C) 2020-2022 Free Software Foundation, Inc.

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

#ifndef RUST_AST_RESOLVE_EXPR_H
#define RUST_AST_RESOLVE_EXPR_H

#include "rust-ast-resolve-base.h"
#include "rust-ast-full.h"

namespace Rust {
namespace Resolver {

class ResolvePath : public ResolverBase
{
  using Rust::Resolver::ResolverBase::visit;

public:
  static void go (AST::PathInExpression *expr, NodeId parent)
  {
    ResolvePath resolver (parent);
    resolver.resolve_path (expr);
  }

  static void go (AST::QualifiedPathInExpression *expr, NodeId parent)
  {
    ResolvePath resolver (parent);
    resolver.resolve_path (expr);
  }

private:
  ResolvePath (NodeId parent) : ResolverBase (parent) {}

  void resolve_path (AST::PathInExpression *expr);

  void resolve_path (AST::QualifiedPathInExpression *expr);

  void resolve_segments (CanonicalPath prefix, size_t offs,
			 std::vector<AST::PathExprSegment> &segs,
			 NodeId expr_node_id, Location expr_locus);
};

class ResolveExpr : public ResolverBase
{
  using Rust::Resolver::ResolverBase::visit;

public:
  static void go (AST::Expr *expr, NodeId parent, const CanonicalPath &prefix,
		  const CanonicalPath &canonical_prefix);

  void visit (AST::MacroInvocation &expr) override;

  void visit (AST::TupleIndexExpr &expr) override;

  void visit (AST::TupleExpr &expr) override;

  void visit (AST::PathInExpression &expr) override;

  void visit (AST::QualifiedPathInExpression &expr) override;

  void visit (AST::ReturnExpr &expr) override;

  void visit (AST::CallExpr &expr) override;

  void visit (AST::MethodCallExpr &expr) override;

  void visit (AST::AssignmentExpr &expr) override;

  void visit (AST::IdentifierExpr &expr) override;

  void visit (AST::ArithmeticOrLogicalExpr &expr) override;

  void visit (AST::CompoundAssignmentExpr &expr) override;

  void visit (AST::ComparisonExpr &expr) override;

  void visit (AST::LazyBooleanExpr &expr) override;

  void visit (AST::NegationExpr &expr) override;

  void visit (AST::TypeCastExpr &expr) override;

  void visit (AST::IfExpr &expr) override;

  void visit (AST::IfExprConseqElse &expr) override;

  void visit (AST::IfExprConseqIf &expr) override;

  void visit (AST::BlockExpr &expr) override;

  void visit (AST::UnsafeBlockExpr &expr) override;

  void visit (AST::ArrayElemsValues &elems) override;

  void visit (AST::ArrayExpr &expr) override;

  void visit (AST::ArrayIndexExpr &expr) override;

  void visit (AST::ArrayElemsCopied &elems) override;

  // this this an empty struct constructor like 'S {}'
  void visit (AST::StructExprStruct &struct_expr) override;

  // this this a struct constructor with fields
  void visit (AST::StructExprStructFields &struct_expr) override;

  void visit (AST::GroupedExpr &expr) override;

  void visit (AST::FieldAccessExpr &expr) override;

  void visit (AST::LoopExpr &expr) override;

  void visit (AST::BreakExpr &expr) override;

  void visit (AST::WhileLoopExpr &expr) override;

  void visit (AST::ContinueExpr &expr) override;

  void visit (AST::BorrowExpr &expr) override;

  void visit (AST::DereferenceExpr &expr) override;

  void visit (AST::MatchExpr &expr) override;

  void visit (AST::RangeFromToExpr &expr) override;

  void visit (AST::RangeFromExpr &expr) override;

  void visit (AST::RangeToExpr &expr) override;

  void visit (AST::RangeFullExpr &expr) override;

  void visit (AST::RangeFromToInclExpr &expr) override;

protected:
  void resolve_expr (AST::Expr *e, NodeId parent)
  {
    ResolveExpr::go (e, parent, prefix, canonical_prefix);
  }

private:
  ResolveExpr (NodeId parent, const CanonicalPath &prefix,
	       const CanonicalPath &canonical_prefix)
    : ResolverBase (parent), prefix (prefix),
      canonical_prefix (canonical_prefix)
  {}

  const CanonicalPath &prefix;
  const CanonicalPath &canonical_prefix;
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_AST_RESOLVE_EXPR_H
