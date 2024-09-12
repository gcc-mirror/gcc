// Copyright (C) 2020-2025 Free Software Foundation, Inc.

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
#include "rust-ast.h"
#include "rust-ast-resolve-pattern.h"
#include "rust-expr.h"

namespace Rust {
namespace Resolver {

class ResolveExpr : public ResolverBase
{
  using Rust::Resolver::ResolverBase::visit;

public:
  static void go (AST::Expr &expr, const CanonicalPath &prefix,
		  const CanonicalPath &canonical_prefix,
		  bool funny_error = false);

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
  void visit (AST::IfLetExpr &expr) override;
  void visit (AST::IfLetExprConseqElse &expr) override;
  void visit (AST::BlockExpr &expr) override;
  void visit (AST::InlineAsm &expr) override;
  void visit (AST::UnsafeBlockExpr &expr) override;
  void visit (AST::ArrayElemsValues &elems) override;
  void visit (AST::ArrayExpr &expr) override;
  void visit (AST::ArrayIndexExpr &expr) override;
  void visit (AST::ArrayElemsCopied &elems) override;
  void visit (AST::StructExprStruct &struct_expr) override;
  void visit (AST::StructExprStructFields &struct_expr) override;
  void visit (AST::GroupedExpr &expr) override;
  void visit (AST::FieldAccessExpr &expr) override;
  void visit (AST::LoopExpr &expr) override;
  void visit (AST::BreakExpr &expr) override;
  void visit (AST::WhileLoopExpr &expr) override;
  void visit (AST::ForLoopExpr &expr) override;
  void visit (AST::ContinueExpr &expr) override;
  void visit (AST::BorrowExpr &expr) override;
  void visit (AST::DereferenceExpr &expr) override;
  void visit (AST::MatchExpr &expr) override;
  void visit (AST::RangeFromToExpr &expr) override;
  void visit (AST::RangeFromExpr &expr) override;
  void visit (AST::RangeToExpr &expr) override;
  void visit (AST::RangeFullExpr &expr) override;
  void visit (AST::RangeFromToInclExpr &expr) override;
  void visit (AST::ClosureExprInner &expr) override;
  void visit (AST::ClosureExprInnerTyped &expr) override;
  void visit (AST::ErrorPropagationExpr &expr) override;

protected:
  void resolve_closure_param (AST::ClosureParam &param,
			      std::vector<PatternBinding> &bindings);

private:
  ResolveExpr (const CanonicalPath &prefix,
	       const CanonicalPath &canonical_prefix, bool funny_error);

  const CanonicalPath &prefix;
  const CanonicalPath &canonical_prefix;
  bool funny_error;
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_AST_RESOLVE_EXPR_H
