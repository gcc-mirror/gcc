// Copyright (C) 2020-2024 Free Software Foundation, Inc.

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

#ifndef RUST_COMPILE_BLOCK
#define RUST_COMPILE_BLOCK

#include "rust-compile-base.h"
#include "rust-hir-visitor.h"

namespace Rust {
namespace Compile {

class CompileBlock : private HIRCompileBase
{
public:
  static tree compile (HIR::BlockExpr *expr, Context *ctx, Bvariable *result);

protected:
  void visit (HIR::BlockExpr &expr);

private:
  CompileBlock (Context *ctx, Bvariable *result);

  tree translated;
  Bvariable *result;
};

class CompileConditionalBlocks : public HIRCompileBase,
				 public HIR::HIRExpressionVisitor
{
public:
  static tree compile (HIR::IfExpr *expr, Context *ctx, Bvariable *result)
  {
    CompileConditionalBlocks resolver (ctx, result);
    expr->accept_vis (resolver);
    return resolver.translated;
  }

  void visit (HIR::IfExpr &expr) override;
  void visit (HIR::IfExprConseqElse &expr) override;

  // Empty visit for unused Expression HIR nodes.
  void visit (HIR::PathInExpression &) override {}
  void visit (HIR::QualifiedPathInExpression &) override {}
  void visit (HIR::ClosureExpr &) override {}
  void visit (HIR::StructExprFieldIdentifier &) override {}
  void visit (HIR::StructExprFieldIdentifierValue &) override {}
  void visit (HIR::StructExprFieldIndexValue &) override {}
  void visit (HIR::StructExprStruct &) override {}
  void visit (HIR::StructExprStructFields &) override {}
  void visit (HIR::LiteralExpr &) override {}
  void visit (HIR::BorrowExpr &) override {}
  void visit (HIR::DereferenceExpr &) override {}
  void visit (HIR::ErrorPropagationExpr &) override {}
  void visit (HIR::NegationExpr &) override {}
  void visit (HIR::ArithmeticOrLogicalExpr &) override {}
  void visit (HIR::ComparisonExpr &) override {}
  void visit (HIR::LazyBooleanExpr &) override {}
  void visit (HIR::TypeCastExpr &) override {}
  void visit (HIR::AssignmentExpr &) override {}
  void visit (HIR::CompoundAssignmentExpr &) override {}
  void visit (HIR::GroupedExpr &) override {}
  void visit (HIR::ArrayExpr &) override {}
  void visit (HIR::ArrayIndexExpr &) override {}
  void visit (HIR::TupleExpr &) override {}
  void visit (HIR::TupleIndexExpr &) override {}
  void visit (HIR::CallExpr &) override {}
  void visit (HIR::MethodCallExpr &) override {}
  void visit (HIR::FieldAccessExpr &) override {}
  void visit (HIR::BlockExpr &) override {}
  void visit (HIR::ContinueExpr &) override {}
  void visit (HIR::BreakExpr &) override {}
  void visit (HIR::RangeFromToExpr &) override {}
  void visit (HIR::RangeFromExpr &) override {}
  void visit (HIR::RangeToExpr &) override {}
  void visit (HIR::RangeFullExpr &) override {}
  void visit (HIR::RangeFromToInclExpr &) override {}
  void visit (HIR::RangeToInclExpr &) override {}
  void visit (HIR::ReturnExpr &) override {}
  void visit (HIR::UnsafeBlockExpr &) override {}
  void visit (HIR::LoopExpr &) override {}
  void visit (HIR::WhileLoopExpr &) override {}
  void visit (HIR::WhileLetLoopExpr &) override {}
  void visit (HIR::IfLetExpr &) override {}
  void visit (HIR::IfLetExprConseqElse &) override {}
  void visit (HIR::MatchExpr &) override {}
  void visit (HIR::AwaitExpr &) override {}
  void visit (HIR::AsyncBlockExpr &) override {}

private:
  CompileConditionalBlocks (Context *ctx, Bvariable *result)
    : HIRCompileBase (ctx), translated (nullptr), result (result)
  {}

  tree translated;
  Bvariable *result;
};

class CompileExprWithBlock : public HIRCompileBase,
			     public HIR::HIRExpressionVisitor
{
public:
  static tree compile (HIR::ExprWithBlock *expr, Context *ctx,
		       Bvariable *result)
  {
    CompileExprWithBlock resolver (ctx, result);
    expr->accept_vis (resolver);
    return resolver.translated;
  }

  void visit (HIR::IfExpr &expr) override
  {
    translated = CompileConditionalBlocks::compile (&expr, ctx, result);
  }

  void visit (HIR::IfExprConseqElse &expr) override
  {
    translated = CompileConditionalBlocks::compile (&expr, ctx, result);
  }

  void visit (HIR::BlockExpr &expr) override
  {
    translated = CompileBlock::compile (&expr, ctx, result);
  }

  // Empty visit for unused Expression HIR nodes.
  void visit (HIR::PathInExpression &) override {}
  void visit (HIR::QualifiedPathInExpression &) override {}
  void visit (HIR::ClosureExpr &) override {}
  void visit (HIR::StructExprFieldIdentifier &) override {}
  void visit (HIR::StructExprFieldIdentifierValue &) override {}
  void visit (HIR::StructExprFieldIndexValue &) override {}
  void visit (HIR::StructExprStruct &) override {}
  void visit (HIR::StructExprStructFields &) override {}
  void visit (HIR::LiteralExpr &) override {}
  void visit (HIR::BorrowExpr &) override {}
  void visit (HIR::DereferenceExpr &) override {}
  void visit (HIR::ErrorPropagationExpr &) override {}
  void visit (HIR::NegationExpr &) override {}
  void visit (HIR::ArithmeticOrLogicalExpr &) override {}
  void visit (HIR::ComparisonExpr &) override {}
  void visit (HIR::LazyBooleanExpr &) override {}
  void visit (HIR::TypeCastExpr &) override {}
  void visit (HIR::AssignmentExpr &) override {}
  void visit (HIR::CompoundAssignmentExpr &) override {}
  void visit (HIR::GroupedExpr &) override {}
  void visit (HIR::ArrayExpr &) override {}
  void visit (HIR::ArrayIndexExpr &) override {}
  void visit (HIR::TupleExpr &) override {}
  void visit (HIR::TupleIndexExpr &) override {}
  void visit (HIR::CallExpr &) override {}
  void visit (HIR::MethodCallExpr &) override {}
  void visit (HIR::FieldAccessExpr &) override {}
  void visit (HIR::ContinueExpr &) override {}
  void visit (HIR::BreakExpr &) override {}
  void visit (HIR::RangeFromToExpr &) override {}
  void visit (HIR::RangeFromExpr &) override {}
  void visit (HIR::RangeToExpr &) override {}
  void visit (HIR::RangeFullExpr &) override {}
  void visit (HIR::RangeFromToInclExpr &) override {}
  void visit (HIR::RangeToInclExpr &) override {}
  void visit (HIR::ReturnExpr &) override {}
  void visit (HIR::UnsafeBlockExpr &) override {}
  void visit (HIR::LoopExpr &) override {}
  void visit (HIR::WhileLoopExpr &) override {}
  void visit (HIR::WhileLetLoopExpr &) override {}
  void visit (HIR::IfLetExpr &) override {}
  void visit (HIR::IfLetExprConseqElse &) override {}
  void visit (HIR::MatchExpr &) override {}
  void visit (HIR::AwaitExpr &) override {}
  void visit (HIR::AsyncBlockExpr &) override {}

private:
  CompileExprWithBlock (Context *ctx, Bvariable *result)
    : HIRCompileBase (ctx), translated (nullptr), result (result)
  {}

  tree translated;
  Bvariable *result;
};

} // namespace Compile
} // namespace Rust

#endif // RUST_COMPILE_BLOCK
