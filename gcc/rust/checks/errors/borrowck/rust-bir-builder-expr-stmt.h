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

#ifndef RUST_BIR_BUILDER_EXPR_H
#define RUST_BIR_BUILDER_EXPR_H

#include "rust-hir-visitor.h"
#include "rust-bir-builder-internal.h"

namespace Rust {
namespace BIR {

/**
 * Compiles expressions into a BIR place.
 * See AbstractExprBuilder for API usage docs (mainly `return_place` and
 * `return_expr`).
 */
class ExprStmtBuilder : public AbstractExprBuilder, public HIR::HIRStmtVisitor
{
public:
  explicit ExprStmtBuilder (BuilderContext &ctx) : AbstractExprBuilder (ctx) {}

  /** Entry point. */
  PlaceId build (HIR::Expr &expr, PlaceId place = INVALID_PLACE)
  {
    return visit_expr (expr, place);
  }

private:
  template <typename T>
  std::vector<PlaceId> visit_list (std::vector<std::unique_ptr<T>> &list)
  {
    std::vector<PlaceId> result;
    for (auto &elem : list)
      {
	result.push_back (visit_expr (*elem));
      }
    return result;
  }

  /** Common infrastructure for loops. */
  BuilderContext::LoopAndLabelCtx &setup_loop (HIR::BaseLoopExpr &expr);

  BuilderContext::LoopAndLabelCtx &get_label_ctx (HIR::Lifetime &label);
  BuilderContext::LoopAndLabelCtx &get_unnamed_loop_ctx ();

protected: // Expr
  void visit (HIR::ClosureExpr &expr) override;
  void visit (HIR::StructExprStructFields &fields) override;
  void visit (HIR::StructExprStruct &expr) override;
  void visit (HIR::LiteralExpr &expr) override;
  void visit (HIR::BorrowExpr &expr) override;
  void visit (HIR::DereferenceExpr &expr) override;
  void visit (HIR::ErrorPropagationExpr &expr) override;
  void visit (HIR::NegationExpr &expr) override;
  void visit (HIR::ArithmeticOrLogicalExpr &expr) override;
  void visit (HIR::ComparisonExpr &expr) override;
  void visit (HIR::LazyBooleanExpr &expr) override;
  void visit (HIR::TypeCastExpr &expr) override;
  void visit (HIR::AssignmentExpr &expr) override;
  void visit (HIR::CompoundAssignmentExpr &expr) override;
  void visit (HIR::GroupedExpr &expr) override;
  void visit (HIR::ArrayExpr &expr) override;
  void visit (HIR::ArrayIndexExpr &expr) override;
  void visit (HIR::TupleExpr &expr) override;
  void visit (HIR::TupleIndexExpr &expr) override;
  void visit (HIR::CallExpr &expr) override;
  void visit (HIR::MethodCallExpr &expr) override;
  void visit (HIR::FieldAccessExpr &expr) override;
  void visit (HIR::BlockExpr &block) override;
  void visit (HIR::ContinueExpr &cont) override;
  void visit (HIR::BreakExpr &brk) override;
  void visit (HIR::RangeFromToExpr &range) override;
  void visit (HIR::RangeFromExpr &expr) override;
  void visit (HIR::RangeToExpr &expr) override;
  void visit (HIR::RangeFullExpr &expr) override;
  void visit (HIR::RangeFromToInclExpr &expr) override;
  void visit (HIR::RangeToInclExpr &expr) override;
  void visit (HIR::ReturnExpr &ret) override;
  void visit (HIR::UnsafeBlockExpr &expr) override;
  void visit (HIR::LoopExpr &expr) override;
  void visit (HIR::WhileLoopExpr &expr) override;
  void visit (HIR::WhileLetLoopExpr &expr) override;
  void visit (HIR::IfExpr &expr) override;
  void visit (HIR::IfExprConseqElse &expr) override;

  void visit (HIR::IfLetExpr &expr) override;
  void visit (HIR::IfLetExprConseqElse &expr) override;
  void visit (HIR::MatchExpr &expr) override;
  void visit (HIR::AwaitExpr &expr) override;
  void visit (HIR::AsyncBlockExpr &expr) override;

protected: // Nodes not containing executable code. Nothing to do.
  void visit (HIR::QualifiedPathInExpression &expr) override;
  void visit (HIR::PathInExpression &expr) override;

protected: // Handled by other visitors
  void visit (HIR::StructExprFieldIdentifier &field) override
  {
    rust_unreachable ();
  }
  void visit (HIR::StructExprFieldIdentifierValue &field) override
  {
    rust_unreachable ();
  }
  void visit (HIR::StructExprFieldIndexValue &field) override
  {
    rust_unreachable ();
  }

protected: // Stmt
  void visit (HIR::LetStmt &stmt) override;

  void visit (HIR::ExprStmt &stmt) override;

protected: // Ignored.
  // Only executable code of a single function/method is translated.
  void visit (HIR::EnumItemTuple &tuple) override {}
  void visit (HIR::EnumItemStruct &a_struct) override {}
  void visit (HIR::EnumItem &item) override {}
  void visit (HIR::TupleStruct &tuple_struct) override {}
  void visit (HIR::EnumItemDiscriminant &discriminant) override {}
  void visit (HIR::TypePathSegmentFunction &segment) override {}
  void visit (HIR::TypePath &path) override {}
  void visit (HIR::QualifiedPathInType &path) override {}
  void visit (HIR::Module &module) override {}
  void visit (HIR::ExternCrate &crate) override {}
  void visit (HIR::UseDeclaration &use_decl) override {}
  void visit (HIR::Function &function) override {}
  void visit (HIR::TypeAlias &type_alias) override {}
  void visit (HIR::StructStruct &struct_item) override {}
  void visit (HIR::Enum &enum_item) override {}
  void visit (HIR::Union &union_item) override {}
  void visit (HIR::ConstantItem &const_item) override {}
  void visit (HIR::StaticItem &static_item) override {}
  void visit (HIR::Trait &trait) override {}
  void visit (HIR::ImplBlock &impl) override {}
  void visit (HIR::ExternBlock &block) override {}
  void visit (HIR::EmptyStmt &stmt) override {}
};

} // namespace BIR
} // namespace Rust

#endif // RUST_BIR_BUILDER_EXPR_H
