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

#ifndef RUST_HIR_TYPE_CHECK_EXPR
#define RUST_HIR_TYPE_CHECK_EXPR

#include "rust-hir-type-check-base.h"
#include "rust-hir-visitor.h"
#include "rust-tyty.h"

namespace Rust {
namespace Resolver {

class TypeCheckExpr : private TypeCheckBase, private HIR::HIRExpressionVisitor
{
public:
  static TyTy::BaseType *Resolve (HIR::Expr *expr);

  void visit (HIR::TupleIndexExpr &expr) override;
  void visit (HIR::TupleExpr &expr) override;
  void visit (HIR::ReturnExpr &expr) override;
  void visit (HIR::CallExpr &expr) override;
  void visit (HIR::MethodCallExpr &expr) override;
  void visit (HIR::AssignmentExpr &expr) override;
  void visit (HIR::CompoundAssignmentExpr &expr) override;
  void visit (HIR::LiteralExpr &expr) override;
  void visit (HIR::ArithmeticOrLogicalExpr &expr) override;
  void visit (HIR::ComparisonExpr &expr) override;
  void visit (HIR::LazyBooleanExpr &expr) override;
  void visit (HIR::NegationExpr &expr) override;
  void visit (HIR::IfExpr &expr) override;
  void visit (HIR::IfExprConseqElse &expr) override;
  void visit (HIR::IfLetExpr &expr) override;
  void visit (HIR::IfLetExprConseqElse &) override;
  void visit (HIR::BlockExpr &expr) override;
  void visit (HIR::UnsafeBlockExpr &expr) override;
  void visit (HIR::ArrayIndexExpr &expr) override;
  void visit (HIR::ArrayExpr &expr) override;
  void visit (HIR::StructExprStruct &struct_expr) override;
  void visit (HIR::StructExprStructFields &struct_expr) override;
  void visit (HIR::GroupedExpr &expr) override;
  void visit (HIR::FieldAccessExpr &expr) override;
  void visit (HIR::QualifiedPathInExpression &expr) override;
  void visit (HIR::PathInExpression &expr) override;
  void visit (HIR::LoopExpr &expr) override;
  void visit (HIR::BreakExpr &expr) override;
  void visit (HIR::ContinueExpr &expr) override;
  void visit (HIR::BorrowExpr &expr) override;
  void visit (HIR::DereferenceExpr &expr) override;
  void visit (HIR::TypeCastExpr &expr) override;
  void visit (HIR::MatchExpr &expr) override;
  void visit (HIR::RangeFromToExpr &expr) override;
  void visit (HIR::RangeFromExpr &expr) override;
  void visit (HIR::RangeToExpr &expr) override;
  void visit (HIR::RangeFullExpr &expr) override;
  void visit (HIR::RangeFromToInclExpr &expr) override;
  void visit (HIR::WhileLoopExpr &expr) override;
  void visit (HIR::ClosureExpr &expr) override;

  // TODO
  void visit (HIR::ErrorPropagationExpr &) override {}
  void visit (HIR::RangeToInclExpr &) override {}
  void visit (HIR::WhileLetLoopExpr &) override {}

  // lets not worry about async yet....
  void visit (HIR::AwaitExpr &) override {}
  void visit (HIR::AsyncBlockExpr &) override {}

  // don't need to implement these see rust-hir-type-check-struct-field.h
  void visit (HIR::StructExprFieldIdentifier &) override
  {
    rust_unreachable ();
  }
  void visit (HIR::StructExprFieldIndexValue &) override
  {
    rust_unreachable ();
  }
  void visit (HIR::StructExprFieldIdentifierValue &) override
  {
    rust_unreachable ();
  }

protected:
  bool
  resolve_operator_overload (Analysis::RustLangItem::ItemType lang_item_type,
			     HIR::OperatorExprMeta expr, TyTy::BaseType *lhs,
			     TyTy::BaseType *rhs);

  bool resolve_fn_trait_call (HIR::CallExpr &expr,
			      TyTy::BaseType *function_tyty,
			      TyTy::BaseType **result);

  HIR::PathIdentSegment resolve_possible_fn_trait_call_method_name (
    TyTy::BaseType &receiver, TyTy::TypeBoundPredicate *associated_predicate);

private:
  TypeCheckExpr ();

  TyTy::BaseType *resolve_root_path (HIR::PathInExpression &expr,
				     size_t *offset,
				     NodeId *root_resolved_node_id);

  void resolve_segments (NodeId root_resolved_node_id,
			 std::vector<HIR::PathExprSegment> &segments,
			 size_t offset, TyTy::BaseType *tyseg,
			 const Analysis::NodeMapping &expr_mappings,
			 location_t expr_locus);

  bool
  validate_arithmetic_type (const TyTy::BaseType *tyty,
			    HIR::ArithmeticOrLogicalExpr::ExprType expr_type);

  /* The return value of TypeCheckExpr::Resolve */
  TyTy::BaseType *infered;
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_HIR_TYPE_CHECK_EXPR
