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

#ifndef RUST_COMPILE_EXPR
#define RUST_COMPILE_EXPR

#include "rust-compile-base.h"
#include "rust-hir-visitor.h"

namespace Rust {
namespace Compile {

class CompileExpr : private HIRCompileBase, protected HIR::HIRExpressionVisitor
{
public:
  static tree Compile (HIR::Expr *expr, Context *ctx);

  void visit (HIR::TupleIndexExpr &expr) override;
  void visit (HIR::TupleExpr &expr) override;
  void visit (HIR::ReturnExpr &expr) override;
  void visit (HIR::CallExpr &expr) override;
  void visit (HIR::MethodCallExpr &expr) override;
  void visit (HIR::LiteralExpr &expr) override;
  void visit (HIR::AssignmentExpr &expr) override;
  void visit (HIR::CompoundAssignmentExpr &expr) override;
  void visit (HIR::ArrayIndexExpr &expr) override;
  void visit (HIR::ArrayExpr &expr) override;
  void visit (HIR::ArithmeticOrLogicalExpr &expr) override;
  void visit (HIR::ComparisonExpr &expr) override;
  void visit (HIR::LazyBooleanExpr &expr) override;
  void visit (HIR::NegationExpr &expr) override;
  void visit (HIR::TypeCastExpr &expr) override;
  void visit (HIR::IfExpr &expr) override;
  void visit (HIR::IfExprConseqElse &expr) override;
  void visit (HIR::BlockExpr &expr) override;
  void visit (HIR::UnsafeBlockExpr &expr) override;
  void visit (HIR::StructExprStruct &struct_expr) override;
  void visit (HIR::StructExprStructFields &struct_expr) override;
  void visit (HIR::GroupedExpr &expr) override;
  void visit (HIR::FieldAccessExpr &expr) override;
  void visit (HIR::QualifiedPathInExpression &expr) override;
  void visit (HIR::PathInExpression &expr) override;
  void visit (HIR::LoopExpr &expr) override;
  void visit (HIR::WhileLoopExpr &expr) override;
  void visit (HIR::BreakExpr &expr) override;
  void visit (HIR::ContinueExpr &expr) override;
  void visit (HIR::BorrowExpr &expr) override;
  void visit (HIR::DereferenceExpr &expr) override;
  void visit (HIR::MatchExpr &expr) override;
  void visit (HIR::RangeFromToExpr &expr) override;
  void visit (HIR::RangeFromExpr &expr) override;
  void visit (HIR::RangeToExpr &expr) override;
  void visit (HIR::RangeFullExpr &expr) override;
  void visit (HIR::RangeFromToInclExpr &expr) override;
  void visit (HIR::ClosureExpr &expr) override;

  // TODO
  void visit (HIR::ErrorPropagationExpr &) override {}
  void visit (HIR::RangeToInclExpr &) override {}

  // TODO
  // these need to be sugared in the HIR to if statements and a match
  void visit (HIR::WhileLetLoopExpr &) override {}
  void visit (HIR::IfLetExpr &) override {}
  void visit (HIR::IfLetExprConseqElse &) override {}

  // lets not worry about async yet....
  void visit (HIR::AwaitExpr &) override {}
  void visit (HIR::AsyncBlockExpr &) override {}

  // nothing to do for these
  void visit (HIR::StructExprFieldIdentifier &) override {}
  void visit (HIR::StructExprFieldIdentifierValue &) override {}
  void visit (HIR::StructExprFieldIndexValue &) override {}

protected:
  tree get_fn_addr_from_dyn (const TyTy::DynamicObjectType *dyn,
			     TyTy::BaseType *receiver, TyTy::FnType *fntype,
			     tree receiver_ref, location_t expr_locus);

  tree get_receiver_from_dyn (const TyTy::DynamicObjectType *dyn,
			      TyTy::BaseType *receiver, TyTy::FnType *fntype,
			      tree receiver_ref, location_t expr_locus);

  tree resolve_operator_overload (LangItem::Kind lang_item_type,
				  HIR::OperatorExprMeta expr, tree lhs,
				  tree rhs, HIR::Expr *lhs_expr,
				  HIR::Expr *rhs_expr);

  tree compile_bool_literal (const HIR::LiteralExpr &expr,
			     const TyTy::BaseType *tyty);

  tree compile_integer_literal (const HIR::LiteralExpr &expr,
				const TyTy::BaseType *tyty);

  tree compile_float_literal (const HIR::LiteralExpr &expr,
			      const TyTy::BaseType *tyty);

  tree compile_char_literal (const HIR::LiteralExpr &expr,
			     const TyTy::BaseType *tyty);

  tree compile_byte_literal (const HIR::LiteralExpr &expr,
			     const TyTy::BaseType *tyty);

  tree compile_string_literal (const HIR::LiteralExpr &expr,
			       const TyTy::BaseType *tyty);

  tree compile_byte_string_literal (const HIR::LiteralExpr &expr,
				    const TyTy::BaseType *tyty);

  tree type_cast_expression (tree type_to_cast_to, tree expr, location_t locus);

  tree array_value_expr (location_t expr_locus,
			 const TyTy::ArrayType &array_tyty, tree array_type,
			 HIR::ArrayElemsValues &elems);

  tree array_copied_expr (location_t expr_locus,
			  const TyTy::ArrayType &array_tyty, tree array_type,
			  HIR::ArrayElemsCopied &elems);

protected:
  tree generate_closure_function (HIR::ClosureExpr &expr,
				  TyTy::ClosureType &closure_tyty,
				  tree compiled_closure_tyty);

  tree generate_closure_fntype (HIR::ClosureExpr &expr,
				const TyTy::ClosureType &closure_tyty,
				tree compiled_closure_tyty,
				TyTy::FnType **fn_tyty);

  bool generate_possible_fn_trait_call (HIR::CallExpr &expr, tree receiver,
					tree *result);

private:
  CompileExpr (Context *ctx);

  tree translated;
};

} // namespace Compile
} // namespace Rust

#endif // RUST_COMPILE_EXPR
