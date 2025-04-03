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

#include "rust-ast-lower-expr.h"
#include "optional.h"
#include "rust-ast-lower-base.h"
#include "rust-ast-lower-block.h"
#include "rust-ast-lower-struct-field-expr.h"
#include "rust-ast-lower-pattern.h"
#include "rust-ast-lower-type.h"
#include "rust-ast.h"
#include "rust-diagnostics.h"
#include "rust-system.h"
#include "tree/rust-hir-expr.h"

namespace Rust {
namespace HIR {

ASTLoweringExpr::ASTLoweringExpr ()
  : ASTLoweringBase (), translated (nullptr), translated_array_elems (nullptr),
    terminated (false)
{}

HIR::Expr *
ASTLoweringExpr::translate (AST::Expr &expr, bool *terminated)
{
  ASTLoweringExpr resolver;
  expr.accept_vis (resolver);
  if (resolver.translated == nullptr)
    {
      rust_fatal_error (expr.get_locus (), "Failed to lower expr: [%s]",
			expr.as_string ().c_str ());
      return nullptr;
    }

  resolver.mappings.insert_hir_expr (resolver.translated);
  resolver.mappings.insert_location (
    resolver.translated->get_mappings ().get_hirid (), expr.get_locus ());

  if (terminated != nullptr)
    *terminated = resolver.terminated;

  return resolver.translated;
}

void
ASTLoweringExpr::visit (AST::TupleIndexExpr &expr)
{
  HIR::Expr *tuple_expr
    = ASTLoweringExpr::translate (expr.get_tuple_expr (), &terminated);

  auto crate_num = mappings.get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, expr.get_node_id (),
				 mappings.get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);

  translated
    = new HIR::TupleIndexExpr (mapping, std::unique_ptr<HIR::Expr> (tuple_expr),
			       expr.get_tuple_index (), expr.get_outer_attrs (),
			       expr.get_locus ());
}

void
ASTLoweringExpr::visit (AST::TupleExpr &expr)
{
  std::vector<std::unique_ptr<HIR::Expr>> tuple_elements;
  for (auto &e : expr.get_tuple_elems ())
    {
      HIR::Expr *t = ASTLoweringExpr::translate (*e);
      tuple_elements.push_back (std::unique_ptr<HIR::Expr> (t));
    }

  auto crate_num = mappings.get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, expr.get_node_id (),
				 mappings.get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);

  translated
    = new HIR::TupleExpr (std::move (mapping), std::move (tuple_elements),
			  expr.get_inner_attrs (), expr.get_outer_attrs (),
			  expr.get_locus ());
}

void
ASTLoweringExpr::visit (AST::IfExpr &expr)
{
  translated = ASTLoweringIfBlock::translate (expr, &terminated);
}

void
ASTLoweringExpr::visit (AST::IfExprConseqElse &expr)
{
  translated = ASTLoweringIfBlock::translate (expr, &terminated);
}

void
ASTLoweringExpr::visit (AST::IfLetExpr &expr)
{
  translated = ASTLoweringIfLetBlock::translate (expr);
}

void
ASTLoweringExpr::visit (AST::IfLetExprConseqElse &expr)
{
  translated = ASTLoweringIfLetBlock::translate (expr);
}

void
ASTLoweringExpr::visit (AST::BlockExpr &expr)
{
  translated = ASTLoweringBlock::translate (expr, &terminated);
}

void
ASTLoweringExpr::visit (AST::UnsafeBlockExpr &expr)
{
  translated = ASTLoweringBlock::translate (expr, &terminated);
}

void
ASTLoweringExpr::visit (AST::PathInExpression &expr)
{
  translated = ASTLowerPathInExpression::translate (expr);
}

void
ASTLoweringExpr::visit (AST::QualifiedPathInExpression &expr)
{
  translated = ASTLowerQualPathInExpression::translate (expr);
}

void
ASTLoweringExpr::visit (AST::BoxExpr &expr)
{
  // Not implemented
  rust_unreachable ();
}

void
ASTLoweringExpr::visit (AST::ReturnExpr &expr)
{
  terminated = true;
  HIR::Expr *return_expr
    = expr.has_returned_expr ()
	? ASTLoweringExpr::translate (expr.get_returned_expr ())
	: nullptr;

  auto crate_num = mappings.get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, expr.get_node_id (),
				 mappings.get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);

  translated = new HIR::ReturnExpr (mapping, expr.get_locus (),
				    std::unique_ptr<HIR::Expr> (return_expr));
}

void
ASTLoweringExpr::visit (AST::CallExpr &expr)
{
  HIR::Expr *func = ASTLoweringExpr::translate (expr.get_function_expr ());

  auto const &in_params = expr.get_params ();
  std::vector<std::unique_ptr<HIR::Expr>> params;
  for (auto &param : in_params)
    {
      auto trans = ASTLoweringExpr::translate (*param);
      params.push_back (std::unique_ptr<HIR::Expr> (trans));
    }

  auto crate_num = mappings.get_current_crate ();
  Analysis::NodeMapping mapping (
    crate_num, UNKNOWN_NODEID /* this can map back to the AST*/,
    mappings.get_next_hir_id (crate_num), UNKNOWN_LOCAL_DEFID);

  translated = new HIR::CallExpr (mapping, std::unique_ptr<HIR::Expr> (func),
				  std::move (params), expr.get_outer_attrs (),
				  expr.get_locus ());
}

void
ASTLoweringExpr::visit (AST::MethodCallExpr &expr)
{
  HIR::PathExprSegment method_path
    = lower_path_expr_seg (expr.get_method_name ());

  HIR::Expr *receiver = ASTLoweringExpr::translate (expr.get_receiver_expr ());

  auto const &in_params = expr.get_params ();
  std::vector<std::unique_ptr<HIR::Expr>> params;
  for (auto &param : in_params)
    {
      auto trans = ASTLoweringExpr::translate (*param);
      params.push_back (std::unique_ptr<HIR::Expr> (trans));
    }

  auto crate_num = mappings.get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, expr.get_node_id (),
				 mappings.get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);

  translated
    = new HIR::MethodCallExpr (mapping, std::unique_ptr<HIR::Expr> (receiver),
			       method_path, std::move (params),
			       expr.get_outer_attrs (), expr.get_locus ());
}

void
ASTLoweringExpr::visit (AST::AssignmentExpr &expr)
{
  HIR::Expr *lhs = ASTLoweringExpr::translate (expr.get_left_expr ());
  HIR::Expr *rhs = ASTLoweringExpr::translate (expr.get_right_expr ());

  auto crate_num = mappings.get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, expr.get_node_id (),
				 mappings.get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);

  translated
    = new HIR::AssignmentExpr (mapping, std::unique_ptr<HIR::Expr> (lhs),
			       std::unique_ptr<HIR::Expr> (rhs),
			       expr.get_locus ());
}

void
ASTLoweringExpr::visit (AST::IdentifierExpr &expr)
{
  auto crate_num = mappings.get_current_crate ();
  Analysis::NodeMapping mapping1 (crate_num, expr.get_node_id (),
				  mappings.get_next_hir_id (crate_num),
				  UNKNOWN_LOCAL_DEFID);
  Analysis::NodeMapping mapping2 (mapping1);

  HIR::PathIdentSegment ident_seg (expr.get_ident ().as_string ());
  HIR::PathExprSegment seg (mapping1, ident_seg, expr.get_locus (),
			    HIR::GenericArgs::create_empty ());
  translated = new HIR::PathInExpression (mapping2, {seg}, expr.get_locus (),
					  false, expr.get_outer_attrs ());
}

void
ASTLoweringExpr::visit (AST::ArrayExpr &expr)
{
  expr.get_array_elems ()->accept_vis (*this);
  rust_assert (translated_array_elems != nullptr);
  HIR::ArrayElems *elems = translated_array_elems;

  auto crate_num = mappings.get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, expr.get_node_id (),
				 mappings.get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);

  translated
    = new HIR::ArrayExpr (mapping, std::unique_ptr<HIR::ArrayElems> (elems),
			  expr.get_inner_attrs (), expr.get_outer_attrs (),
			  expr.get_locus ());
}

void
ASTLoweringExpr::visit (AST::ArrayIndexExpr &expr)
{
  HIR::Expr *array_expr = ASTLoweringExpr::translate (expr.get_array_expr ());
  HIR::Expr *array_index_expr
    = ASTLoweringExpr::translate (expr.get_index_expr ());

  auto crate_num = mappings.get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, expr.get_node_id (),
				 mappings.get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);

  translated
    = new HIR::ArrayIndexExpr (mapping, std::unique_ptr<HIR::Expr> (array_expr),
			       std::unique_ptr<HIR::Expr> (array_index_expr),
			       expr.get_outer_attrs (), expr.get_locus ());
}

void
ASTLoweringExpr::visit (AST::ArrayElemsValues &elems)
{
  std::vector<std::unique_ptr<HIR::Expr>> elements;
  for (auto &elem : elems.get_values ())
    {
      HIR::Expr *translated_elem = ASTLoweringExpr::translate (*elem);
      elements.push_back (std::unique_ptr<HIR::Expr> (translated_elem));
    }

  auto crate_num = mappings.get_current_crate ();
  Analysis::NodeMapping mapping (mappings.get_current_crate (),
				 elems.get_node_id (),
				 mappings.get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);

  translated_array_elems
    = new HIR::ArrayElemsValues (mapping, std::move (elements));
}

void
ASTLoweringExpr::visit (AST::ArrayElemsCopied &elems)
{
  HIR::Expr *element = ASTLoweringExpr::translate (elems.get_elem_to_copy ());
  HIR::Expr *num_copies = ASTLoweringExpr::translate (elems.get_num_copies ());

  auto crate_num = mappings.get_current_crate ();
  Analysis::NodeMapping mapping (mappings.get_current_crate (),
				 elems.get_node_id (),
				 mappings.get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);

  translated_array_elems
    = new HIR::ArrayElemsCopied (mapping, std::unique_ptr<HIR::Expr> (element),
				 std::unique_ptr<HIR::Expr> (num_copies));
}

void
ASTLoweringExpr::visit (AST::LiteralExpr &expr)
{
  auto crate_num = mappings.get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, expr.get_node_id (),
				 mappings.get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);

  HIR::Literal l = lower_literal (expr.get_literal ());
  translated = new HIR::LiteralExpr (mapping, std::move (l), expr.get_locus (),
				     expr.get_outer_attrs ());
}

void
ASTLoweringExpr::visit (AST::ArithmeticOrLogicalExpr &expr)
{
  HIR::Expr *lhs = ASTLoweringExpr::translate (expr.get_left_expr ());
  rust_assert (lhs != nullptr);
  HIR::Expr *rhs = ASTLoweringExpr::translate (expr.get_right_expr ());
  rust_assert (rhs != nullptr);

  auto crate_num = mappings.get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, expr.get_node_id (),
				 mappings.get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);

  translated = new HIR::ArithmeticOrLogicalExpr (
    mapping, std::unique_ptr<HIR::Expr> (lhs), std::unique_ptr<HIR::Expr> (rhs),
    expr.get_expr_type (), expr.get_locus ());
}

void
ASTLoweringExpr::visit (AST::ComparisonExpr &expr)
{
  HIR::Expr *lhs = ASTLoweringExpr::translate (expr.get_left_expr ());
  rust_assert (lhs != nullptr);
  HIR::Expr *rhs = ASTLoweringExpr::translate (expr.get_right_expr ());
  rust_assert (rhs != nullptr);

  auto crate_num = mappings.get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, expr.get_node_id (),
				 mappings.get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);

  translated
    = new HIR::ComparisonExpr (mapping, std::unique_ptr<HIR::Expr> (lhs),
			       std::unique_ptr<HIR::Expr> (rhs),
			       expr.get_expr_type (), expr.get_locus ());
}

void
ASTLoweringExpr::visit (AST::LazyBooleanExpr &expr)
{
  HIR::Expr *lhs = ASTLoweringExpr::translate (expr.get_left_expr ());
  rust_assert (lhs != nullptr);
  HIR::Expr *rhs = ASTLoweringExpr::translate (expr.get_right_expr ());
  rust_assert (rhs != nullptr);

  auto crate_num = mappings.get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, expr.get_node_id (),
				 mappings.get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);

  translated
    = new HIR::LazyBooleanExpr (mapping, std::unique_ptr<HIR::Expr> (lhs),
				std::unique_ptr<HIR::Expr> (rhs),
				expr.get_expr_type (), expr.get_locus ());
}

void
ASTLoweringExpr::visit (AST::NegationExpr &expr)
{
  HIR::Expr *negated_value
    = ASTLoweringExpr::translate (expr.get_negated_expr ());

  auto crate_num = mappings.get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, expr.get_node_id (),
				 mappings.get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);
  translated
    = new HIR::NegationExpr (mapping,
			     std::unique_ptr<HIR::Expr> (negated_value),
			     expr.get_expr_type (), expr.get_outer_attrs (),
			     expr.get_locus ());
}

void
ASTLoweringExpr::visit (AST::TypeCastExpr &expr)
{
  HIR::Expr *expr_to_cast_to
    = ASTLoweringExpr::translate (expr.get_casted_expr ());
  HIR::Type *type_to_cast_to
    = lower_type_no_bounds (expr.get_type_to_cast_to ());

  auto crate_num = mappings.get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, expr.get_node_id (),
				 mappings.get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);

  translated
    = new HIR::TypeCastExpr (mapping,
			     std::unique_ptr<HIR::Expr> (expr_to_cast_to),
			     std::unique_ptr<HIR::Type> (type_to_cast_to),
			     expr.get_locus ());
}

void
ASTLoweringExpr::visit (AST::CompoundAssignmentExpr &expr)
{
  ArithmeticOrLogicalOperator op;
  switch (expr.get_expr_type ())
    {
    case CompoundAssignmentOperator::ADD:
      op = ArithmeticOrLogicalOperator::ADD;
      break;
    case CompoundAssignmentOperator::SUBTRACT:
      op = ArithmeticOrLogicalOperator::SUBTRACT;
      break;
    case CompoundAssignmentOperator::MULTIPLY:
      op = ArithmeticOrLogicalOperator::MULTIPLY;
      break;
    case CompoundAssignmentOperator::DIVIDE:
      op = ArithmeticOrLogicalOperator::DIVIDE;
      break;
    case CompoundAssignmentOperator::MODULUS:
      op = ArithmeticOrLogicalOperator::MODULUS;
      break;
    case CompoundAssignmentOperator::BITWISE_AND:
      op = ArithmeticOrLogicalOperator::BITWISE_AND;
      break;
    case CompoundAssignmentOperator::BITWISE_OR:
      op = ArithmeticOrLogicalOperator::BITWISE_OR;
      break;
    case CompoundAssignmentOperator::BITWISE_XOR:
      op = ArithmeticOrLogicalOperator::BITWISE_XOR;
      break;
    case CompoundAssignmentOperator::LEFT_SHIFT:
      op = ArithmeticOrLogicalOperator::LEFT_SHIFT;
      break;
    case CompoundAssignmentOperator::RIGHT_SHIFT:
      op = ArithmeticOrLogicalOperator::RIGHT_SHIFT;
      break;
    default:
      rust_unreachable ();
    }

  HIR::Expr *asignee_expr = ASTLoweringExpr::translate (expr.get_left_expr ());
  HIR::Expr *value = ASTLoweringExpr::translate (expr.get_right_expr ());

  auto crate_num = mappings.get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, expr.get_node_id (),
				 mappings.get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);

  translated = new HIR::CompoundAssignmentExpr (
    mapping, std::unique_ptr<HIR::Expr> (asignee_expr),
    std::unique_ptr<HIR::Expr> (value), op, expr.get_locus ());
}

void
ASTLoweringExpr::visit (AST::StructExprStruct &struct_expr)
{
  HIR::PathInExpression *path
    = ASTLowerPathInExpression::translate (struct_expr.get_struct_name ());
  HIR::PathInExpression copied_path (*path);
  delete path;

  auto crate_num = mappings.get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, struct_expr.get_node_id (),
				 mappings.get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);

  translated = new HIR::StructExprStruct (mapping, copied_path,
					  struct_expr.get_inner_attrs (),
					  struct_expr.get_outer_attrs (),
					  struct_expr.get_locus ());
}

void
ASTLoweringExpr::visit (AST::StructExprStructFields &struct_expr)
{
  // bit of a hack for now
  HIR::PathInExpression *path
    = ASTLowerPathInExpression::translate (struct_expr.get_struct_name ());
  HIR::PathInExpression copied_path (*path);
  delete path;

  tl::optional<std::unique_ptr<HIR::StructBase>> base = tl::nullopt;
  if (struct_expr.has_struct_base ())
    {
      HIR::Expr *translated_base = ASTLoweringExpr::translate (
	struct_expr.get_struct_base ().get_base_struct ());
      base = tl::optional<std::unique_ptr<HIR::StructBase>> (
	std::make_unique<StructBase> (
	  std::unique_ptr<HIR::Expr> (translated_base)));
    }

  auto const &in_fields = struct_expr.get_fields ();
  std::vector<std::unique_ptr<HIR::StructExprField>> fields;
  for (auto &field : in_fields)
    {
      HIR::StructExprField *translated
	= ASTLowerStructExprField::translate (*field);
      fields.push_back (std::unique_ptr<HIR::StructExprField> (translated));
    }

  auto crate_num = mappings.get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, struct_expr.get_node_id (),
				 mappings.get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);

  translated
    = new HIR::StructExprStructFields (mapping, copied_path, std::move (fields),
				       struct_expr.get_locus (),
				       std::move (base),
				       struct_expr.get_inner_attrs (),
				       struct_expr.get_outer_attrs ());
}

void
ASTLoweringExpr::visit (AST::GroupedExpr &expr)
{
  HIR::Expr *paren_expr
    = ASTLoweringExpr::translate (expr.get_expr_in_parens ());

  auto crate_num = mappings.get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, expr.get_node_id (),
				 mappings.get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);

  translated
    = new HIR::GroupedExpr (mapping, std::unique_ptr<HIR::Expr> (paren_expr),
			    expr.get_inner_attrs (), expr.get_outer_attrs (),
			    expr.get_locus ());
}

void
ASTLoweringExpr::visit (AST::FieldAccessExpr &expr)
{
  HIR::Expr *receiver = ASTLoweringExpr::translate (expr.get_receiver_expr ());

  auto crate_num = mappings.get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, expr.get_node_id (),
				 mappings.get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);
  translated
    = new HIR::FieldAccessExpr (mapping, std::unique_ptr<HIR::Expr> (receiver),
				expr.get_field_name (), expr.get_outer_attrs (),
				expr.get_locus ());
}

void
ASTLoweringExpr::visit (AST::LoopExpr &expr)
{
  translated = ASTLoweringExprWithBlock::translate (expr, &terminated);
}

void
ASTLoweringExpr::visit (AST::WhileLoopExpr &expr)
{
  translated = ASTLoweringExprWithBlock::translate (expr, &terminated);
}

void
ASTLoweringExpr::visit (AST::ForLoopExpr &expr)
{
  rust_unreachable ();
}

void
ASTLoweringExpr::visit (AST::BreakExpr &expr)
{
  tl::optional<HIR::Lifetime> break_label = tl::nullopt;
  if (expr.has_label ())
    break_label = lower_lifetime (expr.get_label_unchecked ().get_lifetime ());

  HIR::Expr *break_expr
    = expr.has_break_expr ()
	? ASTLoweringExpr::translate (expr.get_break_expr ())
	: nullptr;

  auto crate_num = mappings.get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, expr.get_node_id (),
				 mappings.get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);

  translated
    = new HIR::BreakExpr (mapping, expr.get_locus (), std ::move (break_label),
			  std::unique_ptr<HIR::Expr> (break_expr),
			  expr.get_outer_attrs ());
}

void
ASTLoweringExpr::visit (AST::ContinueExpr &expr)
{
  tl::optional<HIR::Lifetime> break_label;
  if (expr.has_label ())
    break_label = lower_lifetime (expr.get_label_unchecked ());

  auto crate_num = mappings.get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, expr.get_node_id (),
				 mappings.get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);

  translated
    = new HIR::ContinueExpr (mapping, expr.get_locus (),
			     std ::move (break_label), expr.get_outer_attrs ());
}

void
ASTLoweringExpr::visit (AST::BorrowExpr &expr)
{
  HIR::Expr *borrow_lvalue
    = ASTLoweringExpr::translate (expr.get_borrowed_expr ());

  auto crate_num = mappings.get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, expr.get_node_id (),
				 mappings.get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);

  auto *borrow_expr
    = new HIR::BorrowExpr (mapping, std::unique_ptr<HIR::Expr> (borrow_lvalue),
			   expr.get_mutability (), expr.is_raw_borrow (),
			   expr.get_outer_attrs (), expr.get_locus ());

  if (expr.get_is_double_borrow ())
    {
      NodeId artificial_double_borrow_id = mappings.get_next_node_id ();
      Analysis::NodeMapping mapping (crate_num, artificial_double_borrow_id,
				     mappings.get_next_hir_id (crate_num),
				     UNKNOWN_LOCAL_DEFID);

      borrow_expr
	= new HIR::BorrowExpr (mapping,
			       std::unique_ptr<HIR::Expr> (borrow_expr),
			       expr.get_mutability (), expr.is_raw_borrow (),
			       expr.get_outer_attrs (), expr.get_locus ());
    }

  translated = borrow_expr;
}

void
ASTLoweringExpr::visit (AST::DereferenceExpr &expr)
{
  HIR::Expr *dref_lvalue
    = ASTLoweringExpr::translate (expr.get_dereferenced_expr ());

  auto crate_num = mappings.get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, expr.get_node_id (),
				 mappings.get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);

  translated
    = new HIR::DereferenceExpr (mapping,
				std::unique_ptr<HIR::Expr> (dref_lvalue),
				expr.get_outer_attrs (), expr.get_locus ());
}

void
ASTLoweringExpr::visit (AST::MatchExpr &expr)
{
  translated = ASTLoweringExprWithBlock::translate (expr, &terminated);
}

void
ASTLoweringExpr::visit (AST::RangeFromToExpr &expr)
{
  auto crate_num = mappings.get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, expr.get_node_id (),
				 mappings.get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);

  HIR::Expr *range_from = ASTLoweringExpr::translate (expr.get_from_expr ());
  HIR::Expr *range_to = ASTLoweringExpr::translate (expr.get_to_expr ());

  translated
    = new HIR::RangeFromToExpr (mapping,
				std::unique_ptr<HIR::Expr> (range_from),
				std::unique_ptr<HIR::Expr> (range_to),
				expr.get_locus ());
}

void
ASTLoweringExpr::visit (AST::RangeFromExpr &expr)
{
  auto crate_num = mappings.get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, expr.get_node_id (),
				 mappings.get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);

  HIR::Expr *range_from = ASTLoweringExpr::translate (expr.get_from_expr ());

  translated
    = new HIR::RangeFromExpr (mapping, std::unique_ptr<HIR::Expr> (range_from),
			      expr.get_locus ());
}

void
ASTLoweringExpr::visit (AST::RangeToExpr &expr)
{
  auto crate_num = mappings.get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, expr.get_node_id (),
				 mappings.get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);

  HIR::Expr *range_to = ASTLoweringExpr::translate (expr.get_to_expr ());

  translated
    = new HIR::RangeToExpr (mapping, std::unique_ptr<HIR::Expr> (range_to),
			    expr.get_locus ());
}

void
ASTLoweringExpr::visit (AST::RangeFullExpr &expr)
{
  auto crate_num = mappings.get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, expr.get_node_id (),
				 mappings.get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);

  translated = new HIR::RangeFullExpr (mapping, expr.get_locus ());
}

void
ASTLoweringExpr::visit (AST::RangeFromToInclExpr &expr)
{
  auto crate_num = mappings.get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, expr.get_node_id (),
				 mappings.get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);

  HIR::Expr *range_from = ASTLoweringExpr::translate (expr.get_from_expr ());
  HIR::Expr *range_to = ASTLoweringExpr::translate (expr.get_to_expr ());

  translated
    = new HIR::RangeFromToInclExpr (mapping,
				    std::unique_ptr<HIR::Expr> (range_from),
				    std::unique_ptr<HIR::Expr> (range_to),
				    expr.get_locus ());
}

void
ASTLoweringExpr::visit (AST::ClosureExprInner &expr)
{
  HIR::Expr *closure_expr
    = ASTLoweringExpr::translate (expr.get_definition_expr ());

  std::vector<HIR::ClosureParam> closure_params;
  for (auto &param : expr.get_params ())
    {
      HIR::ClosureParam p = lower_closure_param (param);
      closure_params.push_back (std::move (p));
    }

  auto crate_num = mappings.get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, expr.get_node_id (),
				 mappings.get_next_hir_id (crate_num),
				 mappings.get_next_localdef_id (crate_num));

  translated
    = new HIR::ClosureExpr (mapping, std::move (closure_params),
			    nullptr /* closure_return_type */,
			    std::unique_ptr<HIR::Expr> (closure_expr),
			    expr.get_has_move (), expr.get_outer_attrs (),
			    expr.get_locus ());
}

void
ASTLoweringExpr::visit (AST::ClosureExprInnerTyped &expr)
{
  HIR::Type *closure_return_type = nullptr;
  HIR::Expr *closure_expr
    = ASTLoweringExpr::translate (expr.get_definition_block ());

  std::vector<HIR::ClosureParam> closure_params;
  for (auto &param : expr.get_params ())
    {
      HIR::ClosureParam p = lower_closure_param (param);
      closure_params.push_back (std::move (p));
    }

  auto crate_num = mappings.get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, expr.get_node_id (),
				 mappings.get_next_hir_id (crate_num),
				 mappings.get_next_localdef_id (crate_num));

  translated
    = new HIR::ClosureExpr (mapping, std::move (closure_params),
			    std::unique_ptr<HIR::Type> (closure_return_type),
			    std::unique_ptr<HIR::Expr> (closure_expr),
			    expr.get_has_move (), expr.get_outer_attrs (),
			    expr.get_locus ());
}

HIR::InlineAsmOperand
translate_operand_in (const AST::InlineAsmOperand &operand)
{
  auto in_value = operand.get_in ();

  struct HIR::InlineAsmOperand::In in (
    in_value.reg,
    std::unique_ptr<Expr> (ASTLoweringExpr::translate (*in_value.expr.get ())));
  return in;
}

HIR::InlineAsmOperand
translate_operand_out (const AST::InlineAsmOperand &operand)
{
  auto out_value = operand.get_out ();
  struct HIR::InlineAsmOperand::Out out (out_value.reg, out_value.late,
					 std::unique_ptr<Expr> (
					   ASTLoweringExpr::translate (
					     *out_value.expr.get ())));
  return out;
}
HIR::InlineAsmOperand
translate_operand_inout (const AST::InlineAsmOperand &operand)
{
  auto inout_value = operand.get_in_out ();
  struct HIR::InlineAsmOperand::InOut inout (inout_value.reg, inout_value.late,
					     std::unique_ptr<Expr> (
					       ASTLoweringExpr::translate (
						 *inout_value.expr.get ())));
  return inout;
}
HIR::InlineAsmOperand
translate_operand_split_in_out (const AST::InlineAsmOperand &operand)
{
  auto split_in_out_value = operand.get_split_in_out ();
  struct HIR::InlineAsmOperand::SplitInOut split_in_out (
    split_in_out_value.reg, split_in_out_value.late,
    std::unique_ptr<Expr> (
      ASTLoweringExpr::translate (*split_in_out_value.in_expr.get ())),
    std::unique_ptr<Expr> (
      ASTLoweringExpr::translate (*split_in_out_value.out_expr.get ())));
  return split_in_out;
}
HIR::InlineAsmOperand
translate_operand_const (const AST::InlineAsmOperand &operand)
{
  auto const_value = operand.get_const ();
  struct HIR::AnonConst anon_const (const_value.anon_const.id,
				    std::unique_ptr<Expr> (
				      ASTLoweringExpr::translate (
					*const_value.anon_const.expr.get ())));
  struct HIR::InlineAsmOperand::Const cnst
  {
    anon_const
  };
  return cnst;
}

HIR::InlineAsmOperand
translate_operand_sym (const AST::InlineAsmOperand &operand)
{
  auto sym_value = operand.get_sym ();
  struct HIR::InlineAsmOperand::Sym sym (std::unique_ptr<Expr> (
    ASTLoweringExpr::translate (*sym_value.expr.get ())));
  return sym;
}
HIR::InlineAsmOperand
translate_operand_label (const AST::InlineAsmOperand &operand)
{
  auto label_value = operand.get_label ();
  struct HIR::InlineAsmOperand::Label label (label_value.label_name,
					     std::unique_ptr<Expr> (
					       ASTLoweringExpr::translate (
						 *label_value.expr.get ())));
  return label;
}
HIR::InlineAsmOperand
from_operand (const AST::InlineAsmOperand &operand)
{
  using RegisterType = AST::InlineAsmOperand::RegisterType;
  auto type = operand.get_register_type ();

  /*In,*/
  /*Out,*/
  /*InOut,*/
  /*SplitInOut,*/
  /*Const,*/
  /*Sym,*/
  /*Label,*/
  switch (type)
    {
    case RegisterType::In:
      return translate_operand_in (operand);
    case RegisterType::Out:
      return translate_operand_out (operand);
    case RegisterType::InOut:
      return translate_operand_inout (operand);
    case RegisterType::SplitInOut:
      return translate_operand_split_in_out (operand);
    case RegisterType::Const:
      return translate_operand_const (operand);
    case RegisterType::Sym:
      return translate_operand_sym (operand);
    case RegisterType::Label:
      return translate_operand_label (operand);
    default:
      rust_unreachable ();
    }
}
void
ASTLoweringExpr::visit (AST::InlineAsm &expr)
{
  auto crate_num = mappings.get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, expr.get_node_id (),
				 mappings.get_next_hir_id (crate_num),
				 mappings.get_next_localdef_id (crate_num));

  std::vector<HIR::InlineAsmOperand> hir_operands;
  const std::vector<AST::InlineAsmOperand> &ast_operands = expr.get_operands ();
  /*int ast_operands_size = ast_operands.size ();*/
  for (auto &operand : ast_operands)
    {
      hir_operands.push_back (from_operand (operand));
    }
  /*int hir_operands_size = hir_operands.size ();*/

  /*rust_debug ("{bdbt} : There are %d ast operands prelowering and %d hir "*/
  /*     "operands after lowering\n",*/
  /*     ast_operands_size, hir_operands_size);*/
  translated
    = new HIR::InlineAsm (expr.get_locus (), expr.is_global_asm,
			  expr.get_template_ (), expr.get_template_strs (),
			  hir_operands, expr.get_clobber_abi (),
			  expr.get_options (), mapping);
}
void
ASTLoweringExpr::visit (AST::FormatArgs &fmt)
{
  rust_sorry_at (fmt.get_locus (),
		 "FormatArgs lowering is not implemented yet");
}

} // namespace HIR
} // namespace Rust
