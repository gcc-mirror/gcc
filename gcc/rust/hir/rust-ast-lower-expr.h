// Copyright (C) 2020 Free Software Foundation, Inc.

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

#ifndef RUST_AST_LOWER_EXPR
#define RUST_AST_LOWER_EXPR

#include "rust-diagnostics.h"

#include "rust-ast-lower-base.h"

namespace Rust {
namespace HIR {

class ASTLoweringExpr : public ASTLoweringBase
{
public:
  static HIR::Expr *translate (AST::Expr *expr)
  {
    ASTLoweringExpr resolver;
    expr->accept_vis (resolver);
    if (resolver.translated != nullptr)
      {
	resolver.mappings->insert_hir_expr (
	  resolver.translated->get_mappings ().get_crate_num (),
	  resolver.translated->get_mappings ().get_hirid (),
	  resolver.translated);
      }

    return resolver.translated;
  }

  virtual ~ASTLoweringExpr () {}

  void visit (AST::PathInExpression &expr)
  {
    std::vector<HIR::PathExprSegment> path_segments;
    expr.iterate_path_segments ([&] (AST::PathExprSegment &s) mutable -> bool {
      rust_assert (s.has_generic_args () == false); // TODO

      HIR::PathIdentSegment is (s.get_ident_segment ().as_string ());
      HIR::PathExprSegment seg (is, s.get_locus ());
      path_segments.push_back (seg);
      return true;
    });

    auto crate_num = mappings->get_current_crate ();
    Analysis::NodeMapping mapping (crate_num, expr.get_node_id (),
				   mappings->get_next_hir_id (crate_num),
				   UNKNOWN_LOCAL_DEFID);

    translated = new HIR::PathInExpression (mapping, std::move (path_segments),
					    expr.get_locus (),
					    expr.opening_scope_resolution ());
  }

  void visit (AST::ReturnExpr &expr)
  {
    HIR::Expr *return_expr
      = expr.has_returned_expr ()
	  ? ASTLoweringExpr::translate (expr.get_returned_expr ().get ())
	  : nullptr;

    auto crate_num = mappings->get_current_crate ();
    Analysis::NodeMapping mapping (crate_num, expr.get_node_id (),
				   mappings->get_next_hir_id (crate_num),
				   UNKNOWN_LOCAL_DEFID);

    translated = new HIR::ReturnExpr (mapping, expr.get_locus (),
				      std::unique_ptr<HIR::Expr> (return_expr));
  }

  void visit (AST::CallExpr &expr)
  {
    std::vector<HIR::Attribute> outer_attribs;
    HIR::Expr *func
      = ASTLoweringExpr::translate (expr.get_function_expr ().get ());
    std::vector<std::unique_ptr<HIR::Expr> > params;
    expr.iterate_params ([&] (AST::Expr *p) mutable -> bool {
      auto trans = ASTLoweringExpr::translate (p);
      params.push_back (std::unique_ptr<HIR::Expr> (trans));
      return true;
    });

    auto crate_num = mappings->get_current_crate ();
    Analysis::NodeMapping mapping (
      crate_num, UNKNOWN_NODEID /* this can map back to the AST*/,
      mappings->get_next_hir_id (crate_num), UNKNOWN_LOCAL_DEFID);

    translated
      = new HIR::CallExpr (mapping, std::unique_ptr<HIR::Expr> (func),
			   std::move (params), std::move (outer_attribs),
			   expr.get_locus ());
  }

  void visit (AST::AssignmentExpr &expr)
  {
    HIR::Expr *lhs = ASTLoweringExpr::translate (expr.get_left_expr ().get ());
    HIR::Expr *rhs = ASTLoweringExpr::translate (expr.get_right_expr ().get ());

    auto crate_num = mappings->get_current_crate ();
    Analysis::NodeMapping mapping (crate_num, expr.get_node_id (),
				   mappings->get_next_hir_id (crate_num),
				   UNKNOWN_LOCAL_DEFID);

    translated
      = new HIR::AssignmentExpr (mapping, std::unique_ptr<HIR::Expr> (lhs),
				 std::unique_ptr<HIR::Expr> (rhs),
				 expr.get_locus ());
  }

  void visit (AST::IdentifierExpr &expr)
  {
    auto crate_num = mappings->get_current_crate ();
    Analysis::NodeMapping mapping (crate_num, expr.get_node_id (),
				   mappings->get_next_hir_id (crate_num),
				   UNKNOWN_LOCAL_DEFID);
    translated
      = new HIR::IdentifierExpr (mapping, expr.as_string (), expr.get_locus ());
  }

  void visit (AST::LiteralExpr &expr)
  {
    HIR::Literal::LitType type = HIR::Literal::LitType::CHAR;
    switch (expr.get_lit_type ())
      {
      case AST::Literal::LitType::CHAR:
	type = HIR::Literal::LitType::CHAR;
	break;
      case AST::Literal::LitType::STRING:
	type = HIR::Literal::LitType::STRING;
	break;
      case AST::Literal::LitType::RAW_STRING:
	type = HIR::Literal::LitType::RAW_STRING;
	break;
      case AST::Literal::LitType::BYTE:
	type = HIR::Literal::LitType::BYTE;
	break;
      case AST::Literal::LitType::BYTE_STRING:
	type = HIR::Literal::LitType::BYTE_STRING;
	break;
      case AST::Literal::LitType::RAW_BYTE_STRING:
	type = HIR::Literal::LitType::RAW_BYTE_STRING;
	break;
      case AST::Literal::LitType::INT:
	type = HIR::Literal::LitType::INT;
	break;
      case AST::Literal::LitType::FLOAT:
	type = HIR::Literal::LitType::FLOAT;
	break;
      case AST::Literal::LitType::BOOL:
	type = HIR::Literal::LitType::BOOL;
	break;
      }
    auto crate_num = mappings->get_current_crate ();
    Analysis::NodeMapping mapping (crate_num, expr.get_node_id (),
				   mappings->get_next_hir_id (crate_num),
				   UNKNOWN_LOCAL_DEFID);

    translated = new HIR::LiteralExpr (mapping, expr.as_string (), type,
				       expr.get_locus ());
  }

  void visit (AST::ArithmeticOrLogicalExpr &expr)
  {
    HIR::ArithmeticOrLogicalExpr::ExprType kind
      = HIR::ArithmeticOrLogicalExpr::ExprType::ADD;
    switch (expr.get_expr_type ())
      {
      case AST::ArithmeticOrLogicalExpr::ExprType::ADD:
	kind = HIR::ArithmeticOrLogicalExpr::ExprType::ADD;
	break;
      case AST::ArithmeticOrLogicalExpr::ExprType::SUBTRACT:
	kind = HIR::ArithmeticOrLogicalExpr::ExprType::SUBTRACT;
	break;
      case AST::ArithmeticOrLogicalExpr::ExprType::MULTIPLY:
	kind = HIR::ArithmeticOrLogicalExpr::ExprType::MULTIPLY;
	break;
      case AST::ArithmeticOrLogicalExpr::ExprType::DIVIDE:
	kind = HIR::ArithmeticOrLogicalExpr::ExprType::DIVIDE;
	break;
      case AST::ArithmeticOrLogicalExpr::ExprType::MODULUS:
	kind = HIR::ArithmeticOrLogicalExpr::ExprType::MODULUS;
	break;
      case AST::ArithmeticOrLogicalExpr::ExprType::BITWISE_AND:
	kind = HIR::ArithmeticOrLogicalExpr::ExprType::BITWISE_AND;
	break;
      case AST::ArithmeticOrLogicalExpr::ExprType::BITWISE_OR:
	kind = HIR::ArithmeticOrLogicalExpr::ExprType::BITWISE_OR;
	break;
      case AST::ArithmeticOrLogicalExpr::ExprType::BITWISE_XOR:
	kind = HIR::ArithmeticOrLogicalExpr::ExprType::BITWISE_XOR;
	break;
      case AST::ArithmeticOrLogicalExpr::ExprType::LEFT_SHIFT:
	kind = HIR::ArithmeticOrLogicalExpr::ExprType::LEFT_SHIFT;
	break;
      case AST::ArithmeticOrLogicalExpr::ExprType::RIGHT_SHIFT:
	kind = HIR::ArithmeticOrLogicalExpr::ExprType::RIGHT_SHIFT;
	break;
      }

    HIR::Expr *lhs = ASTLoweringExpr::translate (expr.get_left_expr ().get ());
    rust_assert (lhs != nullptr);
    HIR::Expr *rhs = ASTLoweringExpr::translate (expr.get_right_expr ().get ());
    rust_assert (rhs != nullptr);

    auto crate_num = mappings->get_current_crate ();
    Analysis::NodeMapping mapping (crate_num, expr.get_node_id (),
				   mappings->get_next_hir_id (crate_num),
				   UNKNOWN_LOCAL_DEFID);

    translated
      = new HIR::ArithmeticOrLogicalExpr (mapping,
					  std::unique_ptr<HIR::Expr> (lhs),
					  std::unique_ptr<HIR::Expr> (rhs),
					  kind, expr.get_locus ());
  }

private:
  ASTLoweringExpr () : translated (nullptr) {}

  HIR::Expr *translated;
};

} // namespace HIR
} // namespace Rust

#endif // RUST_AST_LOWER_EXPR
