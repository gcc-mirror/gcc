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

#ifndef RUST_AST_LOWER_STMT
#define RUST_AST_LOWER_STMT

#include "rust-diagnostics.h"

#include "rust-ast-lower-base.h"
#include "rust-ast-lower-type.h"
#include "rust-ast-lower-block.h"
#include "rust-ast-lower-expr.h"
#include "rust-ast-lower-pattern.h"

namespace Rust {
namespace HIR {

class ASTLoweringStmt : public ASTLoweringBase
{
  using Rust::HIR::ASTLoweringBase::visit;

public:
  static HIR::Stmt *translate (AST::Stmt *stmt, bool *terminated)
  {
    ASTLoweringStmt resolver;
    stmt->accept_vis (resolver);
    rust_assert (resolver.translated != nullptr);
    *terminated = resolver.terminated;
    return resolver.translated;
  }

  void visit (AST::ExprStmtWithBlock &stmt) override
  {
    HIR::ExprWithBlock *expr
      = ASTLoweringExprWithBlock::translate (stmt.get_expr ().get (),
					     &terminated);

    auto crate_num = mappings->get_current_crate ();
    Analysis::NodeMapping mapping (crate_num, stmt.get_node_id (),
				   mappings->get_next_hir_id (crate_num),
				   UNKNOWN_LOCAL_DEFID);
    translated
      = new HIR::ExprStmtWithBlock (mapping,
				    std::unique_ptr<HIR::ExprWithBlock> (expr),
				    stmt.get_locus (),
				    !stmt.is_semicolon_followed ());
    mappings->insert_location (crate_num, mapping.get_hirid (),
			       stmt.get_locus ());
    mappings->insert_hir_stmt (crate_num, mapping.get_hirid (), translated);
  }

  void visit (AST::ExprStmtWithoutBlock &stmt) override
  {
    HIR::Expr *expr
      = ASTLoweringExpr::translate (stmt.get_expr ().get (), &terminated);

    auto crate_num = mappings->get_current_crate ();
    Analysis::NodeMapping mapping (crate_num, stmt.get_node_id (),
				   mappings->get_next_hir_id (crate_num),
				   UNKNOWN_LOCAL_DEFID);
    translated
      = new HIR::ExprStmtWithoutBlock (mapping,
				       std::unique_ptr<HIR::Expr> (expr),
				       stmt.get_locus ());
    mappings->insert_location (crate_num, mapping.get_hirid (),
			       stmt.get_locus ());
    mappings->insert_hir_stmt (crate_num, mapping.get_hirid (), translated);
  }

  void visit (AST::LetStmt &stmt) override
  {
    HIR::Pattern *variables
      = ASTLoweringPattern::translate (stmt.get_pattern ().get ());
    HIR::Type *type = stmt.has_type ()
			? ASTLoweringType::translate (stmt.get_type ().get ())
			: nullptr;
    HIR::Expr *init_expression
      = stmt.has_init_expr ()
	  ? ASTLoweringExpr::translate (stmt.get_init_expr ().get ())
	  : nullptr;

    auto crate_num = mappings->get_current_crate ();
    Analysis::NodeMapping mapping (crate_num, stmt.get_node_id (),
				   mappings->get_next_hir_id (crate_num),
				   UNKNOWN_LOCAL_DEFID);
    translated
      = new HIR::LetStmt (mapping, std::unique_ptr<HIR::Pattern> (variables),
			  std::unique_ptr<HIR::Expr> (init_expression),
			  std::unique_ptr<HIR::Type> (type),
			  stmt.get_outer_attrs (), stmt.get_locus ());
    mappings->insert_location (crate_num, mapping.get_hirid (),
			       stmt.get_locus ());
    mappings->insert_hir_stmt (crate_num, mapping.get_hirid (), translated);
  }

  void visit (AST::TupleStruct &struct_decl) override
  {
    std::vector<std::unique_ptr<HIR::GenericParam> > generic_params;
    if (struct_decl.has_generics ())
      {
	generic_params
	  = lower_generic_params (struct_decl.get_generic_params ());
      }

    std::vector<std::unique_ptr<HIR::WhereClauseItem> > where_clause_items;
    HIR::WhereClause where_clause (std::move (where_clause_items));
    HIR::Visibility vis = HIR::Visibility::create_public ();

    std::vector<HIR::TupleField> fields;
    struct_decl.iterate ([&] (AST::TupleField &field) mutable -> bool {
      HIR::Visibility vis = HIR::Visibility::create_public ();
      HIR::Type *type
	= ASTLoweringType::translate (field.get_field_type ().get ());

      auto crate_num = mappings->get_current_crate ();
      Analysis::NodeMapping mapping (crate_num, field.get_node_id (),
				     mappings->get_next_hir_id (crate_num),
				     mappings->get_next_localdef_id (
				       crate_num));

      // FIXME
      // AST::TupleField is missing Location info
      Location field_locus;
      HIR::TupleField translated_field (mapping,
					std::unique_ptr<HIR::Type> (type), vis,
					field_locus, field.get_outer_attrs ());
      fields.push_back (std::move (translated_field));
      return true;
    });

    auto crate_num = mappings->get_current_crate ();
    Analysis::NodeMapping mapping (crate_num, struct_decl.get_node_id (),
				   mappings->get_next_hir_id (crate_num),
				   mappings->get_next_localdef_id (crate_num));

    translated = new HIR::TupleStruct (mapping, std::move (fields),
				       struct_decl.get_identifier (),
				       std::move (generic_params),
				       std::move (where_clause), vis,
				       struct_decl.get_outer_attrs (),
				       struct_decl.get_locus ());

    mappings->insert_hir_stmt (mapping.get_crate_num (), mapping.get_hirid (),
			       translated);
    mappings->insert_location (crate_num, mapping.get_hirid (),
			       struct_decl.get_locus ());
  }

  void visit (AST::StructStruct &struct_decl) override
  {
    std::vector<std::unique_ptr<HIR::GenericParam> > generic_params;
    if (struct_decl.has_generics ())
      {
	generic_params
	  = lower_generic_params (struct_decl.get_generic_params ());
      }

    std::vector<std::unique_ptr<HIR::WhereClauseItem> > where_clause_items;
    HIR::WhereClause where_clause (std::move (where_clause_items));
    HIR::Visibility vis = HIR::Visibility::create_public ();

    bool is_unit = struct_decl.is_unit_struct ();
    std::vector<HIR::StructField> fields;
    struct_decl.iterate ([&] (AST::StructField &field) mutable -> bool {
      HIR::Visibility vis = HIR::Visibility::create_public ();
      HIR::Type *type
	= ASTLoweringType::translate (field.get_field_type ().get ());

      auto crate_num = mappings->get_current_crate ();
      Analysis::NodeMapping mapping (crate_num, field.get_node_id (),
				     mappings->get_next_hir_id (crate_num),
				     mappings->get_next_localdef_id (
				       crate_num));

      // FIXME
      // AST::StructField is missing Location info
      Location field_locus;
      HIR::StructField translated_field (mapping, field.get_field_name (),
					 std::unique_ptr<HIR::Type> (type), vis,
					 field_locus, field.get_outer_attrs ());
      fields.push_back (std::move (translated_field));
      return true;
    });

    auto crate_num = mappings->get_current_crate ();
    Analysis::NodeMapping mapping (crate_num, struct_decl.get_node_id (),
				   mappings->get_next_hir_id (crate_num),
				   mappings->get_next_localdef_id (crate_num));

    translated = new HIR::StructStruct (mapping, std::move (fields),
					struct_decl.get_identifier (),
					std::move (generic_params),
					std::move (where_clause), is_unit, vis,
					struct_decl.get_outer_attrs (),
					struct_decl.get_locus ());

    mappings->insert_hir_stmt (mapping.get_crate_num (), mapping.get_hirid (),
			       translated);
    mappings->insert_location (crate_num, mapping.get_hirid (),
			       struct_decl.get_locus ());
  }

  void visit (AST::EmptyStmt &empty) override
  {
    auto crate_num = mappings->get_current_crate ();
    Analysis::NodeMapping mapping (crate_num, empty.get_node_id (),
				   mappings->get_next_hir_id (crate_num),
				   mappings->get_next_localdef_id (crate_num));

    translated = new HIR::EmptyStmt (mapping, empty.get_locus ());

    mappings->insert_hir_stmt (mapping.get_crate_num (), mapping.get_hirid (),
			       translated);
    mappings->insert_location (crate_num, mapping.get_hirid (),
			       empty.get_locus ());
  }

  void visit (AST::Function &function) override
  {
    // ignore for now and leave empty
    std::vector<std::unique_ptr<HIR::WhereClauseItem> > where_clause_items;
    HIR::WhereClause where_clause (std::move (where_clause_items));
    HIR::FunctionQualifiers qualifiers (
      HIR::FunctionQualifiers::AsyncConstStatus::NONE, false);
    HIR::Visibility vis = HIR::Visibility::create_public ();

    // need
    std::vector<std::unique_ptr<HIR::GenericParam> > generic_params;
    if (function.has_generics ())
      {
	generic_params = lower_generic_params (function.get_generic_params ());
      }

    Identifier function_name = function.get_function_name ();
    Location locus = function.get_locus ();

    std::unique_ptr<HIR::Type> return_type
      = function.has_return_type () ? std::unique_ptr<HIR::Type> (
	  ASTLoweringType::translate (function.get_return_type ().get ()))
				    : nullptr;

    std::vector<HIR::FunctionParam> function_params;
    for (auto &param : function.get_function_params ())
      {
	auto translated_pattern = std::unique_ptr<HIR::Pattern> (
	  ASTLoweringPattern::translate (param.get_pattern ().get ()));
	auto translated_type = std::unique_ptr<HIR::Type> (
	  ASTLoweringType::translate (param.get_type ().get ()));

	auto crate_num = mappings->get_current_crate ();
	Analysis::NodeMapping mapping (crate_num, param.get_node_id (),
				       mappings->get_next_hir_id (crate_num),
				       UNKNOWN_LOCAL_DEFID);

	auto hir_param
	  = HIR::FunctionParam (mapping, std::move (translated_pattern),
				std::move (translated_type),
				param.get_locus ());
	function_params.push_back (hir_param);
      }

    bool terminated = false;
    std::unique_ptr<HIR::BlockExpr> function_body
      = std::unique_ptr<HIR::BlockExpr> (
	ASTLoweringBlock::translate (function.get_definition ().get (),
				     &terminated));

    auto crate_num = mappings->get_current_crate ();
    Analysis::NodeMapping mapping (crate_num, function.get_node_id (),
				   mappings->get_next_hir_id (crate_num),
				   UNKNOWN_LOCAL_DEFID);

    mappings->insert_location (crate_num,
			       function_body->get_mappings ().get_hirid (),
			       function.get_locus ());

    auto fn
      = new HIR::Function (mapping, std::move (function_name),
			   std::move (qualifiers), std::move (generic_params),
			   std::move (function_params), std::move (return_type),
			   std::move (where_clause), std::move (function_body),
			   std::move (vis), function.get_outer_attrs (),
			   HIR::SelfParam::error (), locus);

    mappings->insert_hir_item (mapping.get_crate_num (), mapping.get_hirid (),
			       fn);
    mappings->insert_hir_stmt (mapping.get_crate_num (), mapping.get_hirid (),
			       fn);
    mappings->insert_location (crate_num, mapping.get_hirid (),
			       function.get_locus ());

    // add the mappings for the function params at the end
    for (auto &param : fn->get_function_params ())
      {
	mappings->insert_hir_param (mapping.get_crate_num (),
				    param.get_mappings ().get_hirid (), &param);
	mappings->insert_location (crate_num, mapping.get_hirid (),
				   param.get_locus ());
      }

    translated = fn;
  }

private:
  ASTLoweringStmt () : translated (nullptr), terminated (false) {}

  HIR::Stmt *translated;
  bool terminated;
};

} // namespace HIR
} // namespace Rust

#endif // RUST_AST_LOWER_PATTERN
