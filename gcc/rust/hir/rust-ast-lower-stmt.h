// Copyright (C) 2020-2023 Free Software Foundation, Inc.

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
#include "rust-ast-lower-enumitem.h"
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
    resolver.mappings->insert_location (
      resolver.translated->get_mappings ().get_hirid (),
      resolver.translated->get_locus ());
    resolver.mappings->insert_hir_stmt (resolver.translated);
    if (resolver.translated->is_item ())
      {
	HIR::Item *i = static_cast<HIR::Item *> (resolver.translated);

	auto defid = resolver.translated->get_mappings ().get_defid ();

	resolver.handle_outer_attributes (*i);
	resolver.mappings->insert_hir_item (i);
	resolver.mappings->insert_defid_mapping (defid, i);
      }

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
  }

  void visit (AST::ConstantItem &constant) override
  {
    HIR::Visibility vis = translate_visibility (constant.get_visibility ());

    HIR::Type *type = ASTLoweringType::translate (constant.get_type ().get ());
    HIR::Expr *expr = ASTLoweringExpr::translate (constant.get_expr ().get ());

    auto crate_num = mappings->get_current_crate ();
    Analysis::NodeMapping mapping (crate_num, constant.get_node_id (),
				   mappings->get_next_hir_id (crate_num),
				   mappings->get_next_localdef_id (crate_num));

    translated = new HIR::ConstantItem (mapping, constant.get_identifier (),
					vis, std::unique_ptr<HIR::Type> (type),
					std::unique_ptr<HIR::Expr> (expr),
					constant.get_outer_attrs (),
					constant.get_locus ());
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
  }

  void visit (AST::TupleStruct &struct_decl) override
  {
    std::vector<std::unique_ptr<HIR::GenericParam>> generic_params;
    if (struct_decl.has_generics ())
      {
	generic_params
	  = lower_generic_params (struct_decl.get_generic_params ());
      }

    std::vector<std::unique_ptr<HIR::WhereClauseItem>> where_clause_items;
    HIR::WhereClause where_clause (std::move (where_clause_items));
    HIR::Visibility vis = translate_visibility (struct_decl.get_visibility ());

    std::vector<HIR::TupleField> fields;
    for (AST::TupleField &field : struct_decl.get_fields ())
      {
	HIR::Visibility vis = translate_visibility (field.get_visibility ());
	HIR::Type *type
	  = ASTLoweringType::translate (field.get_field_type ().get ());

	auto crate_num = mappings->get_current_crate ();
	Analysis::NodeMapping mapping (crate_num, field.get_node_id (),
				       mappings->get_next_hir_id (crate_num),
				       mappings->get_next_localdef_id (
					 crate_num));

	HIR::TupleField translated_field (mapping,
					  std::unique_ptr<HIR::Type> (type),
					  vis, field.get_locus (),
					  field.get_outer_attrs ());
	fields.push_back (std::move (translated_field));
      }

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
  }

  void visit (AST::StructStruct &struct_decl) override
  {
    std::vector<std::unique_ptr<HIR::GenericParam>> generic_params;
    if (struct_decl.has_generics ())
      {
	generic_params
	  = lower_generic_params (struct_decl.get_generic_params ());
      }

    std::vector<std::unique_ptr<HIR::WhereClauseItem>> where_clause_items;
    HIR::WhereClause where_clause (std::move (where_clause_items));
    HIR::Visibility vis = translate_visibility (struct_decl.get_visibility ());

    bool is_unit = struct_decl.is_unit_struct ();
    std::vector<HIR::StructField> fields;
    for (AST::StructField &field : struct_decl.get_fields ())
      {
	HIR::Visibility vis = translate_visibility (field.get_visibility ());
	HIR::Type *type
	  = ASTLoweringType::translate (field.get_field_type ().get ());

	auto crate_num = mappings->get_current_crate ();
	Analysis::NodeMapping mapping (crate_num, field.get_node_id (),
				       mappings->get_next_hir_id (crate_num),
				       mappings->get_next_localdef_id (
					 crate_num));

	HIR::StructField translated_field (mapping, field.get_field_name (),
					   std::unique_ptr<HIR::Type> (type),
					   vis, field.get_locus (),
					   field.get_outer_attrs ());

	if (struct_field_name_exists (fields, translated_field))
	  break;

	fields.push_back (std::move (translated_field));
      }

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
  }

  void visit (AST::Union &union_decl) override
  {
    std::vector<std::unique_ptr<HIR::GenericParam>> generic_params;
    if (union_decl.has_generics ())
      {
	generic_params
	  = lower_generic_params (union_decl.get_generic_params ());
      }

    std::vector<std::unique_ptr<HIR::WhereClauseItem>> where_clause_items;
    HIR::WhereClause where_clause (std::move (where_clause_items));
    HIR::Visibility vis = translate_visibility (union_decl.get_visibility ());

    std::vector<HIR::StructField> variants;
    for (AST::StructField &variant : union_decl.get_variants ())
      {
	HIR::Visibility vis = translate_visibility (variant.get_visibility ());
	HIR::Type *type
	  = ASTLoweringType::translate (variant.get_field_type ().get ());

	auto crate_num = mappings->get_current_crate ();
	Analysis::NodeMapping mapping (crate_num, variant.get_node_id (),
				       mappings->get_next_hir_id (crate_num),
				       mappings->get_next_localdef_id (
					 crate_num));

	HIR::StructField translated_variant (mapping, variant.get_field_name (),
					     std::unique_ptr<HIR::Type> (type),
					     vis, variant.get_locus (),
					     variant.get_outer_attrs ());

	if (struct_field_name_exists (variants, translated_variant))
	  break;

	variants.push_back (std::move (translated_variant));
      }

    auto crate_num = mappings->get_current_crate ();
    Analysis::NodeMapping mapping (crate_num, union_decl.get_node_id (),
				   mappings->get_next_hir_id (crate_num),
				   mappings->get_next_localdef_id (crate_num));

    translated
      = new HIR::Union (mapping, union_decl.get_identifier (), vis,
			std::move (generic_params), std::move (where_clause),
			std::move (variants), union_decl.get_outer_attrs (),
			union_decl.get_locus ());
  }

  void visit (AST::Enum &enum_decl) override
  {
    std::vector<std::unique_ptr<HIR::GenericParam>> generic_params;
    if (enum_decl.has_generics ())
      {
	generic_params = lower_generic_params (enum_decl.get_generic_params ());
      }

    std::vector<std::unique_ptr<HIR::WhereClauseItem>> where_clause_items;
    HIR::WhereClause where_clause (std::move (where_clause_items));
    HIR::Visibility vis = translate_visibility (enum_decl.get_visibility ());

    // bool is_unit = enum_decl.is_zero_variant ();
    std::vector<std::unique_ptr<HIR::EnumItem>> items;
    for (auto &variant : enum_decl.get_variants ())
      {
	HIR::EnumItem *hir_item
	  = ASTLoweringEnumItem::translate (variant.get ());
	items.push_back (std::unique_ptr<HIR::EnumItem> (hir_item));
      }

    auto crate_num = mappings->get_current_crate ();
    Analysis::NodeMapping mapping (crate_num, enum_decl.get_node_id (),
				   mappings->get_next_hir_id (crate_num),
				   mappings->get_next_localdef_id (crate_num));

    translated = new HIR::Enum (mapping, enum_decl.get_identifier (), vis,
				std::move (generic_params),
				std::move (where_clause), /* is_unit, */
				std::move (items), enum_decl.get_outer_attrs (),
				enum_decl.get_locus ());
  }

  void visit (AST::EmptyStmt &empty) override
  {
    auto crate_num = mappings->get_current_crate ();
    Analysis::NodeMapping mapping (crate_num, empty.get_node_id (),
				   mappings->get_next_hir_id (crate_num),
				   mappings->get_next_localdef_id (crate_num));

    translated = new HIR::EmptyStmt (mapping, empty.get_locus ());
  }

  void visit (AST::Function &function) override
  {
    // ignore for now and leave empty
    std::vector<std::unique_ptr<HIR::WhereClauseItem>> where_clause_items;
    HIR::WhereClause where_clause (std::move (where_clause_items));
    HIR::FunctionQualifiers qualifiers
      = lower_qualifiers (function.get_qualifiers ());
    HIR::Visibility vis = translate_visibility (function.get_visibility ());

    // need
    std::vector<std::unique_ptr<HIR::GenericParam>> generic_params;
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
				   mappings->get_next_localdef_id (crate_num));

    mappings->insert_location (function_body->get_mappings ().get_hirid (),
			       function.get_locus ());

    auto fn
      = new HIR::Function (mapping, std::move (function_name),
			   std::move (qualifiers), std::move (generic_params),
			   std::move (function_params), std::move (return_type),
			   std::move (where_clause), std::move (function_body),
			   std::move (vis), function.get_outer_attrs (),
			   HIR::SelfParam::error (), locus);

    // add the mappings for the function params at the end
    for (auto &param : fn->get_function_params ())
      {
	mappings->insert_hir_param (&param);
	mappings->insert_location (mapping.get_hirid (), param.get_locus ());
      }

    translated = fn;
  }

  void visit (AST::ExternBlock &extern_block) override
  {
    translated = lower_extern_block (extern_block);
  }

private:
  ASTLoweringStmt () : translated (nullptr), terminated (false) {}

  HIR::Stmt *translated;
  bool terminated;
};

} // namespace HIR
} // namespace Rust

#endif // RUST_AST_LOWER_PATTERN
