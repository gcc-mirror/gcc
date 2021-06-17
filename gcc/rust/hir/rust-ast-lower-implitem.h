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

#ifndef RUST_AST_LOWER_IMPLITEM_H
#define RUST_AST_LOWER_IMPLITEM_H

#include "rust-diagnostics.h"
#include "rust-ast-lower-type.h"
#include "rust-ast-lower-stmt.h"
#include "rust-ast-lower-expr.h"
#include "rust-ast-lower-pattern.h"
#include "rust-ast-lower-block.h"

namespace Rust {
namespace HIR {

class ASTLowerImplItem : public ASTLoweringBase
{
  using Rust::HIR::ASTLoweringBase::visit;

public:
  static HIR::ImplItem *translate (AST::InherentImplItem *item,
				   HirId parent_impl_id)
  {
    ASTLowerImplItem resolver (parent_impl_id);
    item->accept_vis (resolver);
    rust_assert (resolver.translated != nullptr);
    return resolver.translated;
  }

  static HIR::ImplItem *translate (AST::TraitImplItem *item,
				   HirId parent_impl_id)
  {
    ASTLowerImplItem resolver (parent_impl_id);
    item->accept_vis (resolver);
    rust_assert (resolver.translated != nullptr);
    return resolver.translated;
  }

  void visit (AST::ConstantItem &constant) override
  {
    HIR::Visibility vis = HIR::Visibility::create_public ();

    HIR::Type *type = ASTLoweringType::translate (constant.get_type ().get ());
    HIR::Expr *expr = ASTLoweringExpr::translate (constant.get_expr ().get ());

    auto crate_num = mappings->get_current_crate ();
    Analysis::NodeMapping mapping (crate_num, constant.get_node_id (),
				   mappings->get_next_hir_id (crate_num),
				   mappings->get_next_localdef_id (crate_num));

    auto translated_constant
      = new HIR::ConstantItem (mapping, constant.get_identifier (), vis,
			       std::unique_ptr<HIR::Type> (type),
			       std::unique_ptr<HIR::Expr> (expr),
			       constant.get_outer_attrs (),
			       constant.get_locus ());
    translated = translated_constant;

    mappings->insert_hir_implitem (mapping.get_crate_num (),
				   mapping.get_hirid (), parent_impl_id,
				   translated);
    mappings->insert_location (crate_num, mapping.get_hirid (),
			       constant.get_locus ());
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
				   mappings->get_next_localdef_id (crate_num));

    mappings->insert_location (crate_num,
			       function_body->get_mappings ().get_hirid (),
			       function.get_locus ());

    auto fn
      = new HIR::Function (mapping, std::move (function_name),
			   std::move (qualifiers), std::move (generic_params),
			   std::move (function_params), std::move (return_type),
			   std::move (where_clause), std::move (function_body),
			   std::move (vis), function.get_outer_attrs (), locus);

    mappings->insert_hir_implitem (mapping.get_crate_num (),
				   mapping.get_hirid (), parent_impl_id, fn);
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

  void visit (AST::Method &method) override
  {
    // ignore for now and leave empty
    std::vector<std::unique_ptr<HIR::WhereClauseItem> > where_clause_items;
    HIR::WhereClause where_clause (std::move (where_clause_items));
    HIR::FunctionQualifiers qualifiers (
      HIR::FunctionQualifiers::AsyncConstStatus::NONE, false);
    HIR::Visibility vis = HIR::Visibility::create_public ();

    // need
    std::vector<std::unique_ptr<HIR::GenericParam> > generic_params;
    if (method.has_generics ())
      {
	generic_params = lower_generic_params (method.get_generic_params ());
      }
    Identifier method_name = method.get_method_name ();
    Location locus = method.get_locus ();

    HIR::SelfParam self_param = lower_self (method.get_self_param ());

    std::unique_ptr<HIR::Type> return_type
      = method.has_return_type () ? std::unique_ptr<HIR::Type> (
	  ASTLoweringType::translate (method.get_return_type ().get ()))
				  : nullptr;

    std::vector<HIR::FunctionParam> function_params;
    for (auto &param : method.get_function_params ())
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
    std::unique_ptr<HIR::BlockExpr> method_body
      = std::unique_ptr<HIR::BlockExpr> (
	ASTLoweringBlock::translate (method.get_definition ().get (),
				     &terminated));

    auto crate_num = mappings->get_current_crate ();
    Analysis::NodeMapping mapping (crate_num, method.get_node_id (),
				   mappings->get_next_hir_id (crate_num),
				   mappings->get_next_localdef_id (crate_num));
    auto mth
      = new HIR::Method (mapping, std::move (method_name),
			 std::move (qualifiers), std::move (generic_params),
			 std::move (self_param), std::move (function_params),
			 std::move (return_type), std::move (where_clause),
			 std::move (method_body), std::move (vis),
			 method.get_outer_attrs (), locus);

    mappings->insert_hir_implitem (mapping.get_crate_num (),
				   mapping.get_hirid (), parent_impl_id, mth);
    mappings->insert_location (crate_num, mapping.get_hirid (),
			       method.get_locus ());

    // insert mappings for self
    mappings->insert_hir_self_param (crate_num,
				     self_param.get_mappings ().get_hirid (),
				     &self_param);
    mappings->insert_location (crate_num,
			       self_param.get_mappings ().get_hirid (),
			       self_param.get_locus ());

    // add the mappings for the function params at the end
    for (auto &param : mth->get_function_params ())
      {
	mappings->insert_hir_param (mapping.get_crate_num (),
				    param.get_mappings ().get_hirid (), &param);
	mappings->insert_location (crate_num, mapping.get_hirid (),
				   param.get_locus ());
      }

    translated = mth;
  }

private:
  ASTLowerImplItem (HirId parent_impl_id)
    : translated (nullptr), parent_impl_id (parent_impl_id)
  {}

  HIR::ImplItem *translated;
  HirId parent_impl_id;
};

class ASTLowerTraitItem : public ASTLoweringBase
{
  using Rust::HIR::ASTLoweringBase::visit;

public:
  static HIR::TraitItem *translate (AST::TraitItem *item)
  {
    ASTLowerTraitItem resolver;
    item->accept_vis (resolver);
    rust_assert (resolver.translated != nullptr);
    return resolver.translated;
  }

  void visit (AST::TraitItemFunc &func) override
  {
    AST::TraitFunctionDecl &ref = func.get_trait_function_decl ();

    std::vector<std::unique_ptr<HIR::WhereClauseItem> > where_clause_items;
    HIR::WhereClause where_clause (std::move (where_clause_items));
    HIR::FunctionQualifiers qualifiers (
      HIR::FunctionQualifiers::AsyncConstStatus::NONE, false);

    std::vector<std::unique_ptr<HIR::GenericParam> > generic_params;
    if (ref.has_generics ())
      {
	generic_params = lower_generic_params (ref.get_generic_params ());
      }

    std::unique_ptr<HIR::Type> return_type
      = ref.has_return_type () ? std::unique_ptr<HIR::Type> (
	  ASTLoweringType::translate (ref.get_return_type ().get ()))
			       : nullptr;

    std::vector<HIR::FunctionParam> function_params;
    for (auto &param : ref.get_function_params ())
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

    HIR::TraitFunctionDecl decl (ref.get_identifier (), std::move (qualifiers),
				 std::move (generic_params),
				 HIR::SelfParam::error (),
				 std::move (function_params),
				 std::move (return_type),
				 std::move (where_clause));
    HIR::Expr *block_expr
      = func.has_definition ()
	  ? ASTLoweringExpr::translate (func.get_definition ().get ())
	  : nullptr;

    auto crate_num = mappings->get_current_crate ();
    Analysis::NodeMapping mapping (crate_num, func.get_node_id (),
				   mappings->get_next_hir_id (crate_num),
				   UNKNOWN_LOCAL_DEFID);

    translated
      = new HIR::TraitItemFunc (mapping, std::move (decl),
				std::unique_ptr<HIR::Expr> (block_expr),
				func.get_outer_attrs (), func.get_locus ());
  }

  void visit (AST::TraitItemMethod &method) override
  {
    AST::TraitMethodDecl &ref = method.get_trait_method_decl ();

    std::vector<std::unique_ptr<HIR::WhereClauseItem> > where_clause_items;
    HIR::WhereClause where_clause (std::move (where_clause_items));
    HIR::FunctionQualifiers qualifiers (
      HIR::FunctionQualifiers::AsyncConstStatus::NONE, false);

    std::vector<std::unique_ptr<HIR::GenericParam> > generic_params;
    if (ref.has_generics ())
      {
	generic_params = lower_generic_params (ref.get_generic_params ());
      }

    std::unique_ptr<HIR::Type> return_type
      = ref.has_return_type () ? std::unique_ptr<HIR::Type> (
	  ASTLoweringType::translate (ref.get_return_type ().get ()))
			       : nullptr;

    HIR::SelfParam self_param = lower_self (ref.get_self_param ());

    std::vector<HIR::FunctionParam> function_params;
    for (auto &param : ref.get_function_params ())
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

    HIR::TraitFunctionDecl decl (ref.get_identifier (), std::move (qualifiers),
				 std::move (generic_params),
				 std::move (self_param),
				 std::move (function_params),
				 std::move (return_type),
				 std::move (where_clause));
    HIR::Expr *block_expr
      = method.has_definition ()
	  ? ASTLoweringExpr::translate (method.get_definition ().get ())
	  : nullptr;

    auto crate_num = mappings->get_current_crate ();
    Analysis::NodeMapping mapping (crate_num, method.get_node_id (),
				   mappings->get_next_hir_id (crate_num),
				   UNKNOWN_LOCAL_DEFID);

    translated
      = new HIR::TraitItemFunc (mapping, std::move (decl),
				std::unique_ptr<HIR::Expr> (block_expr),
				method.get_outer_attrs (), method.get_locus ());
  }

  void visit (AST::TraitItemConst &constant) override
  {
    HIR::Type *type = ASTLoweringType::translate (constant.get_type ().get ());
    HIR::Expr *expr
      = constant.has_expression ()
	  ? ASTLoweringExpr::translate (constant.get_expr ().get ())
	  : nullptr;

    auto crate_num = mappings->get_current_crate ();
    Analysis::NodeMapping mapping (crate_num, constant.get_node_id (),
				   mappings->get_next_hir_id (crate_num),
				   UNKNOWN_LOCAL_DEFID);

    translated = new HIR::TraitItemConst (mapping, constant.get_identifier (),
					  std::unique_ptr<HIR::Type> (type),
					  std::unique_ptr<HIR::Expr> (expr),
					  constant.get_outer_attrs (),
					  constant.get_locus ());
  }

  void visit (AST::TraitItemType &type) override
  {
    std::vector<std::unique_ptr<HIR::TypeParamBound> > type_param_bounds;

    auto crate_num = mappings->get_current_crate ();
    Analysis::NodeMapping mapping (crate_num, type.get_node_id (),
				   mappings->get_next_hir_id (crate_num),
				   UNKNOWN_LOCAL_DEFID);

    translated
      = new HIR::TraitItemType (mapping, type.get_identifier (),
				std::move (type_param_bounds),
				type.get_outer_attrs (), type.get_locus ());
  }

private:
  ASTLowerTraitItem () : translated (nullptr) {}

  HIR::TraitItem *translated;
};

} // namespace HIR
} // namespace Rust

#endif // RUST_AST_LOWER_IMPLITEM_H
