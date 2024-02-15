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

#include "rust-ast-lower-implitem.h"
#include "rust-ast-lower.h"
#include "rust-ast-lower-type.h"
#include "rust-ast-lower-expr.h"
#include "rust-ast-lower-pattern.h"
#include "rust-ast-lower-block.h"
#include "rust-item.h"

namespace Rust {
namespace HIR {

HIR::ImplItem *
ASTLowerImplItem::translate (AST::AssociatedItem *item, HirId parent_impl_id)
{
  ASTLowerImplItem resolver;
  item->accept_vis (resolver);

  if (resolver.translated != nullptr)
    {
      rust_assert (resolver.item_cast != nullptr);

      auto id = resolver.translated->get_impl_mappings ().get_hirid ();
      auto defid = resolver.translated->get_impl_mappings ().get_defid ();
      auto locus = resolver.translated->get_locus ();

      resolver.handle_outer_attributes (*resolver.item_cast);
      resolver.mappings->insert_hir_implitem (parent_impl_id,
					      resolver.translated);
      resolver.mappings->insert_location (id, locus);
      resolver.mappings->insert_defid_mapping (defid, resolver.item_cast);
    }

  return resolver.translated;
}

void
ASTLowerImplItem::visit (AST::TypeAlias &alias)
{
  std::vector<std::unique_ptr<HIR::WhereClauseItem> > where_clause_items;
  HIR::WhereClause where_clause (std::move (where_clause_items));
  HIR::Visibility vis = translate_visibility (alias.get_visibility ());

  std::vector<std::unique_ptr<HIR::GenericParam> > generic_params;
  if (alias.has_generics ())
    generic_params = lower_generic_params (alias.get_generic_params ());

  HIR::Type *existing_type
    = ASTLoweringType::translate (alias.get_type_aliased ().get ());

  auto crate_num = mappings->get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, alias.get_node_id (),
				 mappings->get_next_hir_id (crate_num),
				 mappings->get_next_localdef_id (crate_num));

  auto type_alias
    = new HIR::TypeAlias (mapping, alias.get_new_type_name (),
			  std::move (generic_params), std::move (where_clause),
			  std::unique_ptr<HIR::Type> (existing_type),
			  std::move (vis), alias.get_outer_attrs (),
			  alias.get_locus ());

  translated = type_alias;
  item_cast = type_alias;
}

void
ASTLowerImplItem::visit (AST::ConstantItem &constant)
{
  HIR::Visibility vis = translate_visibility (constant.get_visibility ());

  HIR::Type *type
    = ASTLoweringType::translate (constant.get_type ().get (), true);
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
  item_cast = translated_constant;
}

void
ASTLowerImplItem::visit (AST::Function &function)
{
  // ignore for now and leave empty
  std::vector<std::unique_ptr<HIR::WhereClauseItem> > where_clause_items;
  for (auto &item : function.get_where_clause ().get_items ())
    {
      HIR::WhereClauseItem *i
	= ASTLowerWhereClauseItem::translate (*item.get ());
      where_clause_items.push_back (std::unique_ptr<HIR::WhereClauseItem> (i));
    }

  HIR::WhereClause where_clause (std::move (where_clause_items));
  HIR::FunctionQualifiers qualifiers
    = lower_qualifiers (function.get_qualifiers ());
  HIR::Visibility vis = translate_visibility (function.get_visibility ());

  // need
  std::vector<std::unique_ptr<HIR::GenericParam> > generic_params;
  if (function.has_generics ())
    {
      generic_params = lower_generic_params (function.get_generic_params ());
    }
  Identifier function_name = function.get_function_name ();
  location_t locus = function.get_locus ();

  HIR::SelfParam self_param = HIR::SelfParam::error ();
  if (function.has_self_param ())
    self_param = lower_self (function.get_self_param ());

  std::unique_ptr<HIR::Type> return_type
    = function.has_return_type () ? std::unique_ptr<HIR::Type> (
	ASTLoweringType::translate (function.get_return_type ().get ()))
				  : nullptr;

  std::vector<HIR::FunctionParam> function_params;
  for (auto &p : function.get_function_params ())
    {
      if (p->is_self () || p->is_variadic ())
	continue;
      auto param = static_cast<AST::FunctionParam *> (p.get ());

      auto translated_pattern = std::unique_ptr<HIR::Pattern> (
	ASTLoweringPattern::translate (param->get_pattern ().get ()));
      auto translated_type = std::unique_ptr<HIR::Type> (
	ASTLoweringType::translate (param->get_type ().get ()));

      auto crate_num = mappings->get_current_crate ();
      Analysis::NodeMapping mapping (crate_num, param->get_node_id (),
				     mappings->get_next_hir_id (crate_num),
				     UNKNOWN_LOCAL_DEFID);

      auto hir_param
	= HIR::FunctionParam (mapping, std::move (translated_pattern),
			      std::move (translated_type), param->get_locus ());
      function_params.push_back (std::move (hir_param));
    }

  bool terminated = false;
  std::unique_ptr<HIR::BlockExpr> function_body
    = std::unique_ptr<HIR::BlockExpr> (
      ASTLoweringBlock::translate (function.get_definition ()->get (),
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
			 std::move (self_param), locus);

  if (!fn->get_self_param ().is_error ())
    {
      // insert mappings for self
      mappings->insert_hir_self_param (&fn->get_self_param ());
      mappings->insert_location (
	fn->get_self_param ().get_mappings ().get_hirid (),
	fn->get_self_param ().get_locus ());
    }

  // add the mappings for the function params at the end
  for (auto &param : fn->get_function_params ())
    {
      mappings->insert_hir_param (&param);
      mappings->insert_location (mapping.get_hirid (), param.get_locus ());
    }

  translated = fn;
  item_cast = fn;
}

HIR::TraitItem *
ASTLowerTraitItem::translate (AST::AssociatedItem *item)
{
  ASTLowerTraitItem resolver;
  item->accept_vis (resolver);

  if (resolver.translated != nullptr)
    {
      auto id = resolver.translated->get_mappings ().get_hirid ();
      auto defid = resolver.translated->get_mappings ().get_defid ();
      auto locus = resolver.translated->get_trait_locus ();

      resolver.handle_outer_attributes (*resolver.translated);
      resolver.mappings->insert_hir_trait_item (resolver.translated);
      resolver.mappings->insert_location (id, locus);
      resolver.mappings->insert_defid_mapping (defid, resolver.translated);
    }

  return resolver.translated;
}

void
ASTLowerTraitItem::visit (AST::Function &func)
{
  std::vector<std::unique_ptr<HIR::WhereClauseItem> > where_clause_items;
  HIR::WhereClause where_clause (std::move (where_clause_items));
  HIR::FunctionQualifiers qualifiers
    = lower_qualifiers (func.get_qualifiers ());

  std::vector<std::unique_ptr<HIR::GenericParam> > generic_params;
  if (func.has_generics ())
    generic_params = lower_generic_params (func.get_generic_params ());

  std::unique_ptr<HIR::Type> return_type
    = func.has_return_type () ? std::unique_ptr<HIR::Type> (
	ASTLoweringType::translate (func.get_return_type ().get ()))
			      : nullptr;

  // set self parameter to error if this is a method
  // else lower to hir
  HIR::SelfParam self_param = func.has_self_param ()
				? lower_self (func.get_self_param ())
				: HIR::SelfParam::error ();

  std::vector<HIR::FunctionParam> function_params;
  for (auto &p : func.get_function_params ())
    {
      if (p->is_variadic () || p->is_self ())
	continue;

      auto param = static_cast<AST::FunctionParam *> (p.get ());

      auto translated_pattern = std::unique_ptr<HIR::Pattern> (
	ASTLoweringPattern::translate (param->get_pattern ().get ()));
      auto translated_type = std::unique_ptr<HIR::Type> (
	ASTLoweringType::translate (param->get_type ().get ()));

      auto crate_num = mappings->get_current_crate ();
      Analysis::NodeMapping mapping (crate_num, param->get_node_id (),
				     mappings->get_next_hir_id (crate_num),
				     UNKNOWN_LOCAL_DEFID);

      auto hir_param
	= HIR::FunctionParam (mapping, std::move (translated_pattern),
			      std::move (translated_type), param->get_locus ());
      function_params.push_back (hir_param);
    }

  HIR::TraitFunctionDecl decl (func.get_function_name (),
			       std::move (qualifiers),
			       std::move (generic_params),
			       std::move (self_param),
			       std::move (function_params),
			       std::move (return_type),
			       std::move (where_clause));
  bool terminated = false;
  std::unique_ptr<HIR::BlockExpr> block_expr
    = func.has_body () ? std::unique_ptr<HIR::BlockExpr> (
	ASTLoweringBlock::translate (func.get_definition ()->get (),
				     &terminated))
		       : nullptr;

  auto crate_num = mappings->get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, func.get_node_id (),
				 mappings->get_next_hir_id (crate_num),
				 mappings->get_next_localdef_id (crate_num));

  auto *trait_item
    = new HIR::TraitItemFunc (mapping, std::move (decl), std::move (block_expr),
			      func.get_outer_attrs (), func.get_locus ());
  translated = trait_item;
  if (func.has_self_param ())
    {
      // insert mappings for self
      mappings->insert_hir_self_param (&self_param);
      mappings->insert_location (self_param.get_mappings ().get_hirid (),
				 self_param.get_locus ());
    }

  // add the mappings for the function params at the end
  for (auto &param : trait_item->get_decl ().get_function_params ())
    {
      mappings->insert_hir_param (&param);
      mappings->insert_location (mapping.get_hirid (), param.get_locus ());
    }
}

void
ASTLowerTraitItem::visit (AST::TraitItemConst &constant)
{
  HIR::Type *type = ASTLoweringType::translate (constant.get_type ().get ());
  HIR::Expr *expr = constant.has_expression ()
		      ? ASTLoweringExpr::translate (constant.get_expr ().get ())
		      : nullptr;

  auto crate_num = mappings->get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, constant.get_node_id (),
				 mappings->get_next_hir_id (crate_num),
				 mappings->get_next_localdef_id (crate_num));

  HIR::TraitItemConst *trait_item
    = new HIR::TraitItemConst (mapping, constant.get_identifier (),
			       std::unique_ptr<HIR::Type> (type),
			       std::unique_ptr<HIR::Expr> (expr),
			       constant.get_outer_attrs (),
			       constant.get_locus ());
  translated = trait_item;
}

void
ASTLowerTraitItem::visit (AST::TraitItemType &type)
{
  std::vector<std::unique_ptr<HIR::TypeParamBound> > type_param_bounds;
  auto crate_num = mappings->get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, type.get_node_id (),
				 mappings->get_next_hir_id (crate_num),
				 mappings->get_next_localdef_id (crate_num));

  HIR::TraitItemType *trait_item
    = new HIR::TraitItemType (mapping, type.get_identifier (),
			      std::move (type_param_bounds),
			      type.get_outer_attrs (), type.get_locus ());
  translated = trait_item;
}

} // namespace HIR
} // namespace Rust
