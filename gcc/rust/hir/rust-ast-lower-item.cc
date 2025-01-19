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

#include "rust-ast-lower-item.h"
#include "rust-diagnostics.h"
#include "rust-ast-lower.h"
#include "rust-ast-lower-base.h"
#include "rust-ast-lower-enumitem.h"
#include "rust-ast-lower-type.h"
#include "rust-ast-lower-implitem.h"
#include "rust-ast-lower-expr.h"
#include "rust-ast-lower-pattern.h"
#include "rust-ast-lower-block.h"
#include "rust-item.h"

namespace Rust {
namespace HIR {

HIR::Item *
ASTLoweringItem::translate (AST::Item &item)
{
  ASTLoweringItem resolver;
  item.accept_vis (resolver);

  if (resolver.translated != nullptr)
    {
      auto id = resolver.translated->get_mappings ().get_hirid ();
      auto defid = resolver.translated->get_mappings ().get_defid ();
      auto locus = resolver.translated->get_locus ();

      resolver.handle_outer_attributes (*resolver.translated);
      resolver.mappings->insert_ast_item (&item);
      resolver.mappings->insert_hir_item (resolver.translated);
      resolver.mappings->insert_location (id, locus);
      resolver.mappings->insert_defid_mapping (defid, resolver.translated);
    }

  return resolver.translated;
}

void
ASTLoweringItem::visit (AST::Module &module)
{
  auto crate_num = mappings->get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, module.get_node_id (),
				 mappings->get_next_hir_id (crate_num),
				 mappings->get_next_localdef_id (crate_num));

  // should be lowered from module.get_vis()
  HIR::Visibility vis = translate_visibility (module.get_visibility ());

  auto items = std::vector<std::unique_ptr<Item>> ();

  for (auto &item : module.get_items ())
    {
      auto transitem = translate (*item);
      // The item may be null if it doesn't need to live in the HIR - for
      // example, macro rules definitions
      if (transitem)
	items.push_back (std::unique_ptr<Item> (transitem));
    }

  // should be lowered/copied from module.get_in/outer_attrs()
  AST::AttrVec inner_attrs = module.get_inner_attrs ();
  AST::AttrVec outer_attrs = module.get_outer_attrs ();

  translated
    = new HIR::Module (mapping, module.get_name (), module.get_locus (),
		       std::move (items), std::move (vis),
		       std::move (inner_attrs), std::move (outer_attrs));
  mappings->insert_module (static_cast<Module *> (translated));
}

void
ASTLoweringItem::visit (AST::TypeAlias &alias)
{
  std::vector<std::unique_ptr<HIR::WhereClauseItem>> where_clause_items;
  for (auto &item : alias.get_where_clause ().get_items ())
    {
      HIR::WhereClauseItem *i
	= ASTLowerWhereClauseItem::translate (*item.get ());
      where_clause_items.push_back (std::unique_ptr<HIR::WhereClauseItem> (i));
    }

  HIR::WhereClause where_clause (std::move (where_clause_items));
  HIR::Visibility vis = translate_visibility (alias.get_visibility ());

  std::vector<std::unique_ptr<HIR::GenericParam>> generic_params;
  if (alias.has_generics ())
    generic_params = lower_generic_params (alias.get_generic_params ());

  HIR::Type *existing_type
    = ASTLoweringType::translate (alias.get_type_aliased ());

  auto crate_num = mappings->get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, alias.get_node_id (),
				 mappings->get_next_hir_id (crate_num),
				 mappings->get_next_localdef_id (crate_num));

  translated
    = new HIR::TypeAlias (mapping, alias.get_new_type_name (),
			  std::move (generic_params), std::move (where_clause),
			  std::unique_ptr<HIR::Type> (existing_type),
			  std::move (vis), alias.get_outer_attrs (),
			  alias.get_locus ());
}

void
ASTLoweringItem::visit (AST::TupleStruct &struct_decl)
{
  std::vector<std::unique_ptr<HIR::GenericParam>> generic_params;
  if (struct_decl.has_generics ())
    {
      generic_params = lower_generic_params (struct_decl.get_generic_params ());
    }

  std::vector<std::unique_ptr<HIR::WhereClauseItem>> where_clause_items;
  for (auto &item : struct_decl.get_where_clause ().get_items ())
    {
      HIR::WhereClauseItem *i
	= ASTLowerWhereClauseItem::translate (*item.get ());
      where_clause_items.push_back (std::unique_ptr<HIR::WhereClauseItem> (i));
    }

  HIR::WhereClause where_clause (std::move (where_clause_items));
  HIR::Visibility vis = translate_visibility (struct_decl.get_visibility ());

  std::vector<HIR::TupleField> fields;
  for (AST::TupleField &field : struct_decl.get_fields ())
    {
      if (field.get_field_type ().is_marked_for_strip ())
	continue;

      // FIXME: How do we get the visibility from here?
      HIR::Visibility vis = translate_visibility (field.get_visibility ());
      HIR::Type *type = ASTLoweringType::translate (field.get_field_type ());

      auto crate_num = mappings->get_current_crate ();
      Analysis::NodeMapping mapping (crate_num, field.get_node_id (),
				     mappings->get_next_hir_id (crate_num),
				     mappings->get_next_localdef_id (
				       crate_num));

      HIR::TupleField translated_field (mapping,
					std::unique_ptr<HIR::Type> (type), vis,
					field.get_locus (),
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

void
ASTLoweringItem::visit (AST::StructStruct &struct_decl)
{
  std::vector<std::unique_ptr<HIR::GenericParam>> generic_params;
  if (struct_decl.has_generics ())
    {
      generic_params = lower_generic_params (struct_decl.get_generic_params ());
    }

  std::vector<std::unique_ptr<HIR::WhereClauseItem>> where_clause_items;
  for (auto &item : struct_decl.get_where_clause ().get_items ())
    {
      HIR::WhereClauseItem *i
	= ASTLowerWhereClauseItem::translate (*item.get ());
      where_clause_items.push_back (std::unique_ptr<HIR::WhereClauseItem> (i));
    }

  HIR::WhereClause where_clause (std::move (where_clause_items));

  HIR::Visibility vis = translate_visibility (struct_decl.get_visibility ());

  bool is_unit = struct_decl.is_unit_struct ();
  std::vector<HIR::StructField> fields;
  for (AST::StructField &field : struct_decl.get_fields ())
    {
      if (field.get_field_type ().is_marked_for_strip ())
	continue;

      HIR::Visibility vis = translate_visibility (field.get_visibility ());
      HIR::Type *type = ASTLoweringType::translate (field.get_field_type ());

      auto crate_num = mappings->get_current_crate ();
      Analysis::NodeMapping mapping (crate_num, field.get_node_id (),
				     mappings->get_next_hir_id (crate_num),
				     mappings->get_next_localdef_id (
				       crate_num));

      HIR::StructField translated_field (mapping, field.get_field_name (),
					 std::unique_ptr<HIR::Type> (type), vis,
					 field.get_locus (),
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

void
ASTLoweringItem::visit (AST::Enum &enum_decl)
{
  std::vector<std::unique_ptr<HIR::GenericParam>> generic_params;
  if (enum_decl.has_generics ())
    {
      generic_params = lower_generic_params (enum_decl.get_generic_params ());
    }

  std::vector<std::unique_ptr<HIR::WhereClauseItem>> where_clause_items;
  for (auto &item : enum_decl.get_where_clause ().get_items ())
    {
      HIR::WhereClauseItem *i
	= ASTLowerWhereClauseItem::translate (*item.get ());
      where_clause_items.push_back (std::unique_ptr<HIR::WhereClauseItem> (i));
    }

  HIR::WhereClause where_clause (std::move (where_clause_items));
  HIR::Visibility vis = translate_visibility (enum_decl.get_visibility ());

  // bool is_unit = enum_decl.is_zero_variant ();
  std::vector<std::unique_ptr<HIR::EnumItem>> items;
  for (auto &variant : enum_decl.get_variants ())
    {
      if (variant->is_marked_for_strip ())
	continue;

      HIR::EnumItem *hir_item = ASTLoweringEnumItem::translate (variant.get ());
      items.push_back (std::unique_ptr<HIR::EnumItem> (hir_item));
    }

  auto crate_num = mappings->get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, enum_decl.get_node_id (),
				 mappings->get_next_hir_id (crate_num),
				 mappings->get_next_localdef_id (crate_num));

  HIR::Enum *hir_enum
    = new HIR::Enum (mapping, enum_decl.get_identifier (), vis,
		     std::move (generic_params), std::move (where_clause),
		     std::move (items), enum_decl.get_outer_attrs (),
		     enum_decl.get_locus ());
  translated = hir_enum;
  for (auto &variant : hir_enum->get_variants ())
    {
      mappings->insert_hir_enumitem (hir_enum, variant.get ());
    }
}

void
ASTLoweringItem::visit (AST::Union &union_decl)
{
  std::vector<std::unique_ptr<HIR::GenericParam>> generic_params;
  if (union_decl.has_generics ())
    {
      generic_params = lower_generic_params (union_decl.get_generic_params ());
    }

  std::vector<std::unique_ptr<HIR::WhereClauseItem>> where_clause_items;
  for (auto &item : union_decl.get_where_clause ().get_items ())
    {
      HIR::WhereClauseItem *i
	= ASTLowerWhereClauseItem::translate (*item.get ());
      where_clause_items.push_back (std::unique_ptr<HIR::WhereClauseItem> (i));
    }
  HIR::WhereClause where_clause (std::move (where_clause_items));
  HIR::Visibility vis = translate_visibility (union_decl.get_visibility ());

  std::vector<HIR::StructField> variants;
  for (AST::StructField &variant : union_decl.get_variants ())
    {
      if (variant.get_field_type ().is_marked_for_strip ())
	continue;

      // FIXME: Does visibility apply here?
      HIR::Visibility vis = translate_visibility (variant.get_visibility ());
      HIR::Type *type = ASTLoweringType::translate (variant.get_field_type ());

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

void
ASTLoweringItem::visit (AST::StaticItem &var)
{
  HIR::Visibility vis = translate_visibility (var.get_visibility ());

  HIR::Type *type = ASTLoweringType::translate (var.get_type (), true);
  HIR::Expr *expr = ASTLoweringExpr::translate (var.get_expr ());

  auto crate_num = mappings->get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, var.get_node_id (),
				 mappings->get_next_hir_id (crate_num),
				 mappings->get_next_localdef_id (crate_num));

  translated = new HIR::StaticItem (mapping, var.get_identifier (),
				    var.is_mutable () ? Mutability::Mut
						      : Mutability::Imm,
				    std::unique_ptr<HIR::Type> (type),
				    std::unique_ptr<HIR::Expr> (expr), vis,
				    var.get_outer_attrs (), var.get_locus ());
}

void
ASTLoweringItem::visit (AST::ConstantItem &constant)
{
  HIR::Visibility vis = translate_visibility (constant.get_visibility ());

  HIR::Type *type = ASTLoweringType::translate (constant.get_type (), true);
  HIR::Expr *expr = ASTLoweringExpr::translate (constant.get_expr ());

  auto crate_num = mappings->get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, constant.get_node_id (),
				 mappings->get_next_hir_id (crate_num),
				 mappings->get_next_localdef_id (crate_num));

  translated = new HIR::ConstantItem (mapping, constant.get_identifier (), vis,
				      std::unique_ptr<HIR::Type> (type),
				      std::unique_ptr<HIR::Expr> (expr),
				      constant.get_outer_attrs (),
				      constant.get_locus ());
}

void
ASTLoweringItem::visit (AST::Function &function)
{
  if (function.is_marked_for_strip ())
    return;

  std::vector<std::unique_ptr<HIR::WhereClauseItem>> where_clause_items;
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
  std::vector<std::unique_ptr<HIR::GenericParam>> generic_params;
  if (function.has_generics ())
    {
      generic_params = lower_generic_params (function.get_generic_params ());
    }
  Identifier function_name = function.get_function_name ();
  location_t locus = function.get_locus ();

  std::unique_ptr<HIR::Type> return_type
    = function.has_return_type () ? std::unique_ptr<HIR::Type> (
	ASTLoweringType::translate (function.get_return_type ()))
				  : nullptr;

  std::vector<HIR::FunctionParam> function_params;
  for (auto &p : function.get_function_params ())
    {
      if (p->is_variadic () || p->is_self ())
	continue;
      auto param = static_cast<AST::FunctionParam &> (*p);

      auto translated_pattern = std::unique_ptr<HIR::Pattern> (
	ASTLoweringPattern::translate (param.get_pattern ()));
      auto translated_type = std::unique_ptr<HIR::Type> (
	ASTLoweringType::translate (param.get_type ()));

      auto crate_num = mappings->get_current_crate ();
      Analysis::NodeMapping mapping (crate_num, param.get_node_id (),
				     mappings->get_next_hir_id (crate_num),
				     UNKNOWN_LOCAL_DEFID);

      auto hir_param
	= HIR::FunctionParam (mapping, std::move (translated_pattern),
			      std::move (translated_type), param.get_locus ());
      function_params.push_back (std::move (hir_param));
    }

  bool terminated = false;
  std::unique_ptr<HIR::BlockExpr> function_body
    = std::unique_ptr<HIR::BlockExpr> (
      ASTLoweringBlock::translate (*function.get_definition ().value (),
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

void
ASTLoweringItem::visit (AST::InherentImpl &impl_block)
{
  std::vector<std::unique_ptr<HIR::WhereClauseItem>> where_clause_items;
  for (auto &item : impl_block.get_where_clause ().get_items ())
    {
      HIR::WhereClauseItem *i = ASTLowerWhereClauseItem::translate (*item);
      where_clause_items.push_back (std::unique_ptr<HIR::WhereClauseItem> (i));
    }

  HIR::WhereClause where_clause (std::move (where_clause_items));
  HIR::Visibility vis = translate_visibility (impl_block.get_visibility ());

  std::vector<std::unique_ptr<HIR::GenericParam>> generic_params;
  if (impl_block.has_generics ())
    {
      generic_params = lower_generic_params (impl_block.get_generic_params ());

      for (auto &generic_param : generic_params)
	{
	  switch (generic_param->get_kind ())
	    {
	      case HIR::GenericParam::GenericKind::TYPE: {
		const HIR::TypeParam &t
		  = static_cast<const HIR::TypeParam &> (*generic_param);

		if (t.has_type ())
		  {
		    rich_location rich_locus (line_table, t.get_locus ());
		    rich_locus.add_fixit_replace (
		      t.get_locus (),
		      "for more information, see issue #36887 "
		      "<https://github.com/rust-lang/rust/issues/36887>");
		    rust_error_at (rich_locus,
				   "defaults for type parameters are only "
				   "allowed in %<struct%>, %<enum%>, %<type%>, "
				   "or %<trait%> definitions");
		  }
	      }
	      break;

	    default:
	      break;
	    }
	}
    }

  HIR::Type *impl_type = ASTLoweringType::translate (impl_block.get_type ());

  auto crate_num = mappings->get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, impl_block.get_node_id (),
				 mappings->get_next_hir_id (crate_num),
				 mappings->get_next_localdef_id (crate_num));

  std::vector<std::unique_ptr<HIR::ImplItem>> impl_items;
  std::vector<HirId> impl_item_ids;
  for (auto &impl_item : impl_block.get_impl_items ())
    {
      if (impl_item->is_marked_for_strip ())
	continue;

      HIR::ImplItem *lowered
	= ASTLowerImplItem::translate (*impl_item, mapping.get_hirid ());
      rust_assert (lowered != nullptr);
      impl_items.push_back (std::unique_ptr<HIR::ImplItem> (lowered));
      impl_item_ids.push_back (lowered->get_impl_mappings ().get_hirid ());
    }

  BoundPolarity polarity = BoundPolarity::RegularBound;
  HIR::ImplBlock *hir_impl_block = new HIR::ImplBlock (
    mapping, std::move (impl_items), std::move (generic_params),
    std::unique_ptr<HIR::Type> (impl_type), nullptr, where_clause, polarity,
    vis, impl_block.get_inner_attrs (), impl_block.get_outer_attrs (),
    impl_block.get_locus ());
  translated = hir_impl_block;

  mappings->insert_hir_impl_block (hir_impl_block);
  for (auto &impl_item_id : impl_item_ids)
    {
      mappings->insert_impl_item_mapping (impl_item_id, hir_impl_block);
    }
}

void
ASTLoweringItem::visit (AST::Trait &trait)
{
  std::vector<std::unique_ptr<HIR::WhereClauseItem>> where_clause_items;
  for (auto &item : trait.get_where_clause ().get_items ())
    {
      HIR::WhereClauseItem *i
	= ASTLowerWhereClauseItem::translate (*item.get ());
      where_clause_items.push_back (std::unique_ptr<HIR::WhereClauseItem> (i));
    }
  HIR::WhereClause where_clause (std::move (where_clause_items));

  HIR::Visibility vis = translate_visibility (trait.get_visibility ());

  std::vector<std::unique_ptr<HIR::GenericParam>> generic_params;
  if (trait.has_generics ())
    {
      generic_params = lower_generic_params (trait.get_generic_params ());
    }

  std::vector<std::unique_ptr<HIR::TypeParamBound>> type_param_bounds;
  if (trait.has_type_param_bounds ())
    {
      for (auto &bound : trait.get_type_param_bounds ())
	{
	  HIR::TypeParamBound *b = lower_bound (*bound);
	  type_param_bounds.push_back (
	    std::unique_ptr<HIR::TypeParamBound> (b));
	}
    }

  std::vector<std::unique_ptr<HIR::TraitItem>> trait_items;
  std::vector<HirId> trait_item_ids;
  for (auto &item : trait.get_trait_items ())
    {
      if (item->is_marked_for_strip ())
	continue;

      HIR::TraitItem *lowered = ASTLowerTraitItem::translate (*item);
      trait_items.push_back (std::unique_ptr<HIR::TraitItem> (lowered));
      trait_item_ids.push_back (lowered->get_mappings ().get_hirid ());
    }

  auto crate_num = mappings->get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, trait.get_node_id (),
				 mappings->get_next_hir_id (crate_num),
				 mappings->get_next_localdef_id (crate_num));

  auto trait_unsafety = Unsafety::Normal;
  if (trait.is_unsafe ())
    {
      trait_unsafety = Unsafety::Unsafe;
    }

  HIR::Trait *hir_trait
    = new HIR::Trait (mapping, trait.get_identifier (), trait_unsafety,
		      std::move (generic_params), std::move (type_param_bounds),
		      where_clause, std::move (trait_items), vis,
		      trait.get_outer_attrs (), trait.get_locus ());
  translated = hir_trait;

  for (auto trait_item_id : trait_item_ids)
    {
      mappings->insert_trait_item_mapping (trait_item_id, hir_trait);
    }
}

void
ASTLoweringItem::visit (AST::TraitImpl &impl_block)
{
  std::vector<std::unique_ptr<HIR::WhereClauseItem>> where_clause_items;
  for (auto &item : impl_block.get_where_clause ().get_items ())
    {
      HIR::WhereClauseItem *i = ASTLowerWhereClauseItem::translate (*item);
      where_clause_items.push_back (std::unique_ptr<HIR::WhereClauseItem> (i));
    }
  HIR::WhereClause where_clause (std::move (where_clause_items));
  HIR::Visibility vis = translate_visibility (impl_block.get_visibility ());

  std::vector<std::unique_ptr<HIR::GenericParam>> generic_params;
  if (impl_block.has_generics ())
    {
      generic_params = lower_generic_params (impl_block.get_generic_params ());

      for (auto &generic_param : generic_params)
	{
	  switch (generic_param->get_kind ())
	    {
	      case HIR::GenericParam::GenericKind::TYPE: {
		const HIR::TypeParam &t
		  = static_cast<const HIR::TypeParam &> (*generic_param);

		if (t.has_type ())
		  {
		    rich_location rich_locus (line_table, t.get_locus ());
		    rich_locus.add_fixit_replace (
		      t.get_locus (), "for more information, see issue #36887 "
				      "<https://github.com/rust-lang/rust/"
				      "issues/36887>");
		    rust_error_at (rich_locus,
				   "defaults for type parameters are only "
				   "allowed in %<struct%>, %<enum%>, %<type%>, "
				   "or %<trait%> definitions");
		  }
	      }
	      break;

	    default:
	      break;
	    }
	}
    }

  HIR::Type *impl_type = ASTLoweringType::translate (impl_block.get_type ());
  HIR::TypePath *trait_ref
    = ASTLowerTypePath::translate (impl_block.get_trait_path ());

  auto crate_num = mappings->get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, impl_block.get_node_id (),
				 mappings->get_next_hir_id (crate_num),
				 mappings->get_next_localdef_id (crate_num));

  std::vector<std::unique_ptr<HIR::ImplItem>> impl_items;
  std::vector<HirId> impl_item_ids;
  for (auto &impl_item : impl_block.get_impl_items ())
    {
      if (impl_item->is_marked_for_strip ())
	continue;

      HIR::ImplItem *lowered
	= ASTLowerImplItem::translate (*impl_item, mapping.get_hirid ());
      rust_assert (lowered != nullptr);
      impl_items.push_back (std::unique_ptr<HIR::ImplItem> (lowered));
      impl_item_ids.push_back (lowered->get_impl_mappings ().get_hirid ());
    }

  BoundPolarity polarity = impl_block.is_exclam ()
			     ? BoundPolarity::RegularBound
			     : BoundPolarity::NegativeBound;
  HIR::ImplBlock *hir_impl_block = new HIR::ImplBlock (
    mapping, std::move (impl_items), std::move (generic_params),
    std::unique_ptr<HIR::Type> (impl_type),
    std::unique_ptr<HIR::TypePath> (trait_ref), where_clause, polarity, vis,
    impl_block.get_inner_attrs (), impl_block.get_outer_attrs (),
    impl_block.get_locus ());
  translated = hir_impl_block;

  mappings->insert_hir_impl_block (hir_impl_block);
  for (auto &impl_item_id : impl_item_ids)
    {
      mappings->insert_impl_item_mapping (impl_item_id, hir_impl_block);
    }
}

void
ASTLoweringItem::visit (AST::ExternBlock &extern_block)
{
  translated = lower_extern_block (extern_block);
}

void
ASTLoweringItem::visit (AST::MacroRulesDefinition &def)
{
  lower_macro_definition (def);
}

HIR::SimplePath
ASTLoweringSimplePath::translate (const AST::SimplePath &path)
{
  ASTLoweringSimplePath resolver;

  return resolver.lower (path);
}

HIR::SimplePathSegment
ASTLoweringSimplePath::lower (const AST::SimplePathSegment &segment)
{
  auto crate_num = mappings->get_current_crate ();
  auto node_id = segment.get_node_id ();

  auto mapping = Analysis::NodeMapping (crate_num, node_id,
					mappings->get_next_hir_id (crate_num),
					UNKNOWN_LOCAL_DEFID);

  auto hir_seg = HIR::SimplePathSegment (mapping);

  mappings->insert_node_to_hir (node_id, mapping.get_hirid ());
  // mappings->insert_simple_path_segment (crate_num, node_id, &segment);

  return hir_seg;
}

HIR::SimplePath
ASTLoweringSimplePath::lower (const AST::SimplePath &path)
{
  auto segments = std::vector<HIR::SimplePathSegment> ();
  for (auto &segment : path.get_segments ())
    segments.emplace_back (lower (segment));

  auto crate_num = mappings->get_current_crate ();
  auto node_id = path.get_node_id ();

  auto mapping = Analysis::NodeMapping (crate_num, node_id,
					mappings->get_next_hir_id (crate_num),
					UNKNOWN_LOCAL_DEFID);

  auto lowered
    = HIR::SimplePath (std::move (segments), mapping, path.get_locus ());

  mappings->insert_node_to_hir (node_id, mapping.get_hirid ());
  // mappings->insert_simple_path (crate_num, node_id, &path);

  return lowered;
}

} // namespace HIR
} // namespace Rust
