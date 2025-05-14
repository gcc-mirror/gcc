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

#ifndef RUST_AST_LOWER_EXTERN_ITEM
#define RUST_AST_LOWER_EXTERN_ITEM

#include "rust-ast-lower-base.h"
#include "rust-ast-lower-type.h"
#include "rust-ast-lower.h"
#include "rust-hir-full-decls.h"

namespace Rust {
namespace HIR {

class ASTLoweringExternItem : public ASTLoweringBase
{
  using Rust::HIR::ASTLoweringBase::visit;

public:
  static HIR::ExternalItem *translate (AST::ExternalItem *item,
				       HirId parent_hirid)
  {
    ASTLoweringExternItem resolver;
    item->accept_vis (resolver);

    rust_assert (resolver.translated != nullptr);
    resolver.mappings.insert_hir_extern_item (resolver.translated,
					      parent_hirid);
    resolver.mappings.insert_location (
      resolver.translated->get_mappings ().get_hirid (),
      resolver.translated->get_locus ());

    return resolver.translated;
  }

  void visit (AST::ExternalStaticItem &item) override
  {
    HIR::Visibility vis = translate_visibility (item.get_visibility ());
    HIR::Type *static_type = ASTLoweringType::translate (item.get_type ());

    auto crate_num = mappings.get_current_crate ();
    Analysis::NodeMapping mapping (crate_num, item.get_node_id (),
				   mappings.get_next_hir_id (crate_num),
				   mappings.get_next_localdef_id (crate_num));

    translated = new HIR::ExternalStaticItem (
      mapping, item.get_identifier (), std::unique_ptr<HIR::Type> (static_type),
      item.is_mut () ? Mutability::Mut : Mutability::Imm, std::move (vis),
      item.get_outer_attrs (), item.get_locus ());
  }

  void visit (AST::Function &function) override
  {
    std::vector<std::unique_ptr<HIR::WhereClauseItem> > where_clause_items;
    HIR::WhereClause where_clause (std::move (where_clause_items));
    HIR::Visibility vis = translate_visibility (function.get_visibility ());

    std::vector<std::unique_ptr<HIR::GenericParam> > generic_params;
    if (function.has_generics ())
      generic_params = lower_generic_params (function.get_generic_params ());

    HIR::Type *return_type
      = function.has_return_type ()
	  ? ASTLoweringType::translate (function.get_return_type ())
	  : nullptr;

    bool is_variadic = function.is_variadic ();
    auto begin = function.get_function_params ().begin ();
    auto end = is_variadic ? function.get_function_params ().end () - 1
			   : function.get_function_params ().end ();

    std::vector<HIR::NamedFunctionParam> function_params;
    for (auto it = begin; it != end; it++)
      {
	auto &param = static_cast<AST::FunctionParam &> (**it);

	if (param.is_variadic () || param.is_self ())
	  continue;
	auto param_kind = param.get_pattern ().get_pattern_kind ();

	rust_assert (param_kind == AST::Pattern::Kind::Identifier
		     || param_kind == AST::Pattern::Kind::Wildcard);
	auto &param_ident
	  = static_cast<AST::IdentifierPattern &> (param.get_pattern ());
	Identifier param_name = param_kind == AST::Pattern::Kind::Identifier
				  ? param_ident.get_ident ()
				  : std::string ("_");

	HIR::Type *param_type = ASTLoweringType::translate (param.get_type ());

	auto crate_num = mappings.get_current_crate ();
	Analysis::NodeMapping mapping (crate_num, param.get_node_id (),
				       mappings.get_next_hir_id (crate_num),
				       mappings.get_next_localdef_id (
					 crate_num));

	function_params.push_back (
	  HIR::NamedFunctionParam (mapping, param_name,
				   std::unique_ptr<HIR::Type> (param_type)));
      }

    auto crate_num = mappings.get_current_crate ();
    Analysis::NodeMapping mapping (crate_num, function.get_node_id (),
				   mappings.get_next_hir_id (crate_num),
				   mappings.get_next_localdef_id (crate_num));

    translated = new HIR::ExternalFunctionItem (
      mapping, function.get_function_name (), std::move (generic_params),
      std::unique_ptr<HIR::Type> (return_type), std::move (where_clause),
      std::move (function_params), is_variadic, std::move (vis),
      function.get_outer_attrs (), function.get_locus ());
  }

  void visit (AST::ExternalTypeItem &type) override
  {
    auto crate_num = mappings.get_current_crate ();
    Analysis::NodeMapping mapping (crate_num, type.get_node_id (),
				   mappings.get_next_hir_id (crate_num),
				   mappings.get_next_localdef_id (crate_num));

    HIR::Visibility vis = translate_visibility (type.get_visibility ());

    translated = new HIR::ExternalTypeItem (mapping, type.get_identifier (),
					    vis, type.get_locus ());
  }

private:
  ASTLoweringExternItem () : translated (nullptr) {}

  HIR::ExternalItem *translated;
};

} // namespace HIR
} // namespace Rust

#endif // RUST_AST_LOWER_ITEM
