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

#include "rust-toplevel-name-resolver-2.0.h"
#include "rust-ast-full.h"
#include "rust-hir-map.h"
#include "rust-attribute-values.h"

namespace Rust {
namespace Resolver2_0 {

TopLevel::TopLevel (NameResolutionContext &resolver)
  : DefaultResolver (resolver)
{}

template <typename T>
void
TopLevel::insert_or_error_out (const Identifier &identifier, const T &node,
			       Namespace ns)
{
  auto loc = node.get_locus ();
  auto node_id = node.get_node_id ();

  // keep track of each node's location to provide useful errors
  node_locations.emplace (node_id, loc);

  auto result = ctx.insert (identifier, node_id, ns);

  if (!result)
    {
      rich_location rich_loc (line_table, loc);
      rich_loc.add_range (node_locations[result.error ().existing]);

      rust_error_at (rich_loc, ErrorCode::E0428, "%qs defined multiple times",
		     identifier.as_string ().c_str ());
    }
}

void
TopLevel::go (AST::Crate &crate)
{
  for (auto &item : crate.items)
    item->accept_vis (*this);
}

void
TopLevel::visit (AST::Module &module)
{
  // FIXME: Do we need to insert the module in the type namespace?

  auto sub_visitor = [this, &module] () {
    for (auto &item : module.get_items ())
      item->accept_vis (*this);
  };

  ctx.scoped (Rib::Kind::Module, module.get_node_id (), sub_visitor,
	      module.get_name ());
}

template <typename PROC_MACRO>
static void
insert_macros (std::vector<PROC_MACRO> &macros, NameResolutionContext &ctx)
{
  for (auto &macro : macros)
    {
      auto res = ctx.macros.insert (macro.get_name (), macro.get_node_id ());

      if (!res)
	{
	  rust_error_at (UNKNOWN_LOCATION, ErrorCode::E0428,
			 "macro %qs defined multiple times",
			 macro.get_name ().c_str ());
	}
    }
}

void
TopLevel::visit (AST::ExternCrate &crate)
{
  CrateNum num;
  rust_assert (Analysis::Mappings::get ()->lookup_crate_name (
    crate.get_referenced_crate (), num));

  auto attribute_macros
    = Analysis::Mappings::get ()->lookup_attribute_proc_macros (num);

  auto bang_macros = Analysis::Mappings::get ()->lookup_bang_proc_macros (num);

  auto derive_macros
    = Analysis::Mappings::get ()->lookup_derive_proc_macros (num);

  auto sub_visitor = [&] () {
    // TODO: Find a way to keep this part clean without the double dispatch.
    if (derive_macros.has_value ())
      {
	insert_macros (derive_macros.value (), ctx);
	for (auto &macro : derive_macros.value ())
	  Analysis::Mappings::get ()->insert_derive_proc_macro_def (macro);
      }
    if (attribute_macros.has_value ())
      {
	insert_macros (attribute_macros.value (), ctx);
	for (auto &macro : attribute_macros.value ())
	  Analysis::Mappings::get ()->insert_attribute_proc_macro_def (macro);
      }
    if (bang_macros.has_value ())
      {
	insert_macros (bang_macros.value (), ctx);
	for (auto &macro : bang_macros.value ())
	  Analysis::Mappings::get ()->insert_bang_proc_macro_def (macro);
      }
  };

  if (crate.has_as_clause ())
    ctx.scoped (Rib::Kind::Module, crate.get_node_id (), sub_visitor,
		crate.get_as_clause ());
  else
    ctx.scoped (Rib::Kind::Module, crate.get_node_id (), sub_visitor,
		crate.get_referenced_crate ());
}

static bool
is_macro_export (AST::MacroRulesDefinition &def)
{
  for (const auto &attr : def.get_outer_attrs ())
    if (attr.get_path ().as_string () == Values::Attributes::MACRO_EXPORT)
      return true;

  return false;
}

void
TopLevel::visit (AST::MacroRulesDefinition &macro)
{
  // we do not insert macros in the current rib as that needs to be done in the
  // textual scope of the Early pass. we only insert them in the root of the
  // crate if they are marked with #[macro_export]. The execption to this is
  // macros 2.0, which get resolved and inserted like regular items.

  if (is_macro_export (macro))
    {
      auto res = ctx.macros.insert_at_root (macro.get_rule_name (),
					    macro.get_node_id ());
      if (!res)
	{
	  // TODO: Factor this
	  rich_location rich_loc (line_table, macro.get_locus ());
	  rich_loc.add_range (node_locations[res.error ().existing]);

	  rust_error_at (rich_loc, ErrorCode::E0428,
			 "macro %qs defined multiple times",
			 macro.get_rule_name ().as_string ().c_str ());
	}
    }

  if (macro.get_kind () == AST::MacroRulesDefinition::MacroKind::DeclMacro)
    insert_or_error_out (macro.get_rule_name (), macro, Namespace::Macros);

  auto mappings = Analysis::Mappings::get ();
  AST::MacroRulesDefinition *tmp = nullptr;
  if (mappings->lookup_macro_def (macro.get_node_id (), &tmp))
    return;

  mappings->insert_macro_def (&macro);
}

void
TopLevel::visit (AST::Function &function)
{
  insert_or_error_out (function.get_function_name (), function,
		       Namespace::Values);

  auto def_fn
    = [this, &function] () { function.get_definition ()->accept_vis (*this); };

  ctx.scoped (Rib::Kind::Function, function.get_node_id (), def_fn);
}

void
TopLevel::visit (AST::BlockExpr &expr)
{
  // extracting the lambda from the `scoped` call otherwise the code looks like
  // a hot turd thanks to our .clang-format

  auto sub_vis = [this, &expr] () {
    for (auto &stmt : expr.get_statements ())
      stmt->accept_vis (*this);

    if (expr.has_tail_expr ())
      expr.get_tail_expr ()->accept_vis (*this);
  };

  ctx.scoped (Rib::Kind::Normal, expr.get_node_id (), sub_vis);
}

void
TopLevel::visit (AST::StaticItem &static_item)
{
  auto sub_vis
    = [this, &static_item] () { static_item.get_expr ()->accept_vis (*this); };

  ctx.scoped (Rib::Kind::Item, static_item.get_node_id (), sub_vis);
}

void
TopLevel::visit (AST::TraitItemFunc &item)
{
  auto def_vis
    = [this, &item] () { item.get_definition ()->accept_vis (*this); };

  if (item.has_definition ())
    ctx.scoped (Rib::Kind::Function, item.get_node_id (), def_vis);
}

void
TopLevel::visit (AST::StructStruct &struct_item)
{
  insert_or_error_out (struct_item.get_struct_name (), struct_item,
		       Namespace::Types);

  // Do we need to insert the constructor in the value namespace as well?

  // Do we need to do anything if the struct is a unit struct?
  if (struct_item.is_unit_struct ())
    insert_or_error_out (struct_item.get_struct_name (), struct_item,
			 Namespace::Values);
}

void
TopLevel::visit (AST::TupleStruct &tuple_struct)
{
  insert_or_error_out (tuple_struct.get_struct_name (), tuple_struct,
		       Namespace::Types);
}

void
TopLevel::visit (AST::EnumItem &variant)
{
  insert_or_error_out (variant.get_identifier (), variant, Namespace::Types);
}

void
TopLevel::visit (AST::EnumItemTuple &variant)
{
  insert_or_error_out (variant.get_identifier (), variant, Namespace::Types);
}

void
TopLevel::visit (AST::EnumItemStruct &variant)
{
  insert_or_error_out (variant.get_identifier (), variant, Namespace::Types);
}

void
TopLevel::visit (AST::EnumItemDiscriminant &variant)
{
  insert_or_error_out (variant.get_identifier (), variant, Namespace::Types);
}

void
TopLevel::visit (AST::Enum &enum_item)
{
  insert_or_error_out (enum_item.get_identifier (), enum_item,
		       Namespace::Types);

  auto field_vis = [this, &enum_item] () {
    for (auto &variant : enum_item.get_variants ())
      variant->accept_vis (*this);
  };

  ctx.scoped (Rib::Kind::Item /* FIXME: Is that correct? */,
	      enum_item.get_node_id (), field_vis, enum_item.get_identifier ());
}

void
TopLevel::visit (AST::Union &union_item)
{
  insert_or_error_out (union_item.get_identifier (), union_item,
		       Namespace::Types);
}

void
TopLevel::visit (AST::ConstantItem &const_item)
{
  auto expr_vis
    = [this, &const_item] () { const_item.get_expr ()->accept_vis (*this); };

  ctx.scoped (Rib::Kind::ConstantItem, const_item.get_node_id (), expr_vis);
}

} // namespace Resolver2_0
} // namespace Rust
