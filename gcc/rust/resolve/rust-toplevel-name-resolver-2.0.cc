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

#include "rust-toplevel-name-resolver-2.0.h"
#include "input.h"
#include "optional.h"
#include "rust-ast-full.h"
#include "rust-hir-map.h"
#include "rust-attribute-values.h"

namespace Rust {
namespace Resolver2_0 {

TopLevel::TopLevel (NameResolutionContext &resolver)
  : DefaultResolver (resolver), dirty (false)
{}

template <typename T>
void
TopLevel::insert_enum_variant_or_error_out (const Identifier &identifier,
					    const T &node)
{
  insert_enum_variant_or_error_out (identifier, node.get_locus (),
				    node.get_node_id ());
}

void
TopLevel::check_multiple_insertion_error (
  tl::expected<NodeId, DuplicateNameError> result, const Identifier &identifier,
  const location_t &locus, const NodeId node_id)
{
  if (result)
    dirty = true;
  else if (result.error ().existing != node_id)
    {
      rich_location rich_loc (line_table, locus);
      rich_loc.add_range (node_locations[result.error ().existing]);

      rust_error_at (rich_loc, ErrorCode::E0428, "%qs defined multiple times",
		     identifier.as_string ().c_str ());
    }
}
void
TopLevel::insert_enum_variant_or_error_out (const Identifier &identifier,
					    const location_t &locus,
					    const NodeId node_id)
{
  // keep track of each node's location to provide useful errors
  node_locations.emplace (node_id, locus);

  auto result = ctx.insert_variant (identifier, node_id);
  check_multiple_insertion_error (result, identifier, locus, node_id);
}

template <typename T>
void
TopLevel::insert_or_error_out (const Identifier &identifier, const T &node,
			       Namespace ns)
{
  insert_or_error_out (identifier, node.get_locus (), node.get_node_id (), ns);
}

void
TopLevel::insert_or_error_out (const Identifier &identifier,
			       const location_t &locus, const NodeId &node_id,
			       Namespace ns)
{
  // keep track of each node's location to provide useful errors
  node_locations.emplace (node_id, locus);

  auto result = ctx.insert (identifier, node_id, ns);
  check_multiple_insertion_error (result, identifier, locus, node_id);
}

void
TopLevel::go (AST::Crate &crate)
{
  // we do not include builtin types in the top-level definition collector, as
  // they are not used until `Late`. furthermore, we run this visitor multiple
  // times in a row in a fixed-point fashion, so it would make the code
  // responsible for this ugly and perfom a lot of error checking.

  for (auto &item : crate.items)
    item->accept_vis (*this);
}

void
TopLevel::visit (AST::Module &module)
{
  insert_or_error_out (module.get_name (), module, Namespace::Types);

  // Parse the module's items if they haven't been expanded and the file
  // should be parsed (i.e isn't hidden behind an untrue or impossible cfg
  // directive
  // TODO: make sure this is right
  // TODO: avoid loading items if cfg attributes are present?
  //       might not be needed if this runs after early resolution?
  // This was copied from the old early resolver method
  // 'accumulate_escaped_macros'
  if (module.get_kind () == AST::Module::UNLOADED)
    {
      module.load_items ();

      // If the module was previously unloaded, then we don't want to visit it
      // this time around as the CfgStrip hasn't run on its inner items yet.
      // Skip it for now, mark the visitor as dirty and try again

      dirty = true;

      return;
    }

  DefaultResolver::visit (module);

  if (Analysis::Mappings::get ().lookup_ast_module (module.get_node_id ())
      == tl::nullopt)
    Analysis::Mappings::get ().insert_ast_module (&module);
}

void
TopLevel::visit (AST::Trait &trait)
{
  insert_or_error_out (trait.get_identifier ().as_string (), trait,
		       Namespace::Types);

  DefaultResolver::visit (trait);
}

void
TopLevel::visit (AST::InherentImpl &impl)
{
  auto inner_fn = [this, &impl] () {
    insert_or_error_out (Identifier ("Self", impl.get_type ().get_locus ()),
			 impl.get_type (), Namespace::Types);

    // We do want to visit with the default visitor instead of default resolver
    // because we don't want to insert the scope twice.
    AST::DefaultASTVisitor::visit (impl);
  };

  ctx.scoped (Rib::Kind::TraitOrImpl, impl.get_node_id (), inner_fn);
}

void
TopLevel::visit (AST::TraitImpl &impl)
{
  auto inner_fn = [this, &impl] () {
    insert_or_error_out (Identifier ("Self", impl.get_type ().get_locus ()),
			 impl.get_type (), Namespace::Types);

    // We do want to visit using the default visitor instead of default resolver
    // because we don't want to insert the scope twice.
    AST::DefaultASTVisitor::visit (impl);
  };

  ctx.scoped (Rib::Kind::TraitOrImpl, impl.get_node_id (), inner_fn);
}

void
TopLevel::visit (AST::TraitItemType &trait_item)
{
  insert_or_error_out (trait_item.get_identifier ().as_string (), trait_item,
		       Namespace::Types);

  DefaultResolver::visit (trait_item);
}

template <typename PROC_MACRO>
static void
insert_macros (std::vector<PROC_MACRO> &macros, NameResolutionContext &ctx)
{
  for (auto &macro : macros)
    {
      auto res = ctx.macros.insert (macro.get_name (), macro.get_node_id ());

      if (!res && res.error ().existing != macro.get_node_id ())
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
  auto &mappings = Analysis::Mappings::get ();
  auto num_opt = mappings.lookup_crate_name (crate.get_referenced_crate ());

  if (!num_opt)
    {
      rust_error_at (crate.get_locus (), "unknown crate %qs",
		     crate.get_referenced_crate ().c_str ());
      return;
    }

  CrateNum num = *num_opt;

  auto attribute_macros = mappings.lookup_attribute_proc_macros (num);

  auto bang_macros = mappings.lookup_bang_proc_macros (num);

  auto derive_macros = mappings.lookup_derive_proc_macros (num);

  auto sub_visitor = [&] () {
    // TODO: Find a way to keep this part clean without the double dispatch.
    if (derive_macros.has_value ())
      {
	insert_macros (derive_macros.value (), ctx);
	for (auto &macro : derive_macros.value ())
	  mappings.insert_derive_proc_macro_def (macro);
      }
    if (attribute_macros.has_value ())
      {
	insert_macros (attribute_macros.value (), ctx);
	for (auto &macro : attribute_macros.value ())
	  mappings.insert_attribute_proc_macro_def (macro);
      }
    if (bang_macros.has_value ())
      {
	insert_macros (bang_macros.value (), ctx);
	for (auto &macro : bang_macros.value ())
	  mappings.insert_bang_proc_macro_def (macro);
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
      if (!res && res.error ().existing != macro.get_node_id ())
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

  auto &mappings = Analysis::Mappings::get ();
  if (mappings.lookup_macro_def (macro.get_node_id ()))
    return;

  mappings.insert_macro_def (&macro);
}

void
TopLevel::visit (AST::Function &function)
{
  insert_or_error_out (function.get_function_name (), function,
		       Namespace::Values);

  DefaultResolver::visit (function);
}

void
TopLevel::visit (AST::StaticItem &static_item)
{
  insert_or_error_out (static_item.get_identifier (), static_item,
		       Namespace::Values);

  DefaultResolver::visit (static_item);
}

void
TopLevel::visit (AST::ExternalStaticItem &static_item)
{
  insert_or_error_out (static_item.get_identifier ().as_string (), static_item,
		       Namespace::Values);

  DefaultResolver::visit (static_item);
}

void
TopLevel::visit (AST::StructStruct &struct_item)
{
  auto generic_vis = [this, &struct_item] () {
    for (auto &g : struct_item.get_generic_params ())
      {
	g->accept_vis (*this);
      }
  };

  ctx.scoped (Rib::Kind::Item, struct_item.get_node_id (), generic_vis);

  insert_or_error_out (struct_item.get_struct_name (), struct_item,
		       Namespace::Types);

  // Do we need to insert the constructor in the value namespace as well?

  // Do we need to do anything if the struct is a unit struct?
  if (struct_item.is_unit_struct ())
    insert_or_error_out (struct_item.get_struct_name (), struct_item,
			 Namespace::Values);
}

void
TopLevel::visit (AST::TypeParam &type_param)
{
  insert_or_error_out (type_param.get_type_representation (), type_param,
		       Namespace::Types);

  DefaultResolver::visit (type_param);
}

void
TopLevel::visit (AST::ConstGenericParam &const_param)
{
  insert_or_error_out (const_param.get_name (), const_param, Namespace::Values);

  DefaultResolver::visit (const_param);
}

void
TopLevel::visit (AST::TupleStruct &tuple_struct)
{
  insert_or_error_out (tuple_struct.get_struct_name (), tuple_struct,
		       Namespace::Types);

  insert_or_error_out (tuple_struct.get_struct_name (), tuple_struct,
		       Namespace::Values);

  DefaultResolver::visit (tuple_struct);
}

void
TopLevel::visit (AST::EnumItem &variant)
{
  insert_enum_variant_or_error_out (variant.get_identifier (), variant);
}

void
TopLevel::visit (AST::EnumItemTuple &variant)
{
  insert_enum_variant_or_error_out (variant.get_identifier (), variant);
}

void
TopLevel::visit (AST::EnumItemStruct &variant)
{
  insert_enum_variant_or_error_out (variant.get_identifier (), variant);
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

  DefaultResolver::visit (enum_item);
}

void
TopLevel::visit (AST::Union &union_item)
{
  insert_or_error_out (union_item.get_identifier (), union_item,
		       Namespace::Types);

  DefaultResolver::visit (union_item);
}

void
TopLevel::visit (AST::ConstantItem &const_item)
{
  insert_or_error_out (const_item.get_identifier (), const_item,
		       Namespace::Values);

  DefaultResolver::visit (const_item);
}

void
TopLevel::visit (AST::TypeAlias &type_item)
{
  insert_or_error_out (type_item.get_new_type_name (), type_item,
		       Namespace::Types);

  DefaultResolver::visit (type_item);
}

static void
flatten_rebind (
  const AST::UseTreeRebind &glob,
  std::vector<std::pair<AST::SimplePath, AST::UseTreeRebind>> &rebind_paths);

static void
flatten_list (
  const AST::UseTreeList &glob, std::vector<AST::SimplePath> &paths,
  std::vector<AST::SimplePath> &glob_paths,
  std::vector<std::pair<AST::SimplePath, AST::UseTreeRebind>> &rebind_paths,
  NameResolutionContext &ctx);
static void
flatten_glob (const AST::UseTreeGlob &glob,
	      std::vector<AST::SimplePath> &glob_paths,
	      NameResolutionContext &ctx);

static void
flatten (
  const AST::UseTree *tree, std::vector<AST::SimplePath> &paths,
  std::vector<AST::SimplePath> &glob_paths,
  std::vector<std::pair<AST::SimplePath, AST::UseTreeRebind>> &rebind_paths,
  NameResolutionContext &ctx)
{
  switch (tree->get_kind ())
    {
      case AST::UseTree::Rebind: {
	auto rebind = static_cast<const AST::UseTreeRebind *> (tree);
	flatten_rebind (*rebind, rebind_paths);
	break;
      }
      case AST::UseTree::List: {
	auto list = static_cast<const AST::UseTreeList *> (tree);
	flatten_list (*list, paths, glob_paths, rebind_paths, ctx);
	break;
      }
      case AST::UseTree::Glob: {
	auto glob = static_cast<const AST::UseTreeGlob *> (tree);
	flatten_glob (*glob, glob_paths, ctx);
	break;
      }
      break;
    }
}

static void
flatten_rebind (
  const AST::UseTreeRebind &rebind,
  std::vector<std::pair<AST::SimplePath, AST::UseTreeRebind>> &rebind_paths)
{
  rebind_paths.emplace_back (rebind.get_path (), rebind);
}

/** Prefix a list of subpath
 * @param prefix A prefix for all subpath
 * @param subs List of subpath to prefix
 * @param size List where results should be stored
 */
static void
prefix_subpaths (AST::SimplePath prefix, std::vector<AST::SimplePath> subs,
		 std::vector<AST::SimplePath> &results)
{
  for (auto &sub : subs)
    {
      auto new_path = prefix;
      std::copy (sub.get_segments ().begin (), sub.get_segments ().end (),
		 std::back_inserter (new_path.get_segments ()));
      results.emplace_back (new_path);
    }
}

static void
prefix_rebinds (
  AST::SimplePath prefix,
  std::vector<std::pair<AST::SimplePath, AST::UseTreeRebind>> subs,
  std::vector<std::pair<AST::SimplePath, AST::UseTreeRebind>> &results)
{
  for (auto &sub : subs)
    {
      auto new_path = prefix;
      std::copy (sub.first.get_segments ().begin (),
		 sub.first.get_segments ().end (),
		 std::back_inserter (new_path.get_segments ()));
      results.emplace_back (std::make_pair (new_path, sub.second));
    }
}

static void
flatten_list (
  const AST::UseTreeList &list, std::vector<AST::SimplePath> &paths,
  std::vector<AST::SimplePath> &glob_paths,
  std::vector<std::pair<AST::SimplePath, AST::UseTreeRebind>> &rebind_paths,
  NameResolutionContext &ctx)
{
  auto prefix = AST::SimplePath::create_empty ();
  if (list.has_path ())
    prefix = list.get_path ();

  for (const auto &tree : list.get_trees ())
    {
      auto sub_paths = std::vector<AST::SimplePath> ();
      auto sub_globs = std::vector<AST::SimplePath> ();
      auto sub_rebinds
	= std::vector<std::pair<AST::SimplePath, AST::UseTreeRebind>> ();
      flatten (tree.get (), sub_paths, sub_globs, sub_rebinds, ctx);

      prefix_subpaths (prefix, sub_paths, paths);
      prefix_subpaths (prefix, sub_globs, glob_paths);
      prefix_rebinds (prefix, sub_rebinds, rebind_paths);
    }
}

static void
flatten_glob (const AST::UseTreeGlob &glob, std::vector<AST::SimplePath> &paths,
	      NameResolutionContext &ctx)
{
  if (glob.has_path ())
    paths.emplace_back (glob.get_path ());
}

void
TopLevel::visit (AST::UseDeclaration &use)
{
  auto paths = std::vector<AST::SimplePath> ();
  auto glob_path = std::vector<AST::SimplePath> ();
  auto rebind_path
    = std::vector<std::pair<AST::SimplePath, AST::UseTreeRebind>> ();

  auto &values_rib = ctx.values.peek ();
  auto &types_rib = ctx.types.peek ();
  auto &macros_rib = ctx.macros.peek ();

  // FIXME: How do we handle `use foo::{self}` imports? Some beforehand cleanup?
  // How do we handle module imports in general? Should they get added to all
  // namespaces?

  const auto &tree = use.get_tree ();
  flatten (tree.get (), paths, glob_path, rebind_path, this->ctx);

  auto imports = std::vector<ImportKind> ();

  for (auto &&path : paths)
    imports.emplace_back (
      ImportKind::Simple (std::move (path), values_rib, types_rib, macros_rib));

  for (auto &&glob : glob_path)
    imports.emplace_back (
      ImportKind::Glob (std::move (glob), values_rib, types_rib, macros_rib));

  for (auto &&rebind : rebind_path)
    imports.emplace_back (
      ImportKind::Rebind (std::move (rebind.first), std::move (rebind.second),
			  values_rib, types_rib, macros_rib));

  imports_to_resolve.insert ({use.get_node_id (), std::move (imports)});
}

} // namespace Resolver2_0
} // namespace Rust
