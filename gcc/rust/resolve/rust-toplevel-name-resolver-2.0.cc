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

void
GlobbingVisitor::go (AST::Module *module)
{
  for (auto &i : module->get_items ())
    visit (i);
}

void
GlobbingVisitor::visit (AST::Module &module)
{
  if (module.get_visibility ().is_public ())
    ctx.insert_shadowable (module.get_name (), module.get_node_id (),
			   Namespace::Types);
}

void
GlobbingVisitor::visit (AST::MacroRulesDefinition &macro)
{
  if (macro.get_visibility ().is_public ())
    ctx.insert_shadowable (macro.get_rule_name (), macro.get_node_id (),
			   Namespace::Macros);
}

void
GlobbingVisitor::visit (AST::Function &function)
{
  if (function.get_visibility ().is_public ())
    ctx.insert_shadowable (function.get_function_name (),
			   function.get_node_id (), Namespace::Values);
}

void
GlobbingVisitor::visit (AST::StaticItem &static_item)
{
  if (static_item.get_visibility ().is_public ())
    ctx.insert_shadowable (static_item.get_identifier (),
			   static_item.get_node_id (), Namespace::Values);
}

void
GlobbingVisitor::visit (AST::StructStruct &struct_item)
{
  if (struct_item.get_visibility ().is_public ())
    {
      ctx.insert_shadowable (struct_item.get_identifier (),
			     struct_item.get_node_id (), Namespace::Types);
      if (struct_item.is_unit_struct ())
	ctx.insert_shadowable (struct_item.get_identifier (),
			       struct_item.get_node_id (), Namespace::Values);
    }
}

void
GlobbingVisitor::visit (AST::TupleStruct &tuple_struct)
{
  if (tuple_struct.get_visibility ().is_public ())
    {
      ctx.insert_shadowable (tuple_struct.get_identifier (),
			     tuple_struct.get_node_id (), Namespace::Types);

      ctx.insert_shadowable (tuple_struct.get_identifier (),
			     tuple_struct.get_node_id (), Namespace::Values);
    }
}

void
GlobbingVisitor::visit (AST::Enum &enum_item)
{
  if (enum_item.get_visibility ().is_public ())
    ctx.insert_shadowable (enum_item.get_identifier (),
			   enum_item.get_node_id (), Namespace::Types);
}

void
GlobbingVisitor::visit (AST::Union &union_item)
{
  if (union_item.get_visibility ().is_public ())
    ctx.insert_shadowable (union_item.get_identifier (),
			   union_item.get_node_id (), Namespace::Values);
}

void
GlobbingVisitor::visit (AST::ConstantItem &const_item)
{
  if (const_item.get_visibility ().is_public ())
    ctx.insert_shadowable (const_item.get_identifier (),
			   const_item.get_node_id (), Namespace::Values);
}

void
GlobbingVisitor::visit (AST::ExternCrate &crate)
{}

void
GlobbingVisitor::visit (AST::UseDeclaration &use)
{
  // Handle cycles ?
}

TopLevel::TopLevel (NameResolutionContext &resolver)
  : DefaultResolver (resolver)
{}

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

  if (!result && result.error ().existing != node_id)
    {
      rich_location rich_loc (line_table, locus);
      rich_loc.add_range (node_locations[result.error ().existing]);

      rust_error_at (rich_loc, ErrorCode::E0428, "%qs defined multiple times",
		     identifier.as_string ().c_str ());
    }
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

  auto sub_visitor = [this, &module] () {
    for (auto &item : module.get_items ())
      item->accept_vis (*this);
  };

  ctx.scoped (Rib::Kind::Module, module.get_node_id (), sub_visitor,
	      module.get_name ());

  if (Analysis::Mappings::get ()->lookup_ast_module (module.get_node_id ())
      == tl::nullopt)
    Analysis::Mappings::get ()->insert_ast_module (&module);
}

void
TopLevel::visit (AST::Trait &trait)
{
  // FIXME: This Self injection is dodgy. It even lead to issues with metadata
  // export in the past (#2349). We cannot tell appart injected parameters from
  // regular ones. Dumping generic parameters highlights this Self in metadata,
  // during debug or proc macro collection. This is clearly a hack.
  //
  // For now I'll keep it here in the new name resolver even if it should
  // probably not be there. We need to find another way to solve this.
  // Maybe an additional attribute to Trait ?
  //
  // From old resolver:
  //// we need to inject an implicit self TypeParam here
  //// FIXME: which location should be used for Rust::Identifier `Self`?
  AST::TypeParam *implicit_self
    = new AST::TypeParam ({"Self"}, trait.get_locus ());
  trait.insert_implict_self (
    std::unique_ptr<AST::GenericParam> (implicit_self));

  DefaultResolver::visit (trait);
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

  DefaultResolver::visit (function);
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
      expr.get_tail_expr ().accept_vis (*this);
  };

  ctx.scoped (Rib::Kind::Normal, expr.get_node_id (), sub_vis);
}

void
TopLevel::visit (AST::StaticItem &static_item)
{
  auto sub_vis
    = [this, &static_item] () { static_item.get_expr ().accept_vis (*this); };

  ctx.scoped (Rib::Kind::Item, static_item.get_node_id (), sub_vis);
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

  insert_or_error_out (tuple_struct.get_struct_name (), tuple_struct,
		       Namespace::Values);
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
  insert_or_error_out (const_item.get_identifier (), const_item,
		       Namespace::Values);

  auto expr_vis
    = [this, &const_item] () { const_item.get_expr ().accept_vis (*this); };

  ctx.scoped (Rib::Kind::ConstantItem, const_item.get_node_id (), expr_vis);
}

bool
TopLevel::handle_use_glob (AST::SimplePath &glob)
{
  auto resolved = ctx.types.resolve_path (glob.get_segments ());
  if (!resolved.has_value ())
    return false;

  auto result
    = Analysis::Mappings::get ()->lookup_ast_module (resolved->get_node_id ());

  if (!result.has_value ())
    return false;

  GlobbingVisitor gvisitor (ctx);
  gvisitor.go (result.value ());

  return true;
}

bool
TopLevel::handle_use_dec (AST::SimplePath &path)
{
  auto locus = path.get_final_segment ().get_locus ();
  auto declared_name = path.get_final_segment ().as_string ();

  // in what namespace do we perform path resolution? All of them? see which one
  // matches? Error out on ambiguities?
  // so, apparently, for each one that matches, add it to the proper namespace
  // :(

  auto found = false;

  auto resolve_and_insert
    = [this, &found, &declared_name, locus] (Namespace ns,
					     const AST::SimplePath &path) {
	tl::optional<Rib::Definition> resolved = tl::nullopt;

	// FIXME: resolve_path needs to return an `expected<NodeId, Error>` so
	// that we can improve it with hints or location or w/ever. and maybe
	// only emit it the first time.
	switch (ns)
	  {
	  case Namespace::Values:
	    resolved = ctx.values.resolve_path (path.get_segments ());
	    break;
	  case Namespace::Types:
	    resolved = ctx.types.resolve_path (path.get_segments ());
	    break;
	  case Namespace::Macros:
	    resolved = ctx.macros.resolve_path (path.get_segments ());
	    break;
	  case Namespace::Labels:
	    // TODO: Is that okay?
	    rust_unreachable ();
	  }

	// FIXME: Ugly
	(void) resolved.map ([this, &found, &declared_name, locus, ns,
			      path] (Rib::Definition def) {
	  found = true;

	  // what do we do with the id?
	  insert_or_error_out (declared_name, locus, def.get_node_id (), ns);
	  auto result = node_forwarding.find (def.get_node_id ());
	  if (result != node_forwarding.cend ()
	      && result->second != path.get_node_id ())
	    rust_error_at (path.get_locus (), "%qs defined multiple times",
			   declared_name.c_str ());
	  else // No previous thing has inserted this into our scope
	    node_forwarding.insert ({def.get_node_id (), path.get_node_id ()});

	  return def.get_node_id ();
	});
      };

  resolve_and_insert (Namespace::Values, path);
  resolve_and_insert (Namespace::Types, path);
  resolve_and_insert (Namespace::Macros, path);

  return found;
}

bool
TopLevel::handle_rebind (std::pair<AST::SimplePath, AST::UseTreeRebind> &rebind)
{
  auto &path = rebind.first;

  location_t locus = UNKNOWN_LOCATION;
  std::string declared_name;

  switch (rebind.second.get_new_bind_type ())
    {
    case AST::UseTreeRebind::NewBindType::IDENTIFIER:
      declared_name = rebind.second.get_identifier ().as_string ();
      locus = rebind.second.get_identifier ().get_locus ();
      break;
    case AST::UseTreeRebind::NewBindType::NONE:
      declared_name = path.get_final_segment ().as_string ();
      locus = path.get_final_segment ().get_locus ();
      break;
    case AST::UseTreeRebind::NewBindType::WILDCARD:
      rust_unreachable ();
      break;
    }

  // in what namespace do we perform path resolution? All
  // of them? see which one matches? Error out on
  // ambiguities? so, apparently, for each one that
  // matches, add it to the proper namespace
  // :(
  auto found = false;

  auto resolve_and_insert = [this, &found, &declared_name,
			     locus] (Namespace ns,
				     const AST::SimplePath &path) {
    tl::optional<Rib::Definition> resolved = tl::nullopt;
    tl::optional<Rib::Definition> resolved_bind = tl::nullopt;

    std::vector<AST::SimplePathSegment> declaration_v
      = {AST::SimplePathSegment (declared_name, locus)};
    // FIXME: resolve_path needs to return an `expected<NodeId, Error>` so
    // that we can improve it with hints or location or w/ever. and maybe
    // only emit it the first time.
    switch (ns)
      {
      case Namespace::Values:
	resolved = ctx.values.resolve_path (path.get_segments ());
	resolved_bind = ctx.values.resolve_path (declaration_v);
	break;
      case Namespace::Types:
	resolved = ctx.types.resolve_path (path.get_segments ());
	resolved_bind = ctx.types.resolve_path (declaration_v);
	break;
      case Namespace::Macros:
	resolved = ctx.macros.resolve_path (path.get_segments ());
	resolved_bind = ctx.macros.resolve_path (declaration_v);
	break;
      case Namespace::Labels:
	// TODO: Is that okay?
	rust_unreachable ();
      }

    resolved.map ([this, &found, &declared_name, locus, ns, path,
		   &resolved_bind] (Rib::Definition def) {
      found = true;

      insert_or_error_out (declared_name, locus, def.get_node_id (), ns);
      if (resolved_bind.has_value ())
	{
	  auto bind_def = resolved_bind.value ();
	  // what do we do with the id?
	  auto result = node_forwarding.find (bind_def.get_node_id ());
	  if (result != node_forwarding.cend ()
	      && result->second != path.get_node_id ())
	    rust_error_at (path.get_locus (), "%qs defined multiple times",
			   declared_name.c_str ());
	}
      else
	{
	  // No previous thing has inserted this into our scope
	  node_forwarding.insert ({def.get_node_id (), path.get_node_id ()});
	}
      return def.get_node_id ();
    });
  };

  // do this for all namespaces (even Labels?)

  resolve_and_insert (Namespace::Values, path);
  resolve_and_insert (Namespace::Types, path);
  resolve_and_insert (Namespace::Macros, path);

  // TODO: No labels? No, right?

  return found;
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

  // FIXME: How do we handle `use foo::{self}` imports? Some beforehand cleanup?
  // How do we handle module imports in general? Should they get added to all
  // namespaces?

  const auto &tree = use.get_tree ();
  flatten (tree.get (), paths, glob_path, rebind_path, this->ctx);

  for (auto &path : paths)
    if (!handle_use_dec (path))
      rust_error_at (path.get_final_segment ().get_locus (), ErrorCode::E0433,
		     "unresolved import %qs", path.as_string ().c_str ());

  for (auto &glob : glob_path)
    if (!handle_use_glob (glob))
      rust_error_at (glob.get_final_segment ().get_locus (), ErrorCode::E0433,
		     "unresolved import %qs", glob.as_string ().c_str ());

  for (auto &rebind : rebind_path)
    if (!handle_rebind (rebind))
      rust_error_at (rebind.first.get_final_segment ().get_locus (),
		     ErrorCode::E0433, "unresolved import %qs",
		     rebind.first.as_string ().c_str ());
}

} // namespace Resolver2_0
} // namespace Rust
