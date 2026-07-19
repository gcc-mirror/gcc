// Copyright (C) 2020-2026 Free Software Foundation, Inc.

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

#include "rust-early-name-resolver-2.0.h"
#include "optional.h"
#include "options.h"
#include "rust-ast.h"
#include "rust-diagnostics.h"
#include "rust-hir-map.h"
#include "rust-item.h"
#include "rust-name-resolution-context.h"
#include "rust-rib.h"
#include "rust-toplevel-name-resolver-2.0.h"
#include "rust-attributes.h"
#include "rust-finalize-imports-2.0.h"
#include "rust-attribute-values.h"
#include "rust-identifier-path.h"
#include "rust-session-manager.h"

namespace Rust {
namespace Resolver2_0 {

Early::Early (NameResolutionContext &ctx)
  : DefaultResolver (ctx), toplevel (TopLevel (ctx)), dirty (false)
{}

void
Early::try_insert_once (AST::MacroInvocation &invocation, NodeId resolved)
{
  auto leaf_macro = ctx.macros.find_leaf_definition (resolved);

  // Sometimes the import itself isn't resolved yet this turn of the fixed-point
  if (!leaf_macro)
    return;

  // TODO: Should we use `ctx.map_usage()`?

  auto definition = ctx.mappings.lookup_macro_def (leaf_macro->id);

  if (!ctx.mappings.lookup_macro_invocation (invocation))
    ctx.mappings.insert_macro_invocation (invocation, definition.value ());
}

void
Early::insert_once (AST::MacroRulesDefinition &def)
{
  if (!ctx.mappings.lookup_macro_def (def.get_node_id ()))
    ctx.mappings.insert_macro_def (&def);
}

void
Early::go (AST::Crate &crate)
{
  // First we go through TopLevel resolution to get all our declared items
  toplevel.go (crate);

  // We start with resolving the list of imports that `TopLevel` has built for
  // us

  dirty = toplevel.is_dirty ();

  // We now proceed with resolving macros, which can be nested in almost any
  // items
  textual_scope.push ();

  visit (crate);

  textual_scope.pop ();

  // handle IdentifierPattern vs PathInExpression disambiguation
  IdentifierPathPass::go (crate, ctx, std::move (ident_path_to_convert));
}

bool
Early::resolve_glob_import (NodeId use_dec_id, TopLevel::ImportKind &&glob)
{
  auto resolved = ctx.resolve_path (glob.to_resolve, Namespace::Types);
  if (!resolved.has_value ())
    return false;

  auto result = Analysis::Mappings::get ().lookup_glob_container (
    resolved->definition.get_node_id ());

  if (!result)
    return false;

  // here, we insert the module's NodeId into the import_mappings and will look
  // up the module proper in `FinalizeImports`
  // The namespace does not matter here since we are dealing with a glob
  // FIXME: Does the namespace not matter? Is that valid?
  // TODO: Ugly
  import_mappings.insert (use_dec_id,
			  ImportPair (std::move (glob),
				      ImportData::Glob (resolved->definition)));

  return true;
}

bool
Early::resolve_simple_import (NodeId use_dec_id, TopLevel::ImportKind &&import)
{
  auto definitions = resolve_path_in_all_ns (import.to_resolve);

  // if we've found at least one definition, then we're good
  if (definitions.empty ())
    return false;

  auto &imports = import_mappings.new_or_access (use_dec_id);

  imports.emplace_back (
    ImportPair (std::move (import),
		ImportData::Simple (std::move (definitions))));

  return true;
}

bool
Early::resolve_rebind_import (NodeId use_dec_id,
			      TopLevel::ImportKind &&rebind_import)
{
  NodeId import_id = UNKNOWN_NODEID;
  auto &path = rebind_import.to_resolve;
  auto &rebind = rebind_import.rebind.value ();

  switch (rebind.get_new_bind_type ())
    {
    case AST::UseTreeRebind::NewBindType::IDENTIFIER:
      import_id = rebind.get_node_id ();
      break;
    case AST::UseTreeRebind::NewBindType::NONE:
      {
	const auto &segments = path.get_segments ();
	// We don't want to insert `self` with `use module::self`
	if (path.get_final_segment ().is_lower_self_seg ())
	  {
	    // Erroneous `self` or `{self}` use declaration
	    if (segments.size () == 1)
	      break;
	    import_id = segments[segments.size () - 2].get_node_id ();
	  }
	else
	  {
	    import_id = path.get_final_segment ().get_node_id ();
	  }
	break;
      }
    case AST::UseTreeRebind::NewBindType::WILDCARD:
      // nothing
      break;
    }

  if (ctx.lookup (import_id, Namespace::Types))
    return true;

  auto definitions = resolve_path_in_all_ns (rebind_import.to_resolve);

  // if we've found at least one definition, then we're good
  if (definitions.empty ())
    return false;
  for (const auto &def : definitions)
    {
      if (def.definition.is_ambiguous ())
	{
	  rich_location rich_locus (line_table,
				    rebind_import.to_resolve.get_locus ());
	  rust_error_at (rich_locus, ErrorCode::E0659, "%qs is ambiguous",
			 rebind_import.to_resolve.as_string ().c_str ());
	  return true;
	}
    }

  auto &imports = import_mappings.new_or_access (use_dec_id);

  imports.emplace_back (
    ImportPair (std::move (rebind_import),
		ImportData::Rebind (std::move (definitions))));

  return true;
}

void
Early::build_import_mapping (
  std::pair<NodeId, std::vector<TopLevel::ImportKind>> &&use_import)
{
  auto found = false;
  auto use_dec_id = use_import.first;

  for (auto &&import : use_import.second)
    {
      // We create a copy of the path in case of errors, since the `import` will
      // be moved into the newly created import mappings
      auto path = import.to_resolve;

      // used to skip the "unresolved import" error
      // if we output other errors during resolution
      size_t old_error_count = macro_resolve_errors.size ();

      switch (import.kind)
	{
	case TopLevel::ImportKind::Kind::Glob:
	  found = resolve_glob_import (use_dec_id, std::move (import));
	  break;
	case TopLevel::ImportKind::Kind::Simple:
	  found = resolve_simple_import (use_dec_id, std::move (import));
	  break;
	case TopLevel::ImportKind::Kind::Rebind:
	  found = resolve_rebind_import (use_dec_id, std::move (import));
	  break;
	}

      if (!found && old_error_count == macro_resolve_errors.size ())
	collect_error (Error (path.get_final_segment ().get_locus (),
			      ErrorCode::E0433, "unresolved import %qs",
			      path.as_string ().c_str ()));
    }
}

void
Early::TextualScope::push ()
{
  // push a new empty scope
  scopes.emplace_back ();
}

void
Early::TextualScope::pop ()
{
  rust_assert (!scopes.empty ());

  scopes.pop_back ();
}

void
Early::TextualScope::insert (std::string name, NodeId id)
{
  rust_assert (!scopes.empty ());

  // we can ignore the return value as we always want the latest defined macro
  // to shadow a previous one - so if two macros have the same name and get
  // inserted with the same key, it's not a bug
  scopes.back ().insert ({name, id});
}

tl::optional<NodeId>
Early::TextualScope::get (const std::string &name)
{
  for (auto iterator = scopes.rbegin (); iterator != scopes.rend (); iterator++)
    {
      auto scope = *iterator;
      auto found = scope.find (name);
      if (found != scope.end ())
	return found->second;
    }

  return tl::nullopt;
}

void
Early::visit (AST::MacroRulesDefinition &def)
{
  DefaultResolver::visit (def);

  textual_scope.insert (def.get_rule_name ().as_string (), def.get_node_id ());
  insert_once (def);
}

void
Early::visit (AST::BlockExpr &block)
{
  textual_scope.push ();

  DefaultResolver::visit (block);

  textual_scope.pop ();
}

void
Early::visit (AST::Module &module)
{
  bool is_macro_use = false;

  for (const auto &attr : module.get_outer_attrs ())
    {
      if (attr.get_path ().as_string () == Values::Attributes::MACRO_USE)
	{
	  is_macro_use = true;
	  break;
	}
    }

  if (!is_macro_use)
    textual_scope.push ();

  DefaultResolver::visit (module);

  if (!is_macro_use)
    textual_scope.pop ();
}

void
Early::visit (AST::MacroInvocation &invoc)
{
  auto &path = invoc.get_invoc_data ().get_path ();

  // We special case the `offset_of!()` macro if the flag is here, otherwise
  // we accept whatever `offset_of!()` definition we resolved to.
  auto resolve_offset_of = Session::get_instance ().should_support_offset_of ()
			   && (path.as_string () == "offset_of");

  if (invoc.get_kind () == AST::MacroInvocation::InvocKind::Builtin)
    for (auto &pending_invoc : invoc.get_pending_eager_invocations ())
      pending_invoc->accept_vis (*this);

  // When a macro is invoked by an unqualified identifier (not part of a
  // multi-part path), it is first looked up in textual scoping. If this does
  // not yield any results, then it is looked up in path-based scoping. If the
  // macro's name is qualified with a path, then it is only looked up in
  // path-based scoping.

  // https://doc.rust-lang.org/reference/macros-by-example.html#path-based-scope

  tl::optional<NameResolutionContext::NamespacedDefinition> ns_def
    = tl::nullopt;
  if (path.get_segments ().size () == 1)
    ns_def = textual_scope.get (path.get_final_segment ().as_string ())
	       .map ([] (NodeId id) {
		 return NameResolutionContext::NamespacedDefinition (
		   Rib::Definition::NonShadowable (id), Namespace::Macros);
	       });

  // we won't have changed `definition` from `nullopt` if there are more
  // than one segments in our path
  if (!ns_def.has_value ())
    ns_def = ctx.resolve_path (path, Namespace::Macros);

  // if the definition still does not have a value, then it's an error - unless
  // we should automatically resolve offset_of!() calls
  if (!ns_def.has_value ())
    {
      if (!resolve_offset_of)
	collect_error (Error (invoc.get_locus (), ErrorCode::E0433,
			      "could not resolve macro invocation %qs",
			      path.as_string ().c_str ()));
      return;
    }

  try_insert_once (invoc, ns_def->definition.get_node_id ());

  // now do we need to keep mappings or something? or insert "uses" into our
  // ForeverStack? can we do that? are mappings simpler?
  auto &mappings = Analysis::Mappings::get ();
  auto rules_def
    = mappings.lookup_macro_def (ns_def->definition.get_node_id ());

  // Macro definition not found, maybe it is not expanded yet.
  if (!rules_def)
    return;

  if (mappings.lookup_macro_invocation (invoc))
    return;

  mappings.insert_macro_invocation (invoc, rules_def.value ());
}

void
Early::visit_derive_attribute (AST::Attribute &attr,
			       Analysis::Mappings &mappings)
{
  auto traits = attr.get_traits_to_derive ();
  for (auto &trait : traits)
    {
      auto ns_def = ctx.resolve_path (trait.get (), Namespace::Macros);
      if (!ns_def.has_value ())
	{
	  // FIXME: Change to proper error message
	  collect_error (Error (trait.get ().get_locus (),
				"could not resolve trait %qs",
				trait.get ().as_string ().c_str ()));
	  continue;
	}

      auto pm_def = mappings.lookup_derive_proc_macro_def (
	ns_def->definition.get_node_id ());

      if (pm_def.has_value ())
	mappings.insert_derive_proc_macro_invocation (trait, pm_def.value ());
    }
}

void
Early::visit_non_builtin_attribute (AST::Attribute &attr,
				    Analysis::Mappings &mappings,
				    std::string &name)
{
  auto ns_def = ctx.resolve_path (attr.get_path (), Namespace::Macros);
  if (!ns_def.has_value ())
    {
      // FIXME: Change to proper error message
      collect_error (Error (attr.get_locus (),
			    "could not resolve attribute macro invocation %qs",
			    name.c_str ()));
      return;
    }
  auto pm_def = mappings.lookup_attribute_proc_macro_def (
    ns_def->definition.get_node_id ());

  if (!pm_def.has_value ())
    return;

  mappings.insert_attribute_proc_macro_invocation (attr.get_path (),
						   pm_def.value ());
}

void
Early::visit (AST::Attribute &attr)
{
  auto &mappings = Analysis::Mappings::get ();

  auto name = attr.get_path ().get_segments ().at (0).get_segment_name ();
  auto is_not_builtin = [&name] (AST::Attribute &attr) {
    return Analysis::BuiltinAttributeMappings::get ()
      ->lookup_builtin (name)
      .is_error ();
  };

  if (attr.is_derive ())
    {
      visit_derive_attribute (attr, mappings);
    }
  else if (is_not_builtin (attr)) // Do not resolve builtins
    {
      visit_non_builtin_attribute (attr, mappings, name);
    }

  DefaultResolver::visit (attr);
}

void
Early::finalize_simple_import (const Early::ImportPair &mapping)
{
  // FIXME: We probably need to store namespace information

  auto import = mapping.import_kind.to_resolve;
  auto import_id = import.get_final_segment ().get_node_id ();
  auto data = mapping.data;
  auto identifier = import.get_final_segment ().get_segment_name ();

  for (auto &&definition : data.definitions ())
    {
      ctx.map_usage (Usage (import_id),
		     Definition (definition.definition.get_node_id ()),
		     definition.ns);

      toplevel.insert_or_error_out (identifier, import.get_locus (),
				    definition.definition.get_node_id (),
				    definition.ns);

      dirty = dirty || toplevel.is_dirty ();
    }
}

void
Early::finalize_glob_import (NameResolutionContext &ctx,
			     const Early::ImportPair &mapping)
{
  auto container = Analysis::Mappings::get ().lookup_glob_container (
    mapping.data.container ().get_node_id ());

  rust_assert (container);

  if (mapping.import_kind.is_prelude)
    {
      rust_assert (container.value ()->get_glob_container_kind ()
		   == AST::GlobContainer::Kind::Module);

      ctx.prelude = mapping.data.container ().get_node_id ();
    }

  GlobbingVisitor glob_visit (ctx);
  glob_visit.go (container.value ());
  dirty |= glob_visit.is_dirty ();
}

void
Early::finalize_rebind_import (const Early::ImportPair &mapping)
{
  // We can fetch the value here as `resolve_rebind` will only be called on
  // imports of the right kind
  auto &path = mapping.import_kind.to_resolve;
  auto &rebind = mapping.import_kind.rebind.value ();
  auto data = mapping.data;

  NodeId import_id = UNKNOWN_NODEID;
  std::string declared_name;

  // FIXME: This needs to be done in `FinalizeImports`
  switch (rebind.get_new_bind_type ())
    {
    case AST::UseTreeRebind::NewBindType::IDENTIFIER:
      declared_name = rebind.get_identifier ().as_string ();
      import_id = rebind.get_node_id ();
      break;
    case AST::UseTreeRebind::NewBindType::NONE:
      {
	const auto &segments = path.get_segments ();
	// We don't want to insert `self` with `use module::self`
	if (path.get_final_segment ().is_lower_self_seg ())
	  {
	    // Erroneous `self` or `{self}` use declaration
	    if (segments.size () == 1)
	      return;
	    declared_name = segments[segments.size () - 2].as_string ();
	    import_id = segments[segments.size () - 2].get_node_id ();
	  }
	else
	  {
	    declared_name = path.get_final_segment ().as_string ();
	    import_id = path.get_final_segment ().get_node_id ();
	  }
	break;
      }
    case AST::UseTreeRebind::NewBindType::WILDCARD:
      // We don't want to insert it into the trie
      return;
    }

  for (auto &&definition : data.definitions ())
    {
      ctx.map_usage (Usage (import_id),
		     Definition (definition.definition.get_node_id ()),
		     definition.ns);

      toplevel.insert_or_error_out (declared_name, path.get_locus (),
				    definition.definition.get_node_id (),
				    definition.ns);

      dirty = dirty || toplevel.is_dirty ();

      // Map the import to the glob container if it exists - this is important
      // for 2-stepped glob imports which refer to glob containers, e.g.
      //
      // enum Foo { ... }
      // pub use Foo;
      // use self::Foo::*;
      auto &mappings = Analysis::Mappings::get ();
      if (auto container = mappings.lookup_glob_container (
	    definition.definition.get_node_id ()))
	mappings.insert_glob_container (import_id, container.value ());
    }
}

void
Early::visit (AST::UseDeclaration &decl)
{
  // We do not want to visit the use trees, we're only looking for top level
  // rebind. eg. `use something;` or `use something::other;`
  if (decl.get_tree ()->get_kind () == AST::UseTree::Kind::Rebind)
    {
      auto &rebind = static_cast<AST::UseTreeRebind &> (*decl.get_tree ());
      if (rebind.get_path ().get_final_segment ().is_lower_self_seg ())
	{
	  collect_error (
	    Error (decl.get_locus (), ErrorCode::E0429,
		   "%<self%> imports are only allowed within a { } list"));
	}
    }

  auto &imports = toplevel.get_imports_to_resolve ();
  auto current_import = imports.find (decl.get_node_id ());
  if (current_import != imports.end ())
    {
      build_import_mapping (*current_import);
    }

  // Once this is done, we finalize their resolution
  for (const auto &mapping : import_mappings.get (decl.get_node_id ()))
    switch (mapping.import_kind.kind)
      {
      case TopLevel::ImportKind::Kind::Glob:
	finalize_glob_import (ctx, mapping);
	break;
      case TopLevel::ImportKind::Kind::Simple:
	finalize_simple_import (mapping);
	break;
      case TopLevel::ImportKind::Kind::Rebind:
	finalize_rebind_import (mapping);
	break;
      }

  DefaultResolver::visit (decl);
}

void
Early::visit (AST::UseTreeList &use_list)
{
  if (!use_list.has_path ())
    {
      for (auto &&tree : use_list.get_trees ())
	{
	  if (tree->get_kind () == AST::UseTree::Kind::Rebind)
	    {
	      auto &rebind = static_cast<AST::UseTreeRebind &> (*tree);
	      auto path_size = rebind.get_path ().get_segments ().size ();
	      if (path_size == 1
		  && rebind.get_path ()
		       .get_final_segment ()
		       .is_lower_self_seg ())
		{
		  collect_error (Error (rebind.get_locus (), ErrorCode::E0431,
					"%<self%> import can only appear in an "
					"import list with a non-empty prefix"));
		}
	    }
	}
    }
  DefaultResolver::visit (use_list);
}

void
Early::visit (AST::IdentifierPattern &identifier)
{
  // check if this is *really* a path pattern
  if (!identifier.get_is_ref () && !identifier.get_is_mut ()
      && !identifier.has_subpattern ())
    {
      auto res = ctx.values.get (identifier.get_ident ());
      if (res)
	{
	  if (res->is_ambiguous ())
	    rust_error_at (identifier.get_locus (), ErrorCode::E0659,
			   "%qs is ambiguous",
			   identifier.get_ident ().as_string ().c_str ());
	  else
	    {
	      // HACK: bail out if the definition is a function
	      if (!ctx.mappings.is_function_node (res->get_node_id ()))
		ident_path_to_convert.insert (identifier.get_node_id ());
	    }
	}
    }
}

} // namespace Resolver2_0
} // namespace Rust
