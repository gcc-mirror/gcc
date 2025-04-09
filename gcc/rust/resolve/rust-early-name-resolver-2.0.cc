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

#include "rust-early-name-resolver-2.0.h"
#include "rust-ast-full.h"
#include "rust-diagnostics.h"
#include "rust-toplevel-name-resolver-2.0.h"
#include "rust-attributes.h"
#include "rust-finalize-imports-2.0.h"
#include "rust-attribute-values.h"

namespace Rust {
namespace Resolver2_0 {

Early::Early (NameResolutionContext &ctx)
  : DefaultResolver (ctx), toplevel (TopLevel (ctx)), dirty (false)
{}

void
Early::insert_once (AST::MacroInvocation &invocation, NodeId resolved)
{
  // TODO: Should we use `ctx.mark_resolved()`?
  auto definition = ctx.mappings.lookup_macro_def (resolved);

  if (!ctx.mappings.lookup_macro_invocation (invocation))
    ctx.mappings.insert_macro_invocation (invocation, definition.value ());
}

void
Early::insert_once (AST::MacroRulesDefinition &def)
{
  // TODO: Should we use `ctx.mark_resolved()`?
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
  for (auto &item : crate.items)
    item->accept_vis (*this);
  textual_scope.pop ();
}

bool
Early::resolve_glob_import (NodeId use_dec_id, TopLevel::ImportKind &&glob)
{
  auto resolved
    = ctx.resolve_path (glob.to_resolve.get_segments (), Namespace::Types);
  if (!resolved.has_value ())
    return false;

  auto result
    = Analysis::Mappings::get ().lookup_ast_module (resolved->get_node_id ());
  if (!result)
    return false;

  // here, we insert the module's NodeId into the import_mappings and will look
  // up the module proper in `FinalizeImports`
  // The namespace does not matter here since we are dealing with a glob
  // TODO: Ugly
  import_mappings.insert (use_dec_id,
			  ImportPair (std::move (glob),
				      ImportData::Glob (*resolved)));

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
  auto definitions = resolve_path_in_all_ns (rebind_import.to_resolve);

  // if we've found at least one definition, then we're good
  if (definitions.empty ())
    return false;

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

      if (!found)
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
  auto path = invoc.get_invoc_data ().get_path ();

  if (invoc.get_kind () == AST::MacroInvocation::InvocKind::Builtin)
    for (auto &pending_invoc : invoc.get_pending_eager_invocations ())
      pending_invoc->accept_vis (*this);

  // When a macro is invoked by an unqualified identifier (not part of a
  // multi-part path), it is first looked up in textual scoping. If this does
  // not yield any results, then it is looked up in path-based scoping. If the
  // macro's name is qualified with a path, then it is only looked up in
  // path-based scoping.

  // https://doc.rust-lang.org/reference/macros-by-example.html#path-based-scope

  tl::optional<Rib::Definition> definition = tl::nullopt;
  if (path.get_segments ().size () == 1)
    definition
      = textual_scope.get (path.get_final_segment ().as_string ())
	  .map ([] (NodeId id) { return Rib::Definition::NonShadowable (id); });

  // we won't have changed `definition` from `nullopt` if there are more
  // than one segments in our path
  if (!definition.has_value ())
    definition = ctx.resolve_path (path.get_segments (), Namespace::Macros);

  // if the definition still does not have a value, then it's an error
  if (!definition.has_value ())
    {
      collect_error (Error (invoc.get_locus (), ErrorCode::E0433,
			    "could not resolve macro invocation %qs",
			    path.as_string ().c_str ()));
      return;
    }

  insert_once (invoc, definition->get_node_id ());

  // now do we need to keep mappings or something? or insert "uses" into our
  // ForeverStack? can we do that? are mappings simpler?
  auto &mappings = Analysis::Mappings::get ();
  auto rules_def = mappings.lookup_macro_def (definition->get_node_id ());

  // Macro definition not found, maybe it is not expanded yet.
  if (!rules_def)
    return;

  if (mappings.lookup_macro_invocation (invoc))
    return;

  mappings.insert_macro_invocation (invoc, rules_def.value ());
}

void
Early::visit_attributes (std::vector<AST::Attribute> &attrs)
{
  auto &mappings = Analysis::Mappings::get ();

  for (auto &attr : attrs)
    {
      auto name = attr.get_path ().get_segments ().at (0).get_segment_name ();

      if (attr.is_derive ())
	{
	  auto traits = attr.get_traits_to_derive ();
	  for (auto &trait : traits)
	    {
	      auto definition = ctx.resolve_path (trait.get ().get_segments (),
						  Namespace::Macros);
	      if (!definition.has_value ())
		{
		  // FIXME: Change to proper error message
		  collect_error (Error (trait.get ().get_locus (),
					"could not resolve trait %qs",
					trait.get ().as_string ().c_str ()));
		  continue;
		}

	      auto pm_def = mappings.lookup_derive_proc_macro_def (
		definition->get_node_id ());

	      if (pm_def.has_value ())
		mappings.insert_derive_proc_macro_invocation (trait,
							      pm_def.value ());
	    }
	}
      else if (Analysis::BuiltinAttributeMappings::get ()
		 ->lookup_builtin (name)
		 .is_error ()) // Do not resolve builtins
	{
	  auto definition = ctx.resolve_path (attr.get_path ().get_segments (),
					      Namespace::Macros);
	  if (!definition.has_value ())
	    {
	      // FIXME: Change to proper error message
	      collect_error (
		Error (attr.get_locus (),
		       "could not resolve attribute macro invocation"));
	      return;
	    }
	  auto pm_def = mappings.lookup_attribute_proc_macro_def (
	    definition->get_node_id ());

	  rust_assert (pm_def.has_value ());

	  mappings.insert_attribute_proc_macro_invocation (attr.get_path (),
							   pm_def.value ());
	}
    }
}

void
Early::visit (AST::Function &fn)
{
  visit_attributes (fn.get_outer_attrs ());
  DefaultResolver::visit (fn);
}

void
Early::visit (AST::StructStruct &s)
{
  visit_attributes (s.get_outer_attrs ());
  DefaultResolver::visit (s);
}

void
Early::finalize_simple_import (const Early::ImportPair &mapping)
{
  // FIXME: We probably need to store namespace information

  auto locus = mapping.import_kind.to_resolve.get_locus ();
  auto data = mapping.data;
  auto identifier
    = mapping.import_kind.to_resolve.get_final_segment ().get_segment_name ();

  for (auto &&definition : data.definitions ())
    toplevel
      .insert_or_error_out (
	identifier, locus, definition.first.get_node_id (), definition.second /* TODO: This isn't clear - it would be better if it was called .ns or something */);
}

void
Early::finalize_glob_import (NameResolutionContext &ctx,
			     const Early::ImportPair &mapping)
{
  auto module = Analysis::Mappings::get ().lookup_ast_module (
    mapping.data.module ().get_node_id ());
  rust_assert (module);

  GlobbingVisitor glob_visitor (ctx);
  glob_visitor.go (module.value ());
}

void
Early::finalize_rebind_import (const Early::ImportPair &mapping)
{
  // We can fetch the value here as `resolve_rebind` will only be called on
  // imports of the right kind
  auto &path = mapping.import_kind.to_resolve;
  auto &rebind = mapping.import_kind.rebind.value ();
  auto data = mapping.data;

  location_t locus = UNKNOWN_LOCATION;
  std::string declared_name;

  // FIXME: This needs to be done in `FinalizeImports`
  switch (rebind.get_new_bind_type ())
    {
    case AST::UseTreeRebind::NewBindType::IDENTIFIER:
      declared_name = rebind.get_identifier ().as_string ();
      locus = rebind.get_identifier ().get_locus ();
      break;
      case AST::UseTreeRebind::NewBindType::NONE: {
	const auto &segments = path.get_segments ();
	// We don't want to insert `self` with `use module::self`
	if (path.get_final_segment ().is_lower_self_seg ())
	  {
	    rust_assert (segments.size () > 1);
	    declared_name = segments[segments.size () - 2].as_string ();
	  }
	else
	  declared_name = path.get_final_segment ().as_string ();
	locus = path.get_final_segment ().get_locus ();
	break;
      }
    case AST::UseTreeRebind::NewBindType::WILDCARD:
      rust_unreachable ();
      break;
    }

  for (auto &&definition : data.definitions ())
    toplevel.insert_or_error_out (
      declared_name, locus, definition.first.get_node_id (), definition.second /* TODO: This isn't clear - it would be better if it was called .ns or something */);
}

void
Early::visit (AST::UseDeclaration &decl)
{
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

} // namespace Resolver2_0
} // namespace Rust
