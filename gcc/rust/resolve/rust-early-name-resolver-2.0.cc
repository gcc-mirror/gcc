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

namespace Rust {
namespace Resolver2_0 {

Early::Early (NameResolutionContext &ctx) : DefaultResolver (ctx) {}

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
  auto toplevel = TopLevel (ctx);
  toplevel.go (crate);

  // We start with resolving the list of imports that `TopLevel` has built for
  // us
  for (auto &&import : toplevel.get_imports_to_resolve ())
    build_import_mapping (std::move (import));

  // Once this is done, we finalize their resolution
  FinalizeImports::go (import_mappings, toplevel, ctx);

  // We now proceed with resolving macros, which can be nested in almost any
  // items
  textual_scope.push ();
  for (auto &item : crate.items)
    item->accept_vis (*this);
  textual_scope.pop ();
}

bool
Early::resolve_glob_import (TopLevel::ImportKind &&glob)
{
  auto resolved = ctx.types.resolve_path (glob.to_resolve.get_segments ());
  if (!resolved.has_value ())
    return false;

  auto result
    = Analysis::Mappings::get ().lookup_ast_module (resolved->get_node_id ());
  if (!result)
    return false;

  // here, we insert the module's NodeId into the import_mappings and will look
  // up the module proper in `FinalizeImports`
  import_mappings.insert ({std::move (glob), resolved->get_node_id ()});

  return true;
}

bool
Early::resolve_simple_import (TopLevel::ImportKind &&import)
{
  // TODO: Fix documentation - the function has changed slightly

  const auto &path = import.to_resolve;
  // auto locus = path.get_final_segment ().get_locus ();
  // auto declared_name = path.get_final_segment ().as_string ();

  // In that function, we only need to declare a new definition - the use path.
  // the resolution needs to happpen in the EarlyNameResolver. So the
  // definitions we'll add will be the path's NodeId - that makes sense, as we
  // need one definition per path declared in a Use tree. so all good.
  // alright, now in what namespace do we declare them? all of them? do we only
  // declare them in the EarlyNameResolver? this is dodgy

  // in what namespace do we perform path resolution? All of them? see which one
  // matches? Error out on ambiguities?
  // so, apparently, for each one that matches, add it to the proper namespace
  // :(

  return ctx.values.resolve_path (path.get_segments ())
    .or_else ([&] () { return ctx.types.resolve_path (path.get_segments ()); })
    .or_else ([&] () { return ctx.macros.resolve_path (path.get_segments ()); })
    .map ([&] (Rib::Definition def) {
      import_mappings.insert ({std::move (import), def.get_node_id ()});
    })
    .has_value ();

  //    switch (ns)
  //      {
  //      case Namespace::Values:
  // resolved = ctx.values.resolve_path (path.get_segments ());
  // break;
  //      case Namespace::Types:
  // resolved = ctx.types.resolve_path (path.get_segments ());
  // break;
  //      case Namespace::Macros:
  // resolved = ctx.macros.resolve_path (path.get_segments ());
  // break;
  //      case Namespace::Labels:
  // // TODO: Is that okay?
  // rust_unreachable ();
  //      }

  // FIXME: Ugly
  //   (void) resolved.map ([this, &found, path, import] (Rib::Definition
  //   def) {
  //     found = true;

  //     import_mappings.insert ({std::move (import), def.get_node_id
  //     ()});

  //     // what do we do with the id?
  //     // insert_or_error_out (declared_name, locus, def.get_node_id (),
  //     // ns); auto result = node_forwarding.find (def.get_node_id ());
  //     if
  //     // (result != node_forwarding.cend ()
  //     //     && result->second != path.get_node_id ())
  //     //   rust_error_at (path.get_locus (), "%qs defined multiple
  //     times",
  //     //    declared_name.c_str ());
  //     // else // No previous thing has inserted this into our scope
  //     //   node_forwarding.insert ({def.get_node_id (),
  //     path.get_node_id
  //     //   ()});

  //     return def.get_node_id ();
  //   });
  // };

  // resolve_and_insert (path);

  // return found;
}

bool
Early::resolve_rebind_import (TopLevel::ImportKind &&rebind_import)
{
  auto &path = rebind_import.to_resolve;

  return ctx.values.resolve_path (path.get_segments ())
    .or_else ([&] () { return ctx.types.resolve_path (path.get_segments ()); })
    .or_else ([&] () { return ctx.macros.resolve_path (path.get_segments ()); })
    .map ([&] (Rib::Definition def) {
      import_mappings.insert ({std::move (rebind_import), def.get_node_id ()});
    })
    .has_value ();
}

void
Early::build_import_mapping (TopLevel::ImportKind &&import)
{
  auto found = false;

  // We create a copy of the path in case of errors, since the `import` will be
  // moved into the newly created import mappings
  auto path = import.to_resolve;

  switch (import.kind)
    {
    case TopLevel::ImportKind::Kind::Glob:
      found = resolve_glob_import (std::move (import));
      break;
    case TopLevel::ImportKind::Kind::Simple:
      found = resolve_simple_import (std::move (import));
      break;
    case TopLevel::ImportKind::Kind::Rebind:
      found = resolve_rebind_import (std::move (import));
      break;
    }

  if (!found)
    rust_error_at (path.get_final_segment ().get_locus (), ErrorCode::E0433,
		   "unresolved import %qs", path.as_string ().c_str ());
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
  textual_scope.push ();

  DefaultResolver::visit (module);

  textual_scope.pop ();
}

void
Early::visit (AST::MacroInvocation &invoc)
{
  auto path = invoc.get_invoc_data ().get_path ();

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
    definition = ctx.macros.resolve_path (path.get_segments ());

  // if the definition still does not have a value, then it's an error
  if (!definition.has_value ())
    {
      collect_error (Error (invoc.get_locus (), ErrorCode::E0433,
			    "could not resolve macro invocation"));
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
	      auto definition
		= ctx.macros.resolve_path (trait.get ().get_segments ());
	      if (!definition.has_value ())
		{
		  // FIXME: Change to proper error message
		  rust_error_at (trait.get ().get_locus (),
				 "could not resolve trait");
		  continue;
		}

	      auto pm_def = mappings.lookup_derive_proc_macro_def (
		definition->get_node_id ());

	      rust_assert (pm_def.has_value ());

	      mappings.insert_derive_proc_macro_invocation (trait,
							    pm_def.value ());
	    }
	}
      else if (Analysis::BuiltinAttributeMappings::get ()
		 ->lookup_builtin (name)
		 .is_error ()) // Do not resolve builtins
	{
	  auto definition
	    = ctx.macros.resolve_path (attr.get_path ().get_segments ());
	  if (!definition.has_value ())
	    {
	      // FIXME: Change to proper error message
	      rust_error_at (attr.get_locus (),
			     "could not resolve attribute macro invocation");
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

} // namespace Resolver2_0
} // namespace Rust
