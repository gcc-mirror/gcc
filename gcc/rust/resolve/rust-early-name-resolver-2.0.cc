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

#include "rust-early-name-resolver-2.0.h"
#include "rust-ast-full.h"
#include "rust-toplevel-name-resolver-2.0.h"
#include "rust-attributes.h"

namespace Rust {
namespace Resolver2_0 {

Early::Early (NameResolutionContext &ctx) : DefaultResolver (ctx) {}

void
Early::insert_once (AST::MacroInvocation &invocation, NodeId resolved)
{
  // TODO: Should we use `ctx.mark_resolved()`?
  AST::MacroRulesDefinition *definition;
  auto ok = ctx.mappings.lookup_macro_def (resolved, &definition);

  rust_assert (ok);

  AST::MacroRulesDefinition *existing;
  auto exists = ctx.mappings.lookup_macro_invocation (invocation, &existing);

  if (!exists)
    ctx.mappings.insert_macro_invocation (invocation, definition);
}

void
Early::insert_once (AST::MacroRulesDefinition &def)
{
  // TODO: Should we use `ctx.mark_resolved()`?
  AST::MacroRulesDefinition *definition;
  auto exists = ctx.mappings.lookup_macro_def (def.get_node_id (), &definition);

  if (!exists)
    ctx.mappings.insert_macro_def (&def);
}

void
Early::go (AST::Crate &crate)
{
  // First we go through TopLevel resolution to get all our declared items
  auto toplevel = TopLevel (ctx);
  toplevel.go (crate);

  textual_scope.push ();

  // Then we proceed to the proper "early" name resolution: Import and macro
  // name resolution
  for (auto &item : crate.items)
    item->accept_vis (*this);

  textual_scope.pop ();
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

  tl::optional<NodeId> definition = tl::nullopt;
  if (path.get_segments ().size () == 1)
    definition = textual_scope.get (path.get_final_segment ().as_string ());

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

  insert_once (invoc, *definition);

  // now do we need to keep mappings or something? or insert "uses" into our
  // ForeverStack? can we do that? are mappings simpler?
  auto mappings = Analysis::Mappings::get ();
  AST::MacroRulesDefinition *rules_def = nullptr;
  if (!mappings->lookup_macro_def (definition.value (), &rules_def))
    {
      // Macro definition not found, maybe it is not expanded yet.
      return;
    }

  AST::MacroRulesDefinition *tmp_def = nullptr;
  if (mappings->lookup_macro_invocation (invoc, &tmp_def))
    return;

  mappings->insert_macro_invocation (invoc, rules_def);
}

void
Early::visit_attributes (std::vector<AST::Attribute> &attrs)
{
  auto mappings = Analysis::Mappings::get ();

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

	      auto pm_def
		= mappings->lookup_derive_proc_macro_def (definition.value ());

	      rust_assert (pm_def.has_value ());

	      mappings->insert_derive_proc_macro_invocation (trait,
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
	  auto pm_def
	    = mappings->lookup_attribute_proc_macro_def (definition.value ());

	  rust_assert (pm_def.has_value ());

	  mappings->insert_attribute_proc_macro_invocation (attr.get_path (),
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
