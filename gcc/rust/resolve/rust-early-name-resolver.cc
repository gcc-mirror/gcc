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

#include "rust-early-name-resolver.h"
#include "rust-ast-full.h"
#include "rust-name-resolver.h"
#include "rust-macro-builtins.h"
#include "rust-attribute-values.h"

namespace Rust {
namespace Resolver {

// Check if a module contains the `#[macro_use]` attribute
static bool
is_macro_use_module (const AST::Module &mod)
{
  for (const auto &attr : mod.get_outer_attrs ())
    if (attr.get_path ().as_string () == Values::Attributes::MACRO_USE)
      return true;

  return false;
}

std::vector<std::unique_ptr<AST::Item>>
EarlyNameResolver::accumulate_escaped_macros (AST::Module &module)
{
  if (!is_macro_use_module (module))
    return {};

  // Parse the module's items if they haven't been expanded and the file
  // should be parsed (i.e isn't hidden behind an untrue or impossible cfg
  // directive)
  if (module.get_kind () == AST::Module::UNLOADED)
    module.load_items ();

  std::vector<std::unique_ptr<AST::Item>> escaped_macros;

  scoped (module.get_node_id (), [&module, &escaped_macros, this] {
    for (auto &item : module.get_items ())
      {
	if (item->get_ast_kind () == AST::Kind::MODULE)
	  {
	    auto &module = *static_cast<AST::Module *> (item.get ());
	    auto new_macros = accumulate_escaped_macros (module);

	    std::move (new_macros.begin (), new_macros.end (),
		       std::back_inserter (escaped_macros));

	    continue;
	  }

	if (item->get_ast_kind () == AST::Kind::MACRO_RULES_DEFINITION)
	  escaped_macros.emplace_back (item->clone_item ());
      }
  });

  return escaped_macros;
}

EarlyNameResolver::EarlyNameResolver ()
  : current_scope (UNKNOWN_NODEID), resolver (*Resolver::get ()),
    mappings (*Analysis::Mappings::get ())
{}

void
EarlyNameResolver::go (AST::Crate &crate)
{
  visit (crate);
}

void
EarlyNameResolver::resolve_generic_args (AST::GenericArgs &generic_args)
{
  for (auto &arg : generic_args.get_generic_args ())
    arg.accept_vis (*this);

  for (auto &arg : generic_args.get_binding_args ())
    arg.get_type ().accept_vis (*this);
}

void
EarlyNameResolver::resolve_qualified_path_type (AST::QualifiedPathType &path)
{
  path.get_type ().accept_vis (*this);

  if (path.has_as_clause ())
    path.get_as_type_path ().accept_vis (*this);
}

void
EarlyNameResolver::visit (AST::Crate &crate)
{
  std::vector<std::unique_ptr<AST::Item>> new_items;
  auto items = crate.take_items ();

  scoped (crate.get_node_id (), [&items, &new_items, this] {
    for (auto &&item : items)
      {
	auto new_macros = std::vector<std::unique_ptr<AST::Item>> ();

	if (item->get_ast_kind () == AST::Kind::MODULE)
	  new_macros = accumulate_escaped_macros (
	    *static_cast<AST::Module *> (item.get ()));

	new_items.emplace_back (std::move (item));
	std::move (new_macros.begin (), new_macros.end (),
		   std::back_inserter (new_items));
      }
  });

  crate.set_items (std::move (new_items));

  scoped (crate.get_node_id (), [&crate, this] () {
    for (auto &item : crate.items)
      item->accept_vis (*this);
  });
}

void
EarlyNameResolver::visit (AST::DelimTokenTree &)
{}

void
EarlyNameResolver::visit (AST::AttrInputMetaItemContainer &)
{}

void
EarlyNameResolver::visit (AST::IdentifierExpr &)
{}

void
EarlyNameResolver::visit (AST::LifetimeParam &)
{}

void
EarlyNameResolver::visit (AST::ConstGenericParam &)
{}

// FIXME: ARTHUR: Do we need to perform macro resolution for paths as well?
// std::arch::asm!()?
void
EarlyNameResolver::visit (AST::PathInExpression &path)
{
  for (auto &segment : path.get_segments ())
    if (segment.has_generic_args ())
      resolve_generic_args (segment.get_generic_args ());
}

void
EarlyNameResolver::visit (AST::TypePathSegmentGeneric &segment)
{
  if (segment.has_generic_args ())
    resolve_generic_args (segment.get_generic_args ());
}

void
EarlyNameResolver::visit (AST::QualifiedPathInExpression &path)
{
  resolve_qualified_path_type (path.get_qualified_path_type ());

  for (auto &segment : path.get_segments ())
    if (segment.has_generic_args ())
      resolve_generic_args (segment.get_generic_args ());
}

void
EarlyNameResolver::visit (AST::QualifiedPathInType &path)
{
  resolve_qualified_path_type (path.get_qualified_path_type ());

  for (auto &segment : path.get_segments ())
    segment->accept_vis (*this);
}

void
EarlyNameResolver::visit (AST::LiteralExpr &)
{}

void
EarlyNameResolver::visit (AST::AttrInputLiteral &)
{}

void
EarlyNameResolver::visit (AST::AttrInputMacro &)
{}

void
EarlyNameResolver::visit (AST::MetaItemLitExpr &)
{}

void
EarlyNameResolver::visit (AST::MetaItemPathLit &)
{}

void
EarlyNameResolver::visit (AST::StructExprStruct &)
{}

void
EarlyNameResolver::visit (AST::StructExprFieldIdentifier &)
{}

void
EarlyNameResolver::visit (AST::StructExprStructBase &)
{}

void
EarlyNameResolver::visit (AST::BlockExpr &expr)
{
  scoped (expr.get_node_id (), [&expr, this] () {
    for (auto &stmt : expr.get_statements ())
      stmt->accept_vis (*this);

    if (expr.has_tail_expr ())
      expr.get_tail_expr ().accept_vis (*this);
  });
}

void
EarlyNameResolver::visit (AST::ContinueExpr &)
{}

void
EarlyNameResolver::visit (AST::RangeFullExpr &)
{}

void
EarlyNameResolver::visit (AST::ForLoopExpr &expr)
{
  scoped (expr.get_node_id (), [&expr, this] () {
    expr.get_pattern ().accept_vis (*this);
    expr.get_iterator_expr ().accept_vis (*this);
    expr.get_loop_block ().accept_vis (*this);
  });
}

void
EarlyNameResolver::visit (AST::IfLetExpr &expr)
{
  expr.get_value_expr ().accept_vis (*this);

  scoped (expr.get_node_id (),
	  [&expr, this] () { expr.get_if_block ().accept_vis (*this); });
}

void
EarlyNameResolver::visit (AST::MatchExpr &expr)
{
  expr.get_scrutinee_expr ().accept_vis (*this);

  scoped (expr.get_node_id (), [&expr, this] () {
    for (auto &arm : expr.get_match_cases ())
      {
	scoped (arm.get_node_id (), [&arm, this] () {
	  if (arm.get_arm ().has_match_arm_guard ())
	    arm.get_arm ().get_guard_expr ().accept_vis (*this);

	  for (auto &pattern : arm.get_arm ().get_patterns ())
	    pattern->accept_vis (*this);

	  arm.get_expr ().accept_vis (*this);
	});
      }
  });
}

void
EarlyNameResolver::visit (AST::LifetimeWhereClauseItem &)
{}

void
EarlyNameResolver::visit (AST::Module &module)
{
  if (module.get_kind () == AST::Module::UNLOADED)
    module.load_items ();

  // so we need to only go "one scope down" for fetching macros. Macros within
  // functions are still scoped only within that function. But we have to be
  // careful because nested modules with #[macro_use] actually works!
  std::vector<std::unique_ptr<AST::Item>> new_items;
  auto items = module.take_items ();

  scoped (module.get_node_id (), [&items, &new_items, this] {
    for (auto &&item : items)
      {
	auto new_macros = std::vector<std::unique_ptr<AST::Item>> ();

	if (item->get_ast_kind () == AST::Kind::MODULE)
	  new_macros = accumulate_escaped_macros (
	    *static_cast<AST::Module *> (item.get ()));

	new_items.emplace_back (std::move (item));
	std::move (new_macros.begin (), new_macros.end (),
		   std::back_inserter (new_items));
      }
  });

  module.set_items (std::move (new_items));

  scoped (module.get_node_id (), [&module, this] () {
    for (auto &item : module.get_items ())
      item->accept_vis (*this);
  });
}

void
EarlyNameResolver::visit (AST::ExternCrate &)
{}

void
EarlyNameResolver::visit (AST::UseTreeGlob &)
{}

void
EarlyNameResolver::visit (AST::UseTreeList &)
{}

void
EarlyNameResolver::visit (AST::UseTreeRebind &)
{}

void
EarlyNameResolver::visit (AST::UseDeclaration &)
{}

void
EarlyNameResolver::visit (AST::EnumItem &)
{}

void
EarlyNameResolver::visit (AST::Union &)
{}

void
EarlyNameResolver::visit (AST::TraitItemType &)
{}

void
EarlyNameResolver::visit (AST::Trait &trait)
{
  for (auto &generic : trait.get_generic_params ())
    generic->accept_vis (*this);

  scoped (trait.get_node_id (), [&trait, this] () {
    for (auto &item : trait.get_trait_items ())
      item->accept_vis (*this);
  });
}

void
EarlyNameResolver::visit (AST::InherentImpl &impl)
{
  impl.get_type ().accept_vis (*this);

  for (auto &generic : impl.get_generic_params ())
    generic->accept_vis (*this);

  scoped (impl.get_node_id (), [&impl, this] () {
    for (auto &item : impl.get_impl_items ())
      item->accept_vis (*this);
  });
}

void
EarlyNameResolver::visit (AST::TraitImpl &impl)
{
  impl.get_type ().accept_vis (*this);

  for (auto &generic : impl.get_generic_params ())
    generic->accept_vis (*this);

  scoped (impl.get_node_id (), [&impl, this] () {
    for (auto &item : impl.get_impl_items ())
      item->accept_vis (*this);
  });
}

void
EarlyNameResolver::visit (AST::ExternalTypeItem &item)
{
  // nothing to do?
}

void
EarlyNameResolver::visit (AST::ExternBlock &block)
{
  scoped (block.get_node_id (), [&block, this] () {
    for (auto &item : block.get_extern_items ())
      item->accept_vis (*this);
  });
}

void
EarlyNameResolver::visit (AST::MacroMatchRepetition &)
{}

void
EarlyNameResolver::visit (AST::MacroMatcher &)
{}

void
EarlyNameResolver::visit (AST::MacroRulesDefinition &rules_def)
{
  auto path = CanonicalPath::new_seg (rules_def.get_node_id (),
				      rules_def.get_rule_name ().as_string ());
  resolver.get_macro_scope ().insert (path, rules_def.get_node_id (),
				      rules_def.get_locus ());

  /* Since the EarlyNameResolver runs multiple time (fixed point algorithm)
   * we could be inserting the same macro def over and over again until we
   * implement some optimizations */
  // FIXME: ARTHUR: Remove that lookup and add proper optimizations instead
  AST::MacroRulesDefinition *tmp = nullptr;
  if (mappings.lookup_macro_def (rules_def.get_node_id (), &tmp))
    return;

  mappings.insert_macro_def (&rules_def);
  rust_debug_loc (rules_def.get_locus (), "inserting macro def: [%s]",
		  path.get ().c_str ());
}

void
EarlyNameResolver::visit (AST::MacroInvocation &invoc)
{
  auto &invoc_data = invoc.get_invoc_data ();
  auto has_semicolon = invoc.has_semicolon ();

  if (invoc.get_kind () == AST::MacroInvocation::InvocKind::Builtin)
    for (auto &pending_invoc : invoc.get_pending_eager_invocations ())
      pending_invoc->accept_vis (*this);

  // ??
  // switch on type of macro:
  //  - '!' syntax macro (inner switch)
  //      - procedural macro - "A token-based function-like macro"
  //      - 'macro_rules' (by example/pattern-match) macro? or not? "an
  // AST-based function-like macro"
  //      - else is unreachable
  //  - attribute syntax macro (inner switch)
  //  - procedural macro attribute syntax - "A token-based attribute
  // macro"
  //      - legacy macro attribute syntax? - "an AST-based attribute macro"
  //      - non-macro attribute: mark known
  //      - else is unreachable
  //  - derive macro (inner switch)
  //      - derive or legacy derive - "token-based" vs "AST-based"
  //      - else is unreachable
  //  - derive container macro - unreachable

  // lookup the rules for this macro
  NodeId resolved_node = UNKNOWN_NODEID;
  NodeId source_node = UNKNOWN_NODEID;
  if (has_semicolon)
    source_node = invoc.get_macro_node_id ();
  else
    source_node = invoc.get_node_id ();
  auto seg
    = CanonicalPath::new_seg (source_node, invoc_data.get_path ().as_string ());

  bool found = resolver.get_macro_scope ().lookup (seg, &resolved_node);
  if (!found)
    {
      rust_error_at (invoc.get_locus (), "unknown macro: [%s]",
		     seg.get ().c_str ());
      return;
    }

  // lookup the rules
  AST::MacroRulesDefinition *rules_def = nullptr;
  bool ok = mappings.lookup_macro_def (resolved_node, &rules_def);
  rust_assert (ok);

  auto &outer_attrs = rules_def->get_outer_attrs ();
  bool is_builtin
    = std::any_of (outer_attrs.begin (), outer_attrs.end (),
		   [] (AST::Attribute attr) {
		     return attr.get_path ()
			    == Values::Attributes::RUSTC_BUILTIN_MACRO;
		   });

  if (is_builtin)
    {
      auto builtin_kind
	= builtin_macro_from_string (rules_def->get_rule_name ().as_string ());
      invoc.map_to_builtin (builtin_kind.value ());
    }

  auto attributes = rules_def->get_outer_attrs ();

  /* Since the EarlyNameResolver runs multiple time (fixed point algorithm)
   * we could be inserting the same macro def over and over again until we
   * implement some optimizations */
  // FIXME: ARTHUR: Remove that lookup and add proper optimizations instead
  AST::MacroRulesDefinition *tmp_def = nullptr;
  if (mappings.lookup_macro_invocation (invoc, &tmp_def))
    return;

  mappings.insert_macro_invocation (invoc, rules_def);
}

// FIXME: ARTHUR: Do we need to resolve these as well here?

void
EarlyNameResolver::visit (AST::MetaItemPath &)
{}

void
EarlyNameResolver::visit (AST::MetaItemSeq &)
{}

void
EarlyNameResolver::visit (AST::MetaNameValueStr &)
{}

void
EarlyNameResolver::visit (AST::MetaListPaths &)
{}

void
EarlyNameResolver::visit (AST::MetaListNameValueStr &)
{}

void
EarlyNameResolver::visit (AST::RangePatternBoundLiteral &)
{}

void
EarlyNameResolver::visit (AST::RangePatternBoundPath &)
{}

void
EarlyNameResolver::visit (AST::RangePatternBoundQualPath &)
{}

void
EarlyNameResolver::visit (AST::StructPatternFieldIdent &)
{}

void
EarlyNameResolver::visit (AST::StructPattern &)
{}

void
EarlyNameResolver::visit (AST::TupleStructPattern &pattern)
{
  pattern.get_items ().accept_vis (*this);
}

void
EarlyNameResolver::visit (AST::TraitBound &)
{}

void
EarlyNameResolver::visit (AST::ImplTraitType &)
{}

void
EarlyNameResolver::visit (AST::TraitObjectType &)
{}

void
EarlyNameResolver::visit (AST::ParenthesisedType &)
{}

void
EarlyNameResolver::visit (AST::ImplTraitTypeOneBound &)
{}

void
EarlyNameResolver::visit (AST::TraitObjectTypeOneBound &)
{}

void
EarlyNameResolver::visit (AST::TupleType &)
{}

void
EarlyNameResolver::visit (AST::RawPointerType &)
{}

void
EarlyNameResolver::visit (AST::ReferenceType &)
{}

void
EarlyNameResolver::visit (AST::ArrayType &)
{}

void
EarlyNameResolver::visit (AST::SliceType &)
{}

void
EarlyNameResolver::visit (AST::InferredType &)
{}

} // namespace Resolver
} // namespace Rust
