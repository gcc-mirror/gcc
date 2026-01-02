// Copyright (C) 2025-2026 Free Software Foundation, Inc.

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

#include "rust-unused-checker.h"
#include "rust-hir-expr.h"
#include "rust-hir-item.h"

#include "options.h"
#include "rust-keyword-values.h"

namespace Rust {
namespace Analysis {
UnusedChecker::UnusedChecker ()
  : nr_context (
    Resolver2_0::ImmutableNameResolutionContext::get ().resolver ()),
    mappings (Analysis::Mappings::get ()), unused_context (UnusedContext ())
{}
void
UnusedChecker::go (HIR::Crate &crate)
{
  UnusedCollector collector (unused_context);
  collector.go (crate);
  for (auto &item : crate.get_items ())
    item->accept_vis (*this);
}

void
UnusedChecker::visit (HIR::ConstantItem &item)
{
  std::string var_name = item.get_identifier ().as_string ();
  auto id = item.get_mappings ().get_hirid ();
  if (!unused_context.is_variable_used (id) && var_name[0] != '_')
    rust_warning_at (item.get_locus (), OPT_Wunused_variable,
		     "unused variable %qs",
		     item.get_identifier ().as_string ().c_str ());
}

void
UnusedChecker::visit (HIR::StaticItem &item)
{
  std::string var_name = item.get_identifier ().as_string ();
  auto id = item.get_mappings ().get_hirid ();
  if (!unused_context.is_variable_used (id) && var_name[0] != '_')
    rust_warning_at (item.get_locus (), OPT_Wunused_variable,
		     "unused variable %qs",
		     item.get_identifier ().as_string ().c_str ());
}

void
UnusedChecker::visit (HIR::TraitItemFunc &item)
{
  // TODO: check trait item functions if they are not derived.
}
void
UnusedChecker::visit (HIR::IdentifierPattern &pattern)
{
  std::string var_name = pattern.get_identifier ().as_string ();
  auto id = pattern.get_mappings ().get_hirid ();
  if (!unused_context.is_variable_used (id)
      && var_name != Values::Keywords::SELF && var_name[0] != '_')
    rust_warning_at (pattern.get_locus (), OPT_Wunused_variable,
		     "unused variable %qs",
		     pattern.get_identifier ().as_string ().c_str ());

  if (pattern.is_mut () && !unused_context.is_mut_used (id)
      && var_name != Values::Keywords::SELF && var_name[0] != '_')
    rust_warning_at (pattern.get_locus (), OPT_Wunused_variable,
		     "unused mut %qs",
		     pattern.get_identifier ().as_string ().c_str ());
}
void

UnusedChecker::visit (HIR::AssignmentExpr &expr)

{
  const auto &lhs = expr.get_lhs ();
  auto var_name = lhs.to_string ();
  NodeId ast_node_id = lhs.get_mappings ().get_nodeid ();
  NodeId def_id = nr_context.lookup (ast_node_id).value ();
  HirId id = mappings.lookup_node_to_hir (def_id).value ();
  if (unused_context.is_variable_assigned (id, lhs.get_mappings ().get_hirid ())
      && var_name[0] != '_')
    rust_warning_at (lhs.get_locus (), OPT_Wunused_variable,
		     "unused assignment %qs", var_name.c_str ());
}

void
UnusedChecker::visit (HIR::StructPatternFieldIdent &pattern)
{
  std::string var_name = pattern.get_identifier ().as_string ();
  auto id = pattern.get_mappings ().get_hirid ();
  if (!unused_context.is_variable_used (id)
      && var_name != Values::Keywords::SELF && var_name[0] != '_')
    rust_warning_at (pattern.get_locus (), OPT_Wunused_variable,
		     "unused variable %qs",
		     pattern.get_identifier ().as_string ().c_str ());

  if (pattern.is_mut () && !unused_context.is_mut_used (id)
      && var_name != Values::Keywords::SELF && var_name[0] != '_')
    rust_warning_at (pattern.get_locus (), OPT_Wunused_variable,
		     "unused mut %qs",
		     pattern.get_identifier ().as_string ().c_str ());
}

} // namespace Analysis
} // namespace Rust
