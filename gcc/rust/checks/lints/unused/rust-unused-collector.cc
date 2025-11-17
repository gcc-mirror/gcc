// Copyright (C) 2025 Free Software Foundation, Inc.

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

#include "rust-unused-collector.h"
#include "rust-hir-expr.h"
#include "rust-hir-full-decls.h"
#include "rust-hir-item.h"
#include "rust-hir-path.h"
#include "rust-hir-pattern.h"
#include "rust-immutable-name-resolution-context.h"

namespace Rust {
namespace Analysis {
UnusedCollector::UnusedCollector (UnusedContext &context)
  : nr_context (
    Resolver2_0::ImmutableNameResolutionContext::get ().resolver ()),
    mappings (Analysis::Mappings::get ()), unused_context (context)
{}
void
UnusedCollector::go (HIR::Crate &crate)
{
  for (auto &item : crate.get_items ())
    item->accept_vis (*this);
}

void
UnusedCollector::visit (HIR::PathInExpression &expr)
{
  mark_path_used (expr);
  walk (expr);
}

void
UnusedCollector::visit (HIR::QualifiedPathInExpression &expr)
{
  mark_path_used (expr);
  walk (expr);
}

void
UnusedCollector::visit (HIR::StructExprFieldIdentifier &ident)
{
  mark_path_used (ident);
  walk (ident);
}

void
UnusedCollector::visit (HIR::AssignmentExpr &expr)
{
  auto def_id = get_def_id (expr.get_lhs ());
  HirId id = expr.get_lhs ().get_mappings ().get_hirid ();
  unused_context.remove_mut (def_id);
  unused_context.add_assign (def_id, id);
  visit_outer_attrs (expr);
  expr.get_rhs ().accept_vis (*this);
}

void
UnusedCollector::visit (HIR::IdentifierPattern &pattern)
{
  if (pattern.is_mut ())
    unused_context.add_mut (pattern.get_mappings ().get_hirid ());

  walk (pattern);
}

void
UnusedCollector::visit (HIR::StructPatternFieldIdent &pattern)
{
  if (pattern.is_mut ())
    unused_context.add_mut (pattern.get_mappings ().get_hirid ());

  walk (pattern);
}

} // namespace Analysis
} // namespace Rust
