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

#include "rust-ast-resolve-toplevel.h"
#include "rust-ast-resolve-item.h"
#include "rust-ast-resolve-stmt.h"
#include "rust-ast-resolve-implitem.h"

namespace Rust {
namespace Resolver {

void
ResolveStmt::visit (AST::ExternBlock &extern_block)
{
  resolve_visibility (extern_block.get_visibility ());
  for (auto &item : extern_block.get_extern_items ())
    {
      ResolveToplevelExternItem::go (*item, CanonicalPath::create_empty ());
      ResolveExternItem::go (*item, prefix, canonical_prefix);
    }
}

void
ResolveStmt::visit (AST::Trait &trait)
{
  ResolveTopLevel::go (trait, prefix, canonical_prefix);
  ResolveItem::go (trait, prefix, canonical_prefix);
}

void
ResolveStmt::visit (AST::InherentImpl &impl_block)
{
  ResolveTopLevel::go (impl_block, prefix, canonical_prefix);
  ResolveItem::go (impl_block, prefix, canonical_prefix);
}

void
ResolveStmt::visit (AST::TraitImpl &impl_block)
{
  ResolveTopLevel::go (impl_block, prefix, canonical_prefix);
  ResolveItem::go (impl_block, prefix, canonical_prefix);
}

void
ResolveStmt::visit (AST::StaticItem &var)
{
  auto decl = CanonicalPath::new_seg (var.get_node_id (),
				      var.get_identifier ().as_string ());
  auto path = decl;
  auto cpath = canonical_prefix.append (decl);
  mappings.insert_canonical_path (var.get_node_id (), cpath);

  resolver->get_name_scope ().insert (
    path, var.get_node_id (), var.get_locus (), false, Rib::ItemType::Static,
    [&] (const CanonicalPath &, NodeId, location_t locus) -> void {
      rich_location r (line_table, var.get_locus ());
      r.add_range (locus);
      redefined_error (r);
    });

  ResolveType::go (var.get_type ());
  ResolveExpr::go (var.get_expr (), path, cpath);
}

} // namespace Resolver
} // namespace Rust
