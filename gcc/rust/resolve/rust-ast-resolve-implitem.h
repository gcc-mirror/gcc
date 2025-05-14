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

#ifndef RUST_AST_RESOLVE_IMPLITEM_H
#define RUST_AST_RESOLVE_IMPLITEM_H

#include "rust-ast-resolve-base.h"
#include "rust-ast-resolve-type.h"
#include "rust-ast-full.h"

namespace Rust {
namespace Resolver {

class ResolveToplevelImplItem : public ResolverBase
{
  using Rust::Resolver::ResolverBase::visit;

public:
  static void go (AST::AssociatedItem &item, const CanonicalPath &prefix)
  {
    if (item.is_marked_for_strip ())
      return;

    ResolveToplevelImplItem resolver (prefix);
    item.accept_vis (resolver);
  }

  void visit (AST::TypeAlias &type) override
  {
    auto decl = CanonicalPath::new_seg (type.get_node_id (),
					type.get_new_type_name ().as_string ());
    auto path = prefix.append (decl);

    resolver->get_type_scope ().insert (
      path, type.get_node_id (), type.get_locus (), false, Rib::ItemType::Type,
      [&] (const CanonicalPath &, NodeId, location_t locus) -> void {
	rich_location r (line_table, type.get_locus ());
	r.add_range (locus);
	redefined_error (r);
      });
  }

  void visit (AST::ConstantItem &constant) override
  {
    auto decl = CanonicalPath::new_seg (constant.get_node_id (),
					constant.get_identifier ());
    auto path = prefix.append (decl);

    resolver->get_name_scope ().insert (
      path, constant.get_node_id (), constant.get_locus (), false,
      Rib::ItemType::Const,
      [&] (const CanonicalPath &, NodeId, location_t locus) -> void {
	rich_location r (line_table, constant.get_locus ());
	r.add_range (locus);
	redefined_error (r);
      });
  }

  void visit (AST::Function &function) override
  {
    auto decl
      = CanonicalPath::new_seg (function.get_node_id (),
				function.get_function_name ().as_string ());
    auto path = prefix.append (decl);

    resolver->get_name_scope ().insert (
      path, function.get_node_id (), function.get_locus (), false,
      Rib::ItemType::Function,
      [&] (const CanonicalPath &, NodeId, location_t locus) -> void {
	rich_location r (line_table, function.get_locus ());
	r.add_range (locus);
	redefined_error (r);
      });
  }

private:
  ResolveToplevelImplItem (const CanonicalPath &prefix)
    : ResolverBase (), prefix (prefix)
  {
    rust_assert (!prefix.is_empty ());
  }

  const CanonicalPath &prefix;
};

class ResolveTopLevelTraitItems : public ResolverBase
{
  using Rust::Resolver::ResolverBase::visit;

public:
  static void go (AST::AssociatedItem *item, const CanonicalPath &prefix,
		  const CanonicalPath &canonical_prefix)
  {
    ResolveTopLevelTraitItems resolver (prefix, canonical_prefix);
    item->accept_vis (resolver);
  };

  void visit (AST::Function &function) override
  {
    auto decl
      = CanonicalPath::new_seg (function.get_node_id (),
				function.get_function_name ().as_string ());
    auto path = prefix.append (decl);
    auto cpath = canonical_prefix.append (decl);

    resolver->get_name_scope ().insert (
      path, function.get_node_id (), function.get_locus (), false,
      Rib::ItemType::Function,
      [&] (const CanonicalPath &, NodeId, location_t locus) -> void {
	rich_location r (line_table, function.get_locus ());
	r.add_range (locus);
	redefined_error (r);
      });

    mappings.insert_canonical_path (function.get_node_id (), cpath);
  }

  void visit (AST::TraitItemConst &constant) override
  {
    auto decl
      = CanonicalPath::new_seg (constant.get_node_id (),
				constant.get_identifier ().as_string ());
    auto path = prefix.append (decl);
    auto cpath = canonical_prefix.append (decl);

    resolver->get_name_scope ().insert (
      path, constant.get_node_id (), constant.get_locus (), false,
      Rib::ItemType::Const,
      [&] (const CanonicalPath &, NodeId, location_t locus) -> void {
	rich_location r (line_table, constant.get_locus ());
	r.add_range (locus);
	redefined_error (r);
      });

    mappings.insert_canonical_path (constant.get_node_id (), cpath);
  }

  void visit (AST::TraitItemType &type) override
  {
    auto decl = CanonicalPath::new_seg (type.get_node_id (),
					type.get_identifier ().as_string ());
    auto path = prefix.append (decl);
    auto cpath = canonical_prefix.append (decl);

    resolver->get_type_scope ().insert (
      path, type.get_node_id (), type.get_locus (), false, Rib::ItemType::Type,
      [&] (const CanonicalPath &, NodeId, location_t locus) -> void {
	rich_location r (line_table, type.get_locus ());
	r.add_range (locus);
	redefined_error (r);
      });

    mappings.insert_canonical_path (type.get_node_id (), cpath);
  }

private:
  ResolveTopLevelTraitItems (const CanonicalPath &prefix,
			     const CanonicalPath &canonical_prefix)
    : ResolverBase (), prefix (prefix), canonical_prefix (canonical_prefix)
  {}

  const CanonicalPath &prefix;
  const CanonicalPath &canonical_prefix;
};

class ResolveToplevelExternItem : public ResolverBase
{
  using Rust::Resolver::ResolverBase::visit;

public:
  static void go (AST::ExternalItem &item, const CanonicalPath &prefix)
  {
    ResolveToplevelExternItem resolver (prefix);
    item.accept_vis (resolver);
  };

  void visit (AST::Function &function) override
  {
    auto decl
      = CanonicalPath::new_seg (function.get_node_id (),
				function.get_function_name ().as_string ());
    auto path = prefix.append (decl);

    resolver->get_name_scope ().insert (
      path, function.get_node_id (), function.get_locus (), false,
      Rib::ItemType::Function,
      [&] (const CanonicalPath &, NodeId, location_t locus) -> void {
	rich_location r (line_table, function.get_locus ());
	r.add_range (locus);
	redefined_error (r);
      });

    NodeId current_module = resolver->peek_current_module_scope ();
    mappings.insert_module_child_item (current_module, decl);
  }

  void visit (AST::ExternalStaticItem &item) override
  {
    auto decl = CanonicalPath::new_seg (item.get_node_id (),
					item.get_identifier ().as_string ());
    auto path = prefix.append (decl);

    resolver->get_name_scope ().insert (
      path, item.get_node_id (), item.get_locus (), false,
      Rib::ItemType::Static,
      [&] (const CanonicalPath &, NodeId, location_t locus) -> void {
	rich_location r (line_table, item.get_locus ());
	r.add_range (locus);
	redefined_error (r);
      });

    NodeId current_module = resolver->peek_current_module_scope ();
    mappings.insert_module_child_item (current_module, decl);
  }

  void visit (AST::ExternalTypeItem &type) override
  {
    auto decl = CanonicalPath::new_seg (type.get_node_id (),
					type.get_identifier ().as_string ());
    auto path = prefix.append (decl);

    resolver->get_type_scope ().insert (
      path, type.get_node_id (), type.get_locus (), false, Rib::ItemType::Type,
      [&] (const CanonicalPath &, NodeId, location_t locus) -> void {
	rich_location r (line_table, type.get_locus ());
	r.add_range (locus);
	redefined_error (r);
      });

    NodeId current_module = resolver->peek_current_module_scope ();
    mappings.insert_module_child_item (current_module, decl);
  }

private:
  ResolveToplevelExternItem (const CanonicalPath &prefix)
    : ResolverBase (), prefix (prefix)
  {}

  const CanonicalPath &prefix;
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_AST_RESOLVE_IMPLITEM_H
