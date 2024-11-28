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

#ifndef RUST_AST_RESOLVE_TOPLEVEL_H
#define RUST_AST_RESOLVE_TOPLEVEL_H

#include "rust-ast-resolve-base.h"
#include "rust-ast-resolve-implitem.h"
#include "rust-name-resolver.h"

namespace Rust {
namespace Resolver {

class ResolveTopLevel : public ResolverBase
{
  using Rust::Resolver::ResolverBase::visit;

public:
  static void go (AST::Item &item, const CanonicalPath &prefix,
		  const CanonicalPath &canonical_prefix)
  {
    if (item.is_marked_for_strip ())
      return;

    ResolveTopLevel resolver (prefix, canonical_prefix);
    item.accept_vis (resolver);

    NodeId current_module = resolver.resolver->peek_current_module_scope ();
    resolver.mappings->insert_child_item_to_parent_module_mapping (
      item.get_node_id (), current_module);
  }

  void visit (AST::Module &module) override
  {
    auto mod = CanonicalPath::new_seg (module.get_node_id (),
				       module.get_name ().as_string ());
    auto path = prefix.append (mod);
    auto cpath = canonical_prefix.append (mod);

    resolver->get_name_scope ().insert (
      path, module.get_node_id (), module.get_locus (), false,
      Rib::ItemType::Module,
      [&] (const CanonicalPath &, NodeId, location_t locus) -> void {
	rich_location r (line_table, module.get_locus ());
	r.add_range (locus);
	rust_error_at (r, "redefined multiple times");
      });

    NodeId current_module = resolver->peek_current_module_scope ();
    mappings->insert_module_child_item (current_module, mod);
    mappings->insert_module_child (current_module, module.get_node_id ());

    resolver->push_new_module_scope (module.get_node_id ());
    for (auto &item : module.get_items ())
      ResolveTopLevel::go (*item, path, cpath);

    resolver->pop_module_scope ();

    mappings->insert_canonical_path (module.get_node_id (), cpath);
  }

  void visit (AST::TypeAlias &alias) override
  {
    auto talias
      = CanonicalPath::new_seg (alias.get_node_id (),
				alias.get_new_type_name ().as_string ());
    auto path = prefix.append (talias);
    auto cpath = canonical_prefix.append (talias);

    resolver->get_type_scope ().insert (
      path, alias.get_node_id (), alias.get_locus (), false,
      Rib::ItemType::Type,
      [&] (const CanonicalPath &, NodeId, location_t locus) -> void {
	rich_location r (line_table, alias.get_locus ());
	r.add_range (locus);
	rust_error_at (r, "redefined multiple times");
      });

    NodeId current_module = resolver->peek_current_module_scope ();
    mappings->insert_module_child_item (current_module, talias);
    mappings->insert_canonical_path (alias.get_node_id (), cpath);
  }

  void visit (AST::TupleStruct &struct_decl) override
  {
    auto decl
      = CanonicalPath::new_seg (struct_decl.get_node_id (),
				struct_decl.get_identifier ().as_string ());
    auto path = prefix.append (decl);
    auto cpath = canonical_prefix.append (decl);

    resolver->get_type_scope ().insert (
      path, struct_decl.get_node_id (), struct_decl.get_locus (), false,
      Rib::ItemType::Type,
      [&] (const CanonicalPath &, NodeId, location_t locus) -> void {
	rich_location r (line_table, struct_decl.get_locus ());
	r.add_range (locus);
	rust_error_at (r, "redefined multiple times");
      });

    NodeId current_module = resolver->peek_current_module_scope ();
    mappings->insert_module_child_item (current_module, decl);
    mappings->insert_canonical_path (struct_decl.get_node_id (), cpath);
  }

  void visit (AST::Enum &enum_decl) override
  {
    auto decl
      = CanonicalPath::new_seg (enum_decl.get_node_id (),
				enum_decl.get_identifier ().as_string ());
    auto path = prefix.append (decl);
    auto cpath = canonical_prefix.append (decl);

    resolver->get_type_scope ().insert (
      path, enum_decl.get_node_id (), enum_decl.get_locus (), false,
      Rib::ItemType::Type,
      [&] (const CanonicalPath &, NodeId, location_t locus) -> void {
	rich_location r (line_table, enum_decl.get_locus ());
	r.add_range (locus);
	rust_error_at (r, "redefined multiple times");
      });

    resolver->push_new_module_scope (enum_decl.get_node_id ());
    for (auto &variant : enum_decl.get_variants ())
      ResolveTopLevel::go (*variant, path, cpath);

    resolver->pop_module_scope ();

    NodeId current_module = resolver->peek_current_module_scope ();
    mappings->insert_module_child_item (current_module, decl);
    mappings->insert_canonical_path (enum_decl.get_node_id (), cpath);
  }

  void visit (AST::EnumItem &item) override
  {
    auto decl = CanonicalPath::new_seg (item.get_node_id (),
					item.get_identifier ().as_string ());
    auto path = prefix.append (decl);
    auto cpath = canonical_prefix.append (decl);

    resolver->get_type_scope ().insert (
      path, item.get_node_id (), item.get_locus (), false, Rib::ItemType::Type,
      [&] (const CanonicalPath &, NodeId, location_t locus) -> void {
	rich_location r (line_table, item.get_locus ());
	r.add_range (locus);
	rust_error_at (r, "redefined multiple times");
      });

    mappings->insert_canonical_path (item.get_node_id (), cpath);

    NodeId current_module = resolver->peek_current_module_scope ();
    mappings->insert_module_child_item (current_module, decl);
    mappings->insert_module_child (current_module, item.get_node_id ());
  }

  void visit (AST::EnumItemTuple &item) override
  {
    auto decl = CanonicalPath::new_seg (item.get_node_id (),
					item.get_identifier ().as_string ());
    auto path = prefix.append (decl);
    auto cpath = canonical_prefix.append (decl);

    resolver->get_type_scope ().insert (
      path, item.get_node_id (), item.get_locus (), false, Rib::ItemType::Type,
      [&] (const CanonicalPath &, NodeId, location_t locus) -> void {
	rich_location r (line_table, item.get_locus ());
	r.add_range (locus);
	rust_error_at (r, "redefined multiple times");
      });

    mappings->insert_canonical_path (item.get_node_id (), cpath);

    NodeId current_module = resolver->peek_current_module_scope ();
    mappings->insert_module_child_item (current_module, decl);
    mappings->insert_module_child (current_module, item.get_node_id ());
  }

  void visit (AST::EnumItemStruct &item) override
  {
    auto decl = CanonicalPath::new_seg (item.get_node_id (),
					item.get_identifier ().as_string ());
    auto path = prefix.append (decl);
    auto cpath = canonical_prefix.append (decl);

    resolver->get_type_scope ().insert (
      path, item.get_node_id (), item.get_locus (), false, Rib::ItemType::Type,
      [&] (const CanonicalPath &, NodeId, location_t locus) -> void {
	rich_location r (line_table, item.get_locus ());
	r.add_range (locus);
	rust_error_at (r, "redefined multiple times");
      });

    mappings->insert_canonical_path (item.get_node_id (), cpath);

    NodeId current_module = resolver->peek_current_module_scope ();
    mappings->insert_module_child_item (current_module, decl);
    mappings->insert_module_child (current_module, item.get_node_id ());
  }

  void visit (AST::EnumItemDiscriminant &item) override
  {
    auto decl = CanonicalPath::new_seg (item.get_node_id (),
					item.get_identifier ().as_string ());
    auto path = prefix.append (decl);
    auto cpath = canonical_prefix.append (decl);

    resolver->get_type_scope ().insert (
      path, item.get_node_id (), item.get_locus (), false, Rib::ItemType::Type,
      [&] (const CanonicalPath &, NodeId, location_t locus) -> void {
	rich_location r (line_table, item.get_locus ());
	r.add_range (locus);
	rust_error_at (r, "redefined multiple times");
      });

    mappings->insert_canonical_path (item.get_node_id (), cpath);

    NodeId current_module = resolver->peek_current_module_scope ();
    mappings->insert_module_child_item (current_module, decl);
    mappings->insert_module_child (current_module, item.get_node_id ());
  }

  void visit (AST::StructStruct &struct_decl) override
  {
    auto decl
      = CanonicalPath::new_seg (struct_decl.get_node_id (),
				struct_decl.get_identifier ().as_string ());
    auto path = prefix.append (decl);
    auto cpath = canonical_prefix.append (decl);

    resolver->get_type_scope ().insert (
      path, struct_decl.get_node_id (), struct_decl.get_locus (), false,
      Rib::ItemType::Type,
      [&] (const CanonicalPath &, NodeId, location_t locus) -> void {
	rich_location r (line_table, struct_decl.get_locus ());
	r.add_range (locus);
	rust_error_at (r, "redefined multiple times");
      });

    NodeId current_module = resolver->peek_current_module_scope ();
    mappings->insert_module_child_item (current_module, decl);
    mappings->insert_canonical_path (struct_decl.get_node_id (), cpath);
  }

  void visit (AST::Union &union_decl) override
  {
    auto decl
      = CanonicalPath::new_seg (union_decl.get_node_id (),
				union_decl.get_identifier ().as_string ());
    auto path = prefix.append (decl);
    auto cpath = canonical_prefix.append (decl);

    resolver->get_type_scope ().insert (
      path, union_decl.get_node_id (), union_decl.get_locus (), false,
      Rib::ItemType::Type,
      [&] (const CanonicalPath &, NodeId, location_t locus) -> void {
	rich_location r (line_table, union_decl.get_locus ());
	r.add_range (locus);
	rust_error_at (r, "redefined multiple times");
      });

    NodeId current_module = resolver->peek_current_module_scope ();
    mappings->insert_module_child_item (current_module, decl);
    mappings->insert_canonical_path (union_decl.get_node_id (), cpath);
  }

  void visit (AST::StaticItem &var) override
  {
    auto decl = CanonicalPath::new_seg (var.get_node_id (),
					var.get_identifier ().as_string ());
    auto path = prefix.append (decl);
    auto cpath = canonical_prefix.append (decl);

    resolver->get_name_scope ().insert (
      path, var.get_node_id (), var.get_locus (), false, Rib::ItemType::Static,
      [&] (const CanonicalPath &, NodeId, location_t locus) -> void {
	rich_location r (line_table, var.get_locus ());
	r.add_range (locus);
	rust_error_at (r, "redefined multiple times");
      });

    NodeId current_module = resolver->peek_current_module_scope ();
    mappings->insert_module_child_item (current_module, decl);
    mappings->insert_canonical_path (var.get_node_id (), cpath);
  }

  void visit (AST::ConstantItem &constant) override
  {
    auto decl = CanonicalPath::new_seg (constant.get_node_id (),
					constant.get_identifier ());
    auto path = prefix.append (decl);
    auto cpath = canonical_prefix.append (decl);

    resolver->get_name_scope ().insert (
      path, constant.get_node_id (), constant.get_locus (), false,
      Rib::ItemType::Const,
      [&] (const CanonicalPath &, NodeId, location_t locus) -> void {
	rich_location r (line_table, constant.get_locus ());
	r.add_range (locus);
	rust_error_at (r, "redefined multiple times");
      });

    NodeId current_module = resolver->peek_current_module_scope ();
    mappings->insert_module_child_item (current_module, decl);
    mappings->insert_canonical_path (constant.get_node_id (), cpath);
  }

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
	rust_error_at (r, "redefined multiple times");
      });

    NodeId current_module = resolver->peek_current_module_scope ();
    mappings->insert_module_child_item (current_module, decl);
    mappings->insert_canonical_path (function.get_node_id (), cpath);
  }

  void visit (AST::InherentImpl &impl_block) override
  {
    std::string raw_impl_type_path = impl_block.get_type ().as_string ();
    CanonicalPath impl_type_seg
      = CanonicalPath::new_seg (impl_block.get_type ().get_node_id (),
				raw_impl_type_path);

    CanonicalPath impl_type
      = CanonicalPath::inherent_impl_seg (impl_block.get_node_id (),
					  impl_type_seg);
    CanonicalPath impl_prefix = prefix.append (impl_type_seg);

    for (auto &impl_item : impl_block.get_impl_items ())
      ResolveToplevelImplItem::go (*impl_item, impl_prefix);
  }

  void visit (AST::TraitImpl &impl_block) override
  {
    std::string raw_impl_type_path = impl_block.get_type ().as_string ();
    CanonicalPath impl_type_seg
      = CanonicalPath::new_seg (impl_block.get_type ().get_node_id (),
				raw_impl_type_path);

    std::string raw_trait_type_path = impl_block.get_trait_path ().as_string ();
    CanonicalPath trait_type_seg
      = CanonicalPath::new_seg (impl_block.get_trait_path ().get_node_id (),
				raw_trait_type_path);

    CanonicalPath projection
      = CanonicalPath::trait_impl_projection_seg (impl_block.get_node_id (),
						  trait_type_seg,
						  impl_type_seg);
    CanonicalPath impl_prefix = prefix.append (projection);

    resolver->get_name_scope ().insert (
      impl_prefix, impl_block.get_node_id (), impl_block.get_locus (), false,
      Rib::ItemType::TraitImpl,
      [&] (const CanonicalPath &, NodeId, location_t locus) -> void {
	rich_location r (line_table, impl_block.get_locus ());
	r.add_range (locus);
	rust_error_at (r, "redefined multiple times");
      });

    for (auto &impl_item : impl_block.get_impl_items ())
      ResolveToplevelImplItem::go (*impl_item, impl_prefix);
  }

  void visit (AST::Trait &trait) override
  {
    auto decl = CanonicalPath::new_seg (trait.get_node_id (),
					trait.get_identifier ().as_string ());
    auto path = prefix.append (decl);
    auto cpath = canonical_prefix.append (decl);

    resolver->get_type_scope ().insert (
      path, trait.get_node_id (), trait.get_locus (), false,
      Rib::ItemType::Trait,
      [&] (const CanonicalPath &, NodeId, location_t locus) -> void {
	rich_location r (line_table, trait.get_locus ());
	r.add_range (locus);
	rust_error_at (r, "redefined multiple times");
      });

    for (auto &item : trait.get_trait_items ())
      ResolveTopLevelTraitItems::go (item.get (), path, cpath);

    NodeId current_module = resolver->peek_current_module_scope ();
    mappings->insert_module_child_item (current_module, decl);
    mappings->insert_canonical_path (trait.get_node_id (), cpath);
  }

  void visit (AST::ExternBlock &extern_block) override
  {
    for (auto &item : extern_block.get_extern_items ())
      {
	ResolveToplevelExternItem::go (*item, prefix);
      }
  }

  void visit (AST::ExternCrate &extern_crate) override
  {
    if (extern_crate.is_marked_for_strip ())
      return;

    NodeId resolved_crate = UNKNOWN_NODEID;
    if (extern_crate.references_self ())
      {
	CrateNum crate_num = mappings->get_current_crate ();
	bool ok = mappings->crate_num_to_nodeid (crate_num, resolved_crate);
	rust_assert (ok);
      }
    else
      {
	CrateNum found_crate_num = UNKNOWN_CRATENUM;
	bool found
	  = mappings->lookup_crate_name (extern_crate.get_referenced_crate (),
					 found_crate_num);
	if (!found)
	  {
	    rust_error_at (extern_crate.get_locus (), "unknown crate %qs",
			   extern_crate.get_referenced_crate ().c_str ());
	    return;
	  }

	bool ok
	  = mappings->crate_num_to_nodeid (found_crate_num, resolved_crate);
	if (!ok)
	  {
	    rust_internal_error_at (extern_crate.get_locus (),
				    "failed to resolve crate to nodeid");
	    return;
	  }
      }

    if (resolved_crate == UNKNOWN_NODEID)
      {
	rust_error_at (extern_crate.get_locus (), "failed to resolve crate");
	return;
      }

    // mark the node as resolved
    resolver->insert_resolved_name (extern_crate.get_node_id (),
				    resolved_crate);
    CanonicalPath decl
      = extern_crate.has_as_clause ()
	  ? CanonicalPath::new_seg (extern_crate.get_node_id (),
				    extern_crate.get_as_clause ())
	  : CanonicalPath::new_seg (extern_crate.get_node_id (),
				    extern_crate.get_referenced_crate ());

    resolver->get_type_scope ().insert (
      decl, resolved_crate, extern_crate.get_locus (), false,
      Rib::ItemType::ExternCrate,
      [&] (const CanonicalPath &, NodeId, location_t locus) -> void {
	rich_location r (line_table, extern_crate.get_locus ());
	r.add_range (locus);
	rust_error_at (r, "redefined multiple times");
      });
  }

private:
  ResolveTopLevel (const CanonicalPath &prefix,
		   const CanonicalPath &canonical_prefix)
    : ResolverBase (), prefix (prefix), canonical_prefix (canonical_prefix)
  {}

  const CanonicalPath &prefix;
  const CanonicalPath &canonical_prefix;
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_AST_RESOLVE_TOPLEVEL_H
