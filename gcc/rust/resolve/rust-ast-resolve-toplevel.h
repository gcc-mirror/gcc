// Copyright (C) 2020 Free Software Foundation, Inc.

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
#include "rust-ast-resolve-type.h"
#include "rust-ast-resolve-implitem.h"
#include "rust-ast-full.h"
#include "rust-name-resolver.h"

namespace Rust {
namespace Resolver {

class ResolveTopLevel : public ResolverBase
{
  using Rust::Resolver::ResolverBase::visit;

public:
  static void go (AST::Item *item,
		  const CanonicalPath &prefix = CanonicalPath::create_empty ())
  {
    ResolveTopLevel resolver (prefix);
    item->accept_vis (resolver);
  };

  void visit (AST::Module &module) override
  {
    auto path = prefix.append (
      CanonicalPath::new_seg (module.get_node_id (), module.get_name ()));
    resolver->get_name_scope ().insert (
      path, module.get_node_id (), module.get_locus (), false,
      [&] (const CanonicalPath &, NodeId, Location locus) -> void {
	RichLocation r (module.get_locus ());
	r.add_range (locus);
	rust_error_at (r, "redefined multiple times");
      });

    resolver->insert_new_definition (module.get_node_id (),
				     Definition{module.get_node_id (),
						module.get_node_id ()});

    for (auto &item : module.get_items ())
      ResolveTopLevel::go (item.get (), path);
  }

  void visit (AST::TypeAlias &alias) override
  {
    auto path
      = prefix.append (CanonicalPath::new_seg (alias.get_node_id (),
					       alias.get_new_type_name ()));
    resolver->get_type_scope ().insert (
      path, alias.get_node_id (), alias.get_locus (), false,
      [&] (const CanonicalPath &, NodeId, Location locus) -> void {
	RichLocation r (alias.get_locus ());
	r.add_range (locus);
	rust_error_at (r, "redefined multiple times");
      });
  }

  void visit (AST::TupleStruct &struct_decl) override
  {
    auto path
      = prefix.append (CanonicalPath::new_seg (struct_decl.get_node_id (),
					       struct_decl.get_identifier ()));
    resolver->get_type_scope ().insert (
      path, struct_decl.get_node_id (), struct_decl.get_locus (), false,
      [&] (const CanonicalPath &, NodeId, Location locus) -> void {
	RichLocation r (struct_decl.get_locus ());
	r.add_range (locus);
	rust_error_at (r, "redefined multiple times");
      });
  }

  void visit (AST::StructStruct &struct_decl) override
  {
    auto path
      = prefix.append (CanonicalPath::new_seg (struct_decl.get_node_id (),
					       struct_decl.get_identifier ()));
    resolver->get_type_scope ().insert (
      path, struct_decl.get_node_id (), struct_decl.get_locus (), false,
      [&] (const CanonicalPath &, NodeId, Location locus) -> void {
	RichLocation r (struct_decl.get_locus ());
	r.add_range (locus);
	rust_error_at (r, "redefined multiple times");
      });
  }

  void visit (AST::Union &union_decl) override
  {
    auto path
      = prefix.append (CanonicalPath::new_seg (union_decl.get_node_id (),
					       union_decl.get_identifier ()));
    resolver->get_type_scope ().insert (
      path, union_decl.get_node_id (), union_decl.get_locus (), false,
      [&] (const CanonicalPath &, NodeId, Location locus) -> void {
	RichLocation r (union_decl.get_locus ());
	r.add_range (locus);
	rust_error_at (r, "redefined multiple times");
      });
  }

  void visit (AST::StaticItem &var) override
  {
    auto path = prefix.append (
      CanonicalPath::new_seg (var.get_node_id (), var.get_identifier ()));
    resolver->get_name_scope ().insert (
      path, var.get_node_id (), var.get_locus (), false,
      [&] (const CanonicalPath &, NodeId, Location locus) -> void {
	RichLocation r (var.get_locus ());
	r.add_range (locus);
	rust_error_at (r, "redefined multiple times");
      });
    resolver->insert_new_definition (var.get_node_id (),
				     Definition{var.get_node_id (),
						var.get_node_id ()});
    resolver->mark_decl_mutability (var.get_node_id (), var.is_mutable ());
  }

  void visit (AST::ConstantItem &constant) override
  {
    auto path
      = prefix.append (ResolveConstantItemToCanonicalPath::resolve (constant));
    resolver->get_name_scope ().insert (
      path, constant.get_node_id (), constant.get_locus (), false,
      [&] (const CanonicalPath &, NodeId, Location locus) -> void {
	RichLocation r (constant.get_locus ());
	r.add_range (locus);
	rust_error_at (r, "redefined multiple times");
      });
    resolver->insert_new_definition (constant.get_node_id (),
				     Definition{constant.get_node_id (),
						constant.get_node_id ()});
  }

  void visit (AST::Function &function) override
  {
    auto path
      = prefix.append (ResolveFunctionItemToCanonicalPath::resolve (function));
    resolver->get_name_scope ().insert (
      path, function.get_node_id (), function.get_locus (), false,
      [&] (const CanonicalPath &, NodeId, Location locus) -> void {
	RichLocation r (function.get_locus ());
	r.add_range (locus);
	rust_error_at (r, "redefined multiple times");
      });
    resolver->insert_new_definition (function.get_node_id (),
				     Definition{function.get_node_id (),
						function.get_node_id ()});

    // if this does not get a reference it will be determined to be unused
    // lets give it a fake reference to itself
    if (function.get_function_name ().compare ("main") == 0)
      {
	resolver->insert_resolved_name (function.get_node_id (),
					function.get_node_id ());
      }
  }

  void visit (AST::InherentImpl &impl_block) override
  {
    bool canonicalize_type_args = !impl_block.has_generics ();
    bool type_resolve_generic_args = false;
    CanonicalPath impl_type
      = ResolveTypeToCanonicalPath::resolve (*impl_block.get_type ().get (),
					     canonicalize_type_args,
					     type_resolve_generic_args);
    CanonicalPath impl_prefix = prefix.append (impl_type);

    for (auto &impl_item : impl_block.get_impl_items ())
      ResolveToplevelImplItem::go (impl_item.get (), impl_prefix);
  }

  void visit (AST::TraitImpl &impl_block) override
  {
    bool canonicalize_type_args = !impl_block.has_generics ();
    bool type_resolve_generic_args = false;

    CanonicalPath impl_type_seg
      = ResolveTypeToCanonicalPath::resolve (*impl_block.get_type ().get (),
					     canonicalize_type_args,
					     type_resolve_generic_args);
    CanonicalPath trait_type_seg
      = ResolveTypeToCanonicalPath::resolve (impl_block.get_trait_path (),
					     canonicalize_type_args,
					     type_resolve_generic_args);

    CanonicalPath projection
      = TraitImplProjection::resolve (impl_block.get_node_id (), trait_type_seg,
				      impl_type_seg);
    CanonicalPath impl_prefix = prefix.append (projection);

    resolver->get_name_scope ().insert (
      impl_prefix, impl_block.get_node_id (), impl_block.get_locus (), false,
      [&] (const CanonicalPath &, NodeId, Location locus) -> void {
	RichLocation r (impl_block.get_locus ());
	r.add_range (locus);
	rust_error_at (r, "redefined multiple times");
      });
    resolver->insert_new_definition (impl_block.get_node_id (),
				     Definition{impl_block.get_node_id (),
						impl_block.get_node_id ()});
    resolver->insert_resolved_name (impl_block.get_node_id (),
				    impl_block.get_node_id ());

    for (auto &impl_item : impl_block.get_impl_items ())
      ResolveToplevelImplItem::go (impl_item.get (), impl_prefix);
  }

  void visit (AST::Trait &trait) override
  {
    CanonicalPath path = prefix.append (
      CanonicalPath::new_seg (trait.get_node_id (), trait.get_identifier ()));
    resolver->get_type_scope ().insert (
      path, trait.get_node_id (), trait.get_locus (), false,
      [&] (const CanonicalPath &, NodeId, Location locus) -> void {
	RichLocation r (trait.get_locus ());
	r.add_range (locus);
	rust_error_at (r, "redefined multiple times");
      });

    for (auto &item : trait.get_trait_items ())
      ResolveTopLevelTraitItems::go (item.get (), path);
  }

  void visit (AST::ExternBlock &extern_block) override
  {
    for (auto &item : extern_block.get_extern_items ())
      {
	ResolveToplevelExternItem::go (item.get (), prefix);
      }
  }

private:
  ResolveTopLevel (const CanonicalPath &prefix)
    : ResolverBase (UNKNOWN_NODEID), prefix (prefix)
  {}

  const CanonicalPath &prefix;
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_AST_RESOLVE_TOPLEVEL_H
