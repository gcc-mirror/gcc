// Copyright (C) 2020-2022 Free Software Foundation, Inc.

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
  static void go (AST::InherentImplItem *item, const CanonicalPath &prefix)
  {
    if (item->is_marked_for_strip ())
      return;

    ResolveToplevelImplItem resolver (prefix);
    item->accept_vis (resolver);
  }

  static void go (AST::TraitImplItem *item, const CanonicalPath &prefix)
  {
    if (item->is_marked_for_strip ())
      return;

    ResolveToplevelImplItem resolver (prefix);
    item->accept_vis (resolver);
  }

  void visit (AST::MacroInvocation &invoc) override
  {
    if (!invoc.has_semicolon ())
      return;

    AST::ASTFragment &fragment = invoc.get_fragment ();
    for (auto &node : fragment.get_nodes ())
      node.accept_vis (*this);
  }

  void visit (AST::TypeAlias &type) override
  {
    auto decl
      = CanonicalPath::new_seg (type.get_node_id (), type.get_new_type_name ());
    auto path = prefix.append (decl);

    resolver->get_type_scope ().insert (
      path, type.get_node_id (), type.get_locus (), false,
      [&] (const CanonicalPath &, NodeId, Location locus) -> void {
	RichLocation r (type.get_locus ());
	r.add_range (locus);
	rust_error_at (r, "redefined multiple times");
      });
  }

  void visit (AST::ConstantItem &constant) override
  {
    auto decl = ResolveConstantItemToCanonicalPath::resolve (constant);
    auto path = prefix.append (decl);

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
    auto decl = ResolveFunctionItemToCanonicalPath::resolve (function);
    auto path = prefix.append (decl);

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
  }

  void visit (AST::Method &method) override
  {
    auto decl = ResolveMethodItemToCanonicalPath::resolve (method);
    auto path = prefix.append (decl);

    resolver->get_name_scope ().insert (
      path, method.get_node_id (), method.get_locus (), false,
      [&] (const CanonicalPath &, NodeId, Location locus) -> void {
	RichLocation r (method.get_locus ());
	r.add_range (locus);
	rust_error_at (r, "redefined multiple times");
      });
    resolver->insert_new_definition (method.get_node_id (),
				     Definition{method.get_node_id (),
						method.get_node_id ()});
  }

private:
  ResolveToplevelImplItem (const CanonicalPath &prefix)
    : ResolverBase (UNKNOWN_NODEID), prefix (prefix)
  {
    rust_assert (!prefix.is_empty ());
  }

  const CanonicalPath &prefix;
};

class ResolveTopLevelTraitItems : public ResolverBase
{
  using Rust::Resolver::ResolverBase::visit;

public:
  static void go (AST::TraitItem *item, const CanonicalPath &prefix,
		  const CanonicalPath &canonical_prefix)
  {
    ResolveTopLevelTraitItems resolver (prefix, canonical_prefix);
    item->accept_vis (resolver);
  };

  void visit (AST::MacroInvocation &invoc) override
  {
    if (!invoc.has_semicolon ())
      return;

    AST::ASTFragment &fragment = invoc.get_fragment ();
    for (auto &node : fragment.get_nodes ())
      node.accept_vis (*this);
  }

  void visit (AST::TraitItemFunc &function) override
  {
    auto decl = ResolveTraitItemFunctionToCanonicalPath::resolve (function);
    auto path = prefix.append (decl);
    auto cpath = canonical_prefix.append (decl);

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

    mappings->insert_canonical_path (mappings->get_current_crate (),
				     function.get_node_id (), cpath);
  }

  void visit (AST::TraitItemMethod &method) override
  {
    auto decl = ResolveTraitItemMethodToCanonicalPath::resolve (method);
    auto path = prefix.append (decl);
    auto cpath = canonical_prefix.append (decl);

    resolver->get_name_scope ().insert (
      path, method.get_node_id (), method.get_locus (), false,
      [&] (const CanonicalPath &, NodeId, Location locus) -> void {
	RichLocation r (method.get_locus ());
	r.add_range (locus);
	rust_error_at (r, "redefined multiple times");
      });
    resolver->insert_new_definition (method.get_node_id (),
				     Definition{method.get_node_id (),
						method.get_node_id ()});

    mappings->insert_canonical_path (mappings->get_current_crate (),
				     method.get_node_id (), cpath);
  }

  void visit (AST::TraitItemConst &constant) override
  {
    auto decl = ResolveTraitItemConstToCanonicalPath::resolve (constant);
    auto path = prefix.append (decl);
    auto cpath = canonical_prefix.append (decl);

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

    mappings->insert_canonical_path (mappings->get_current_crate (),
				     constant.get_node_id (), cpath);
  }

  void visit (AST::TraitItemType &type) override
  {
    auto decl = ResolveTraitItemTypeToCanonicalPath::resolve (type);
    auto path = prefix.append (decl);
    auto cpath = canonical_prefix.append (decl);

    resolver->get_type_scope ().insert (
      path, type.get_node_id (), type.get_locus (), false,
      [&] (const CanonicalPath &, NodeId, Location locus) -> void {
	RichLocation r (type.get_locus ());
	r.add_range (locus);
	rust_error_at (r, "redefined multiple times");
      });

    mappings->insert_canonical_path (mappings->get_current_crate (),
				     type.get_node_id (), cpath);
  }

private:
  ResolveTopLevelTraitItems (const CanonicalPath &prefix,
			     const CanonicalPath &canonical_prefix)
    : ResolverBase (UNKNOWN_NODEID), prefix (prefix),
      canonical_prefix (canonical_prefix)
  {}

  const CanonicalPath &prefix;
  const CanonicalPath &canonical_prefix;
};

class ResolveToplevelExternItem : public ResolverBase
{
  using Rust::Resolver::ResolverBase::visit;

public:
  static void go (AST::ExternalItem *item, const CanonicalPath &prefix)
  {
    ResolveToplevelExternItem resolver (prefix);
    item->accept_vis (resolver);
  };

  void visit (AST::MacroInvocation &invoc) override
  {
    if (!invoc.has_semicolon ())
      return;

    AST::ASTFragment &fragment = invoc.get_fragment ();
    for (auto &node : fragment.get_nodes ())
      node.accept_vis (*this);
  }

  void visit (AST::ExternalFunctionItem &function) override
  {
    auto decl = CanonicalPath::new_seg (function.get_node_id (),
					function.get_identifier ());
    auto path = prefix.append (decl);

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
  }

  void visit (AST::ExternalStaticItem &item) override
  {
    auto decl
      = CanonicalPath::new_seg (item.get_node_id (), item.get_identifier ());
    auto path = prefix.append (decl);

    resolver->get_name_scope ().insert (
      path, item.get_node_id (), item.get_locus (), false,
      [&] (const CanonicalPath &, NodeId, Location locus) -> void {
	RichLocation r (item.get_locus ());
	r.add_range (locus);
	rust_error_at (r, "redefined multiple times");
      });
    resolver->insert_new_definition (item.get_node_id (),
				     Definition{item.get_node_id (),
						item.get_node_id ()});
  }

private:
  ResolveToplevelExternItem (const CanonicalPath &prefix)
    : ResolverBase (UNKNOWN_NODEID), prefix (prefix)
  {}

  const CanonicalPath &prefix;
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_AST_RESOLVE_IMPLITEM_H
