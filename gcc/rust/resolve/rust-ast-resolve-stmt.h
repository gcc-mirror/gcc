// Copyright (C) 2020-2023 Free Software Foundation, Inc.

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

#ifndef RUST_AST_RESOLVE_STMT_H
#define RUST_AST_RESOLVE_STMT_H

#include "rust-ast-resolve-base.h"
#include "rust-ast-full.h"
#include "rust-ast-resolve-type.h"
#include "rust-ast-resolve-pattern.h"
#include "rust-ast-resolve-expr.h"

namespace Rust {
namespace Resolver {

class ResolveStmt : public ResolverBase
{
  using Rust::Resolver::ResolverBase::visit;

public:
  static void go (AST::Stmt *stmt, const CanonicalPath &prefix,
		  const CanonicalPath &canonical_prefix,
		  const CanonicalPath &enum_prefix)
  {
    if (stmt->is_marked_for_strip ())
      return;

    ResolveStmt resolver (prefix, canonical_prefix, enum_prefix);
    stmt->accept_vis (resolver);
  }

  void visit (AST::ExprStmtWithBlock &stmt) override
  {
    ResolveExpr::go (stmt.get_expr ().get (), prefix, canonical_prefix);
  }

  void visit (AST::ExprStmtWithoutBlock &stmt) override
  {
    ResolveExpr::go (stmt.get_expr ().get (), prefix, canonical_prefix);
  }

  void visit (AST::ConstantItem &constant) override
  {
    auto decl = CanonicalPath::new_seg (constant.get_node_id (),
					constant.get_identifier ());
    auto path = decl; // this ensures we have the correct relative resolution
    auto cpath = canonical_prefix.append (decl);
    mappings->insert_canonical_path (constant.get_node_id (), cpath);

    resolver->get_name_scope ().insert (
      path, constant.get_node_id (), constant.get_locus (), false,
      Rib::ItemType::Const,
      [&] (const CanonicalPath &, NodeId, Location locus) -> void {
	RichLocation r (constant.get_locus ());
	r.add_range (locus);
	rust_error_at (r, "redefined multiple times");
      });

    ResolveType::go (constant.get_type ().get ());
    ResolveExpr::go (constant.get_expr ().get (), prefix, canonical_prefix);
  }

  void visit (AST::LetStmt &stmt) override
  {
    if (stmt.has_init_expr ())
      {
	ResolveExpr::go (stmt.get_init_expr ().get (), prefix,
			 canonical_prefix);
      }

    PatternDeclaration::go (stmt.get_pattern ().get (), Rib::ItemType::Var);
    if (stmt.has_type ())
      ResolveType::go (stmt.get_type ().get ());
  }

  void visit (AST::TupleStruct &struct_decl) override
  {
    auto decl = CanonicalPath::new_seg (struct_decl.get_node_id (),
					struct_decl.get_identifier ());
    auto path = decl; // this ensures we have the correct relative resolution
    auto cpath = canonical_prefix.append (decl);
    mappings->insert_canonical_path (struct_decl.get_node_id (), cpath);

    resolver->get_type_scope ().insert (
      path, struct_decl.get_node_id (), struct_decl.get_locus (), false,
      Rib::ItemType::Type,
      [&] (const CanonicalPath &, NodeId, Location locus) -> void {
	RichLocation r (struct_decl.get_locus ());
	r.add_range (locus);
	rust_error_at (r, "redefined multiple times");
      });

    NodeId scope_node_id = struct_decl.get_node_id ();
    resolver->get_type_scope ().push (scope_node_id);

    if (struct_decl.has_generics ())
      {
	for (auto &generic : struct_decl.get_generic_params ())
	  ResolveGenericParam::go (generic.get (), prefix, canonical_prefix);
      }

    for (AST::TupleField &field : struct_decl.get_fields ())
      ResolveType::go (field.get_field_type ().get ());

    resolver->get_type_scope ().pop ();
  }

  void visit (AST::Enum &enum_decl) override
  {
    auto decl = CanonicalPath::new_seg (enum_decl.get_node_id (),
					enum_decl.get_identifier ());
    auto path = decl; // this ensures we have the correct relative resolution
    auto cpath = canonical_prefix.append (decl);
    mappings->insert_canonical_path (enum_decl.get_node_id (), cpath);

    resolver->get_type_scope ().insert (
      path, enum_decl.get_node_id (), enum_decl.get_locus (), false,
      Rib::ItemType::Type,
      [&] (const CanonicalPath &, NodeId, Location locus) -> void {
	RichLocation r (enum_decl.get_locus ());
	r.add_range (locus);
	rust_error_at (r, "redefined multiple times");
      });

    NodeId scope_node_id = enum_decl.get_node_id ();
    resolver->get_type_scope ().push (scope_node_id);

    if (enum_decl.has_generics ())
      {
	for (auto &generic : enum_decl.get_generic_params ())
	  ResolveGenericParam::go (generic.get (), prefix, canonical_prefix);
      }

    for (auto &variant : enum_decl.get_variants ())
      ResolveStmt::go (variant.get (), path, canonical_prefix, path);

    resolver->get_type_scope ().pop ();
  }

  void visit (AST::EnumItem &item) override
  {
    auto decl = enum_prefix.append (
      CanonicalPath::new_seg (item.get_node_id (), item.get_identifier ()));
    auto path = decl; // this ensures we have the correct relative resolution
    auto cpath = canonical_prefix.append (decl);
    mappings->insert_canonical_path (item.get_node_id (), cpath);

    resolver->get_type_scope ().insert (
      path, item.get_node_id (), item.get_locus (), false, Rib::ItemType::Type,
      [&] (const CanonicalPath &, NodeId, Location locus) -> void {
	RichLocation r (item.get_locus ());
	r.add_range (locus);
	rust_error_at (r, "redefined multiple times");
      });

    // Done, no fields.
  }

  void visit (AST::EnumItemTuple &item) override
  {
    auto decl = enum_prefix.append (
      CanonicalPath::new_seg (item.get_node_id (), item.get_identifier ()));
    auto path = decl; // this ensures we have the correct relative resolution
    auto cpath = canonical_prefix.append (decl);
    mappings->insert_canonical_path (item.get_node_id (), cpath);

    resolver->get_type_scope ().insert (
      path, item.get_node_id (), item.get_locus (), false, Rib::ItemType::Type,
      [&] (const CanonicalPath &, NodeId, Location locus) -> void {
	RichLocation r (item.get_locus ());
	r.add_range (locus);
	rust_error_at (r, "redefined multiple times");
      });

    for (auto &field : item.get_tuple_fields ())
      {
	if (field.get_field_type ()->is_marked_for_strip ())
	  continue;

	ResolveType::go (field.get_field_type ().get ());
      }
  }

  void visit (AST::EnumItemStruct &item) override
  {
    auto decl = enum_prefix.append (
      CanonicalPath::new_seg (item.get_node_id (), item.get_identifier ()));
    auto path = decl; // this ensures we have the correct relative resolution
    auto cpath = canonical_prefix.append (decl);
    mappings->insert_canonical_path (item.get_node_id (), cpath);

    resolver->get_type_scope ().insert (
      path, item.get_node_id (), item.get_locus (), false, Rib::ItemType::Type,
      [&] (const CanonicalPath &, NodeId, Location locus) -> void {
	RichLocation r (item.get_locus ());
	r.add_range (locus);
	rust_error_at (r, "redefined multiple times");
      });

    for (auto &field : item.get_struct_fields ())
      {
	if (field.get_field_type ()->is_marked_for_strip ())
	  continue;

	ResolveType::go (field.get_field_type ().get ());
      }
  }

  void visit (AST::EnumItemDiscriminant &item) override
  {
    auto decl = enum_prefix.append (
      CanonicalPath::new_seg (item.get_node_id (), item.get_identifier ()));
    auto path = decl; // this ensures we have the correct relative resolution
    auto cpath = canonical_prefix.append (decl);
    mappings->insert_canonical_path (item.get_node_id (), cpath);

    resolver->get_type_scope ().insert (
      path, item.get_node_id (), item.get_locus (), false, Rib::ItemType::Type,
      [&] (const CanonicalPath &, NodeId, Location locus) -> void {
	RichLocation r (item.get_locus ());
	r.add_range (locus);
	rust_error_at (r, "redefined multiple times");
      });

    // Done, no fields.
  }

  void visit (AST::StructStruct &struct_decl) override
  {
    auto decl = CanonicalPath::new_seg (struct_decl.get_node_id (),
					struct_decl.get_identifier ());
    auto path = decl; // this ensures we have the correct relative resolution
    auto cpath = canonical_prefix.append (decl);
    mappings->insert_canonical_path (struct_decl.get_node_id (), cpath);

    resolver->get_type_scope ().insert (
      path, struct_decl.get_node_id (), struct_decl.get_locus (), false,
      Rib::ItemType::Type,
      [&] (const CanonicalPath &, NodeId, Location locus) -> void {
	RichLocation r (struct_decl.get_locus ());
	r.add_range (locus);
	rust_error_at (r, "redefined multiple times");
      });

    NodeId scope_node_id = struct_decl.get_node_id ();
    resolver->get_type_scope ().push (scope_node_id);

    if (struct_decl.has_generics ())
      {
	for (auto &generic : struct_decl.get_generic_params ())
	  ResolveGenericParam::go (generic.get (), prefix, canonical_prefix);
      }

    for (AST::StructField &field : struct_decl.get_fields ())
      {
	if (field.get_field_type ()->is_marked_for_strip ())
	  continue;

	ResolveType::go (field.get_field_type ().get ());
      }

    resolver->get_type_scope ().pop ();
  }

  void visit (AST::Union &union_decl) override
  {
    auto decl = CanonicalPath::new_seg (union_decl.get_node_id (),
					union_decl.get_identifier ());
    auto path = decl; // this ensures we have the correct relative resolution
    auto cpath = canonical_prefix.append (decl);
    mappings->insert_canonical_path (union_decl.get_node_id (), cpath);

    resolver->get_type_scope ().insert (
      path, union_decl.get_node_id (), union_decl.get_locus (), false,
      Rib::ItemType::Type,
      [&] (const CanonicalPath &, NodeId, Location locus) -> void {
	RichLocation r (union_decl.get_locus ());
	r.add_range (locus);
	rust_error_at (r, "redefined multiple times");
      });

    NodeId scope_node_id = union_decl.get_node_id ();
    resolver->get_type_scope ().push (scope_node_id);

    if (union_decl.has_generics ())
      for (auto &generic : union_decl.get_generic_params ())
	ResolveGenericParam::go (generic.get (), prefix, canonical_prefix);

    for (AST::StructField &field : union_decl.get_variants ())
      {
	if (field.get_field_type ()->is_marked_for_strip ())
	  continue;

	ResolveType::go (field.get_field_type ().get ());
      }

    resolver->get_type_scope ().pop ();
  }

  void visit (AST::Function &function) override
  {
    auto decl = CanonicalPath::new_seg (function.get_node_id (),
					function.get_function_name ());
    auto path = decl; // this ensures we have the correct relative resolution
    auto cpath = canonical_prefix.append (decl);
    mappings->insert_canonical_path (function.get_node_id (), cpath);

    resolver->get_name_scope ().insert (
      path, function.get_node_id (), function.get_locus (), false,
      Rib::ItemType::Function,
      [&] (const CanonicalPath &, NodeId, Location locus) -> void {
	RichLocation r (function.get_locus ());
	r.add_range (locus);
	rust_error_at (r, "redefined multiple times");
      });

    NodeId scope_node_id = function.get_node_id ();
    resolver->get_name_scope ().push (scope_node_id);
    resolver->get_type_scope ().push (scope_node_id);
    resolver->get_label_scope ().push (scope_node_id);
    resolver->push_new_name_rib (resolver->get_name_scope ().peek ());
    resolver->push_new_type_rib (resolver->get_type_scope ().peek ());
    resolver->push_new_label_rib (resolver->get_type_scope ().peek ());

    if (function.has_generics ())
      for (auto &generic : function.get_generic_params ())
	ResolveGenericParam::go (generic.get (), prefix, canonical_prefix);

    if (function.has_return_type ())
      ResolveType::go (function.get_return_type ().get ());

    // we make a new scope so the names of parameters are resolved and shadowed
    // correctly
    for (auto &param : function.get_function_params ())
      {
	ResolveType::go (param.get_type ().get ());
	PatternDeclaration::go (param.get_pattern ().get (),
				Rib::ItemType::Param);
      }

    // resolve the function body
    ResolveExpr::go (function.get_definition ().get (), path, cpath);

    resolver->get_name_scope ().pop ();
    resolver->get_type_scope ().pop ();
    resolver->get_label_scope ().pop ();
  }

  void visit (AST::ExternBlock &extern_block) override;

private:
  ResolveStmt (const CanonicalPath &prefix,
	       const CanonicalPath &canonical_prefix,
	       const CanonicalPath &enum_prefix)
    : ResolverBase (), prefix (prefix), canonical_prefix (canonical_prefix),
      enum_prefix (enum_prefix)
  {}

  const CanonicalPath &prefix;
  const CanonicalPath &canonical_prefix;

  /* item declaration statements are not given a canonical path, but enum items
   * (variants) do inherit the enum path/identifier name.  */
  const CanonicalPath &enum_prefix;
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_AST_RESOLVE_STMT_H
