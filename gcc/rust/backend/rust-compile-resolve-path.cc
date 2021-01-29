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

#include "rust-linemap.h"
#include "rust-backend.h"
#include "rust-compile-resolve-path.h"
#include "rust-compile-item.h"

namespace Rust {
namespace Compile {

void
ResolvePathRef::visit (HIR::PathInExpression &expr)
{
  // need to look up the reference for this identifier
  NodeId ref_node_id = UNKNOWN_NODEID;
  if (ctx->get_resolver ()->lookup_resolved_name (
	expr.get_mappings ().get_nodeid (), &ref_node_id))
    {
      Resolver::Definition def;
      if (!ctx->get_resolver ()->lookup_definition (ref_node_id, &def))
	{
	  rust_error_at (expr.get_locus (),
			 "unknown reference for resolved name");
	  return;
	}
      ref_node_id = def.parent;
    }

  // this can fail because it might be a Constructor for something
  // in that case the caller should attempt ResolvePathType::Compile
  if (ref_node_id == UNKNOWN_NODEID)
    return;

  HirId ref;
  if (!ctx->get_mappings ()->lookup_node_to_hir (
	expr.get_mappings ().get_crate_num (), ref_node_id, &ref))
    {
      rust_error_at (expr.get_locus (), "reverse call path lookup failure");
      return;
    }

  // might be a constant
  if (ctx->lookup_const_decl (ref, &resolved))
    return;

  // this might be a variable reference or a function reference
  Bvariable *var = nullptr;
  if (ctx->lookup_var_decl (ref, &var))
    {
      resolved = ctx->get_backend ()->var_expression (var, expr.get_locus ());
      return;
    }

  // must be a function call
  Bfunction *fn = nullptr;
  if (!ctx->lookup_function_decl (ref, &fn))
    {
      // this might fail because its a forward decl so we can attempt to
      // resolve it now
      HIR::Item *resolved_item = ctx->get_mappings ()->lookup_hir_item (
	expr.get_mappings ().get_crate_num (), ref);
      if (resolved_item == nullptr)
	{
	  rust_error_at (expr.get_locus (), "failed to lookup forward decl");
	  return;
	}

      CompileItem::compile (resolved_item, ctx);
      if (!ctx->lookup_function_decl (ref, &fn))
	{
	  rust_error_at (expr.get_locus (), "forward decl was not compiled");
	  return;
	}
    }

  resolved
    = ctx->get_backend ()->function_code_expression (fn, expr.get_locus ());
}

void
ResolvePathType::visit (HIR::PathInExpression &expr)
{
  // need to look up the reference for this identifier
  NodeId ref_node_id;
  if (!ctx->get_resolver ()->lookup_resolved_type (
	expr.get_mappings ().get_nodeid (), &ref_node_id))
    {
      return;
    }

  HirId ref;
  if (!ctx->get_mappings ()->lookup_node_to_hir (
	expr.get_mappings ().get_crate_num (), ref_node_id, &ref))
    {
      rust_error_at (expr.get_locus (), "reverse lookup failure");
      return;
    }

  // assumes paths are functions for now
  if (!ctx->lookup_compiled_types (ref, &resolved))
    {
      rust_error_at (expr.get_locus (), "forward decl was not compiled");
      return;
    }
}

} // namespace Compile
} // namespace Rust
