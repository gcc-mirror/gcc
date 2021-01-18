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

#include "rust-hir-type-check.h"
#include "rust-hir-full.h"
#include "rust-hir-type-check-toplevel.h"
#include "rust-hir-type-check-item.h"
#include "rust-hir-type-check-expr.h"
#include "rust-hir-type-check-struct-field.h"

namespace Rust {
namespace Resolver {

void
TypeResolution::Resolve (HIR::Crate &crate)
{
  for (auto it = crate.items.begin (); it != crate.items.end (); it++)
    TypeCheckTopLevel::Resolve (it->get ());

  for (auto it = crate.items.begin (); it != crate.items.end (); it++)
    TypeCheckItem::Resolve (it->get ());
}

// RUST_HIR_TYPE_CHECK_EXPR
void
TypeCheckExpr::visit (HIR::BlockExpr &expr)
{
  TyTy::TyBase *block_tyty
    = new TyTy::UnitType (expr.get_mappings ().get_hirid ());

  expr.iterate_stmts ([&] (HIR::Stmt *s) mutable -> bool {
    bool is_final_stmt = expr.is_final_stmt (s);
    bool is_final_expr = is_final_stmt && !expr.has_expr ();

    auto infered = TypeCheckStmt::Resolve (s, is_final_expr);
    if (is_final_expr)
      {
	delete block_tyty;
	block_tyty = infered;
      }

    return true;
  });

  // tail expression must be checked as part of the caller since
  // the result of this is very dependant on what we expect it to be
  if (expr.has_expr ())
    TypeCheckExpr::Resolve (expr.expr.get ());

  // now that the stmts have been resolved we must resolve the block of locals
  // and make sure the variables have been resolved
  auto body_mappings = expr.get_mappings ();
  Rib *rib = nullptr;
  if (!resolver->find_name_rib (body_mappings.get_nodeid (), &rib))
    {
      rust_fatal_error (expr.get_locus (), "failed to lookup locals per block");
      return;
    }
  TyTyResolver::Resolve (rib, mappings, resolver, context);

  infered = block_tyty;
}

// RUST_HIR_TYPE_CHECK_STRUCT_FIELD

void
TypeCheckStructExpr::visit (HIR::StructExprStructFields &struct_expr)
{
  struct_expr.get_struct_name ().accept_vis (*this);
  if (struct_path_resolved == nullptr)
    {
      rust_fatal_error (struct_expr.get_struct_name ().get_locus (),
			"Failed to resolve type");
      return;
    }

  struct_expr.iterate ([&] (HIR::StructExprField *field) mutable -> bool {
    resolved_field = nullptr;
    field->accept_vis (*this);
    if (resolved_field == nullptr)
      {
	rust_fatal_error (field->get_locus (),
			  "failed to resolve type for field");
	return false;
      }

    context->insert_type (field->get_mappings ().get_hirid (), resolved_field);
    return true;
  });

  TyTy::TyBase *expr_type = struct_path_resolved;
  if (struct_expr.has_struct_base ())
    {
      TyTy::TyBase *base_resolved
	= TypeCheckExpr::Resolve (struct_expr.struct_base->base_struct.get ());
      expr_type = expr_type->combine (base_resolved);
      if (resolved == nullptr)
	{
	  rust_fatal_error (
	    struct_expr.struct_base->base_struct->get_locus_slow (),
	    "incompatible types for base struct reference");
	  return;
	}
    }
  else if (fields_assigned.size () != struct_path_resolved->num_fields ())
    {
      rust_fatal_error (struct_expr.get_locus (),
			"some fields are not fully assigned");
      return;
    }

  resolved = expr_type;
}

void
TypeCheckStructExpr::visit (HIR::PathInExpression &expr)
{
  NodeId ast_node_id = expr.get_mappings ().get_nodeid ();

  // then lookup the reference_node_id
  NodeId ref_node_id;
  if (!resolver->lookup_resolved_name (ast_node_id, &ref_node_id))
    {
      if (!resolver->lookup_resolved_type (ast_node_id, &ref_node_id))
	{
	  rust_error_at (expr.get_locus (),
			 "Failed to lookup reference for node: %s",
			 expr.as_string ().c_str ());
	  return;
	}
    }

  // node back to HIR
  HirId ref;
  if (!mappings->lookup_node_to_hir (expr.get_mappings ().get_crate_num (),
				     ref_node_id, &ref))
    {
      rust_error_at (expr.get_locus (), "reverse lookup failure");
      return;
    }

  // the base reference for this name _must_ have a type set
  TyTy::TyBase *lookup;
  if (!context->lookup_type (ref, &lookup))
    {
      rust_error_at (mappings->lookup_location (ref),
		     "consider giving this a type: %s",
		     expr.as_string ().c_str ());
      return;
    }

  if (lookup->get_kind () != TyTy::TypeKind::ADT)
    {
      rust_fatal_error (mappings->lookup_location (ref),
			"expected an ADT type");
      return;
    }
  struct_path_resolved = (TyTy::ADTType *) lookup;
}

void
TypeCheckStructExpr::visit (HIR::StructExprFieldIdentifierValue &field)
{
  auto it = fields_assigned.find (field.field_name);
  if (it != fields_assigned.end ())
    {
      rust_fatal_error (field.get_locus (), "used more than once");
      return;
    }

  TyTy::TyBase *value = TypeCheckExpr::Resolve (field.get_value ());
  TyTy::StructFieldType *field_type
    = struct_path_resolved->get_field (field.field_name);
  if (field_type == nullptr)
    {
      rust_error_at (field.get_locus (), "unknown field");
      return;
    }

  resolved_field = field_type->get_field_type ()->combine (value);
  if (resolved_field != nullptr)
    fields_assigned.insert (field.field_name);
}

void
TypeCheckStructExpr::visit (HIR::StructExprFieldIndexValue &field)
{
  std::string field_name (std::to_string (field.get_tuple_index ()));
  auto it = fields_assigned.find (field_name);
  if (it != fields_assigned.end ())
    {
      rust_fatal_error (field.get_locus (), "used more than once");
      return;
    }

  TyTy::TyBase *value = TypeCheckExpr::Resolve (field.get_value ());
  TyTy::StructFieldType *field_type
    = struct_path_resolved->get_field (field_name);
  if (field_type == nullptr)
    {
      rust_error_at (field.get_locus (), "unknown field");
      return;
    }

  resolved_field = field_type->get_field_type ()->combine (value);
  if (resolved_field != nullptr)
    fields_assigned.insert (field_name);
}

} // namespace Resolver
} // namespace Rust
