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

#include "rust-readonly-check.h"
#include "rust-hir-expr.h"
#include "rust-hir-node.h"
#include "rust-hir-path.h"
#include "rust-hir-map.h"
#include "rust-hir-pattern.h"
#include "rust-mapping-common.h"
#include "rust-system.h"
#include "rust-immutable-name-resolution-context.h"
#include "rust-tyty.h"

namespace Rust {
namespace HIR {

static std::set<HirId> already_assigned_variables = {};

ReadonlyChecker::ReadonlyChecker ()
  : resolver (*Resolver::Resolver::get ()),
    mappings (Analysis::Mappings::get ()),
    context (*Resolver::TypeCheckContext::get ())
{}

void
ReadonlyChecker::go (Crate &crate)
{
  for (auto &item : crate.get_items ())
    item->accept_vis (*this);
}

void
ReadonlyChecker::visit (AssignmentExpr &expr)
{
  Expr &lhs = expr.get_lhs ();
  mutable_context.enter (expr.get_mappings ().get_hirid ());
  lhs.accept_vis (*this);
  mutable_context.exit ();
}

void
ReadonlyChecker::visit (PathInExpression &expr)
{
  if (!mutable_context.is_in_context ())
    return;

  NodeId ast_node_id = expr.get_mappings ().get_nodeid ();
  NodeId def_id;

  auto &nr_ctx
    = Resolver2_0::ImmutableNameResolutionContext::get ().resolver ();
  if (auto id = nr_ctx.lookup (ast_node_id))
    def_id = *id;
  else
    return;

  auto hir_id = mappings.lookup_node_to_hir (def_id);
  if (!hir_id)
    return;

  // Check if the local variable is mutable.
  auto maybe_pattern = mappings.lookup_hir_pattern (*hir_id);
  if (maybe_pattern
      && maybe_pattern.value ()->get_pattern_type ()
	   == HIR::Pattern::PatternType::IDENTIFIER)
    check_variable (static_cast<IdentifierPattern *> (maybe_pattern.value ()),
		    expr.get_locus ());

  // Check if the static item is mutable.
  auto maybe_item = mappings.lookup_hir_item (*hir_id);
  if (maybe_item
      && maybe_item.value ()->get_item_kind () == HIR::Item::ItemKind::Static)
    {
      auto static_item = static_cast<HIR::StaticItem *> (*maybe_item);
      if (!static_item->is_mut ())
	rust_error_at (expr.get_locus (),
		       "assignment of read-only location '%s'",
		       static_item->get_identifier ().as_string ().c_str ());
    }

  // Check if the constant item is mutable.
  if (maybe_item
      && maybe_item.value ()->get_item_kind () == HIR::Item::ItemKind::Constant)
    {
      auto const_item = static_cast<HIR::ConstantItem *> (*maybe_item);
      rust_error_at (expr.get_locus (), "assignment of read-only location '%s'",
		     const_item->get_identifier ().as_string ().c_str ());
    }
}

void
ReadonlyChecker::check_variable (IdentifierPattern *pattern,
				 location_t assigned_loc)
{
  if (!mutable_context.is_in_context ())
    return;

  TyTy::BaseType *type;
  if (context.lookup_type (pattern->get_mappings ().get_hirid (), &type)
      && is_mutable_type (type))
    return;
  if (pattern->is_mut ())
    return;

  auto hir_id = pattern->get_mappings ().get_hirid ();
  if (already_assigned_variables.count (hir_id) > 0)
    rust_error_at (assigned_loc, "assignment of read-only variable '%s'",
		   pattern->as_string ().c_str ());
  already_assigned_variables.insert (hir_id);
}

void
ReadonlyChecker::collect_assignment_identifier (IdentifierPattern &pattern,
						bool has_init_expr)
{
  if (has_init_expr)
    {
      HirId pattern_id = pattern.get_mappings ().get_hirid ();
      already_assigned_variables.insert (pattern_id);
    }
}

void
ReadonlyChecker::collect_assignment_tuple (TuplePattern &tuple_pattern,
					   bool has_init_expr)
{
  switch (tuple_pattern.get_items ().get_item_type ())
    {
    case HIR::TuplePatternItems::ItemType::NO_REST:
      {
	auto &items_no_rest = static_cast<HIR::TuplePatternItemsNoRest &> (
	  tuple_pattern.get_items ());
	for (auto &sub : items_no_rest.get_patterns ())
	  {
	    collect_assignment (*sub, has_init_expr);
	  }
      }
      break;
    case HIR::TuplePatternItems::ItemType::HAS_REST:
      {
	auto &items_has_rest = static_cast<HIR::TuplePatternItemsHasRest &> (
	  tuple_pattern.get_items ());
	for (auto &sub : items_has_rest.get_lower_patterns ())
	  collect_assignment (*sub, has_init_expr);
	for (auto &sub : items_has_rest.get_upper_patterns ())
	  collect_assignment (*sub, has_init_expr);
      }
      break;
    default:
      break;
    }
}

void
ReadonlyChecker::collect_assignment (Pattern &pattern, bool has_init_expr)
{
  switch (pattern.get_pattern_type ())
    {
    case HIR::Pattern::PatternType::IDENTIFIER:
      {
	collect_assignment_identifier (static_cast<IdentifierPattern &> (
					 pattern),
				       has_init_expr);
      }
      break;
    case HIR::Pattern::PatternType::TUPLE:
      {
	auto &tuple_pattern = static_cast<HIR::TuplePattern &> (pattern);
	collect_assignment_tuple (tuple_pattern, has_init_expr);
      }
      break;
    default:
      break;
    }
}

void
ReadonlyChecker::visit (LetStmt &stmt)
{
  HIR::Pattern &pattern = stmt.get_pattern ();
  collect_assignment (pattern, stmt.has_init_expr ());
}

void
ReadonlyChecker::visit (FieldAccessExpr &expr)
{
  if (mutable_context.is_in_context ())
    {
      expr.get_receiver_expr ().accept_vis (*this);
    }
}

void
ReadonlyChecker::visit (TupleIndexExpr &expr)
{
  if (mutable_context.is_in_context ())
    {
      expr.get_tuple_expr ().accept_vis (*this);
    }
}

void
ReadonlyChecker::visit (ArrayIndexExpr &expr)
{
  if (mutable_context.is_in_context ())
    {
      expr.get_array_expr ().accept_vis (*this);
    }
}

void
ReadonlyChecker::visit (TupleExpr &expr)
{
  if (mutable_context.is_in_context ())
    {
      // TODO: Add check for tuple expression
    }
}

void
ReadonlyChecker::visit (LiteralExpr &expr)
{
  if (mutable_context.is_in_context ())
    {
      rust_error_at (expr.get_locus (), "assignment of read-only location");
    }
}

void
ReadonlyChecker::visit (DereferenceExpr &expr)
{
  if (!mutable_context.is_in_context ())
    return;
  TyTy::BaseType *to_deref_type;
  auto to_deref = expr.get_expr ().get_mappings ().get_hirid ();
  if (!context.lookup_type (to_deref, &to_deref_type))
    return;
  if (!is_mutable_type (to_deref_type))
    rust_error_at (expr.get_locus (), "assignment of read-only location");
}

bool
ReadonlyChecker::is_mutable_type (TyTy::BaseType *type)
{
  if (type->get_kind () == TyTy::TypeKind::REF)
    return static_cast<TyTy::ReferenceType *> (type)->is_mutable ();
  if (type->get_kind () == TyTy::TypeKind::POINTER)
    return static_cast<TyTy::PointerType *> (type)->is_mutable ();
  return false;
}
} // namespace HIR
} // namespace Rust
