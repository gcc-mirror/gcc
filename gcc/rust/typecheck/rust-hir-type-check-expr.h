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

#ifndef RUST_HIR_TYPE_CHECK_EXPR
#define RUST_HIR_TYPE_CHECK_EXPR

#include "rust-hir-type-check-base.h"
#include "rust-hir-full.h"
#include "rust-tyty.h"
#include "rust-tyty-call.h"

namespace Rust {
namespace Resolver {

class TypeCheckExpr : public TypeCheckBase
{
public:
  static TyTy::TyBase *Resolve (HIR::Expr *expr)
  {
    TypeCheckExpr resolver;
    expr->accept_vis (resolver);
    if (resolver.infered != nullptr)
      resolver.context->insert_type (expr->get_mappings ().get_hirid (),
				     resolver.infered);

    return resolver.infered;
  }

  void visit (HIR::ReturnExpr &expr)
  {
    auto ret = context->peek_return_type ();
    rust_assert (ret != nullptr);

    auto expr_ty = TypeCheckExpr::Resolve (expr.get_expr ());
    infered = ret->combine (expr_ty);
  }

  void visit (HIR::CallExpr &expr)
  {
    auto fn = expr.get_fnexpr ();
    auto fn_node_id = fn->get_mappings ().get_nodeid ();

    // then lookup the reference_node_id
    NodeId ref_node_id;
    if (!resolver->lookup_resolved_name (fn_node_id, &ref_node_id))
      {
	rust_error_at (expr.get_locus (),
		       "Failed to lookup reference for node: %s",
		       expr.as_string ().c_str ());
	return;
      }

    // node back to HIR
    HirId ref;
    if (!mappings->lookup_node_to_hir (expr.get_mappings ().get_crate_num (),
				       ref_node_id, &ref))
      {
	rust_error_at (expr.get_locus (), "reverse lookup failure");
	return;
      }

    // check if this has a type
    TyTy::TyBase *lookup;
    if (!context->lookup_type (ref, &lookup))
      {
	// FIXME we need to be able to lookup the location info for the
	// reference here
	rust_error_at (expr.get_locus (), "consider giving this a type: %s",
		       expr.as_string ().c_str ());
	return;
      }

    infered = TyTy::TypeCheckCallExpr::go (lookup, expr);
  }

  void visit (HIR::AssignmentExpr &expr)
  {
    auto lhs = TypeCheckExpr::Resolve (expr.get_lhs ());
    auto rhs = TypeCheckExpr::Resolve (expr.get_rhs ());

    infered = lhs->combine (rhs);
  }

  void visit (HIR::IdentifierExpr &expr)
  {
    NodeId ast_node_id = expr.get_mappings ().get_nodeid ();

    // then lookup the reference_node_id
    NodeId ref_node_id;
    if (!resolver->lookup_resolved_name (ast_node_id, &ref_node_id))
      {
	rust_error_at (expr.get_locus (),
		       "Failed to lookup reference for node: %s",
		       expr.as_string ().c_str ());
	return;
      }

    // node back to HIR
    HirId ref;
    if (!mappings->lookup_node_to_hir (expr.get_mappings ().get_crate_num (),
				       ref_node_id, &ref))
      {
	rust_error_at (expr.get_locus (), "reverse lookup failure");
	return;
      }

    // check if this has a type
    TyTy::TyBase *lookup;
    if (!context->lookup_type (ref, &lookup))
      {
	// FIXME we need to be able to lookup the location info for the
	// reference here
	rust_error_at (expr.get_locus (), "consider giving this a type: %s",
		       expr.as_string ().c_str ());
	return;
      }

    // FIXME this needs to be cloned for memory management later on
    infered = lookup;
  }

  void visit (HIR::LiteralExpr &expr)
  {
    switch (expr.get_lit_type ())
      {
      case HIR::Literal::LitType::INT:
	infered = new TyTy::IntType (expr.get_mappings ().get_hirid (),
				     TyTy::IntType::IntKind::I32);
	break;

      case HIR::Literal::LitType::BOOL:
	infered = new TyTy::BoolType (expr.get_mappings ().get_hirid ());
	break;

      default:
	gcc_unreachable ();
	break;
      }
  }

  void visit (HIR::ArithmeticOrLogicalExpr &expr)
  {
    auto lhs = TypeCheckExpr::Resolve (expr.get_lhs ());
    auto rhs = TypeCheckExpr::Resolve (expr.get_rhs ());

    infered = lhs->combine (rhs);
  }

private:
  TypeCheckExpr () : TypeCheckBase (), infered (nullptr) {}

  TyTy::TyBase *infered;
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_HIR_TYPE_CHECK_EXPR
