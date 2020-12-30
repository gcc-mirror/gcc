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
#include "rust-tyty-resolver.h"
#include "rust-hir-type-check-struct-field.h"

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
    // need to overrite the lhs type with this combination
    context->insert_type (expr.get_lhs ()->get_mappings ().get_hirid (),
			  infered);
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

    // these ref_node_ids will resolve to a pattern declaration but we are
    // interested in the definition that this refers to get the parent id
    Definition def;
    if (!resolver->lookup_definition (ref_node_id, &def))
      {
	rust_error_at (expr.get_locus (), "unknown reference");
	return;
      }

    // node back to HIR
    HirId ref;
    if (!mappings->lookup_node_to_hir (expr.get_mappings ().get_crate_num (),
				       def.parent, &ref))
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

    // FIXME this needs to be cloned for memory management later on
    infered = lookup;
  }

  void visit (HIR::LiteralExpr &expr)
  {
    switch (expr.get_lit_type ())
      {
	case HIR::Literal::LitType::INT: {
	  // FIXME:
	  // assume i32 let the combiner functions figure it out
	  // this should look at the suffix of the literal value to check
	  auto ok = context->lookup_builtin ("i32", &infered);
	  rust_assert (ok);
	}
	break;

	case HIR::Literal::LitType::BOOL: {
	  auto ok = context->lookup_builtin ("bool", &infered);
	  rust_assert (ok);
	}
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

  void visit (HIR::ComparisonExpr &expr)
  {
    auto lhs = TypeCheckExpr::Resolve (expr.get_lhs ());
    auto rhs = TypeCheckExpr::Resolve (expr.get_rhs ());

    infered = lhs->combine (rhs);
    // FIXME this will need to turn into bool
  }

  void visit (HIR::LazyBooleanExpr &expr)
  {
    auto lhs = TypeCheckExpr::Resolve (expr.get_lhs ());
    auto rhs = TypeCheckExpr::Resolve (expr.get_rhs ());

    infered = lhs->combine (rhs);
    // FIXME this will need to turn into bool
  }

  void visit (HIR::IfExpr &expr)
  {
    TypeCheckExpr::Resolve (expr.get_if_condition ());
    TypeCheckExpr::Resolve (expr.get_if_block ());
  }

  void visit (HIR::IfExprConseqElse &expr)
  {
    TypeCheckExpr::Resolve (expr.get_if_condition ());
    TypeCheckExpr::Resolve (expr.get_if_block ());
    TypeCheckExpr::Resolve (expr.get_else_block ());
  }

  void visit (HIR::IfExprConseqIf &expr)
  {
    TypeCheckExpr::Resolve (expr.get_if_condition ());
    TypeCheckExpr::Resolve (expr.get_if_block ());
    TypeCheckExpr::Resolve (expr.get_conseq_if_expr ());
  }

  void visit (HIR::BlockExpr &expr);

  void visit (HIR::ArrayIndexExpr &expr)
  {
    // check the index
    TyTy::IntType size_ty (expr.get_index_expr ()->get_mappings ().get_hirid (),
			   TyTy::IntType::I32);
    auto resolved
      = size_ty.combine (TypeCheckExpr::Resolve (expr.get_index_expr ()));
    context->insert_type (expr.get_index_expr ()->get_mappings ().get_hirid (),
			  resolved);

    expr.get_array_expr ()->accept_vis (*this);
    rust_assert (infered != nullptr);

    // extract the element type out now from the base type
    infered = TyTyExtractorArray::ExtractElementTypeFromArray (infered);
  }

  void visit (HIR::ArrayExpr &expr)
  {
    HIR::ArrayElems *elements = expr.get_internal_elements ();
    size_t num_elems = elements->get_num_elements ();

    elements->accept_vis (*this);
    rust_assert (infered_array_elems != nullptr);

    infered = new TyTy::ArrayType (expr.get_mappings ().get_hirid (), num_elems,
				   infered_array_elems);
  }

  void visit (HIR::ArrayElemsValues &elems)
  {
    std::vector<TyTy::TyBase *> types;
    elems.iterate ([&] (HIR::Expr *e) mutable -> bool {
      types.push_back (TypeCheckExpr::Resolve (e));
      return true;
    });

    infered_array_elems = types[0];
    for (size_t i = 1; i < types.size (); i++)
      {
	infered_array_elems = infered_array_elems->combine (types.at (i));
      }
  }

  void visit (HIR::ArrayElemsCopied &elems)
  {
    infered_array_elems = TypeCheckExpr::Resolve (elems.get_elem_to_copy ());
  }

  void visit (HIR::StructExprStructFields &struct_expr)
  {
    infered = TypeCheckStructExpr::Resolve (&struct_expr);
  }

private:
  TypeCheckExpr ()
    : TypeCheckBase (), infered (nullptr), infered_array_elems (nullptr)
  {}

  TyTy::TyBase *infered;
  TyTy::TyBase *infered_array_elems;
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_HIR_TYPE_CHECK_EXPR
