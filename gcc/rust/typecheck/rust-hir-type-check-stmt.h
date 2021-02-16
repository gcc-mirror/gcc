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

#ifndef RUST_HIR_TYPE_CHECK_STMT
#define RUST_HIR_TYPE_CHECK_STMT

#include "rust-hir-type-check-base.h"
#include "rust-hir-full.h"
#include "rust-hir-type-check-type.h"
#include "rust-hir-type-check-expr.h"

namespace Rust {
namespace Resolver {

class TypeCheckStmt : public TypeCheckBase
{
public:
  static TyTy::BaseType *Resolve (HIR::Stmt *stmt, bool inside_loop)
  {
    TypeCheckStmt resolver (inside_loop);
    stmt->accept_vis (resolver);
    return resolver.infered;
  }

  void visit (HIR::ExprStmtWithBlock &stmt)
  {
    infered = TypeCheckExpr::Resolve (stmt.get_expr (), inside_loop);
  }

  void visit (HIR::ExprStmtWithoutBlock &stmt)
  {
    infered = TypeCheckExpr::Resolve (stmt.get_expr (), inside_loop);
  }

  void visit (HIR::LetStmt &stmt)
  {
    infered = new TyTy::UnitType (stmt.get_mappings ().get_hirid ());

    TyTy::BaseType *init_expr_ty = nullptr;
    if (stmt.has_init_expr ())
      {
	init_expr_ty
	  = TypeCheckExpr::Resolve (stmt.get_init_expr (), inside_loop);
	if (init_expr_ty == nullptr)
	  return;

	init_expr_ty = init_expr_ty->clone ();
	auto ref = init_expr_ty->get_ref ();
	init_expr_ty->set_ref (stmt.get_mappings ().get_hirid ());
	init_expr_ty->append_reference (ref);
      }

    TyTy::BaseType *specified_ty = nullptr;
    if (stmt.has_type ())
      specified_ty = TypeCheckType::Resolve (stmt.get_type ());

    // let x:i32 = 123;
    if (specified_ty != nullptr && init_expr_ty != nullptr)
      {
	auto unified_ty = specified_ty->unify (init_expr_ty);
	if (unified_ty->get_kind () == TyTy::TypeKind::ERROR)
	  {
	    rust_fatal_error (stmt.get_locus (),
			      "failure in setting up let stmt type");
	    return;
	  }

	context->insert_type (stmt.get_mappings (), unified_ty);
      }
    else
      {
	// let x:i32;
	if (specified_ty != nullptr)
	  {
	    context->insert_type (stmt.get_mappings (), specified_ty);
	  }
	// let x = 123;
	else if (init_expr_ty != nullptr)
	  {
	    context->insert_type (stmt.get_mappings (), init_expr_ty);
	  }
	// let x;
	else
	  {
	    context->insert_type (
	      stmt.get_mappings (),
	      new TyTy::InferType (stmt.get_mappings ().get_hirid (),
				   TyTy::InferType::InferTypeKind::GENERAL));
	  }
      }
  }

private:
  TypeCheckStmt (bool inside_loop)
    : TypeCheckBase (), infered (nullptr), inside_loop (inside_loop)
  {}

  TyTy::BaseType *infered;
  bool inside_loop;
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_HIR_TYPE_CHECK_STMT
