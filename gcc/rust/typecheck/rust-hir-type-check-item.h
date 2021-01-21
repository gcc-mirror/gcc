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

#ifndef RUST_HIR_TYPE_CHECK_ITEM
#define RUST_HIR_TYPE_CHECK_ITEM

#include "rust-hir-type-check-base.h"
#include "rust-hir-full.h"
#include "rust-hir-type-check-type.h"
#include "rust-hir-type-check-stmt.h"
#include "rust-tyty-visitor.h"
#include "rust-tyty-resolver.h"

namespace Rust {
namespace Resolver {

class ResolveFnType : public TyTy::TyVisitor
{
public:
  ResolveFnType (TyTy::TyBase *base) : base (base), state (nullptr) {}

  TyTy::TyBase *go ()
  {
    base->accept_vis (*this);
    if (state == nullptr)
      gcc_unreachable ();

    return state;
  }

  void visit (TyTy::UnitType &type) override { gcc_unreachable (); }
  void visit (TyTy::InferType &type) override { gcc_unreachable (); }
  void visit (TyTy::StructFieldType &type) override { gcc_unreachable (); }
  void visit (TyTy::ADTType &type) override { gcc_unreachable (); }
  void visit (TyTy::ArrayType &type) override { gcc_unreachable (); }
  void visit (TyTy::BoolType &type) override { gcc_unreachable (); }
  void visit (TyTy::IntType &type) override { gcc_unreachable (); }
  void visit (TyTy::UintType &type) override { gcc_unreachable (); }
  void visit (TyTy::FloatType &type) override { gcc_unreachable (); }
  void visit (TyTy::ErrorType &type) override { gcc_unreachable (); }

  void visit (TyTy::FnType &type) override { state = type.return_type (); }

private:
  TyTy::TyBase *base;
  TyTy::TyBase *state;
};

class TypeCheckItem : public TypeCheckBase
{
public:
  static void Resolve (HIR::Item *item)
  {
    TypeCheckItem resolver;
    item->accept_vis (resolver);
  }

  void visit (HIR::Function &function)
  {
    TyTy::TyBase *fnType;
    if (!context->lookup_type (function.get_mappings ().get_hirid (), &fnType))
      {
	rust_error_at (function.locus, "failed to lookup function type");
	return;
      }

    // need to get the return type from this
    ResolveFnType resolve_fn_type (fnType);
    context->push_return_type (resolve_fn_type.go ());

    TypeCheckExpr::Resolve (function.function_body.get ());
    if (function.function_body->has_expr ())
      {
	auto resolved
	  = TypeCheckExpr::Resolve (function.function_body->expr.get ());
	context->peek_return_type ()->combine (resolved);
      }

    context->pop_return_type ();
  }

private:
  TypeCheckItem () : TypeCheckBase () {}
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_HIR_TYPE_CHECK_ITEM
