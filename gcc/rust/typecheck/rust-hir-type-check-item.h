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

namespace Rust {
namespace Resolver {

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
    TyTy::TyBase *lookup;
    if (!context->lookup_type (function.get_mappings ().get_hirid (), &lookup))
      {
	rust_error_at (function.locus, "failed to lookup function type");
	return;
      }

    if (lookup->get_kind () != TyTy::TypeKind::FNDEF)
      {
	rust_error_at (function.get_locus (),
		       "found invalid type for function [%s]",
		       lookup->as_string ().c_str ());
	return;
      }

    // need to get the return type from this
    TyTy::FnType *resolve_fn_type = (TyTy::FnType *) lookup;
    auto expected_ret_tyty = resolve_fn_type->return_type ();
    context->push_return_type (expected_ret_tyty);

    TypeCheckExpr::Resolve (function.function_body.get ());
    if (function.function_body->has_expr ())
      {
	auto resolved
	  = TypeCheckExpr::Resolve (function.function_body->expr.get ());

	auto ret_resolved = expected_ret_tyty->combine (resolved);
	if (ret_resolved == nullptr)
	  {
	    rust_error_at (function.function_body->expr->get_locus_slow (),
			   "failed to resolve final expression");
	    return;
	  }

	context->peek_return_type ()->append_reference (
	  ret_resolved->get_ref ());
      }

    context->pop_return_type ();
  }

private:
  TypeCheckItem () : TypeCheckBase () {}
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_HIR_TYPE_CHECK_ITEM
