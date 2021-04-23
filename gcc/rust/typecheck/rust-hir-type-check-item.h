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
#include "rust-hir-type-check-implitem.h"
#include "rust-hir-type-check-type.h"
#include "rust-hir-type-check-stmt.h"
#include "rust-tyty-visitor.h"

namespace Rust {
namespace Resolver {

class TypeCheckItem : public TypeCheckBase
{
  using Rust::Resolver::TypeCheckBase::visit;

public:
  static void Resolve (HIR::Item *item)
  {
    TypeCheckItem resolver;
    item->accept_vis (resolver);
  }

  void visit (HIR::InherentImpl &impl_block) override
  {
    TyTy::BaseType *self = nullptr;
    if (!context->lookup_type (
	  impl_block.get_type ()->get_mappings ().get_hirid (), &self))
      {
	rust_error_at (impl_block.get_locus (),
		       "failed to resolve Self for InherentImpl");
	return;
      }

    for (auto &impl_item : impl_block.get_impl_items ())
      TypeCheckImplItem::Resolve (impl_item.get (), self);
  }

  void visit (HIR::Function &function) override
  {
    TyTy::BaseType *lookup;
    if (!context->lookup_type (function.get_mappings ().get_hirid (), &lookup))
      {
	rust_error_at (function.get_locus (), "failed to lookup function type");
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
    TyTy::FnType *resolved_fn_type = static_cast<TyTy::FnType *> (lookup);
    auto expected_ret_tyty = resolved_fn_type->get_return_type ();
    context->push_return_type (expected_ret_tyty);

    auto block_expr_ty
      = TypeCheckExpr::Resolve (function.get_definition ().get (), false);

    context->pop_return_type ();

    if (block_expr_ty->get_kind () != TyTy::NEVER)
      expected_ret_tyty->unify (block_expr_ty);
  }

private:
  TypeCheckItem () : TypeCheckBase () {}
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_HIR_TYPE_CHECK_ITEM
