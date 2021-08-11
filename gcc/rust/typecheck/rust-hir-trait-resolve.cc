// Copyright (C) 2021 Free Software Foundation, Inc.

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

#include "rust-hir-trait-resolve.h"
#include "rust-hir-type-check-expr.h"

namespace Rust {
namespace Resolver {

void
ResolveTraitItemToRef::visit (HIR::TraitItemType &type)
{
  TyTy::BaseType *ty
    = new TyTy::PlaceholderType (type.get_mappings ().get_hirid ());
  context->insert_type (type.get_mappings (), ty);

  // create trait-item-ref
  Location locus = type.get_locus ();
  bool is_optional = false;
  std::string identifier = type.get_name ();

  resolved = TraitItemReference (identifier, is_optional,
				 TraitItemReference::TraitItemType::TYPE, &type,
				 self, substitutions, locus);
}

void
ResolveTraitItemToRef::visit (HIR::TraitItemConst &cst)
{
  // create trait-item-ref
  Location locus = cst.get_locus ();
  bool is_optional = cst.has_expr ();
  std::string identifier = cst.get_name ();

  resolved = TraitItemReference (identifier, is_optional,
				 TraitItemReference::TraitItemType::CONST, &cst,
				 self, substitutions, locus);
}

void
ResolveTraitItemToRef::visit (HIR::TraitItemFunc &fn)
{
  // create trait-item-ref
  Location locus = fn.get_locus ();
  bool is_optional = fn.has_block_defined ();
  std::string identifier = fn.get_decl ().get_function_name ();

  resolved = TraitItemReference (identifier, is_optional,
				 TraitItemReference::TraitItemType::FN, &fn,
				 self, substitutions, locus);
}

// TraitItemReference items

void
TraitItemReference::on_resolved ()
{
  switch (type)
    {
    case CONST:
      resolve_item (static_cast<HIR::TraitItemConst &> (*hir_trait_item));
      break;

    case TYPE:
      resolve_item (static_cast<HIR::TraitItemType &> (*hir_trait_item));
      break;

    case FN:
      resolve_item (static_cast<HIR::TraitItemFunc &> (*hir_trait_item));
      break;

    default:
      break;
    }
}

void
TraitItemReference::resolve_item (HIR::TraitItemType &type)
{
  // TODO
}

void
TraitItemReference::resolve_item (HIR::TraitItemConst &constant)
{
  // TODO
}

void
TraitItemReference::resolve_item (HIR::TraitItemFunc &func)
{
  if (!is_optional ())
    return;

  TyTy::BaseType *item_tyty = get_tyty ();
  if (item_tyty->get_kind () == TyTy::TypeKind::ERROR)
    return;

  // check the block and return types
  rust_assert (item_tyty->get_kind () == TyTy::TypeKind::FNDEF);

  // need to get the return type from this
  TyTy::FnType *resolved_fn_type = static_cast<TyTy::FnType *> (item_tyty);
  auto expected_ret_tyty = resolved_fn_type->get_return_type ();
  context->push_return_type (expected_ret_tyty);

  auto block_expr_ty
    = TypeCheckExpr::Resolve (func.get_block_expr ().get (), false);

  context->pop_return_type ();

  if (block_expr_ty->get_kind () != TyTy::NEVER)
    expected_ret_tyty->unify (block_expr_ty);
}

} // namespace Resolver
} // namespace Rust
