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
				 self, std::move (substitutions), locus);
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
  TyTy::BaseType *ty
    = new TyTy::PlaceholderType (type.get_name (),
				 type.get_mappings ().get_hirid ());
  context->insert_type (type.get_mappings (), ty);
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

void
TraitItemReference::associated_type_set (TyTy::BaseType *ty)
{
  rust_assert (get_trait_item_type () == TraitItemType::TYPE);

  TyTy::BaseType *item_ty = get_tyty ();
  rust_assert (item_ty->get_kind () == TyTy::TypeKind::PLACEHOLDER);
  TyTy::PlaceholderType *placeholder
    = static_cast<TyTy::PlaceholderType *> (item_ty);

  placeholder->set_associated_type (ty->get_ref ());
}

void
TraitItemReference::associated_type_reset ()
{
  rust_assert (get_trait_item_type () == TraitItemType::TYPE);

  TyTy::BaseType *item_ty = get_tyty ();
  rust_assert (item_ty->get_kind () == TyTy::TypeKind::PLACEHOLDER);
  TyTy::PlaceholderType *placeholder
    = static_cast<TyTy::PlaceholderType *> (item_ty);

  placeholder->clear_associated_type ();
}

void
AssociatedImplTrait::setup_associated_types ()
{
  ImplTypeIterator iter (*impl, [&] (HIR::TypeAlias &type) {
    TraitItemReference *resolved_trait_item = nullptr;
    bool ok = trait->lookup_trait_item (type.get_new_type_name (),
					&resolved_trait_item);
    if (!ok)
      return;
    if (resolved_trait_item->get_trait_item_type ()
	!= TraitItemReference::TraitItemType::TYPE)
      return;

    TyTy::BaseType *lookup;
    if (!context->lookup_type (type.get_mappings ().get_hirid (), &lookup))
      return;

    resolved_trait_item->associated_type_set (lookup);
  });
  iter.go ();
}

void
AssociatedImplTrait::reset_associated_types ()
{
  trait->clear_associated_types ();
}

Analysis::NodeMapping
TraitItemReference::get_parent_trait_mappings () const
{
  auto mappings = Analysis::Mappings::get ();

  HIR::Trait *trait
    = mappings->lookup_trait_item_mapping (get_mappings ().get_hirid ());
  rust_assert (trait != nullptr);

  return trait->get_mappings ();
}

TyTy::BaseType *
AssociatedImplTrait::get_projected_type (
  const TraitItemReference *trait_item_ref, TyTy::BaseType *receiver, HirId ref,
  HIR::GenericArgs &trait_generics, Location expr_locus)
{
  TyTy::BaseType *trait_item_tyty = trait_item_ref->get_tyty ()->clone ();

  // we can substitute the Self with the receiver here
  if (trait_item_tyty->get_kind () == TyTy::TypeKind::FNDEF)
    {
      TyTy::FnType *fn = static_cast<TyTy::FnType *> (trait_item_tyty);
      TyTy::SubstitutionParamMapping *param = nullptr;
      for (auto &param_mapping : fn->get_substs ())
	{
	  const HIR::TypeParam &type_param = param_mapping.get_generic_param ();
	  if (type_param.get_type_representation ().compare ("Self") == 0)
	    {
	      param = &param_mapping;
	      break;
	    }
	}
      rust_assert (param != nullptr);

      std::vector<TyTy::SubstitutionArg> mappings;
      mappings.push_back (TyTy::SubstitutionArg (param, receiver->clone ()));

      TyTy::SubstitutionArgumentMappings args (std::move (mappings),
					       expr_locus);
      trait_item_tyty = SubstMapperInternal::Resolve (trait_item_tyty, args);
    }

  if (!trait_generics.is_empty ())
    {
      trait_item_tyty
	= SubstMapper::Resolve (trait_item_tyty, expr_locus, &trait_generics);
    }

  return trait_item_tyty;
}

// rust-hir-path-probe.h

void
PathProbeImplTrait::process_trait_impl_items_for_candidates ()
{
  mappings->iterate_impl_items (
    [&] (HirId id, HIR::ImplItem *item, HIR::ImplBlock *impl) mutable -> bool {
      // just need to check if this is an impl block for this trait the next
      // function checks the receiver
      if (!impl->has_trait_ref ())
	return true;

      TraitReference *resolved
	= TraitResolver::Lookup (*(impl->get_trait_ref ().get ()));
      if (!trait_reference->is_equal (*resolved))
	return true;

      process_impl_item_candidate (id, item, impl);
      return true;
    });
}

} // namespace Resolver
} // namespace Rust
