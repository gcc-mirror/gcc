// Copyright (C) 2021-2022 Free Software Foundation, Inc.

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

#include "rust-hir-type-bounds.h"
#include "rust-hir-trait-resolve.h"

namespace Rust {
namespace Resolver {

void
TypeBoundsProbe::scan ()
{
  std::vector<std::pair<HIR::TypePath *, HIR::ImplBlock *>>
    possible_trait_paths;
  mappings->iterate_impl_blocks (
    [&] (HirId id, HIR::ImplBlock *impl) mutable -> bool {
      // we are filtering for trait-impl-blocks
      if (!impl->has_trait_ref ())
	return true;

      TyTy::BaseType *impl_type = nullptr;
      bool ok
	= context->lookup_type (impl->get_type ()->get_mappings ().get_hirid (),
				&impl_type);
      if (!ok)
	return true;

      if (!receiver->can_eq (impl_type, false))
	return true;

      possible_trait_paths.push_back ({impl->get_trait_ref ().get (), impl});
      return true;
    });

  for (auto &path : possible_trait_paths)
    {
      HIR::TypePath *trait_path = path.first;
      TraitReference *trait_ref = TraitResolver::Resolve (*trait_path);

      if (!trait_ref->is_error ())
	trait_references.push_back ({trait_ref, path.second});
    }
}

TraitReference *
TypeCheckBase::resolve_trait_path (HIR::TypePath &path)
{
  return TraitResolver::Resolve (path);
}

} // namespace Resolver

namespace TyTy {

std::string
TypeBoundPredicate::as_string () const
{
  return get ()->as_string ()
	 + (has_generic_args ()
	      ? std::string ("<") + args->as_string () + std::string (">")
	      : "");
}

const Resolver::TraitReference *
TypeBoundPredicate::get () const
{
  auto context = Resolver::TypeCheckContext::get ();

  Resolver::TraitReference *ref = nullptr;
  bool ok = context->lookup_trait_reference (reference, &ref);
  rust_assert (ok);

  return ref;
}

std::string
TypeBoundPredicate::get_name () const
{
  auto mappings = Analysis::Mappings::get ();
  auto trait = get ();
  auto nodeid = trait->get_mappings ().get_nodeid ();

  const Resolver::CanonicalPath *p = nullptr;
  if (mappings->lookup_canonical_path (mappings->get_current_crate (), nodeid,
				       &p))
    return p->get ();

  return trait->get_name ();
}

bool
TypeBoundPredicate::is_object_safe (bool emit_error, Location locus) const
{
  const Resolver::TraitReference *trait = get ();
  rust_assert (trait != nullptr);
  return trait->is_object_safe (emit_error, locus);
}

void
TypeBoundPredicate::apply_generic_arguments (HIR::GenericArgs *generic_args)
{
  args = generic_args;
  // TODO verify these arguments are valid and not too many were added
}

bool
TypeBoundPredicate::contains_item (const std::string &search) const
{
  auto trait_ref = get ();
  const Resolver::TraitItemReference *trait_item_ref = nullptr;
  return trait_ref->lookup_trait_item (search, &trait_item_ref);
}

TypeBoundPredicateItem
TypeBoundPredicate::lookup_associated_item (const std::string &search) const
{
  auto trait_ref = get ();
  const Resolver::TraitItemReference *trait_item_ref = nullptr;
  if (!trait_ref->lookup_trait_item (search, &trait_item_ref))
    return TypeBoundPredicateItem::error ();

  return TypeBoundPredicateItem (this, trait_item_ref);
}

BaseType *
TypeBoundPredicateItem::get_tyty_for_receiver (
  const TyTy::BaseType *receiver, const HIR::GenericArgs *bound_args)
{
  TyTy::BaseType *trait_item_tyty = get_raw_item ()->get_tyty ();
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

      Location locus; // FIXME
      TyTy::SubstitutionArgumentMappings args (std::move (mappings), locus);
      trait_item_tyty
	= Resolver::SubstMapperInternal::Resolve (trait_item_tyty, args);
    }

  if (!parent->has_generic_args ())
    return trait_item_tyty;

  // FIXME LEAK this should really be const
  const HIR::GenericArgs *args
    = (bound_args != nullptr) ? bound_args : parent->get_generic_args ();
  HIR::GenericArgs *generic_args = new HIR::GenericArgs (*args);
  TyTy::BaseType *resolved
    = Resolver::SubstMapper::Resolve (trait_item_tyty, parent->get_locus (),
				      generic_args);

  return resolved;
}

const Resolver::TraitItemReference *
TypeBoundPredicateItem::get_raw_item () const
{
  return trait_item_ref;
}

bool
TypeBoundPredicateItem::needs_implementation () const
{
  return !get_raw_item ()->is_optional ();
}

} // namespace TyTy
} // namespace Rust
