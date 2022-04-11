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
	{
	  if (!impl_type->can_eq (receiver, false))
	    return true;
	}

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

TyTy::TypeBoundPredicate
TypeCheckBase::get_predicate_from_bound (HIR::TypePath &type_path)
{
  TraitReference *trait = resolve_trait_path (type_path);
  if (trait->is_error ())
    return TyTy::TypeBoundPredicate::error ();

  TyTy::TypeBoundPredicate predicate (*trait, type_path.get_locus ());
  HIR::GenericArgs args
    = HIR::GenericArgs::create_empty (type_path.get_locus ());

  auto &final_seg = type_path.get_final_segment ();
  if (final_seg->is_generic_segment ())
    {
      auto final_generic_seg
	= static_cast<HIR::TypePathSegmentGeneric *> (final_seg.get ());
      if (final_generic_seg->has_generic_args ())
	{
	  args = final_generic_seg->get_generic_args ();
	}
    }

  if (predicate.requires_generic_args ())
    {
      // this is applying generic arguments to a trait reference
      predicate.apply_generic_arguments (&args);
    }

  return predicate;
}

} // namespace Resolver

namespace TyTy {

TypeBoundPredicate::TypeBoundPredicate (
  const Resolver::TraitReference &trait_reference, Location locus)
  : SubstitutionRef (trait_reference.get_trait_substs (),
		     SubstitutionArgumentMappings::error ()),
    reference (trait_reference.get_mappings ().get_defid ()), locus (locus),
    args (HIR::GenericArgs::create_empty ()), error_flag (false)
{}

TypeBoundPredicate::TypeBoundPredicate (
  DefId reference, std::vector<SubstitutionParamMapping> substitutions,
  Location locus)
  : SubstitutionRef (std::move (substitutions),
		     SubstitutionArgumentMappings::error ()),
    reference (reference), locus (locus),
    args (HIR::GenericArgs::create_empty ()), error_flag (false)
{}

TypeBoundPredicate::TypeBoundPredicate (const TypeBoundPredicate &other)
  : SubstitutionRef ({}, other.used_arguments), reference (other.reference),
    locus (other.locus), args (other.args), error_flag (other.error_flag)
{
  substitutions.clear ();
  if (!other.is_error ())
    {
      for (const auto &p : other.get_substs ())
	substitutions.push_back (p.clone ());
    }
}

TypeBoundPredicate &
TypeBoundPredicate::operator= (const TypeBoundPredicate &other)
{
  reference = other.reference;
  locus = other.locus;
  args = other.args;
  error_flag = other.error_flag;
  used_arguments = other.used_arguments;

  substitutions.clear ();
  if (!other.is_error ())
    {
      for (const auto &p : other.get_substs ())
	substitutions.push_back (p.clone ());
    }

  return *this;
}

TypeBoundPredicate
TypeBoundPredicate::error ()
{
  auto p = TypeBoundPredicate (UNKNOWN_DEFID, {}, Location ());
  p.error_flag = true;
  return p;
}

std::string
TypeBoundPredicate::as_string () const
{
  return get ()->as_string () + subst_as_string ();
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
  // we need to get the substitutions argument mappings but also remember that
  // we have an implicit Self argument which we must be careful to respect
  rust_assert (used_arguments.is_empty ());
  rust_assert (!substitutions.empty ());

  // we setup a dummy implict self argument
  SubstitutionArg placeholder_self (&substitutions.front (), nullptr);
  used_arguments.get_mappings ().push_back (std::move (placeholder_self));

  // now actually perform a substitution
  used_arguments = get_mappings_from_generic_args (*generic_args);
  error_flag |= used_arguments.is_error ();
  args = *generic_args;
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
bool
TypeBoundPredicate::is_error () const
{
  auto context = Resolver::TypeCheckContext::get ();

  Resolver::TraitReference *ref = nullptr;
  bool ok = context->lookup_trait_reference (reference, &ref);

  return !ok || error_flag;
}

BaseType *
TypeBoundPredicate::handle_substitions (SubstitutionArgumentMappings mappings)
{
  gcc_unreachable ();
  return nullptr;
}

bool
TypeBoundPredicate::requires_generic_args () const
{
  if (is_error ())
    return false;

  return substitutions.size () > 1 && args.is_empty ();
}

// trait item reference

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

// TypeBoundsMappings

TypeBoundsMappings::TypeBoundsMappings (
  std::vector<TypeBoundPredicate> specified_bounds)
  : specified_bounds (specified_bounds)
{}

std::vector<TypeBoundPredicate> &
TypeBoundsMappings::get_specified_bounds ()
{
  return specified_bounds;
}

const std::vector<TypeBoundPredicate> &
TypeBoundsMappings::get_specified_bounds () const
{
  return specified_bounds;
}

size_t
TypeBoundsMappings::num_specified_bounds () const
{
  return specified_bounds.size ();
}

std::string
TypeBoundsMappings::raw_bounds_as_string () const
{
  std::string buf;
  for (size_t i = 0; i < specified_bounds.size (); i++)
    {
      const TypeBoundPredicate &b = specified_bounds.at (i);
      bool has_next = (i + 1) < specified_bounds.size ();
      buf += b.get_name () + (has_next ? " + " : "");
    }
  return buf;
}

std::string
TypeBoundsMappings::bounds_as_string () const
{
  return "bounds:[" + raw_bounds_as_string () + "]";
}

void
TypeBoundsMappings::add_bound (TypeBoundPredicate predicate)
{
  specified_bounds.push_back (predicate);
}

} // namespace TyTy
} // namespace Rust
