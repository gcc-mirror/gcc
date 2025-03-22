// Copyright (C) 2020-2025 Free Software Foundation, Inc.

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

#include "rust-hir-trait-reference.h"

namespace Rust {
namespace Resolver {

std::string
TraitItemReference::as_string () const
{
  return "(" + trait_item_type_as_string (type) + " " + identifier + " " + ")";
}

bool
TraitItemReference::is_error () const
{
  return type == ERROR;
}

bool
TraitItemReference::is_optional () const
{
  return optional_flag;
}

std::string
TraitItemReference::get_identifier () const
{
  return identifier;
}

TraitItemReference::TraitItemType
TraitItemReference::get_trait_item_type () const
{
  return type;
}

HIR::TraitItem *
TraitItemReference::get_hir_trait_item () const
{
  return hir_trait_item;
}

location_t
TraitItemReference::get_locus () const
{
  return locus;
}

const Analysis::NodeMapping
TraitItemReference::get_mappings () const
{
  return hir_trait_item->get_mappings ();
}

TyTy::BaseType *
TraitItemReference::get_tyty () const
{
  rust_assert (hir_trait_item != nullptr);

  switch (type)
    {
    case CONST:
      return get_type_from_constant (
	static_cast</*const*/ HIR::TraitItemConst &> (*hir_trait_item));
      break;

    case TYPE:
      return get_type_from_typealias (
	static_cast</*const*/ HIR::TraitItemType &> (*hir_trait_item));

    case FN:
      return get_type_from_fn (
	static_cast</*const*/ HIR::TraitItemFunc &> (*hir_trait_item));
      break;

    default:
      return get_error ();
    }

  rust_unreachable ();
  return get_error ();
}

TyTy::ErrorType *
TraitItemReference::get_error () const
{
  return new TyTy::ErrorType (get_mappings ().get_hirid ());
}

TraitReference::TraitReference (
  const HIR::Trait *hir_trait_ref, std::vector<TraitItemReference> item_refs,
  std::vector<TyTy::TypeBoundPredicate> super_traits,
  std::vector<TyTy::SubstitutionParamMapping> substs)
  : hir_trait_ref (hir_trait_ref), item_refs (item_refs),
    super_traits (super_traits)
{
  trait_substs.clear ();
  trait_substs.reserve (substs.size ());
  for (const auto &p : substs)
    trait_substs.push_back (p.clone ());
}

TraitReference::TraitReference (TraitReference const &other)
  : hir_trait_ref (other.hir_trait_ref), item_refs (other.item_refs),
    super_traits (other.super_traits)
{
  trait_substs.clear ();
  trait_substs.reserve (other.trait_substs.size ());
  for (const auto &p : other.trait_substs)
    trait_substs.push_back (p.clone ());
}

TraitReference &
TraitReference::operator= (TraitReference const &other)
{
  hir_trait_ref = other.hir_trait_ref;
  item_refs = other.item_refs;
  super_traits = other.super_traits;

  trait_substs.clear ();
  trait_substs.reserve (other.trait_substs.size ());
  for (const auto &p : other.trait_substs)
    trait_substs.push_back (p.clone ());

  return *this;
}

bool
TraitReference::is_error () const
{
  return hir_trait_ref == nullptr;
}

location_t
TraitReference::get_locus () const
{
  return hir_trait_ref->get_locus ();
}

std::string
TraitReference::get_name () const
{
  rust_assert (!is_error ());
  return hir_trait_ref->get_name ().as_string ();
}

std::string
TraitReference::as_string () const
{
  if (is_error ())
    return "<trait-ref-error-node>";

  std::string item_buf;
  for (auto &item : item_refs)
    {
      item_buf += item.as_string () + ", ";
    }
  return "HIR Trait: " + get_name () + "->"
	 + hir_trait_ref->get_mappings ().as_string () + " [" + item_buf + "]";
}

const HIR::Trait *
TraitReference::get_hir_trait_ref () const
{
  return hir_trait_ref;
}

const Analysis::NodeMapping &
TraitReference::get_mappings () const
{
  return hir_trait_ref->get_mappings ();
}

DefId
TraitReference::get_defid () const
{
  return get_mappings ().get_defid ();
}

bool
TraitReference::lookup_hir_trait_item (const HIR::TraitItem &item,
				       TraitItemReference **ref)
{
  return lookup_trait_item (item.trait_identifier (), ref);
}

bool
TraitReference::lookup_trait_item (const std::string &ident,
				   TraitItemReference **ref)
{
  for (auto &item : item_refs)
    {
      if (ident.compare (item.get_identifier ()) == 0)
	{
	  *ref = &item;
	  return true;
	}
    }
  return false;
}

bool
TraitReference::lookup_trait_item_by_type (
  const std::string &ident, TraitItemReference::TraitItemType type,
  TraitItemReference **ref)
{
  for (auto &item : item_refs)
    {
      if (item.get_trait_item_type () != type)
	continue;

      if (ident.compare (item.get_identifier ()) == 0)
	{
	  *ref = &item;
	  return true;
	}
    }
  return false;
}

bool
TraitReference::lookup_trait_item_by_type (
  const std::string &ident, TraitItemReference::TraitItemType type,
  const TraitItemReference **ref) const
{
  for (auto &item : item_refs)
    {
      if (item.get_trait_item_type () != type)
	continue;

      if (ident.compare (item.get_identifier ()) == 0)
	{
	  *ref = &item;
	  return true;
	}
    }
  return false;
}

bool
TraitReference::lookup_hir_trait_item (const HIR::TraitItem &item,
				       const TraitItemReference **ref) const
{
  return lookup_trait_item (item.trait_identifier (), ref);
}

bool
TraitReference::lookup_trait_item (const std::string &ident,
				   const TraitItemReference **ref,
				   bool lookup_supers) const
{
  for (auto &item : item_refs)
    {
      if (ident.compare (item.get_identifier ()) == 0)
	{
	  *ref = &item;
	  return true;
	}
    }

  if (!lookup_supers)
    return false;

  // lookup super traits
  for (const auto &super_trait : super_traits)
    {
      bool found = super_trait.get ()->lookup_trait_item (ident, ref);
      if (found)
	return true;
    }

  return false;
}

const TraitItemReference *
TraitReference::lookup_trait_item (const std::string &ident,
				   TraitItemReference::TraitItemType type) const
{
  for (auto &item : item_refs)
    {
      if (item.get_trait_item_type () != type)
	continue;

      if (ident.compare (item.get_identifier ()) == 0)
	return &item;
    }

  // lookup super traits
  for (const auto &super_trait : super_traits)
    {
      const TraitItemReference *res
	= super_trait.get ()->lookup_trait_item (ident, type);
      if (!res->is_error ())
	return res;
    }

  return &TraitItemReference::error_node ();
}

size_t
TraitReference::size () const
{
  return item_refs.size ();
}

const std::vector<TraitItemReference> &
TraitReference::get_trait_items () const
{
  return item_refs;
}

void
TraitReference::get_trait_items_and_supers (
  std::vector<const TraitItemReference *> &result) const
{
  for (const auto &item : item_refs)
    result.push_back (&item);

  for (const auto &super_trait : super_traits)
    super_trait.get ()->get_trait_items_and_supers (result);
}

void
TraitReference::on_resolved ()
{
  for (auto &item : item_refs)
    {
      item.on_resolved ();
    }
}

void
TraitReference::clear_associated_types () const
{
  for (const auto &item : item_refs)
    {
      bool is_assoc_type = item.get_trait_item_type ()
			   == TraitItemReference::TraitItemType::TYPE;
      if (is_assoc_type)
	item.associated_type_reset (false);
    }
}

void
TraitReference::clear_associated_type_projections () const
{
  for (const auto &item : item_refs)
    {
      bool is_assoc_type = item.get_trait_item_type ()
			   == TraitItemReference::TraitItemType::TYPE;
      if (is_assoc_type)
	item.associated_type_reset (true);
    }
}

bool
TraitReference::is_equal (const TraitReference &other) const
{
  DefId this_id = get_mappings ().get_defid ();
  DefId other_id = other.get_mappings ().get_defid ();
  return this_id == other_id;
}

std::vector<TyTy::TypeBoundPredicate>
TraitReference::get_super_traits () const
{
  return super_traits;
}

bool
TraitReference::is_object_safe (bool emit_error, location_t locus) const
{
  // https: // doc.rust-lang.org/reference/items/traits.html#object-safety
  std::vector<const TraitReference *> non_object_super_traits;
  for (auto &super_trait : super_traits)
    {
      if (!super_trait.get ()->is_object_safe (false, UNDEF_LOCATION))
	non_object_super_traits.push_back (super_trait.get ());
    }

  std::vector<const Resolver::TraitItemReference *> non_object_safe_items;
  for (auto &item : get_trait_items ())
    {
      if (!item.is_object_safe ())
	non_object_safe_items.push_back (&item);
    }

  bool is_safe
    = non_object_super_traits.empty () && non_object_safe_items.empty ();
  if (emit_error && !is_safe)
    {
      rich_location r (line_table, locus);
      for (auto &item : non_object_super_traits)
	r.add_range (item->get_locus ());
      for (auto &item : non_object_safe_items)
	r.add_range (item->get_locus ());

      rust_error_at (r, "trait bound is not object safe");
    }

  return is_safe;
}

bool
TraitReference::trait_has_generics () const
{
  return !trait_substs.empty ();
}

std::vector<TyTy::SubstitutionParamMapping>
TraitReference::get_trait_substs () const
{
  return trait_substs;
}

bool
TraitReference::satisfies_bound (const TraitReference &reference) const
{
  if (is_equal (reference))
    return true;

  for (const auto &super_trait : super_traits)
    {
      if (super_trait.get ()->satisfies_bound (reference))
	return true;
    }

  return false;
}

AssociatedImplTrait::AssociatedImplTrait (TraitReference *trait,
					  TyTy::TypeBoundPredicate predicate,
					  HIR::ImplBlock *impl,
					  TyTy::BaseType *self,
					  Resolver::TypeCheckContext *context)
  : trait (trait), predicate (predicate), impl (impl), self (self),
    context (context)
{}

TyTy::TypeBoundPredicate &
AssociatedImplTrait::get_predicate ()
{
  return predicate;
}

HIR::ImplBlock *
AssociatedImplTrait::get_impl_block ()
{
  return impl;
}

TyTy::BaseType *
AssociatedImplTrait::get_self ()
{
  return self;
}

const TyTy::BaseType *
AssociatedImplTrait::get_self () const
{
  return self;
}

} // namespace Resolver
} // namespace Rust
