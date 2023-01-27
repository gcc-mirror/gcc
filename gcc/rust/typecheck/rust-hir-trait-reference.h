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

#ifndef RUST_HIR_TRAIT_REF_H
#define RUST_HIR_TRAIT_REF_H

#include "rust-hir-full.h"
#include "rust-tyty-visitor.h"
#include "rust-hir-type-check-util.h"

namespace Rust {
namespace Resolver {

// Data Objects for the associated trait items in a structure we can work with
// https://doc.rust-lang.org/edition-guide/rust-2018/trait-system/associated-constants.html
class TypeCheckContext;
class TraitItemReference
{
public:
  enum TraitItemType
  {
    FN,
    CONST,
    TYPE,
    ERROR
  };

  TraitItemReference (std::string identifier, bool optional, TraitItemType type,
		      HIR::TraitItem *hir_trait_item, TyTy::BaseType *self,
		      std::vector<TyTy::SubstitutionParamMapping> substitutions,
		      Location locus);

  TraitItemReference (TraitItemReference const &other);

  TraitItemReference &operator= (TraitItemReference const &other);

  static TraitItemReference error ()
  {
    return TraitItemReference ("", false, ERROR, nullptr, nullptr, {},
			       Location ());
  }

  static TraitItemReference &error_node ()
  {
    static TraitItemReference error = TraitItemReference::error ();
    return error;
  }

  bool is_error () const { return type == ERROR; }

  std::string as_string () const
  {
    return "(" + trait_item_type_as_string (type) + " " + identifier + " "
	   + ")";
  }

  static std::string trait_item_type_as_string (TraitItemType ty)
  {
    switch (ty)
      {
      case FN:
	return "FN";
      case CONST:
	return "CONST";
      case TYPE:
	return "TYPE";
      case ERROR:
	return "ERROR";
      }
    return "ERROR";
  }

  bool is_optional () const { return optional_flag; }

  std::string get_identifier () const { return identifier; }

  TraitItemType get_trait_item_type () const { return type; }

  HIR::TraitItem *get_hir_trait_item () const { return hir_trait_item; }

  Location get_locus () const { return locus; }

  const Analysis::NodeMapping get_mappings () const
  {
    return hir_trait_item->get_mappings ();
  }

  TyTy::BaseType *get_tyty () const
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

    gcc_unreachable ();
    return get_error ();
  }

  Analysis::NodeMapping get_parent_trait_mappings () const;

  // this is called when the trait is completed resolution and gives the items a
  // chance to run their specific type resolution passes. If we call their
  // resolution on construction it can lead to a case where the trait being
  // resolved recursively trying to resolve the trait itself infinitely since
  // the trait will not be stored in its own map yet
  void on_resolved ();

  void associated_type_set (TyTy::BaseType *ty) const;

  void associated_type_reset () const;

  bool is_object_safe () const;

private:
  TyTy::ErrorType *get_error () const
  {
    return new TyTy::ErrorType (get_mappings ().get_hirid ());
  }

  TyTy::BaseType *get_type_from_typealias (/*const*/
					   HIR::TraitItemType &type) const;

  TyTy::BaseType *
  get_type_from_constant (/*const*/ HIR::TraitItemConst &constant) const;

  TyTy::BaseType *get_type_from_fn (/*const*/ HIR::TraitItemFunc &fn) const;

  bool is_item_resolved () const;
  void resolve_item (HIR::TraitItemType &type);
  void resolve_item (HIR::TraitItemConst &constant);
  void resolve_item (HIR::TraitItemFunc &func);

  std::string identifier;
  bool optional_flag;
  TraitItemType type;
  HIR::TraitItem *hir_trait_item;
  std::vector<TyTy::SubstitutionParamMapping> inherited_substitutions;
  Location locus;

  TyTy::BaseType
    *self; // this is the implict Self TypeParam required for methods
  Resolver::TypeCheckContext *context;
};

// this wraps up the HIR::Trait so we can do analysis on it

class TraitReference
{
public:
  TraitReference (const HIR::Trait *hir_trait_ref,
		  std::vector<TraitItemReference> item_refs,
		  std::vector<const TraitReference *> super_traits,
		  std::vector<TyTy::SubstitutionParamMapping> substs)
    : hir_trait_ref (hir_trait_ref), item_refs (item_refs),
      super_traits (super_traits)
  {
    trait_substs.clear ();
    trait_substs.reserve (substs.size ());
    for (const auto &p : substs)
      trait_substs.push_back (p.clone ());
  }

  TraitReference (TraitReference const &other)
    : hir_trait_ref (other.hir_trait_ref), item_refs (other.item_refs),
      super_traits (other.super_traits)
  {
    trait_substs.clear ();
    trait_substs.reserve (other.trait_substs.size ());
    for (const auto &p : other.trait_substs)
      trait_substs.push_back (p.clone ());
  }

  TraitReference &operator= (TraitReference const &other)
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

  TraitReference (TraitReference &&other) = default;
  TraitReference &operator= (TraitReference &&other) = default;

  static TraitReference error ()
  {
    return TraitReference (nullptr, {}, {}, {});
  }

  bool is_error () const { return hir_trait_ref == nullptr; }

  static TraitReference &error_node ()
  {
    static TraitReference trait_error_node = TraitReference::error ();
    return trait_error_node;
  }

  Location get_locus () const { return hir_trait_ref->get_locus (); }

  std::string get_name () const
  {
    rust_assert (!is_error ());
    return hir_trait_ref->get_name ();
  }

  std::string as_string () const
  {
    if (is_error ())
      return "<trait-ref-error-node>";

    std::string item_buf;
    for (auto &item : item_refs)
      {
	item_buf += item.as_string () + ", ";
      }
    return "HIR Trait: " + get_name () + "->"
	   + hir_trait_ref->get_mappings ().as_string () + " [" + item_buf
	   + "]";
  }

  const HIR::Trait *get_hir_trait_ref () const { return hir_trait_ref; }

  const Analysis::NodeMapping &get_mappings () const
  {
    return hir_trait_ref->get_mappings ();
  }

  DefId get_defid () const { return get_mappings ().get_defid (); }

  bool lookup_hir_trait_item (const HIR::TraitItem &item,
			      TraitItemReference **ref)
  {
    return lookup_trait_item (item.trait_identifier (), ref);
  }

  bool lookup_trait_item (const std::string &ident, TraitItemReference **ref)
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

  bool lookup_trait_item_by_type (const std::string &ident,
				  TraitItemReference::TraitItemType type,
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

  bool lookup_trait_item_by_type (const std::string &ident,
				  TraitItemReference::TraitItemType type,
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

  bool lookup_hir_trait_item (const HIR::TraitItem &item,
			      const TraitItemReference **ref) const
  {
    return lookup_trait_item (item.trait_identifier (), ref);
  }

  bool lookup_trait_item (const std::string &ident,
			  const TraitItemReference **ref) const
  {
    for (auto &item : item_refs)
      {
	if (ident.compare (item.get_identifier ()) == 0)
	  {
	    *ref = &item;
	    return true;
	  }
      }

    // lookup super traits
    for (const auto &super_trait : super_traits)
      {
	bool found = super_trait->lookup_trait_item (ident, ref);
	if (found)
	  return true;
      }

    return false;
  }

  const TraitItemReference *
  lookup_trait_item (const std::string &ident,
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
	  = super_trait->lookup_trait_item (ident, type);
	if (!res->is_error ())
	  return res;
      }

    return &TraitItemReference::error_node ();
  }

  size_t size () const { return item_refs.size (); }

  const std::vector<TraitItemReference> &get_trait_items () const
  {
    return item_refs;
  }

  void get_trait_items_and_supers (
    std::vector<const TraitItemReference *> &result) const
  {
    for (const auto &item : item_refs)
      result.push_back (&item);

    for (const auto &super_trait : super_traits)
      super_trait->get_trait_items_and_supers (result);
  }

  void on_resolved ()
  {
    for (auto &item : item_refs)
      {
	item.on_resolved ();
      }
  }

  void clear_associated_types ()
  {
    for (auto &item : item_refs)
      {
	bool is_assoc_type = item.get_trait_item_type ()
			     == TraitItemReference::TraitItemType::TYPE;
	if (is_assoc_type)
	  item.associated_type_reset ();
      }
  }

  bool is_equal (const TraitReference &other) const
  {
    DefId this_id = get_mappings ().get_defid ();
    DefId other_id = other.get_mappings ().get_defid ();
    return this_id == other_id;
  }

  const std::vector<const TraitReference *> get_super_traits () const
  {
    return super_traits;
  }

  bool is_object_safe (bool emit_error, Location locus) const
  {
    // https: // doc.rust-lang.org/reference/items/traits.html#object-safety
    std::vector<const TraitReference *> non_object_super_traits;
    for (auto &item : super_traits)
      {
	if (!item->is_object_safe (false, Location ()))
	  non_object_super_traits.push_back (item);
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
	RichLocation r (locus);
	for (auto &item : non_object_super_traits)
	  r.add_range (item->get_locus ());
	for (auto &item : non_object_safe_items)
	  r.add_range (item->get_locus ());

	rust_error_at (r, "trait bound is not object safe");
      }

    return is_safe;
  }

  bool trait_has_generics () const { return !trait_substs.empty (); }

  std::vector<TyTy::SubstitutionParamMapping> get_trait_substs () const
  {
    return trait_substs;
  }

  bool satisfies_bound (const TraitReference &reference) const
  {
    if (is_equal (reference))
      return true;

    for (const auto &super_trait : super_traits)
      {
	if (super_trait->satisfies_bound (reference))
	  return true;
      }

    return false;
  }

private:
  const HIR::Trait *hir_trait_ref;
  std::vector<TraitItemReference> item_refs;
  std::vector<const TraitReference *> super_traits;
  std::vector<TyTy::SubstitutionParamMapping> trait_substs;
};

class AssociatedImplTrait
{
public:
  AssociatedImplTrait (TraitReference *trait, HIR::ImplBlock *impl,
		       TyTy::BaseType *self,
		       Resolver::TypeCheckContext *context)
    : trait (trait), impl (impl), self (self), context (context)
  {}

  TraitReference *get_trait () { return trait; }

  HIR::ImplBlock *get_impl_block () { return impl; }

  TyTy::BaseType *get_self () { return self; }

  TyTy::BaseType *
  setup_associated_types (const TyTy::BaseType *self,
			  const TyTy::TypeBoundPredicate &bound);

  void reset_associated_types ();

private:
  TraitReference *trait;
  HIR::ImplBlock *impl;
  TyTy::BaseType *self;
  Resolver::TypeCheckContext *context;
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_HIR_TRAIT_REF_H
