// Copyright (C) 2021-2025 Free Software Foundation, Inc.

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
		      location_t locus);

  TraitItemReference (TraitItemReference const &other);

  TraitItemReference &operator= (TraitItemReference const &other);

  static TraitItemReference error ()
  {
    return TraitItemReference ("", false, ERROR, nullptr, nullptr, {},
			       UNDEF_LOCATION);
  }

  static TraitItemReference &error_node ()
  {
    static TraitItemReference error = TraitItemReference::error ();
    return error;
  }

  bool is_error () const;

  std::string as_string () const;

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

  bool is_optional () const;

  std::string get_identifier () const;

  TraitItemType get_trait_item_type () const;

  HIR::TraitItem *get_hir_trait_item () const;

  location_t get_locus () const;

  const Analysis::NodeMapping get_mappings () const;

  TyTy::BaseType *get_tyty () const;

  Analysis::NodeMapping get_parent_trait_mappings () const;

  // this is called when the trait is completed resolution and gives the items
  // a chance to run their specific type resolution passes. If we call their
  // resolution on construction it can lead to a case where the trait being
  // resolved recursively trying to resolve the trait itself infinitely since
  // the trait will not be stored in its own map yet
  void on_resolved ();

  void associated_type_set (TyTy::BaseType *ty) const;

  void associated_type_reset (bool only_projections) const;

  bool is_object_safe () const;

private:
  TyTy::ErrorType *get_error () const;

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
  location_t locus;

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
		  std::vector<TyTy::TypeBoundPredicate> super_traits,
		  std::vector<TyTy::SubstitutionParamMapping> substs);

  TraitReference (TraitReference const &other);

  TraitReference &operator= (TraitReference const &other);

  TraitReference (TraitReference &&other) = default;
  TraitReference &operator= (TraitReference &&other) = default;

  static TraitReference error ()
  {
    return TraitReference (nullptr, {}, {}, {});
  }

  bool is_error () const;

  static TraitReference &error_node ()
  {
    static TraitReference trait_error_node = TraitReference::error ();
    return trait_error_node;
  }

  location_t get_locus () const;

  std::string get_name () const;

  std::string as_string () const;

  const HIR::Trait *get_hir_trait_ref () const;

  const Analysis::NodeMapping &get_mappings () const;

  DefId get_defid () const;

  bool lookup_hir_trait_item (const HIR::TraitItem &item,
			      TraitItemReference **ref);

  bool lookup_trait_item (const std::string &ident, TraitItemReference **ref);

  bool lookup_trait_item_by_type (const std::string &ident,
				  TraitItemReference::TraitItemType type,
				  TraitItemReference **ref);

  bool lookup_trait_item_by_type (const std::string &ident,
				  TraitItemReference::TraitItemType type,
				  const TraitItemReference **ref) const;

  bool lookup_hir_trait_item (const HIR::TraitItem &item,
			      const TraitItemReference **ref) const;

  bool lookup_trait_item (const std::string &ident,
			  const TraitItemReference **ref,
			  bool lookup_supers = true) const;

  const TraitItemReference *
  lookup_trait_item (const std::string &ident,
		     TraitItemReference::TraitItemType type) const;

  size_t size () const;

  const std::vector<TraitItemReference> &get_trait_items () const;

  void get_trait_items_and_supers (
    std::vector<const TraitItemReference *> &result) const;

  void on_resolved ();

  void clear_associated_types () const;

  void clear_associated_type_projections () const;

  bool is_equal (const TraitReference &other) const;

  std::vector<TyTy::TypeBoundPredicate> get_super_traits () const;

  bool is_object_safe (bool emit_error, location_t locus) const;

  bool trait_has_generics () const;

  std::vector<TyTy::SubstitutionParamMapping> get_trait_substs () const;

  bool satisfies_bound (const TraitReference &reference) const;

private:
  const HIR::Trait *hir_trait_ref;
  std::vector<TraitItemReference> item_refs;
  std::vector<TyTy::TypeBoundPredicate> super_traits;
  std::vector<TyTy::SubstitutionParamMapping> trait_substs;
};

class AssociatedImplTrait
{
public:
  AssociatedImplTrait (TraitReference *trait,
		       TyTy::TypeBoundPredicate predicate, HIR::ImplBlock *impl,
		       TyTy::BaseType *self,
		       Resolver::TypeCheckContext *context);

  TyTy::TypeBoundPredicate &get_predicate ();

  HIR::ImplBlock *get_impl_block ();

  location_t get_locus () const;

  TyTy::BaseType *get_self ();
  const TyTy::BaseType *get_self () const;

  void setup_raw_associated_types ();

  TyTy::BaseType *setup_associated_types (
    const TyTy::BaseType *self, const TyTy::TypeBoundPredicate &bound,
    TyTy::SubstitutionArgumentMappings *args = nullptr, bool infer = true);

  void reset_associated_types ();

private:
  TraitReference *trait;
  TyTy::TypeBoundPredicate predicate;
  HIR::ImplBlock *impl;
  TyTy::BaseType *self;
  Resolver::TypeCheckContext *context;
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_HIR_TRAIT_REF_H
