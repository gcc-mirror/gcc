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

#ifndef RUST_HIR_TRAIT_RESOLVE_H
#define RUST_HIR_TRAIT_RESOLVE_H

#include "rust-hir-type-check-base.h"
#include "rust-hir-full.h"
#include "rust-tyty-visitor.h"
#include "rust-hir-type-check-type.h"
#include "rust-hir-trait-ref.h"

namespace Rust {
namespace Resolver {

class ResolveTraitItemToRef : public TypeCheckBase
{
  using Rust::Resolver::TypeCheckBase::visit;

public:
  static TraitItemReference
  Resolve (HIR::TraitItem &item, TyTy::BaseType *self,
	   std::vector<TyTy::SubstitutionParamMapping> substitutions)
  {
    ResolveTraitItemToRef resolver (self, substitutions);
    item.accept_vis (resolver);
    return resolver.resolved;
  }

  void visit (HIR::TraitItemType &type) override;

  void visit (HIR::TraitItemConst &cst) override;

  void visit (HIR::TraitItemFunc &fn) override;

private:
  ResolveTraitItemToRef (
    TyTy::BaseType *self,
    std::vector<TyTy::SubstitutionParamMapping> substitutions)
    : TypeCheckBase (), resolved (TraitItemReference::error ()), self (self),
      substitutions (substitutions)
  {}

  TraitItemReference resolved;
  TyTy::BaseType *self;
  std::vector<TyTy::SubstitutionParamMapping> substitutions;
};

class TraitResolver : public TypeCheckBase
{
  using Rust::Resolver::TypeCheckBase::visit;

public:
  static TraitReference *Resolve (HIR::TypePath &path)
  {
    TraitResolver resolver;
    return resolver.go (path);
  }

private:
  TraitResolver () : TypeCheckBase () {}

  TraitReference *go (HIR::TypePath &path)
  {
    NodeId ref;
    if (!resolver->lookup_resolved_type (path.get_mappings ().get_nodeid (),
					 &ref))
      {
	rust_error_at (path.get_locus (), "Failed to resolve path to node-id");
	return &TraitReference::error_node ();
      }

    HirId hir_node = UNKNOWN_HIRID;
    if (!mappings->lookup_node_to_hir (mappings->get_current_crate (), ref,
				       &hir_node))
      {
	rust_error_at (path.get_locus (), "Failed to resolve path to hir-id");
	return &TraitReference::error_node ();
      }

    HIR::Item *resolved_item
      = mappings->lookup_hir_item (mappings->get_current_crate (), hir_node);

    rust_assert (resolved_item != nullptr);
    resolved_item->accept_vis (*this);
    rust_assert (trait_reference != nullptr);

    TraitReference *tref = &TraitReference::error_node ();
    if (context->lookup_trait_reference (
	  trait_reference->get_mappings ().get_defid (), &tref))
      {
	return tref;
      }

    TyTy::BaseType *self = nullptr;
    std::vector<TyTy::SubstitutionParamMapping> substitutions;
    for (auto &generic_param : trait_reference->get_generic_params ())
      {
	switch (generic_param.get ()->get_kind ())
	  {
	  case HIR::GenericParam::GenericKind::LIFETIME:
	    // Skipping Lifetime completely until better handling.
	    break;

	    case HIR::GenericParam::GenericKind::TYPE: {
	      auto param_type
		= TypeResolveGenericParam::Resolve (generic_param.get ());
	      context->insert_type (generic_param->get_mappings (), param_type);

	      auto &typaram = static_cast<HIR::TypeParam &> (*generic_param);
	      substitutions.push_back (
		TyTy::SubstitutionParamMapping (typaram, param_type));

	      if (typaram.get_type_representation ().compare ("Self") == 0)
		{
		  self = param_type;
		}
	    }
	    break;
	  }
      }

    rust_assert (self != nullptr);

    std::vector<TraitItemReference> item_refs;
    for (auto &item : trait_reference->get_trait_items ())
      {
	TraitItemReference trait_item_ref
	  = ResolveTraitItemToRef::Resolve (*item.get (), self, substitutions);
	item_refs.push_back (std::move (trait_item_ref));
      }

    TraitReference trait_object (trait_reference, item_refs);
    context->insert_trait_reference (
      trait_reference->get_mappings ().get_defid (), std::move (trait_object));

    tref = &TraitReference::error_node ();
    bool ok = context->lookup_trait_reference (
      trait_reference->get_mappings ().get_defid (), &tref);
    rust_assert (ok);

    // hook to allow the trait to resolve its optional item blocks, we cant
    // resolve the blocks of functions etc because it can end up in a recursive
    // loop of trying to resolve traits as required by the types
    tref->on_resolved ();

    return tref;
  }

  HIR::Trait *trait_reference;

public:
  void visit (HIR::Trait &trait) override { trait_reference = &trait; }
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_HIR_TRAIT_RESOLVE_H
