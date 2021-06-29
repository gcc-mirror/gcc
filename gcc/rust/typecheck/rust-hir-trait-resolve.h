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

  void visit (HIR::TraitItemType &type) override
  {
    // associated types are not typed and only support bounds
    TyTy::BaseType *ty = nullptr;

    // create trait-item-ref
    Location locus = type.get_locus ();
    bool is_optional = false;
    std::string identifier = type.get_name ();

    resolved = TraitItemReference (identifier, is_optional,
				   TraitItemReference::TraitItemType::TYPE,
				   &type, ty, locus);
  }

  void visit (HIR::TraitItemConst &cst) override
  {
    // attempt to lookup the type of the trait item function
    TyTy::BaseType *ty = nullptr;
    if (!context->lookup_type (cst.get_mappings ().get_hirid (), &ty))
      {
	auto resolved = TypeCheckType::Resolve (cst.get_type ().get ());
	if (resolved->get_kind () == TyTy::TypeKind::ERROR)
	  {
	    rust_error_at (cst.get_locus (),
			   "failed to resolve trait constant type");
	    return;
	  }
      }

    // create trait-item-ref
    Location locus = cst.get_locus ();
    bool is_optional = cst.has_expr ();
    std::string identifier = cst.get_name ();

    resolved = TraitItemReference (identifier, is_optional,
				   TraitItemReference::TraitItemType::CONST,
				   &cst, ty, locus);
  }

  void visit (HIR::TraitItemFunc &fn) override
  {
    // FIXME this is duplicated in a few places and could be refactored

    // attempt to lookup the type of the trait item function
    TyTy::BaseType *ty = nullptr;
    if (!context->lookup_type (fn.get_mappings ().get_hirid (), &ty))
      {
	HIR::TraitFunctionDecl &function = fn.get_decl ();
	if (function.has_generics ())
	  {
	    for (auto &generic_param : function.get_generic_params ())
	      {
		switch (generic_param.get ()->get_kind ())
		  {
		  case HIR::GenericParam::GenericKind::LIFETIME:
		    // Skipping Lifetime completely until better handling.
		    break;

		    case HIR::GenericParam::GenericKind::TYPE: {
		      auto param_type = TypeResolveGenericParam::Resolve (
			generic_param.get ());
		      context->insert_type (generic_param->get_mappings (),
					    param_type);

		      substitutions.push_back (TyTy::SubstitutionParamMapping (
			static_cast<HIR::TypeParam &> (*generic_param),
			param_type));
		    }
		    break;
		  }
	      }
	  }

	TyTy::BaseType *ret_type = nullptr;
	if (!function.has_return_type ())
	  ret_type = new TyTy::TupleType (fn.get_mappings ().get_hirid ());
	else
	  {
	    auto resolved
	      = TypeCheckType::Resolve (function.get_return_type ().get ());
	    if (resolved->get_kind () == TyTy::TypeKind::ERROR)
	      {
		rust_error_at (fn.get_locus (),
			       "failed to resolve return type");
		return;
	      }

	    ret_type = resolved->clone ();
	    ret_type->set_ref (
	      function.get_return_type ()->get_mappings ().get_hirid ());
	  }

	std::vector<std::pair<HIR::Pattern *, TyTy::BaseType *> > params;
	if (function.is_method ())
	  {
	    // add the synthetic self param at the front, this is a placeholder
	    // for compilation to know parameter names. The types are ignored
	    // but we reuse the HIR identifier pattern which requires it
	    HIR::SelfParam &self_param = function.get_self ();
	    HIR::IdentifierPattern *self_pattern = new HIR::IdentifierPattern (
	      "self", self_param.get_locus (), self_param.is_ref (),
	      self_param.is_mut (), std::unique_ptr<HIR::Pattern> (nullptr));
	    context->insert_type (self_param.get_mappings (), self->clone ());
	    params.push_back (
	      std::pair<HIR::Pattern *, TyTy::BaseType *> (self_pattern,
							   self->clone ()));
	  }

	for (auto &param : function.get_function_params ())
	  {
	    // get the name as well required for later on
	    auto param_tyty = TypeCheckType::Resolve (param.get_type ());
	    params.push_back (std::pair<HIR::Pattern *, TyTy::BaseType *> (
	      param.get_param_name (), param_tyty));

	    context->insert_type (param.get_mappings (), param_tyty);
	  }

	ty = new TyTy::FnType (fn.get_mappings ().get_hirid (),
			       function.get_function_name (), false,
			       std::move (params), ret_type,
			       std::move (substitutions));
	context->insert_type (fn.get_mappings (), ty);
      }

    // create trait-item-ref
    Location locus = fn.get_locus ();
    bool is_optional = fn.has_block_defined ();
    std::string identifier = fn.get_decl ().get_function_name ();

    resolved = TraitItemReference (identifier, is_optional,
				   TraitItemReference::TraitItemType::FN, &fn,
				   ty, locus);
  }

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
  static TraitReference &Resolve (HIR::TypePath &path)
  {
    TraitResolver resolver;
    return resolver.go (path);
  }

  static TraitReference &error_node ()
  {
    static TraitReference trait_error_node = TraitReference::error ();
    return trait_error_node;
  }

private:
  TraitResolver () : TypeCheckBase () {}

  TraitReference &go (HIR::TypePath &path)
  {
    NodeId ref;
    if (!resolver->lookup_resolved_type (path.get_mappings ().get_nodeid (),
					 &ref))
      {
	rust_fatal_error (path.get_locus (),
			  "Failed to resolve path to node-id");
	return error_node ();
      }

    HirId hir_node = UNKNOWN_HIRID;
    if (!mappings->lookup_node_to_hir (mappings->get_current_crate (), ref,
				       &hir_node))
      {
	rust_fatal_error (path.get_locus (),
			  "Failed to resolve path to hir-id");
	return error_node ();
      }

    HIR::Item *resolved_item
      = mappings->lookup_hir_item (mappings->get_current_crate (), hir_node);

    rust_assert (resolved_item != nullptr);
    resolved_item->accept_vis (*this);
    rust_assert (trait_reference != nullptr);

    TraitReference &tref = error_node ();
    if (context->lookup_trait_reference (
	  trait_reference->get_mappings ().get_defid (), tref))
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

    tref = TraitReference (trait_reference, item_refs);
    context->insert_trait_reference (
      trait_reference->get_mappings ().get_defid (), std::move (tref));

    tref = error_node ();
    bool ok = context->lookup_trait_reference (
      trait_reference->get_mappings ().get_defid (), tref);
    rust_assert (ok);

    return tref;
  }

  HIR::Trait *trait_reference;

public:
  void visit (HIR::Trait &trait) override { trait_reference = &trait; }
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_HIR_TRAIT_RESOLVE_H
