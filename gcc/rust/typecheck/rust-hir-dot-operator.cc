// Copyright (C) 2020-2022 Free Software Foundation, Inc.

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

#include "rust-hir-dot-operator.h"
#include "rust-hir-path-probe.h"
#include "rust-hir-trait-resolve.h"

namespace Rust {
namespace Resolver {

MethodCandidate
MethodResolver::Probe (const TyTy::BaseType *receiver,
		       const HIR::PathIdentSegment &segment_name,
		       bool autoderef_flag)
{
  const TyTy::BaseType *r = receiver;
  std::vector<Adjustment> adjustments;
  while (true)
    {
      auto res = Try (r, segment_name, adjustments);
      if (!res.is_error ())
	return res;

      // 4. deref to to 1, if cannot deref then quit
      if (autoderef_flag)
	return MethodCandidate::get_error ();

      // try unsize
      Adjustment unsize = Adjuster::try_unsize_type (r);
      if (!unsize.is_error ())
	{
	  adjustments.push_back (unsize);
	  auto unsize_r = unsize.get_expected ();
	  auto res = Try (unsize_r, segment_name, adjustments);
	  if (!res.is_error ())
	    {
	      return res;
	    }

	  adjustments.pop_back ();
	}

      Adjustment deref
	= Adjuster::try_deref_type (r, Analysis::RustLangItem::ItemType::DEREF);
      if (!deref.is_error ())
	{
	  auto deref_r = deref.get_expected ();
	  adjustments.push_back (deref);
	  auto res = Try (deref_r, segment_name, adjustments);
	  if (!res.is_error ())
	    {
	      return res;
	    }

	  adjustments.pop_back ();
	}

      Adjustment deref_mut = Adjuster::try_deref_type (
	r, Analysis::RustLangItem::ItemType::DEREF_MUT);
      if (!deref_mut.is_error ())
	{
	  auto deref_r = deref_mut.get_expected ();
	  adjustments.push_back (deref_mut);
	  auto res = Try (deref_r, segment_name, adjustments);
	  if (!res.is_error ())
	    {
	      return res;
	    }

	  adjustments.pop_back ();
	}

      if (!deref_mut.is_error ())
	{
	  auto deref_r = deref_mut.get_expected ();
	  adjustments.push_back (deref_mut);
	  Adjustment raw_deref = Adjuster::try_raw_deref_type (deref_r);
	  adjustments.push_back (raw_deref);
	  deref_r = raw_deref.get_expected ();

	  auto res = Try (deref_r, segment_name, adjustments);
	  if (!res.is_error ())
	    {
	      return res;
	    }

	  adjustments.pop_back ();
	  adjustments.pop_back ();
	}

      if (!deref.is_error ())
	{
	  r = deref.get_expected ();
	  adjustments.push_back (deref);
	}
      Adjustment raw_deref = Adjuster::try_raw_deref_type (r);
      if (raw_deref.is_error ())
	return MethodCandidate::get_error ();

      r = raw_deref.get_expected ();
      adjustments.push_back (raw_deref);
    }
  return MethodCandidate::get_error ();
}

MethodCandidate
MethodResolver::Try (const TyTy::BaseType *r,
		     const HIR::PathIdentSegment &segment_name,
		     std::vector<Adjustment> &adjustments)
{
  PathProbeCandidate c = PathProbeCandidate::get_error ();
  const std::vector<TyTy::TypeBoundPredicate> &specified_bounds
    = r->get_specified_bounds ();

  // 1. try raw
  MethodResolver raw (*r, segment_name, specified_bounds);
  c = raw.select ();
  if (!c.is_error ())
    {
      return MethodCandidate{c, adjustments};
    }

  // 2. try ref
  TyTy::ReferenceType *r1
    = new TyTy::ReferenceType (r->get_ref (), TyTy::TyVar (r->get_ref ()),
			       Mutability::Imm);
  MethodResolver imm_ref (*r1, segment_name, specified_bounds);
  c = imm_ref.select ();
  if (!c.is_error ())
    {
      adjustments.push_back (
	Adjustment (Adjustment::AdjustmentType::IMM_REF, r1));
      return MethodCandidate{c, adjustments};
    }

  // 3. try mut ref
  TyTy::ReferenceType *r2
    = new TyTy::ReferenceType (r->get_ref (), TyTy::TyVar (r->get_ref ()),
			       Mutability::Mut);
  MethodResolver mut_ref (*r2, segment_name, specified_bounds);
  c = mut_ref.select ();
  if (!c.is_error ())
    {
      adjustments.push_back (
	Adjustment (Adjustment::AdjustmentType::MUT_REF, r2));
      return MethodCandidate{c, adjustments};
    }

  return MethodCandidate::get_error ();
}

PathProbeCandidate
MethodResolver::select ()
{
  struct impl_item_candidate
  {
    HIR::Function *item;
    HIR::ImplBlock *impl_block;
    TyTy::FnType *ty;
  };

  // assemble inherent impl items
  std::vector<impl_item_candidate> inherent_impl_fns;
  mappings->iterate_impl_items (
    [&] (HirId id, HIR::ImplItem *item, HIR::ImplBlock *impl) mutable -> bool {
      bool is_trait_impl = impl->has_trait_ref ();
      if (is_trait_impl)
	return true;

      bool is_fn
	= item->get_impl_item_type () == HIR::ImplItem::ImplItemType::FUNCTION;
      if (!is_fn)
	return true;

      HIR::Function *func = static_cast<HIR::Function *> (item);
      if (!func->is_method ())
	return true;

      bool name_matches
	= func->get_function_name ().compare (segment_name.as_string ()) == 0;
      if (!name_matches)
	return true;

      TyTy::BaseType *ty = nullptr;
      if (!context->lookup_type (func->get_mappings ().get_hirid (), &ty))
	return true;
      if (ty->get_kind () == TyTy::TypeKind::ERROR)
	return true;

      rust_assert (ty->get_kind () == TyTy::TypeKind::FNDEF);
      TyTy::FnType *fnty = static_cast<TyTy::FnType *> (ty);

      inherent_impl_fns.push_back ({func, impl, fnty});

      return true;
    });

  struct trait_item_candidate
  {
    const HIR::TraitItemFunc *item;
    const HIR::Trait *trait;
    TyTy::FnType *ty;
    const TraitReference *reference;
    const TraitItemReference *item_ref;
  };

  std::vector<trait_item_candidate> trait_fns;
  mappings->iterate_impl_blocks ([&] (HirId id,
				      HIR::ImplBlock *impl) mutable -> bool {
    bool is_trait_impl = impl->has_trait_ref ();
    if (!is_trait_impl)
      return true;

    // look for impl implementation else lookup the associated trait item
    for (auto &impl_item : impl->get_impl_items ())
      {
	bool is_fn = impl_item->get_impl_item_type ()
		     == HIR::ImplItem::ImplItemType::FUNCTION;
	if (!is_fn)
	  continue;

	HIR::Function *func = static_cast<HIR::Function *> (impl_item.get ());
	if (!func->is_method ())
	  continue;

	bool name_matches
	  = func->get_function_name ().compare (segment_name.as_string ()) == 0;
	if (!name_matches)
	  continue;

	TyTy::BaseType *ty = nullptr;
	if (!context->lookup_type (func->get_mappings ().get_hirid (), &ty))
	  continue;
	if (ty->get_kind () == TyTy::TypeKind::ERROR)
	  continue;

	rust_assert (ty->get_kind () == TyTy::TypeKind::FNDEF);
	TyTy::FnType *fnty = static_cast<TyTy::FnType *> (ty);

	inherent_impl_fns.push_back ({func, impl, fnty});
	return true;
      }

    TraitReference *trait_ref
      = TraitResolver::Resolve (*impl->get_trait_ref ().get ());
    rust_assert (!trait_ref->is_error ());

    auto item_ref
      = trait_ref->lookup_trait_item (segment_name.as_string (),
				      TraitItemReference::TraitItemType::FN);
    if (item_ref->is_error ())
      return true;

    const HIR::Trait *trait = trait_ref->get_hir_trait_ref ();
    HIR::TraitItem *item = item_ref->get_hir_trait_item ();
    rust_assert (item->get_item_kind () == HIR::TraitItem::TraitItemKind::FUNC);
    HIR::TraitItemFunc *func = static_cast<HIR::TraitItemFunc *> (item);

    TyTy::BaseType *ty = item_ref->get_tyty ();
    rust_assert (ty->get_kind () == TyTy::TypeKind::FNDEF);
    TyTy::FnType *fnty = static_cast<TyTy::FnType *> (ty);

    trait_item_candidate candidate{func, trait, fnty, trait_ref, item_ref};
    trait_fns.push_back (candidate);

    return true;
  });

  // lookup specified bounds for an associated item
  struct precdicate_candidate
  {
    TyTy::TypeBoundPredicateItem lookup;
    TyTy::FnType *fntype;
  };

  std::vector<precdicate_candidate> predicate_items;
  for (auto &bound : specified_bounds)
    {
      TyTy::TypeBoundPredicateItem lookup
	= bound.lookup_associated_item (segment_name.as_string ());
      if (lookup.is_error ())
	continue;

      bool is_fn = lookup.get_raw_item ()->get_trait_item_type ()
		   == TraitItemReference::TraitItemType::FN;
      if (!is_fn)
	continue;

      TyTy::BaseType *ty = lookup.get_raw_item ()->get_tyty ();
      rust_assert (ty->get_kind () == TyTy::TypeKind::FNDEF);
      TyTy::FnType *fnty = static_cast<TyTy::FnType *> (ty);

      precdicate_candidate candidate{lookup, fnty};
      predicate_items.push_back (candidate);
    }

  for (auto impl_item : inherent_impl_fns)
    {
      TyTy::FnType *fn = impl_item.ty;
      rust_assert (fn->is_method ());

      TyTy::BaseType *fn_self = fn->get_self_type ();
      if (fn_self->can_eq (&receiver, false))
	{
	  PathProbeCandidate::ImplItemCandidate c{impl_item.item,
						  impl_item.impl_block};
	  return PathProbeCandidate (
	    PathProbeCandidate::CandidateType::IMPL_FUNC, fn,
	    impl_item.item->get_locus (), c);
	}
    }

  for (auto trait_item : trait_fns)
    {
      TyTy::FnType *fn = trait_item.ty;
      rust_assert (fn->is_method ());

      TyTy::BaseType *fn_self = fn->get_self_type ();
      if (fn_self->can_eq (&receiver, false))
	{
	  PathProbeCandidate::TraitItemCandidate c{trait_item.reference,
						   trait_item.item_ref,
						   nullptr};
	  return PathProbeCandidate (
	    PathProbeCandidate::CandidateType::TRAIT_FUNC, fn,
	    trait_item.item->get_locus (), c);
	}
    }

  for (auto predicate : predicate_items)
    {
      TyTy::FnType *fn = predicate.fntype;
      rust_assert (fn->is_method ());

      TyTy::BaseType *fn_self = fn->get_self_type ();
      if (fn_self->can_eq (&receiver, false))
	{
	  const TraitReference *trait_ref
	    = predicate.lookup.get_parent ()->get ();
	  const TraitItemReference *trait_item
	    = predicate.lookup.get_raw_item ();

	  TyTy::BaseType *subst = predicate.lookup.get_tyty_for_receiver (
	    receiver.get_root (),
	    predicate.lookup.get_parent ()->get_generic_args ());

	  PathProbeCandidate::TraitItemCandidate c{trait_ref, trait_item,
						   nullptr};
	  return PathProbeCandidate (
	    PathProbeCandidate::CandidateType::TRAIT_FUNC, subst,
	    trait_item->get_locus (), c);
	}
    }

  return PathProbeCandidate::get_error ();
}

} // namespace Resolver
} // namespace Rust
