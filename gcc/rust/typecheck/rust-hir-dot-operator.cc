// Copyright (C) 2020-2023 Free Software Foundation, Inc.

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

MethodResolver::MethodResolver (bool autoderef_flag,
				const HIR::PathIdentSegment &segment_name)
  : AutoderefCycle (autoderef_flag), segment_name (segment_name), result ()
{}

std::set<MethodCandidate>
MethodResolver::Probe (const TyTy::BaseType *receiver,
		       const HIR::PathIdentSegment &segment_name,
		       bool autoderef_flag)
{
  MethodResolver resolver (autoderef_flag, segment_name);
  resolver.cycle (receiver);
  return resolver.result;
}

void
MethodResolver::try_hook (const TyTy::BaseType &r)
{
  const auto &specified_bounds = r.get_specified_bounds ();
  predicate_items = get_predicate_items (segment_name, r, specified_bounds);
}

bool
MethodResolver::select (const TyTy::BaseType &receiver)
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
      if (!query_type (func->get_mappings ().get_hirid (), &ty))
	return true;
      rust_assert (ty != nullptr);
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
	if (!query_type (func->get_mappings ().get_hirid (), &ty))
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

  rust_debug ("inherent_impl_fns found {%lu}, trait_fns found {%lu}, "
	      "predicate_items found {%lu}",
	      (unsigned long) inherent_impl_fns.size (),
	      (unsigned long) trait_fns.size (),
	      (unsigned long) predicate_items.size ());

  // see the follow for the proper fix to get rid of this we need to assemble
  // candidates based on a match expression gathering the relevant impl blocks
  // https://github.com/rust-lang/rust/blob/7eac88abb2e57e752f3302f02be5f3ce3d7adfb4/compiler/rustc_typeck/src/check/method/probe.rs#L580-L694
  TyTy::set_cmp_autoderef_mode ();

  bool found_possible_candidate = false;
  for (auto &impl_item : inherent_impl_fns)
    {
      bool is_trait_impl_block = impl_item.impl_block->has_trait_ref ();
      if (is_trait_impl_block)
	continue;

      TyTy::FnType *fn = impl_item.ty;
      rust_assert (fn->is_method ());

      TyTy::BaseType *fn_self = fn->get_self_type ();
      rust_debug ("dot-operator impl_item fn_self={%s} can_eq receiver={%s}",
		  fn_self->debug_str ().c_str (),
		  receiver.debug_str ().c_str ());
      if (fn_self->can_eq (&receiver, false))
	{
	  PathProbeCandidate::ImplItemCandidate c{impl_item.item,
						  impl_item.impl_block};
	  auto try_result = MethodCandidate{
	    PathProbeCandidate (PathProbeCandidate::CandidateType::IMPL_FUNC,
				fn, impl_item.item->get_locus (), c),
	    adjustments};
	  result.insert (std::move (try_result));
	  found_possible_candidate = true;
	}
    }
  if (found_possible_candidate)
    {
      TyTy::reset_cmp_autoderef_mode ();
      return true;
    }

  for (auto &impl_item : inherent_impl_fns)
    {
      bool is_trait_impl_block = impl_item.impl_block->has_trait_ref ();
      if (!is_trait_impl_block)
	continue;

      TyTy::FnType *fn = impl_item.ty;
      rust_assert (fn->is_method ());

      TyTy::BaseType *fn_self = fn->get_self_type ();
      rust_debug (
	"dot-operator trait_impl_item fn_self={%s} can_eq receiver={%s}",
	fn_self->debug_str ().c_str (), receiver.debug_str ().c_str ());
      if (fn_self->can_eq (&receiver, false))
	{
	  PathProbeCandidate::ImplItemCandidate c{impl_item.item,
						  impl_item.impl_block};
	  auto try_result = MethodCandidate{
	    PathProbeCandidate (PathProbeCandidate::CandidateType::IMPL_FUNC,
				fn, impl_item.item->get_locus (), c),
	    adjustments};
	  result.insert (std::move (try_result));
	  found_possible_candidate = true;
	}
    }
  if (found_possible_candidate)
    {
      TyTy::reset_cmp_autoderef_mode ();
      return true;
    }

  for (auto trait_item : trait_fns)
    {
      TyTy::FnType *fn = trait_item.ty;
      rust_assert (fn->is_method ());

      TyTy::BaseType *fn_self = fn->get_self_type ();
      rust_debug ("dot-operator trait_item fn_self={%s} can_eq receiver={%s}",
		  fn_self->debug_str ().c_str (),
		  receiver.debug_str ().c_str ());
      if (fn_self->can_eq (&receiver, false))
	{
	  PathProbeCandidate::TraitItemCandidate c{trait_item.reference,
						   trait_item.item_ref,
						   nullptr};
	  auto try_result = MethodCandidate{
	    PathProbeCandidate (PathProbeCandidate::CandidateType::TRAIT_FUNC,
				fn, trait_item.item->get_locus (), c),
	    adjustments};
	  result.insert (std::move (try_result));
	  found_possible_candidate = true;
	}
    }
  if (found_possible_candidate)
    {
      TyTy::reset_cmp_autoderef_mode ();
      return true;
    }

  for (const auto &predicate : predicate_items)
    {
      const TyTy::FnType *fn = predicate.fntype;
      rust_assert (fn->is_method ());

      TyTy::BaseType *fn_self = fn->get_self_type ();
      rust_debug ("dot-operator predicate fn_self={%s} can_eq receiver={%s}",
		  fn_self->debug_str ().c_str (),
		  receiver.debug_str ().c_str ());
      if (fn_self->can_eq (&receiver, false))
	{
	  const TraitReference *trait_ref
	    = predicate.lookup.get_parent ()->get ();
	  const TraitItemReference *trait_item
	    = predicate.lookup.get_raw_item ();

	  PathProbeCandidate::TraitItemCandidate c{trait_ref, trait_item,
						   nullptr};
	  auto try_result = MethodCandidate{
	    PathProbeCandidate (PathProbeCandidate::CandidateType::TRAIT_FUNC,
				fn->clone (), trait_item->get_locus (), c),
	    adjustments};
	  result.insert (std::move (try_result));
	  found_possible_candidate = true;
	}
    }

  TyTy::reset_cmp_autoderef_mode ();
  return found_possible_candidate;
}

std::vector<MethodResolver::predicate_candidate>
MethodResolver::get_predicate_items (
  const HIR::PathIdentSegment &segment_name, const TyTy::BaseType &receiver,
  const std::vector<TyTy::TypeBoundPredicate> &specified_bounds)
{
  std::vector<predicate_candidate> predicate_items;
  for (auto &bound : specified_bounds)
    {
      TyTy::TypeBoundPredicateItem lookup
	= bound.lookup_associated_item (segment_name.as_string ());
      if (lookup.is_error ())
	continue;

      TyTy::BaseType *ty = lookup.get_tyty_for_receiver (&receiver);
      if (ty->get_kind () == TyTy::TypeKind::FNDEF)
	{
	  TyTy::FnType *fnty = static_cast<TyTy::FnType *> (ty);
	  predicate_candidate candidate{lookup, fnty};
	  predicate_items.push_back (candidate);
	}
    }

  return predicate_items;
}

} // namespace Resolver
} // namespace Rust
