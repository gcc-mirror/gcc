// Copyright (C) 2020-2026 Free Software Foundation, Inc.

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

#include "rust-hir-path-probe-type.h"
#include "rust-hir-map.h"
#include "rust-hir-trait-resolve.h"
#include "rust-hir.h"
#include "rust-type-util.h"
#include "rust-tyty.h"

namespace Rust {
namespace Resolver {

TypePathProbeResult
TypePathProbe::Probe (TyTy::BaseType *receiver,
		      const HIR::PathIdentSegment &segment_name)
{
  TypePathProbe probe (receiver, segment_name);
  return probe.probe ();
}

TypePathProbeResult
TypePathProbe::probe ()
{
  switch (receiver->get_kind ())
    {
    case TyTy::TypeKind::PARAM:
    case TyTy::TypeKind::DYNAMIC:
      probe_generic ();
      break;

    case TyTy::TypeKind::ADT:
      probe_adt (static_cast<TyTy::ADTType *> (receiver));
      break;

    default:
      probe_fallback ();
      break;
    }

  return std::move (result);
}

void
TypePathProbe::probe_generic ()
{
  for (const TyTy::TypeBoundPredicate &predicate :
       receiver->get_specified_bounds ())
    {
      auto candidate = process_predicate_for_candidates (predicate);
      insert_candidate (std::move (candidate));
    }
}

void
TypePathProbe::probe_adt (TyTy::ADTType *adt)
{
  auto adt_item = mappings.lookup_defid (adt->get_id ());
  if (!adt_item.has_value ())
    {
      probe_fallback ();
      return;
    }

  NodeId adt_node_id = adt_item.value ()->get_mappings ().get_nodeid ();
  mappings.iterate_adt_impl_items (adt_node_id,
				   [this] (HirId id, HIR::ImplItem *item,
					   HIR::ImplBlock *impl) -> bool {
				     return process_impl_item (id, item, impl);
				   });
}

void
TypePathProbe::probe_fallback ()
{
  mappings.iterate_impl_items (
    [&] (HirId id, HIR::ImplItem *item, HIR::ImplBlock *impl) mutable -> bool {
      return process_impl_item (id, item, impl);
    });
}

bool
TypePathProbe::process_impl_item (HirId id, HIR::ImplItem *item,
				  HIR::ImplBlock *impl)
{
  auto item_name = item->get_impl_item_name ();
  if (search.to_string () != item_name)
    return true;

  HirId impl_ty_id = impl->get_type ().get_mappings ().get_hirid ();
  TyTy::BaseType *impl_block_ty = nullptr;
  if (!query_type (impl_ty_id, &impl_block_ty))
    return true;

  if (!types_compatable (TyTy::TyWithLocation (receiver),
			 TyTy::TyWithLocation (impl_block_ty),
			 impl->get_locus (), false))
    return true;

  // Keep trait impl items at trait position.  In particular, do not query the
  // type of an impl associated-type alias here: projection normalization will
  // select and evaluate the concrete alias later.
  if (impl->has_trait_ref ())
    {
      process_trait_impl_item (impl);
      return true;
    }

  TyTy::BaseType *item_ty = nullptr;
  if (!query_type (id, &item_ty))
    return true;

  PathProbeCandidate::CandidateType candidate_type;
  switch (item->get_impl_item_type ())
    {
    case HIR::ImplItem::FUNCTION:
      candidate_type = PathProbeCandidate::IMPL_FUNC;
      break;

    case HIR::ImplItem::TYPE_ALIAS:
      candidate_type = PathProbeCandidate::IMPL_TYPE_ALIAS;
      break;

    case HIR::ImplItem::CONSTANT:
      candidate_type = PathProbeCandidate::IMPL_CONST;
      break;

    default:
      return true;
    }

  PathProbeCandidate::ImplItemCandidate impl_candidate{item, impl};

  insert_candidate (
    {candidate_type, item_ty, item->get_locus (), impl_candidate});

  return true;
}

void
TypePathProbe::process_trait_impl_item (HIR::ImplBlock *impl)
{
  HIR::TypePath &trait_path = impl->get_trait_ref ();
  TraitReference *trait_ref = TraitResolver::Lookup (trait_path);
  if (trait_ref->is_error ())
    trait_ref = TraitResolver::Resolve (trait_path);
  if (trait_ref->is_error ())
    return;

  TyTy::TypeBoundPredicate predicate (*trait_ref, BoundPolarity::RegularBound,
				      impl->get_locus ());
  auto candidate = process_predicate_for_candidates (predicate);
  if (candidate.is_error ())
    return;

  rust_assert (candidate.is_trait_candidate ());
  candidate.item.trait.impl = impl;
  insert_candidate (std::move (candidate));
}

PathProbeCandidate
TypePathProbe::process_predicate_for_candidates (
  const TyTy::TypeBoundPredicate &predicate)
{
  tl::optional<TyTy::TypeBoundPredicateItem> item
    = predicate.lookup_associated_item (search.to_string ());
  if (!item.has_value ())
    return PathProbeCandidate::get_error ();

  const TraitReference *trait_ref = item->get_parent ()->get ();
  const TraitItemReference *trait_item_ref = item->get_raw_item ();
  PathProbeCandidate::CandidateType candidate_type;
  switch (trait_item_ref->get_trait_item_type ())
    {
    case TraitItemReference::TraitItemType::FN:
      candidate_type = PathProbeCandidate::CandidateType::TRAIT_FUNC;
      break;
    case TraitItemReference::TraitItemType::CONST:
      candidate_type = PathProbeCandidate::CandidateType::TRAIT_ITEM_CONST;
      break;
    case TraitItemReference::TraitItemType::TYPE:
      candidate_type = PathProbeCandidate::CandidateType::TRAIT_TYPE_ALIAS;
      break;

    case TraitItemReference::TraitItemType::ERROR:
    default:
      return PathProbeCandidate::get_error ();
    }

  TyTy::BaseType *trait_item_tyty = item->get_raw_item ()->get_tyty ();
  if (receiver->get_kind () != TyTy::DYNAMIC)
    trait_item_tyty = item->get_tyty_for_receiver (receiver);

  PathProbeCandidate::TraitItemCandidate trait_item_candidate{trait_ref,
							      trait_item_ref,
							      nullptr};
  return {candidate_type, trait_item_tyty, trait_item_ref->get_locus (),
	  trait_item_candidate};
}

void
TypePathProbe::insert_candidate (PathProbeCandidate candidate)
{
  if (candidate.is_error ())
    return;

  bool is_type
    = candidate.type == PathProbeCandidate::CandidateType::IMPL_TYPE_ALIAS
      || candidate.type == PathProbeCandidate::CandidateType::TRAIT_TYPE_ALIAS;
  if (is_type)
    result.type_candidates.insert (std::move (candidate));
  else
    result.non_type_matches.insert (std::move (candidate));
}

} // namespace Resolver
} // namespace Rust
