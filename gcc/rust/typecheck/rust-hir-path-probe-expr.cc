// Copyright (C) 2020-2026 Free Software Foundation, Inc.
//
// This file is part of GCC.
//
// GCC is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free
// Software Foundation; either version 3, or (at your option) any later
// version.
//
// GCC is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
// for more details.
//
// You should have received a copy of the GNU General Public License
// along with GCC; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include "rust-hir-path-probe-expr.h"
#include "rust-hir-item.h"
#include "rust-type-util.h"
#include "rust-hir-type-bounds.h"

namespace Rust {
namespace Resolver {

PathProbeExpr::PathProbeExpr (TyTy::BaseType *receiver,
			      const HIR::PathIdentSegment &query)
  : TypeCheckBase (), receiver (receiver), search (query)
{}

std::set<PathProbeCandidate>
PathProbeExpr::Probe (TyTy::BaseType *receiver,
		      const HIR::PathIdentSegment &segment_name)
{
  PathProbeExpr probe (receiver, segment_name);
  return probe.probe ();
}

std::set<PathProbeCandidate>
PathProbeExpr::probe ()
{
  switch (receiver->get_kind ())
    {
    case TyTy::TypeKind::PARAM:
      probe_bounds ();
      return std::move (candidates);

    case TyTy::TypeKind::ADT:
      {
	auto *adt = static_cast<TyTy::ADTType *> (receiver);
	if (adt->is_enum ())
	  process_enum_item_for_candidates (adt);
	probe_adt_impls (adt);
	break;
      }

    default:
      probe_fallback_impls ();
      break;
    }

  if (candidates.empty ())
    probe_bounds ();

  return std::move (candidates);
}

void
PathProbeExpr::probe_adt_impls (TyTy::ADTType *adt)
{
  auto adt_item = mappings.lookup_defid (adt->get_id ());
  if (!adt_item.has_value ())
    {
      probe_fallback_impls ();
      return;
    }

  NodeId adt_node_id = adt_item.value ()->get_mappings ().get_nodeid ();
  mappings.iterate_adt_impl_items (
    adt_node_id,
    [this] (HirId id, HIR::ImplItem *item, HIR::ImplBlock *impl) -> bool {
      return process_impl_item_candidate (id, item, impl);
    });
}

void
PathProbeExpr::probe_fallback_impls ()
{
  mappings.iterate_impl_items (
    [this] (HirId id, HIR::ImplItem *item, HIR::ImplBlock *impl) -> bool {
      return process_impl_item_candidate (id, item, impl);
    });
}

void
PathProbeExpr::process_enum_item_for_candidates (const TyTy::ADTType *adt)
{
  TyTy::VariantDef *v;
  if (!adt->lookup_variant (search.to_string (), &v))
    return;

  PathProbeCandidate::EnumItemCandidate enum_item_candidate{adt, v};
  PathProbeCandidate candidate{PathProbeCandidate::CandidateType::ENUM_VARIANT,
			       receiver->clone (),
			       mappings.lookup_location (adt->get_ty_ref ()),
			       enum_item_candidate};
  candidates.insert (std::move (candidate));
}

bool
PathProbeExpr::process_impl_item_candidate (HirId id, HIR::ImplItem *item,
					    HIR::ImplBlock *impl)
{
  if (search.to_string () != item->get_impl_item_name ())
    return true;

  HirId impl_ty_id = impl->get_type ().get_mappings ().get_hirid ();
  TyTy::BaseType *impl_block_ty = nullptr;
  if (!query_type (impl_ty_id, &impl_block_ty))
    return true;

  if (!types_compatable (TyTy::TyWithLocation (receiver),
			 TyTy::TyWithLocation (impl_block_ty),
			 impl->get_locus (), false))
    return true;

  PathProbeCandidate::CandidateType candidate_type;
  switch (item->get_impl_item_type ())
    {
    case HIR::ImplItem::FUNCTION:
      candidate_type = PathProbeCandidate::CandidateType::IMPL_FUNC;
      break;
    case HIR::ImplItem::CONSTANT:
      candidate_type = PathProbeCandidate::CandidateType::IMPL_CONST;
      break;
    case HIR::ImplItem::TYPE_ALIAS:
    default:
      return true;
    }

  TyTy::BaseType *item_ty = nullptr;
  if (!query_type (id, &item_ty))
    return true;

  PathProbeCandidate::ImplItemCandidate impl_candidate{item, impl};
  insert_candidate (
    {candidate_type, item_ty, item->get_locus (), impl_candidate});

  return true;
}

void
PathProbeExpr::probe_bounds ()
{
  if (!is_receiver_generic ())
    {
      auto probed_bounds = TypeBoundsProbe::Probe (receiver);
      for (auto &candidate : probed_bounds)
	{
	  const TraitReference *trait_ref = candidate.first;
	  process_associated_trait_for_candidates (trait_ref, candidate.second);
	}
    }

  for (const TyTy::TypeBoundPredicate &predicate :
       receiver->get_specified_bounds ())
    {
      process_predicate_for_candidates (predicate);
    }
}

void
PathProbeExpr::process_associated_trait_for_candidates (
  const TraitReference *trait_ref, HIR::ImplBlock *impl)
{
  const TraitItemReference *trait_item_ref = nullptr;
  if (!trait_ref->lookup_trait_item (search.to_string (), &trait_item_ref))
    return;

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
      return;

    case TraitItemReference::TraitItemType::ERROR:
    default:
      rust_unreachable ();
      break;
    }

  const TyTy::TypeBoundPredicate p (*trait_ref, BoundPolarity::RegularBound,
				    UNDEF_LOCATION);
  TyTy::TypeBoundPredicateItem item (p, trait_item_ref);

  TyTy::BaseType *trait_item_tyty = item.get_raw_item ()->get_tyty ();
  if (receiver->get_kind () != TyTy::DYNAMIC)
    trait_item_tyty = item.get_tyty_for_receiver (receiver);

  PathProbeCandidate::TraitItemCandidate trait_item_candidate{trait_ref,
							      trait_item_ref,
							      impl};
  PathProbeCandidate candidate{candidate_type, trait_item_tyty,
			       trait_item_ref->get_locus (),
			       trait_item_candidate};
  insert_candidate (std::move (candidate));
}

void
PathProbeExpr::process_predicate_for_candidates (
  const TyTy::TypeBoundPredicate &predicate)
{
  const TraitReference *trait_ref = predicate.get ();

  tl::optional<TyTy::TypeBoundPredicateItem> item
    = predicate.lookup_associated_item (search.to_string ());
  if (!item.has_value ())
    return;

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
      return;

    case TraitItemReference::TraitItemType::ERROR:
    default:
      rust_unreachable ();
      break;
    }

  TyTy::BaseType *trait_item_tyty = item->get_raw_item ()->get_tyty ();
  if (receiver->get_kind () != TyTy::DYNAMIC)
    trait_item_tyty = item->get_tyty_for_receiver (receiver);

  PathProbeCandidate::TraitItemCandidate trait_item_candidate{trait_ref,
							      trait_item_ref,
							      nullptr};
  PathProbeCandidate candidate{candidate_type, trait_item_tyty,
			       trait_item_ref->get_locus (),
			       trait_item_candidate};
  insert_candidate (std::move (candidate));
}

void
PathProbeExpr::insert_candidate (PathProbeCandidate candidate)
{
  bool is_type
    = candidate.type == PathProbeCandidate::CandidateType::IMPL_TYPE_ALIAS
      || candidate.type == PathProbeCandidate::CandidateType::TRAIT_TYPE_ALIAS;
  if (!candidate.is_error () && !is_type)
    candidates.insert (std::move (candidate));
}

bool
PathProbeExpr::is_receiver_generic () const
{
  const TyTy::BaseType *root = receiver->get_root ();
  bool receiver_is_type_param = root->get_kind () == TyTy::TypeKind::PARAM;
  bool receiver_is_dyn = root->get_kind () == TyTy::TypeKind::DYNAMIC;
  return receiver_is_type_param || receiver_is_dyn;
}

} // namespace Resolver
} // namespace Rust
