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

// PathProbeExpr

PathProbeExpr::PathProbeExpr (TyTy::BaseType *receiver,
			      const HIR::PathIdentSegment &query,
			      DefId specific_trait_id)
  : TypeCheckBase (), receiver (receiver), search (query),
    current_impl (nullptr), specific_trait_id (specific_trait_id)
{}

std::set<PathProbeCandidate>
PathProbeExpr::Probe (TyTy::BaseType *receiver,
		      const HIR::PathIdentSegment &segment_name,
		      bool probe_impls, bool probe_bounds,
		      bool ignore_mandatory_trait_items,
		      DefId specific_trait_id)
{
  Analysis::Mappings &mappings = Analysis::Mappings::get ();

  PathProbeExpr probe (receiver, segment_name, specific_trait_id);
  if (probe_impls)
    {
      if (receiver->get_kind () == TyTy::TypeKind::ADT)
	{
	  const TyTy::ADTType *adt
	    = static_cast<const TyTy::ADTType *> (receiver);
	  if (adt->is_enum ())
	    probe.process_enum_item_for_candiates (adt);
	}

      probe.process_impl_items_for_candidates ();
    }

  if (!probe_bounds)
    return probe.candidates;

  if (!probe.is_receiver_generic ())
    {
      HIR::Trait *associated_trait = nullptr;
      if (specific_trait_id != UNKNOWN_DEFID)
	{
	  auto item_lookup = mappings.lookup_defid (specific_trait_id);
	  if (item_lookup.has_value ())
	    {
	      HIR::Item *item = item_lookup.value ();
	      rust_assert (item->get_item_kind ()
			   == HIR::Item::ItemKind::Trait);
	      associated_trait = static_cast<HIR::Trait *> (item);
	    }
	}

      auto probed_bounds = TypeBoundsProbe::Probe (receiver, associated_trait);
      for (auto &candidate : probed_bounds)
	{
	  const TraitReference *trait_ref = candidate.first;
	  if (specific_trait_id != UNKNOWN_DEFID)
	    {
	      if (trait_ref->get_mappings ().get_defid () != specific_trait_id)
		continue;
	    }

	  HIR::ImplBlock *impl = candidate.second;
	  probe.process_associated_trait_for_candidates (
	    trait_ref, impl, ignore_mandatory_trait_items);
	}
    }

  for (const TyTy::TypeBoundPredicate &predicate :
       receiver->get_specified_bounds ())
    {
      const TraitReference *trait_ref = predicate.get ();
      if (specific_trait_id != UNKNOWN_DEFID)
	{
	  if (trait_ref->get_mappings ().get_defid () != specific_trait_id)
	    continue;
	}

      probe.process_predicate_for_candidates (predicate,
					      ignore_mandatory_trait_items);
    }

  return probe.candidates;
}

void
PathProbeExpr::visit (HIR::TypeAlias &alias)
{
  Identifier name = alias.get_new_type_name ();
  if (search.to_string ().compare (name.as_string ()) == 0)
    {
      HirId tyid = alias.get_mappings ().get_hirid ();
      TyTy::BaseType *ty = nullptr;
      if (!query_type (tyid, &ty))
	return;

      PathProbeCandidate::ImplItemCandidate impl_item_candidate{&alias,
								current_impl};
      PathProbeCandidate candidate{
	PathProbeCandidate::CandidateType::IMPL_TYPE_ALIAS, ty,
	alias.get_locus (), impl_item_candidate};
      candidates.insert (std::move (candidate));
    }
}

void
PathProbeExpr::visit (HIR::ConstantItem &constant)
{
  Identifier name = constant.get_identifier ();
  if (search.to_string ().compare (name.as_string ()) == 0)
    {
      HirId tyid = constant.get_mappings ().get_hirid ();
      TyTy::BaseType *ty = nullptr;
      if (!query_type (tyid, &ty))
	return;

      PathProbeCandidate::ImplItemCandidate impl_item_candidate{&constant,
								current_impl};
      PathProbeCandidate candidate{
	PathProbeCandidate::CandidateType::IMPL_CONST, ty,
	constant.get_locus (), impl_item_candidate};
      candidates.insert (std::move (candidate));
    }
}

void
PathProbeExpr::visit (HIR::Function &function)
{
  Identifier name = function.get_function_name ();
  if (search.to_string ().compare (name.as_string ()) == 0)
    {
      HirId tyid = function.get_mappings ().get_hirid ();
      TyTy::BaseType *ty = nullptr;
      if (!query_type (tyid, &ty))
	return;

      PathProbeCandidate::ImplItemCandidate impl_item_candidate{&function,
								current_impl};
      PathProbeCandidate candidate{PathProbeCandidate::CandidateType::IMPL_FUNC,
				   ty, function.get_locus (),
				   impl_item_candidate};
      candidates.insert (std::move (candidate));
    }
}

void
PathProbeExpr::process_enum_item_for_candiates (const TyTy::ADTType *adt)
{
  if (specific_trait_id != UNKNOWN_DEFID)
    return;

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

void
PathProbeExpr::process_impl_items_for_candidates ()
{
  if (auto *adt = receiver->try_as<TyTy::ADTType> ())
    {
      auto adt_item = mappings.lookup_defid (adt->get_id ());
      if (adt_item.has_value ())
	{
	  NodeId adt_node_id = adt_item.value ()->get_mappings ().get_nodeid ();
	  mappings.iterate_adt_impl_items (
	    adt_node_id,
	    [&] (HirId id, HIR::ImplItem *item,
		 HIR::ImplBlock *impl) mutable -> bool {
	      process_impl_item_candidate (id, item, impl);
	      return true;
	    });
	  return;
	}
    }

  mappings.iterate_impl_items (
    [&] (HirId id, HIR::ImplItem *item, HIR::ImplBlock *impl) mutable -> bool {
      process_impl_item_candidate (id, item, impl);
      return true;
    });
}

void
PathProbeExpr::process_impl_item_candidate (HirId id, HIR::ImplItem *item,
					    HIR::ImplBlock *impl)
{
  current_impl = impl;
  HirId impl_ty_id = impl->get_type ().get_mappings ().get_hirid ();
  TyTy::BaseType *impl_block_ty = nullptr;
  if (!query_type (impl_ty_id, &impl_block_ty))
    return;

  if (!types_compatable (TyTy::TyWithLocation (receiver),
			 TyTy::TyWithLocation (impl_block_ty),
			 impl->get_locus (), false))
    return;

  // lets visit the impl_item
  item->accept_vis (*this);
}

void
PathProbeExpr::process_associated_trait_for_candidates (
  const TraitReference *trait_ref, HIR::ImplBlock *impl,
  bool ignore_mandatory_trait_items)
{
  const TraitItemReference *trait_item_ref = nullptr;
  if (!trait_ref->lookup_trait_item (search.to_string (), &trait_item_ref))
    return;

  bool trait_item_needs_implementation = !trait_item_ref->is_optional ();
  if (ignore_mandatory_trait_items && trait_item_needs_implementation)
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
      candidate_type = PathProbeCandidate::CandidateType::TRAIT_TYPE_ALIAS;
      break;

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
  candidates.insert (std::move (candidate));
}

void
PathProbeExpr::process_predicate_for_candidates (
  const TyTy::TypeBoundPredicate &predicate, bool ignore_mandatory_trait_items)
{
  const TraitReference *trait_ref = predicate.get ();

  tl::optional<TyTy::TypeBoundPredicateItem> item
    = predicate.lookup_associated_item (search.to_string ());
  if (!item.has_value ())
    return;

  if (ignore_mandatory_trait_items && item->needs_implementation ())
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
      candidate_type = PathProbeCandidate::CandidateType::TRAIT_TYPE_ALIAS;
      break;

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
  candidates.insert (std::move (candidate));
}

std::vector<std::pair<const TraitReference *, HIR::ImplBlock *>>
PathProbeExpr::union_bounds (
  const std::vector<std::pair</*const*/ TraitReference *, HIR::ImplBlock *>> a,
  const std::vector<std::pair<const TraitReference *, HIR::ImplBlock *>> b)
  const
{
  std::map<DefId, std::pair<const TraitReference *, HIR::ImplBlock *>> mapper;
  for (auto &ref : a)
    {
      mapper.insert ({ref.first->get_mappings ().get_defid (), ref});
    }
  for (auto &ref : b)
    {
      mapper.insert ({ref.first->get_mappings ().get_defid (), ref});
    }

  std::vector<std::pair<const TraitReference *, HIR::ImplBlock *>> union_set;

  for (auto it = mapper.begin (); it != mapper.end (); it++)
    union_set.emplace_back (it->second.first, it->second.second);

  return union_set;
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
