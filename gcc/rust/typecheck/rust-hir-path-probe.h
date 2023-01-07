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

#ifndef RUST_HIR_PATH_PROBE_H
#define RUST_HIR_PATH_PROBE_H

#include "rust-hir-type-check-base.h"
#include "rust-hir-full.h"
#include "rust-tyty.h"
#include "rust-substitution-mapper.h"
#include "rust-hir-type-bounds.h"

namespace Rust {
namespace Resolver {

struct PathProbeCandidate
{
  enum CandidateType
  {
    ERROR,

    ENUM_VARIANT,

    IMPL_CONST,
    IMPL_TYPE_ALIAS,
    IMPL_FUNC,

    TRAIT_ITEM_CONST,
    TRAIT_TYPE_ALIAS,
    TRAIT_FUNC,
  };

  struct EnumItemCandidate
  {
    const TyTy::ADTType *parent;
    const TyTy::VariantDef *variant;
  };

  struct ImplItemCandidate
  {
    HIR::ImplItem *impl_item;
    HIR::ImplBlock *parent;
  };

  struct TraitItemCandidate
  {
    const TraitReference *trait_ref;
    const TraitItemReference *item_ref;
    HIR::ImplBlock *impl;
  };

  CandidateType type;
  TyTy::BaseType *ty;
  Location locus;
  union Candidate
  {
    EnumItemCandidate enum_field;
    ImplItemCandidate impl;
    TraitItemCandidate trait;

    Candidate (EnumItemCandidate enum_field) : enum_field (enum_field) {}
    Candidate (ImplItemCandidate impl) : impl (impl) {}
    Candidate (TraitItemCandidate trait) : trait (trait) {}
  } item;

  PathProbeCandidate (CandidateType type, TyTy::BaseType *ty, Location locus,
		      EnumItemCandidate enum_field)
    : type (type), ty (ty), locus (locus), item (enum_field)
  {}

  PathProbeCandidate (CandidateType type, TyTy::BaseType *ty, Location locus,
		      ImplItemCandidate impl)
    : type (type), ty (ty), locus (locus), item (impl)
  {}

  PathProbeCandidate (CandidateType type, TyTy::BaseType *ty, Location locus,
		      TraitItemCandidate trait)
    : type (type), ty (ty), locus (locus), item (trait)
  {}

  std::string as_string () const
  {
    return "PathProbe candidate TODO - as_string";
  }

  bool is_enum_candidate () const { return type == ENUM_VARIANT; }

  bool is_impl_candidate () const
  {
    return type == IMPL_CONST || type == IMPL_TYPE_ALIAS || type == IMPL_FUNC;
  }

  bool is_trait_candidate () const
  {
    return type == TRAIT_ITEM_CONST || type == TRAIT_TYPE_ALIAS
	   || type == TRAIT_FUNC;
  }

  bool is_full_trait_item_candidate () const
  {
    return is_trait_candidate () && item.trait.impl == nullptr;
  }

  static PathProbeCandidate get_error ()
  {
    return PathProbeCandidate (ERROR, nullptr, Location (),
			       ImplItemCandidate{nullptr, nullptr});
  }

  bool is_error () const { return type == ERROR; }

  DefId get_defid () const
  {
    switch (type)
      {
      case ENUM_VARIANT:
	return item.enum_field.variant->get_defid ();
	break;

      case IMPL_CONST:
      case IMPL_TYPE_ALIAS:
      case IMPL_FUNC:
	return item.impl.impl_item->get_impl_mappings ().get_defid ();
	break;

      case TRAIT_ITEM_CONST:
      case TRAIT_TYPE_ALIAS:
      case TRAIT_FUNC:
	return item.trait.item_ref->get_mappings ().get_defid ();
	break;

      case ERROR:
      default:
	return UNKNOWN_DEFID;
      }

    return UNKNOWN_DEFID;
  }

  bool operator< (const PathProbeCandidate &c) const
  {
    return get_defid () < c.get_defid ();
  }
};

class PathProbeType : public TypeCheckBase, public HIR::HIRImplVisitor
{
public:
  static std::set<PathProbeCandidate>
  Probe (const TyTy::BaseType *receiver,
	 const HIR::PathIdentSegment &segment_name, bool probe_impls,
	 bool probe_bounds, bool ignore_mandatory_trait_items,
	 DefId specific_trait_id = UNKNOWN_DEFID)
  {
    PathProbeType probe (receiver, segment_name, specific_trait_id);
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

    if (!probe.is_reciever_generic ())
      {
	std::vector<std::pair<TraitReference *, HIR::ImplBlock *>> probed_bounds
	  = TypeBoundsProbe::Probe (receiver);
	for (auto &candidate : probed_bounds)
	  {
	    const TraitReference *trait_ref = candidate.first;
	    if (specific_trait_id != UNKNOWN_DEFID)
	      {
		if (trait_ref->get_mappings ().get_defid ()
		    != specific_trait_id)
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

  void visit (HIR::TypeAlias &alias) override
  {
    Identifier name = alias.get_new_type_name ();
    if (search.as_string ().compare (name) == 0)
      {
	HirId tyid = alias.get_mappings ().get_hirid ();
	TyTy::BaseType *ty = nullptr;
	bool ok = query_type (tyid, &ty);
	rust_assert (ok);

	PathProbeCandidate::ImplItemCandidate impl_item_candidate{&alias,
								  current_impl};
	PathProbeCandidate candidate{
	  PathProbeCandidate::CandidateType::IMPL_TYPE_ALIAS, ty,
	  alias.get_locus (), impl_item_candidate};
	candidates.insert (std::move (candidate));
      }
  }

  void visit (HIR::ConstantItem &constant) override
  {
    Identifier name = constant.get_identifier ();
    if (search.as_string ().compare (name) == 0)
      {
	HirId tyid = constant.get_mappings ().get_hirid ();
	TyTy::BaseType *ty = nullptr;
	bool ok = query_type (tyid, &ty);
	rust_assert (ok);

	PathProbeCandidate::ImplItemCandidate impl_item_candidate{&constant,
								  current_impl};
	PathProbeCandidate candidate{
	  PathProbeCandidate::CandidateType::IMPL_CONST, ty,
	  constant.get_locus (), impl_item_candidate};
	candidates.insert (std::move (candidate));
      }
  }

  void visit (HIR::Function &function) override
  {
    Identifier name = function.get_function_name ();
    if (search.as_string ().compare (name) == 0)
      {
	HirId tyid = function.get_mappings ().get_hirid ();
	TyTy::BaseType *ty = nullptr;
	bool ok = query_type (tyid, &ty);
	rust_assert (ok);

	PathProbeCandidate::ImplItemCandidate impl_item_candidate{&function,
								  current_impl};
	PathProbeCandidate candidate{
	  PathProbeCandidate::CandidateType::IMPL_FUNC, ty,
	  function.get_locus (), impl_item_candidate};
	candidates.insert (std::move (candidate));
      }
  }

protected:
  void process_enum_item_for_candiates (const TyTy::ADTType *adt)
  {
    if (specific_trait_id != UNKNOWN_DEFID)
      return;

    TyTy::VariantDef *v;
    if (!adt->lookup_variant (search.as_string (), &v))
      return;

    PathProbeCandidate::EnumItemCandidate enum_item_candidate{adt, v};
    PathProbeCandidate candidate{
      PathProbeCandidate::CandidateType::ENUM_VARIANT, receiver->clone (),
      mappings->lookup_location (adt->get_ty_ref ()), enum_item_candidate};
    candidates.insert (std::move (candidate));
  }

  void process_impl_items_for_candidates ()
  {
    mappings->iterate_impl_items ([&] (HirId id, HIR::ImplItem *item,
				       HIR::ImplBlock *impl) mutable -> bool {
      process_impl_item_candidate (id, item, impl);
      return true;
    });
  }

  void process_impl_item_candidate (HirId id, HIR::ImplItem *item,
				    HIR::ImplBlock *impl);

  void
  process_associated_trait_for_candidates (const TraitReference *trait_ref,
					   HIR::ImplBlock *impl,
					   bool ignore_mandatory_trait_items)
  {
    const TraitItemReference *trait_item_ref = nullptr;
    if (!trait_ref->lookup_trait_item (search.as_string (), &trait_item_ref))
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
	gcc_unreachable ();
	break;
      }

    TyTy::BaseType *trait_item_tyty = trait_item_ref->get_tyty ();

    // we can substitute the Self with the receiver here
    if (trait_item_tyty->get_kind () == TyTy::TypeKind::FNDEF)
      {
	TyTy::FnType *fn = static_cast<TyTy::FnType *> (trait_item_tyty);
	TyTy::SubstitutionParamMapping *param = nullptr;
	for (auto &param_mapping : fn->get_substs ())
	  {
	    const HIR::TypeParam &type_param
	      = param_mapping.get_generic_param ();
	    if (type_param.get_type_representation ().compare ("Self") == 0)
	      {
		param = &param_mapping;
		break;
	      }
	  }
	rust_assert (param != nullptr);

	std::vector<TyTy::SubstitutionArg> mappings;
	mappings.push_back (TyTy::SubstitutionArg (param, receiver->clone ()));

	Location locus; // FIXME
	TyTy::SubstitutionArgumentMappings args (std::move (mappings), {},
						 locus);
	trait_item_tyty = SubstMapperInternal::Resolve (trait_item_tyty, args);
      }

    PathProbeCandidate::TraitItemCandidate trait_item_candidate{trait_ref,
								trait_item_ref,
								impl};

    PathProbeCandidate candidate{candidate_type, trait_item_tyty,
				 trait_item_ref->get_locus (),
				 trait_item_candidate};
    candidates.insert (std::move (candidate));
  }

  void
  process_predicate_for_candidates (const TyTy::TypeBoundPredicate &predicate,
				    bool ignore_mandatory_trait_items)
  {
    const TraitReference *trait_ref = predicate.get ();

    TyTy::TypeBoundPredicateItem item
      = predicate.lookup_associated_item (search.as_string ());
    if (item.is_error ())
      return;

    if (ignore_mandatory_trait_items && item.needs_implementation ())
      return;

    const TraitItemReference *trait_item_ref = item.get_raw_item ();
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
	gcc_unreachable ();
	break;
      }

    TyTy::BaseType *trait_item_tyty = item.get_tyty_for_receiver (receiver);
    PathProbeCandidate::TraitItemCandidate trait_item_candidate{trait_ref,
								trait_item_ref,
								nullptr};
    PathProbeCandidate candidate{candidate_type, trait_item_tyty,
				 trait_item_ref->get_locus (),
				 trait_item_candidate};
    candidates.insert (std::move (candidate));
  }

protected:
  PathProbeType (const TyTy::BaseType *receiver,
		 const HIR::PathIdentSegment &query, DefId specific_trait_id)
    : TypeCheckBase (), receiver (receiver), search (query),
      current_impl (nullptr), specific_trait_id (specific_trait_id)
  {}

  std::vector<std::pair<const TraitReference *, HIR::ImplBlock *>>
  union_bounds (
    const std::vector<std::pair</*const*/ TraitReference *, HIR::ImplBlock *>>
      a,
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
      {
	union_set.push_back ({it->second.first, it->second.second});
      }
    return union_set;
  }

  bool is_reciever_generic () const
  {
    const TyTy::BaseType *root = receiver->get_root ();
    bool receiver_is_type_param = root->get_kind () == TyTy::TypeKind::PARAM;
    bool receiver_is_dyn = root->get_kind () == TyTy::TypeKind::DYNAMIC;
    return receiver_is_type_param || receiver_is_dyn;
  }

  const TyTy::BaseType *receiver;
  const HIR::PathIdentSegment &search;
  std::set<PathProbeCandidate> candidates;
  HIR::ImplBlock *current_impl;
  DefId specific_trait_id;
};

class ReportMultipleCandidateError : private TypeCheckBase
{
public:
  static void Report (std::set<PathProbeCandidate> &candidates,
		      const HIR::PathIdentSegment &query, Location query_locus)
  {
    RichLocation r (query_locus);
    for (auto &c : candidates)
      r.add_range (c.locus);

    rust_error_at (r, "multiple applicable items in scope for: %s",
		   query.as_string ().c_str ());
  }
};

class PathProbeImplTrait : public PathProbeType
{
public:
  static std::set<PathProbeCandidate>
  Probe (const TyTy::BaseType *receiver,
	 const HIR::PathIdentSegment &segment_name,
	 const TraitReference *trait_reference)
  {
    PathProbeImplTrait probe (receiver, segment_name, trait_reference);
    // iterate all impls for this trait and receiver
    // then search for possible candidates using base class behaviours
    probe.process_trait_impl_items_for_candidates ();
    return probe.candidates;
  }

private:
  void process_trait_impl_items_for_candidates ();

  PathProbeImplTrait (const TyTy::BaseType *receiver,
		      const HIR::PathIdentSegment &query,
		      const TraitReference *trait_reference)
    : PathProbeType (receiver, query, UNKNOWN_DEFID),
      trait_reference (trait_reference)
  {}

  const TraitReference *trait_reference;
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_HIR_PATH_PROBE_H
