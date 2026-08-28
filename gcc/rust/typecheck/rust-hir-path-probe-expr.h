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

#ifndef RUST_HIR_PATH_PROBE_EXPR_H
#define RUST_HIR_PATH_PROBE_EXPR_H

#include "rust-hir-path-probe.h"

namespace Rust {
namespace Resolver {

class PathProbeExpr : public TypeCheckBase, public HIR::HIRImplVisitor
{
public:
  static std::set<PathProbeCandidate>
  Probe (TyTy::BaseType *receiver, const HIR::PathIdentSegment &segment_name,
	 bool probe_impls, bool probe_bounds, bool ignore_mandatory_trait_items,
	 DefId specific_trait_id = UNKNOWN_DEFID);

  void visit (HIR::TypeAlias &alias) override;
  void visit (HIR::ConstantItem &constant) override;
  void visit (HIR::Function &function) override;

protected:
  void process_enum_item_for_candiates (const TyTy::ADTType *adt);

  void process_impl_items_for_candidates ();

  void process_impl_item_candidate (HirId id, HIR::ImplItem *item,
				    HIR::ImplBlock *impl);

  void
  process_associated_trait_for_candidates (const TraitReference *trait_ref,
					   HIR::ImplBlock *impl,
					   bool ignore_mandatory_trait_items);

  void
  process_predicate_for_candidates (const TyTy::TypeBoundPredicate &predicate,
				    bool ignore_mandatory_trait_items);

protected:
  PathProbeExpr (TyTy::BaseType *receiver, const HIR::PathIdentSegment &query,
		 DefId specific_trait_id);

  std::vector<std::pair<const TraitReference *, HIR::ImplBlock *>>
  union_bounds (
    const std::vector<std::pair</*const*/ TraitReference *, HIR::ImplBlock *>>
      a,
    const std::vector<std::pair<const TraitReference *, HIR::ImplBlock *>> b)
    const;

  bool is_receiver_generic () const;

  TyTy::BaseType *receiver;
  const HIR::PathIdentSegment &search;
  std::set<PathProbeCandidate> candidates;
  HIR::ImplBlock *current_impl;
  DefId specific_trait_id;
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_HIR_PATH_PROBE_EXPR_H
