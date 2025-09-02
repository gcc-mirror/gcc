// Copyright (C) 2020-2025 Free Software Foundation, Inc.

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

#ifndef RUST_HIR_DOT_OPERATOR
#define RUST_HIR_DOT_OPERATOR

#include "rust-hir-path-probe.h"

namespace Rust {
namespace Resolver {

struct MethodCandidate
{
  PathProbeCandidate candidate;
  std::vector<Adjustment> adjustments;

  static MethodCandidate get_error ()
  {
    return {PathProbeCandidate::get_error (), {}};
  }

  bool is_error () const { return candidate.is_error (); }

  DefId get_defid () const { return candidate.get_defid (); }

  bool operator< (const MethodCandidate &c) const
  {
    return get_defid () < c.get_defid ();
  }
};

class MethodResolver : private TypeCheckBase, protected AutoderefCycle
{
public:
  struct predicate_candidate
  {
    TyTy::TypeBoundPredicateItem lookup;
    TyTy::FnType *fntype;
  };

  static std::set<MethodCandidate>
  Probe (TyTy::BaseType *receiver, const HIR::PathIdentSegment &segment_name,
	 bool autoderef_flag = false);

  static std::set<MethodCandidate>
  Select (std::set<MethodCandidate> &candidates, TyTy::BaseType *receiver,
	  std::vector<TyTy::BaseType *> arguments);

  static std::vector<predicate_candidate> get_predicate_items (
    const HIR::PathIdentSegment &segment_name, const TyTy::BaseType &receiver,
    const std::vector<TyTy::TypeBoundPredicate> &specified_bounds);

  struct impl_item_candidate
  {
    HIR::Function *item;
    HIR::ImplBlock *impl_block;
    TyTy::FnType *ty;
  };

  struct trait_item_candidate
  {
    const HIR::TraitItemFunc *item;
    const HIR::Trait *trait;
    TyTy::FnType *ty;
    const TraitReference *reference;
    const TraitItemReference *item_ref;
  };

protected:
  MethodResolver (bool autoderef_flag,
		  const HIR::PathIdentSegment &segment_name);

  void try_hook (const TyTy::BaseType &r) override;

  bool select (TyTy::BaseType &receiver) override;

private:
  std::vector<Adjustment>
  append_adjustments (const std::vector<Adjustment> &adjustments) const;

  std::vector<impl_item_candidate>
  assemble_inherent_impl_candidates (const TyTy::BaseType &receiver);

  void assemble_trait_impl_candidates (
    const TyTy::BaseType &receiver,
    std::vector<impl_item_candidate> &impl_candidates,
    std::vector<trait_item_candidate> &trait_candidates);

  bool try_select_predicate_candidates (TyTy::BaseType &receiver);

  bool try_select_inherent_impl_candidates (
    TyTy::BaseType &receiver,
    const std::vector<impl_item_candidate> &candidates,
    bool trait_impl_blocks_only);

  bool try_select_trait_impl_candidates (
    TyTy::BaseType &receiver,
    const std::vector<trait_item_candidate> &candidates);

private:
  // search
  const HIR::PathIdentSegment &segment_name;
  std::vector<MethodResolver::predicate_candidate> predicate_items;

  // mutable fields
  std::set<MethodCandidate> result;
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_HIR_DOT_OPERATOR
