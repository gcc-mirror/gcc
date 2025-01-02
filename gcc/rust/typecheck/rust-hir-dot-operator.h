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

protected:
  MethodResolver (bool autoderef_flag,
		  const HIR::PathIdentSegment &segment_name);

  void try_hook (const TyTy::BaseType &r) override;

  bool select (TyTy::BaseType &receiver) override;

private:
  std::vector<Adjustment>
  append_adjustments (const std::vector<Adjustment> &adjustments) const;

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
