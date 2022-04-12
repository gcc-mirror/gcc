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
};

class MethodResolver : public TypeCheckBase
{
protected:
  using Rust::Resolver::TypeCheckBase::visit;

public:
  static MethodCandidate Probe (const TyTy::BaseType *receiver,
				const HIR::PathIdentSegment &segment_name,
				bool autoderef_flag = false);

protected:
  struct predicate_candidate
  {
    TyTy::TypeBoundPredicateItem lookup;
    TyTy::FnType *fntype;
  };

  static MethodCandidate Try (const TyTy::BaseType *r,
			      const HIR::PathIdentSegment &segment_name,
			      std::vector<Adjustment> &adjustments);

  static std::vector<predicate_candidate> get_predicate_items (
    const HIR::PathIdentSegment &segment_name, const TyTy::BaseType &receiver,
    const std::vector<TyTy::TypeBoundPredicate> &specified_bounds);

  PathProbeCandidate select ();

  MethodResolver (
    const TyTy::BaseType &receiver, const HIR::PathIdentSegment &segment_name,
    const std::vector<MethodResolver::predicate_candidate> &predicate_items)
    : receiver (receiver), segment_name (segment_name),
      predicate_items (predicate_items)
  {}

  const TyTy::BaseType &receiver;
  const HIR::PathIdentSegment &segment_name;
  const std::vector<MethodResolver::predicate_candidate> &predicate_items;
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_HIR_DOT_OPERATOR
