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

#ifndef RUST_HIR_PATH_PROBE_TYPE_H
#define RUST_HIR_PATH_PROBE_TYPE_H

#include "rust-hir-map.h"
#include "rust-hir-path-probe.h"
#include "rust-tyty.h"

namespace Rust {
namespace Resolver {

struct TypePathProbeResult
{
  std::set<PathProbeCandidate> type_candidates;
  std::set<PathProbeCandidate> non_type_matches;

  bool has_type_candidates () const { return !type_candidates.empty (); }

  bool has_non_type_matches () const { return !non_type_matches.empty (); }

  bool is_empty () const
  {
    return type_candidates.empty () && non_type_matches.empty ();
  }
};

class TypePathProbe
{
public:
  static TypePathProbeResult Probe (TyTy::BaseType *receiver,
				    const HIR::PathIdentSegment &segment_name);

private:
  TypePathProbe (TyTy::BaseType *receiver,
		 const HIR::PathIdentSegment &segment_name)
    : mappings (Analysis::Mappings::get ()), receiver (receiver),
      search (segment_name)
  {}

  TypePathProbeResult probe ();

  void probe_generic ();
  void probe_adt (TyTy::ADTType *adt);
  void probe_fallback ();

  bool process_impl_item (HirId id, HIR::ImplItem *item, HIR::ImplBlock *impl);

  void process_trait_impl_item (HIR::ImplBlock *impl);

  PathProbeCandidate
  process_predicate_for_candidates (const TyTy::TypeBoundPredicate &predicate);

  void insert_candidate (PathProbeCandidate candidate);

  Analysis::Mappings &mappings;
  TyTy::BaseType *receiver;
  const HIR::PathIdentSegment &search;
  TypePathProbeResult result;
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_HIR_PATH_PROBE_TYPE_H
