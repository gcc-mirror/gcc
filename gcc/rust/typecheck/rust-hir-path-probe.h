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

#ifndef RUST_HIR_PATH_PROBE_H
#define RUST_HIR_PATH_PROBE_H

#include "rust-hir-type-check-base.h"
#include "rust-hir-visitor.h"
#include "rust-tyty.h"

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
  location_t locus;
  union Candidate
  {
    EnumItemCandidate enum_field;
    ImplItemCandidate impl;
    TraitItemCandidate trait;

    Candidate (EnumItemCandidate enum_field);
    Candidate (ImplItemCandidate impl);
    Candidate (TraitItemCandidate trait);
  } item;

  PathProbeCandidate (CandidateType type, TyTy::BaseType *ty, location_t locus,
		      EnumItemCandidate enum_field);

  PathProbeCandidate (CandidateType type, TyTy::BaseType *ty, location_t locus,
		      ImplItemCandidate impl);

  PathProbeCandidate (CandidateType type, TyTy::BaseType *ty, location_t locus,
		      TraitItemCandidate trait);

  std::string as_string () const;

  bool is_enum_candidate () const;

  bool is_impl_candidate () const;

  bool is_trait_candidate () const;

  bool is_full_trait_item_candidate () const;

  static PathProbeCandidate get_error ();

  bool is_error () const;

  DefId get_defid () const;

  bool operator< (const PathProbeCandidate &c) const;
};

class ReportMultipleCandidateError : private TypeCheckBase
{
public:
  static void Report (std::set<PathProbeCandidate> &candidates,
		      const HIR::PathIdentSegment &query,
		      location_t query_locus)
  {
    rich_location r (line_table, query_locus);
    for (auto &c : candidates)
      r.add_range (c.locus);

    std::string rich_msg = "multiple " + query.to_string () + " found";
    r.add_fixit_replace (rich_msg.c_str ());

    rust_error_at (r, ErrorCode::E0034,
		   "multiple applicable items in scope for: %qs",
		   query.to_string ().c_str ());
  }
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_HIR_PATH_PROBE_H
