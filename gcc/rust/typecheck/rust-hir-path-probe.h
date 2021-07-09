// Copyright (C) 2020 Free Software Foundation, Inc.

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

namespace Rust {
namespace Resolver {

struct PathProbeCandidate
{
  HIR::ImplItem *impl_item;
  TyTy::BaseType *ty;
};

class PathProbeType : public TypeCheckBase
{
  using Rust::Resolver::TypeCheckBase::visit;

public:
  static std::vector<PathProbeCandidate>
  Probe (TyTy::BaseType *receiver, const HIR::PathIdentSegment &segment_name)
  {
    PathProbeType probe (receiver, segment_name);
    probe.mappings->iterate_impl_items (
      [&] (HirId id, HIR::ImplItem *item,
	   HIR::ImplBlock *impl) mutable -> bool {
	probe.process_candidate (id, item, impl);
	return true;
      });
    return probe.candidates;
  }

  void process_candidate (HirId id, HIR::ImplItem *item, HIR::ImplBlock *impl)
  {
    HirId impl_ty_id = impl->get_type ()->get_mappings ().get_hirid ();
    TyTy::BaseType *impl_block_ty = nullptr;
    bool ok = context->lookup_type (impl_ty_id, &impl_block_ty);
    rust_assert (ok);

    if (!receiver->can_eq (impl_block_ty, false))
      return;

    // lets visit the impl_item
    item->accept_vis (*this);
  }

  void visit (HIR::TypeAlias &alias) override
  {
    Identifier name = alias.get_new_type_name ();
    if (search.as_string ().compare (name) == 0)
      {
	HirId tyid = alias.get_mappings ().get_hirid ();
	TyTy::BaseType *ty = nullptr;
	bool ok = context->lookup_type (tyid, &ty);
	rust_assert (ok);

	PathProbeCandidate candidate{&alias, ty};
	candidates.push_back (std::move (candidate));
      }
  }

  void visit (HIR::ConstantItem &constant) override
  {
    Identifier name = constant.get_identifier ();
    if (search.as_string ().compare (name) == 0)
      {
	HirId tyid = constant.get_mappings ().get_hirid ();
	TyTy::BaseType *ty = nullptr;
	bool ok = context->lookup_type (tyid, &ty);
	rust_assert (ok);

	PathProbeCandidate candidate{&constant, ty};
	candidates.push_back (std::move (candidate));
      }
  }

  void visit (HIR::Function &function) override
  {
    Identifier name = function.get_function_name ();
    if (search.as_string ().compare (name) == 0)
      {
	HirId tyid = function.get_mappings ().get_hirid ();
	TyTy::BaseType *ty = nullptr;
	bool ok = context->lookup_type (tyid, &ty);
	rust_assert (ok);

	PathProbeCandidate candidate{&function, ty};
	candidates.push_back (std::move (candidate));
      }
  }

private:
  PathProbeType (TyTy::BaseType *receiver, const HIR::PathIdentSegment &query)
    : TypeCheckBase (), receiver (receiver), search (query)
  {}

  TyTy::BaseType *receiver;
  const HIR::PathIdentSegment &search;
  std::vector<PathProbeCandidate> candidates;
};

class ReportMultipleCandidateError : private TypeCheckBase
{
  using Rust::Resolver::TypeCheckBase::visit;

public:
  static void Report (std::vector<PathProbeCandidate> &candidates,
		      const HIR::PathIdentSegment &query, Location query_locus)
  {
    RichLocation r (query_locus);
    ReportMultipleCandidateError visitor (r);
    for (auto &c : candidates)
      c.impl_item->accept_vis (visitor);

    rust_error_at (r, "multiple applicable items in scope for: %s",
		   query.as_string ().c_str ());
  }

  void visit (HIR::TypeAlias &alias) override
  {
    r.add_range (alias.get_locus ());
  }

  void visit (HIR::ConstantItem &constant) override
  {
    r.add_range (constant.get_locus ());
  }

  void visit (HIR::Function &function) override
  {
    r.add_range (function.get_locus ());
  }

private:
  ReportMultipleCandidateError (RichLocation &r) : TypeCheckBase (), r (r) {}

  RichLocation &r;
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_HIR_PATH_PROBE_H
