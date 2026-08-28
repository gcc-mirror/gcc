// Copyright (C) 2026 Free Software Foundation, Inc.

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

#ifndef RUST_HIR_PATH_PROBE_IMPL_TRAIT_H
#define RUST_HIR_PATH_PROBE_IMPL_TRAIT_H

#include "rust-hir-path-probe.h"

namespace Rust {
namespace Resolver {

class PathProbeImplTrait : public TypeCheckBase, public HIR::HIRImplVisitor
{
public:
  static std::set<PathProbeCandidate>
  Probe (TyTy::BaseType *receiver, const HIR::PathIdentSegment &segment_name,
	 const TraitReference *trait_reference);

  void visit (HIR::TypeAlias &alias) override;
  void visit (HIR::ConstantItem &constant) override;
  void visit (HIR::Function &function) override;

private:
  PathProbeImplTrait (TyTy::BaseType *receiver,
		      const HIR::PathIdentSegment &query,
		      const TraitReference *trait_reference);

  void process_trait_impl_items_for_candidates ();
  void process_impl_item_candidate (HirId id, HIR::ImplItem *item,
				    HIR::ImplBlock *impl);

  Analysis::Mappings &mappings;
  TyTy::BaseType *receiver;
  const HIR::PathIdentSegment &search;
  const TraitReference *trait_reference;
  std::set<PathProbeCandidate> candidates;
  HIR::ImplBlock *current_impl;
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_HIR_PATH_PROBE_IMPL_TRAIT_H
