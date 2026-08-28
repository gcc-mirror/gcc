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

#include "rust-hir-path-probe-impl-trait.h"
#include "rust-hir-item.h"
#include "rust-type-util.h"

namespace Rust {
namespace Resolver {

PathProbeImplTrait::PathProbeImplTrait (TyTy::BaseType *receiver,
					const HIR::PathIdentSegment &query,
					const TraitReference *trait_reference)
  : TypeCheckBase (), mappings (Analysis::Mappings::get ()),
    receiver (receiver), search (query), trait_reference (trait_reference),
    current_impl (nullptr)
{}

std::set<PathProbeCandidate>
PathProbeImplTrait::Probe (TyTy::BaseType *receiver,
			   const HIR::PathIdentSegment &segment_name,
			   const TraitReference *trait_reference)
{
  PathProbeImplTrait probe (receiver, segment_name, trait_reference);
  probe.process_trait_impl_items_for_candidates ();
  return probe.candidates;
}

void
PathProbeImplTrait::process_trait_impl_items_for_candidates ()
{
  NodeId trait_node_id = trait_reference->get_mappings ().get_nodeid ();
  mappings.iterate_trait_impl_items (trait_node_id,
				     [this] (HirId id, HIR::ImplItem *item,
					     HIR::ImplBlock *impl) -> bool {
				       process_impl_item_candidate (id, item,
								    impl);
				       return true;
				     });
}

void
PathProbeImplTrait::process_impl_item_candidate (HirId id, HIR::ImplItem *item,
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

  item->accept_vis (*this);
}

void
PathProbeImplTrait::visit (HIR::TypeAlias &alias)
{
  if (search.to_string () != alias.get_new_type_name ().as_string ())
    return;

  TyTy::BaseType *ty = nullptr;
  if (!query_type (alias.get_mappings ().get_hirid (), &ty))
    return;

  PathProbeCandidate::ImplItemCandidate item{&alias, current_impl};
  candidates.insert (
    {PathProbeCandidate::IMPL_TYPE_ALIAS, ty, alias.get_locus (), item});
}

void
PathProbeImplTrait::visit (HIR::ConstantItem &constant)
{
  if (search.to_string () != constant.get_identifier ().as_string ())
    return;

  TyTy::BaseType *ty = nullptr;
  if (!query_type (constant.get_mappings ().get_hirid (), &ty))
    return;

  PathProbeCandidate::ImplItemCandidate item{&constant, current_impl};
  candidates.insert (
    {PathProbeCandidate::IMPL_CONST, ty, constant.get_locus (), item});
}

void
PathProbeImplTrait::visit (HIR::Function &function)
{
  if (search.to_string () != function.get_function_name ().as_string ())
    return;

  TyTy::BaseType *ty = nullptr;
  if (!query_type (function.get_mappings ().get_hirid (), &ty))
    return;

  PathProbeCandidate::ImplItemCandidate item{&function, current_impl};
  candidates.insert (
    {PathProbeCandidate::IMPL_FUNC, ty, function.get_locus (), item});
}

} // namespace Resolver
} // namespace Rust
