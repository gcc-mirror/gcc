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

#ifndef RUST_HIR_DOT_OPERATOR
#define RUST_HIR_DOT_OPERATOR

#include "rust-hir-path-probe.h"

namespace Rust {
namespace Resolver {

// lookup if method exists for current type
// if exists: done
// if not: check again for auto-ref and auto-mut-ref
// deref and start again with 1.*/

// https://doc.rust-lang.org/nightly/nomicon/dot-operator.html

class MethodResolution
{
public:
  static PathProbeCandidate *
  Select (std::vector<PathProbeCandidate> &candidates, TyTy::BaseType *receiver,
	  std::vector<Adjustment> &adjustments)
  {
    TyTy::BaseType *r = receiver;
    while (true)
      {
	PathProbeCandidate *c = nullptr;

	// 1. try raw
	c = Try (candidates, r);
	if (c != nullptr)
	  return c;

	// 2. try ref
	TyTy::ReferenceType *r1
	  = new TyTy::ReferenceType (r->get_ref (), TyTy::TyVar (r->get_ref ()),
				     false);
	c = Try (candidates, r1);
	if (c != nullptr)
	  {
	    adjustments.push_back (
	      Adjustment (Adjustment::AdjustmentType::IMM_REF, r1));
	    return c;
	  }

	// 3. try mut ref
	TyTy::ReferenceType *r2
	  = new TyTy::ReferenceType (r->get_ref (), TyTy::TyVar (r->get_ref ()),
				     true);
	c = Try (candidates, r2);
	if (c != nullptr)
	  {
	    adjustments.push_back (
	      Adjustment (Adjustment::AdjustmentType::MUT_REF, r2));
	    return c;
	  }

	// 4. deref to to 1, if cannot deref then quit
	bool can_deref = r->get_kind () == TyTy::TypeKind::REF;
	if (!can_deref)
	  return nullptr;

	// FIXME this needs to use deref trait and fall back to unsized to
	// remove array syntax

	TyTy::ReferenceType *rr = static_cast<TyTy::ReferenceType *> (r);
	r = rr->get_base ();
	adjustments.push_back (
	  Adjustment (Adjustment::AdjustmentType::DEREF_REF, r));
      }
    return nullptr;
  }

private:
  static PathProbeCandidate *Try (std::vector<PathProbeCandidate> &candidates,
				  const TyTy::BaseType *receiver)
  {
    TypeCheckContext *context = TypeCheckContext::get ();

    // probe impls
    for (auto &c : candidates)
      {
	bool is_func = c.type == PathProbeCandidate::CandidateType::IMPL_FUNC;
	HIR::ImplBlock *block = c.item.impl.parent;
	if (is_func && !block->has_trait_ref ())
	  {
	    HIR::Function *func
	      = static_cast<HIR::Function *> (c.item.impl.impl_item);

	    TyTy::BaseType *lookup = nullptr;
	    bool ok = context->lookup_type (func->get_mappings ().get_hirid (),
					    &lookup);
	    rust_assert (ok);
	    rust_assert (lookup->get_kind () == TyTy::TypeKind::FNDEF);

	    TyTy::FnType *fn = static_cast<TyTy::FnType *> (lookup);
	    if (fn->is_method ())
	      {
		TyTy::BaseType *fn_self = fn->get_self_type ();
		if (receiver->can_eq (fn_self, false))
		  {
		    return &c;
		  }
	      }
	  }
      }

    // probe trait impls
    for (auto &c : candidates)
      {
	bool is_func = c.type == PathProbeCandidate::CandidateType::IMPL_FUNC;
	HIR::ImplBlock *block = c.item.impl.parent;
	if (is_func && block->has_trait_ref ())
	  {
	    HIR::Function *func
	      = static_cast<HIR::Function *> (c.item.impl.impl_item);

	    TyTy::BaseType *lookup = nullptr;
	    bool ok = context->lookup_type (func->get_mappings ().get_hirid (),
					    &lookup);
	    rust_assert (ok);
	    rust_assert (lookup->get_kind () == TyTy::TypeKind::FNDEF);

	    TyTy::FnType *fn = static_cast<TyTy::FnType *> (lookup);
	    if (fn->is_method ())
	      {
		TyTy::BaseType *fn_self = fn->get_self_type ();
		if (receiver->can_eq (fn_self, false))
		  {
		    return &c;
		  }
	      }
	  }
      }

    // probe trait bounds
    for (auto &c : candidates)
      {
	bool is_func = c.type == PathProbeCandidate::CandidateType::TRAIT_FUNC;
	if (is_func)
	  {
	    const TraitItemReference *item_ref = c.item.trait.item_ref;
	    TyTy::BaseType *lookup = item_ref->get_tyty ();
	    rust_assert (lookup->get_kind () == TyTy::TypeKind::FNDEF);

	    TyTy::FnType *fn = static_cast<TyTy::FnType *> (lookup);
	    if (fn->is_method ())
	      {
		TyTy::BaseType *fn_self = fn->get_self_type ();
		if (receiver->can_eq (fn_self, false))
		  {
		    return &c;
		  }
	      }
	  }
      }

    return nullptr;
  }
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_HIR_DOT_OPERATOR
