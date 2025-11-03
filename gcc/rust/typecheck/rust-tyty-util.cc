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

#include "rust-hir-type-check.h"
#include "rust-mapping-common.h"
#include "rust-system.h"
#include "rust-tyty.h"

namespace Rust {
namespace TyTy {

TyVar::TyVar (HirId ref) : ref (ref)
{
  // ensure this reference is defined within the context
  auto context = Resolver::TypeCheckContext::get ();
  BaseType *lookup = nullptr;
  bool ok = context->lookup_type (ref, &lookup);
  if (!ok || lookup == nullptr || lookup->get_kind () == TypeKind::ERROR)
    return;
}

BaseType *
TyVar::get_tyty () const
{
  auto context = Resolver::TypeCheckContext::get ();
  BaseType *lookup = nullptr;
  bool ok = context->lookup_type (ref, &lookup);
  if (!ok || lookup == nullptr)
    return nullptr;
  return lookup;
}

TyVar
TyVar::get_implicit_infer_var (location_t locus)
{
  auto &mappings = Analysis::Mappings::get ();
  auto context = Resolver::TypeCheckContext::get ();

  HirId next = mappings.get_next_hir_id ();
  auto infer = new InferType (next, InferType::InferTypeKind::GENERAL,
			      InferType::TypeHint::Default (), locus);

  context->insert_implicit_type (infer->get_ref (), infer);
  mappings.insert_location (infer->get_ref (), locus);

  return TyVar (infer->get_ref ());
}

TyVar
TyVar::get_implicit_const_infer_var (location_t locus, TyVar *implicit_type)
{
  auto &mappings = Analysis::Mappings::get ();
  auto context = Resolver::TypeCheckContext::get ();

  TyVar it = (implicit_type != nullptr) ? *implicit_type
					: get_implicit_infer_var (locus);
  HirId next = mappings.get_next_hir_id ();
  auto infer = new ConstInferType (it.get_tyty (), next, next, {});

  context->insert_implicit_type (infer->get_ref (), infer);
  mappings.insert_location (infer->get_ref (), locus);

  return TyVar (infer->get_ref ());
}

TyVar
TyVar::subst_covariant_var (TyTy::BaseType *orig, TyTy::BaseType *subst)
{
  if (orig->get_kind () != TyTy::TypeKind::PARAM)
    return TyVar (subst->get_ty_ref ());
  else if (subst->get_kind () == TyTy::TypeKind::PARAM)
    {
      TyTy::ParamType *p = static_cast<TyTy::ParamType *> (subst);
      if (p->resolve ()->get_kind () == TyTy::TypeKind::PARAM)
	{
	  return TyVar (subst->get_ty_ref ());
	}
    }

  return TyVar (subst->get_ref ());
}

TyVar
TyVar::clone () const
{
  TyTy::BaseType *base = get_tyty ();
  if (base == nullptr || base->get_kind () == TypeKind::ERROR)
    return TyVar::get_implicit_infer_var (UNKNOWN_LOCATION);
  TyTy::BaseType *c = base->clone ();
  return TyVar (c->get_ref ());
}

TyVar
TyVar::monomorphized_clone () const
{
  auto &mappings = Analysis::Mappings::get ();
  auto context = Resolver::TypeCheckContext::get ();

  TyTy::BaseType *base = get_tyty ();
  if (base == nullptr || base->get_kind () == TypeKind::ERROR)
    return TyVar::get_implicit_infer_var (UNKNOWN_LOCATION);

  // this needs a new hirid
  TyTy::BaseType *c = get_tyty ()->monomorphized_clone ();
  c->set_ref (mappings.get_next_hir_id ());

  // insert it
  context->insert_type (Analysis::NodeMapping (mappings.get_current_crate (),
					       UNKNOWN_NODEID, c->get_ref (),
					       UNKNOWN_LOCAL_DEFID),
			c);

  return TyVar (c->get_ref ());
}

TyWithLocation::TyWithLocation (BaseType *ty, location_t locus)
  : ty (ty), locus (locus)
{}

TyWithLocation::TyWithLocation (BaseType *ty) : ty (ty)
{
  auto &mappings = Analysis::Mappings::get ();
  locus = mappings.lookup_location (ty->get_ref ());
}

} // namespace TyTy
} // namespace Rust
