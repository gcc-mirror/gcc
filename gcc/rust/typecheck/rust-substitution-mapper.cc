// Copyright (C) 2020-2023 Free Software Foundation, Inc.

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

#include "rust-substitution-mapper.h"
#include "rust-hir-type-check.h"

namespace Rust {
namespace Resolver {

TyTy::BaseType *
SubstMapperInternal::Resolve (TyTy::BaseType *base,
			      TyTy::SubstitutionArgumentMappings &mappings)
{
  auto context = TypeCheckContext::get ();

  SubstMapperInternal mapper (base->get_ref (), mappings);
  base->accept_vis (mapper);
  rust_assert (mapper.resolved != nullptr);

  // insert these new implict types into the context
  TyTy::BaseType *unused = nullptr;
  bool is_ty_available
    = context->lookup_type (mapper.resolved->get_ty_ref (), &unused);
  if (!is_ty_available)
    {
      context->insert_type (
	Analysis::NodeMapping (0, 0, mapper.resolved->get_ty_ref (), 0),
	mapper.resolved);
    }
  bool is_ref_available
    = context->lookup_type (mapper.resolved->get_ref (), &unused);
  if (!is_ref_available)
    {
      context->insert_type (Analysis::NodeMapping (0, 0,
						   mapper.resolved->get_ref (),
						   0),
			    mapper.resolved);
    }

  return mapper.resolved;
}

bool
SubstMapperInternal::mappings_are_bound (
  TyTy::BaseType *tyseg, TyTy::SubstitutionArgumentMappings &mappings)
{
  if (tyseg->get_kind () == TyTy::TypeKind::ADT)
    {
      TyTy::ADTType *adt = static_cast<TyTy::ADTType *> (tyseg);
      return adt->are_mappings_bound (mappings);
    }
  else if (tyseg->get_kind () == TyTy::TypeKind::FNDEF)
    {
      TyTy::FnType *fn = static_cast<TyTy::FnType *> (tyseg);
      return fn->are_mappings_bound (mappings);
    }

  return false;
}

} // namespace Resolver
} // namespace Rust
