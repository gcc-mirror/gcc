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

#include "rust-hir-path-probe.h"
#include "rust-hir-item.h"
#include "rust-hir-trait-resolve.h"
#include "rust-type-util.h"
#include "rust-hir-type-bounds.h"
#include "rust-hir-full.h"

namespace Rust {
namespace Resolver {

// PathProbeCandidate

PathProbeCandidate::Candidate::Candidate (EnumItemCandidate enum_field)
  : enum_field (enum_field)
{}

PathProbeCandidate::Candidate::Candidate (ImplItemCandidate impl) : impl (impl)
{}

PathProbeCandidate::Candidate::Candidate (TraitItemCandidate trait)
  : trait (trait)
{}

PathProbeCandidate::PathProbeCandidate (CandidateType type, TyTy::BaseType *ty,
					location_t locus,
					EnumItemCandidate enum_field)
  : type (type), ty (ty), locus (locus), item (enum_field)
{}

PathProbeCandidate::PathProbeCandidate (CandidateType type, TyTy::BaseType *ty,
					location_t locus,
					ImplItemCandidate impl)
  : type (type), ty (ty), locus (locus), item (impl)
{}

PathProbeCandidate::PathProbeCandidate (CandidateType type, TyTy::BaseType *ty,
					location_t locus,
					TraitItemCandidate trait)
  : type (type), ty (ty), locus (locus), item (trait)
{}

std::string
PathProbeCandidate::as_string () const
{
  return "PathProbe candidate TODO - as_string";
}

bool
PathProbeCandidate::is_enum_candidate () const
{
  return type == ENUM_VARIANT;
}

bool
PathProbeCandidate::is_impl_candidate () const
{
  return type == IMPL_CONST || type == IMPL_TYPE_ALIAS || type == IMPL_FUNC;
}

bool
PathProbeCandidate::is_trait_candidate () const
{
  return type == TRAIT_ITEM_CONST || type == TRAIT_TYPE_ALIAS
	 || type == TRAIT_FUNC;
}

bool
PathProbeCandidate::is_full_trait_item_candidate () const
{
  return is_trait_candidate () && item.trait.impl == nullptr;
}

PathProbeCandidate
PathProbeCandidate::get_error ()
{
  return PathProbeCandidate (ERROR, nullptr, UNDEF_LOCATION,
			     ImplItemCandidate{nullptr, nullptr});
}

bool
PathProbeCandidate::is_error () const
{
  return type == ERROR;
}

DefId
PathProbeCandidate::get_defid () const
{
  switch (type)
    {
    case ENUM_VARIANT:
      return item.enum_field.variant->get_defid ();
      break;

    case IMPL_CONST:
    case IMPL_TYPE_ALIAS:
    case IMPL_FUNC:
      return item.impl.impl_item->get_impl_mappings ().get_defid ();
      break;

    case TRAIT_ITEM_CONST:
    case TRAIT_TYPE_ALIAS:
    case TRAIT_FUNC:
      return item.trait.item_ref->get_mappings ().get_defid ();
      break;

    case ERROR:
    default:
      return UNKNOWN_DEFID;
    }

  return UNKNOWN_DEFID;
}

bool
PathProbeCandidate::operator< (const PathProbeCandidate &c) const
{
  return get_defid () < c.get_defid ();
}

} // namespace Resolver
} // namespace Rust
