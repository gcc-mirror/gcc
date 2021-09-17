// Copyright (C) 2021 Free Software Foundation, Inc.

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

#include "rust-hir-type-bounds.h"
#include "rust-hir-trait-resolve.h"

namespace Rust {
namespace Resolver {

void
TypeBoundsProbe::scan ()
{
  std::vector<std::pair<HIR::TypePath *, HIR::ImplBlock *>>
    possible_trait_paths;
  mappings->iterate_impl_blocks (
    [&] (HirId id, HIR::ImplBlock *impl) mutable -> bool {
      // we are filtering for trait-impl-blocks
      if (!impl->has_trait_ref ())
	return true;

      TyTy::BaseType *impl_type = nullptr;
      bool ok
	= context->lookup_type (impl->get_type ()->get_mappings ().get_hirid (),
				&impl_type);
      if (!ok)
	return true;

      if (!receiver->can_eq (impl_type, false))
	return true;

      possible_trait_paths.push_back ({impl->get_trait_ref ().get (), impl});
      return true;
    });

  for (auto &path : possible_trait_paths)
    {
      HIR::TypePath *trait_path = path.first;
      TraitReference *trait_ref = TraitResolver::Resolve (*trait_path);

      if (!trait_ref->is_error ())
	trait_references.push_back ({trait_ref, path.second});
    }
}

TraitReference *
TypeCheckBase::resolve_trait_path (HIR::TypePath &path)
{
  return TraitResolver::Resolve (path);
}

} // namespace Resolver

namespace TyTy {

std::string
TypeBoundPredicate::as_string () const
{
  return get ()->as_string ();
}

const Resolver::TraitReference *
TypeBoundPredicate::get () const
{
  auto context = Resolver::TypeCheckContext::get ();

  Resolver::TraitReference *ref = nullptr;
  bool ok = context->lookup_trait_reference (reference, &ref);
  rust_assert (ok);

  return ref;
}

std::string
TypeBoundPredicate::get_name () const
{
  auto mappings = Analysis::Mappings::get ();
  auto trait = get ();
  auto nodeid = trait->get_mappings ().get_nodeid ();

  const Resolver::CanonicalPath *p = nullptr;
  if (mappings->lookup_canonical_path (mappings->get_current_crate (), nodeid,
				       &p))
    return p->get ();

  return trait->get_name ();
}

bool
TypeBoundPredicate::is_object_safe (bool emit_error, Location locus) const
{
  const Resolver::TraitReference *trait = get ();
  rust_assert (trait != nullptr);
  return trait->is_object_safe (emit_error, locus);
}

} // namespace TyTy
} // namespace Rust
