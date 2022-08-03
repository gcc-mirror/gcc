// Copyright (C) 2020-2022 Free Software Foundation, Inc.

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

#include "rust-coercion.h"

namespace Rust {
namespace Resolver {

AutoderefTypeCoercion::CoercionResult
AutoderefTypeCoercion::Coerce (const TyTy::BaseType *receiver,
			       const TyTy::BaseType *expected, Location locus)
{
  AutoderefTypeCoercion resolver (expected, locus);
  bool ok = resolver.cycle (receiver);
  return ok ? resolver.try_result : CoercionResult::get_error ();
}

AutoderefTypeCoercion::AutoderefTypeCoercion (const TyTy::BaseType *expected,
					      Location locus)
  : AutoderefCycle (false), mappings (Analysis::Mappings::get ()),
    context (TypeCheckContext::get ()), expected (expected), locus (locus),
    try_result (CoercionResult::get_error ())
{}

bool
AutoderefTypeCoercion::cycle (const TyTy::BaseType *receiver)
{
  // FIXME this is not finished and might be super simplified
  // see:
  // https://github.com/rust-lang/rust/blob/7eac88abb2e57e752f3302f02be5f3ce3d7adfb4/compiler/rustc_typeck/src/check/coercion.rs

  if (receiver->get_kind () == TyTy::TypeKind::REF
      && expected->get_kind () == TyTy::TypeKind::REF)
    {
      // if we expect to get a mutable pointer we can't get that from an
      // immutable one so we have to be careful

      const auto &receiver_ref
	= static_cast<const TyTy::ReferenceType &> (*receiver);
      const auto &expected_ref
	= static_cast<const TyTy::ReferenceType &> (*expected);

      // we can allow for mutability changes here by casting down from
      // mutability eg:  mut vs const, we cant take a mutable reference from a
      // const eg:  const vs mut we can take a const reference from a mutable
      // one

      bool mutability_ok
	= !expected_ref.is_mutable ()
	  || (expected_ref.is_mutable () == receiver_ref.is_mutable ());
      if (!mutability_ok)
	{
	  RichLocation r (locus);
	  r.add_range (mappings->lookup_location (receiver_ref.get_ref ()));
	  r.add_range (mappings->lookup_location (expected_ref.get_ref ()));
	  rust_error_at (r, "mismatched mutability");

	  return false;
	}
    }

  return AutoderefCycle::cycle (receiver);
}

bool
AutoderefTypeCoercion::select (const TyTy::BaseType &autoderefed)
{
  if (autoderefed.can_eq (expected, false))
    {
      try_result = CoercionResult{adjustments, autoderefed.clone ()};
      return true;
    }
  return false;
}

} // namespace Resolver
} // namespace Rust
