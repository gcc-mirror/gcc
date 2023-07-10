// Copyright (C) 2020-2024 Free Software Foundation, Inc.

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

#ifndef RUST_CASTS
#define RUST_CASTS

#include "rust-tyty.h"
#include "rust-coercion.h"

namespace Rust {
namespace Resolver {

class TypeCastRules
{
public:
  static TypeCoercionRules::CoercionResult resolve (location_t locus,
						    TyTy::TyWithLocation from,
						    TyTy::TyWithLocation to);

protected:
  TypeCoercionRules::CoercionResult check ();
  TypeCoercionRules::CoercionResult cast_rules ();
  TypeCoercionRules::CoercionResult check_ptr_ptr_cast ();

  void emit_cast_error () const;

protected:
  TypeCastRules (location_t locus, TyTy::TyWithLocation from,
		 TyTy::TyWithLocation to);

  location_t locus;
  TyTy::TyWithLocation from;
  TyTy::TyWithLocation to;
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_CASTS
