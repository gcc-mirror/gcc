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

#ifndef RUST_TYTY_UTIL_H
#define RUST_TYTY_UTIL_H

#include "rust-hir-map.h"

namespace Rust {
namespace TyTy {

class BaseType;

// this is a placeholder for types that can change like inference variables
class TyVar
{
public:
  explicit TyVar (HirId ref);

  HirId get_ref () const { return ref; }

  BaseType *get_tyty () const;

  TyVar clone () const;

  TyVar monomorphized_clone () const;

  static TyVar get_implicit_infer_var (Location locus);

  static TyVar subst_covariant_var (TyTy::BaseType *orig,
				    TyTy::BaseType *subst);

private:
  HirId ref;
};

class TyWithLocation
{
public:
  explicit TyWithLocation (BaseType *ty, Location locus);
  explicit TyWithLocation (BaseType *ty);

  BaseType *get_ty () const { return ty; }
  Location get_locus () const { return locus; }

private:
  BaseType *ty;
  Location locus;
};

} // namespace TyTy
} // namespace Rust

#endif // RUST_TYTY_UTIL_H
