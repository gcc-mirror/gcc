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

#ifndef RUST_TYPE_UTIL
#define RUST_TYPE_UTIL

#include "rust-mapping-common.h"
#include "rust-tyty.h"

namespace Rust {
namespace Resolver {

bool
query_type (HirId reference, TyTy::BaseType **result);

bool
types_compatable (TyTy::TyWithLocation lhs, TyTy::TyWithLocation rhs,
		  location_t unify_locus, bool emit_errors);

TyTy::BaseType *
unify_site (HirId id, TyTy::TyWithLocation lhs, TyTy::TyWithLocation rhs,
	    location_t unify_locus);

TyTy::BaseType *
unify_site_and (HirId id, TyTy::TyWithLocation lhs, TyTy::TyWithLocation rhs,
		location_t unify_locus, bool emit_errors, bool commit_if_ok,
		bool implicit_infer_vars, bool cleanup);

TyTy::BaseType *
coercion_site (HirId id, TyTy::TyWithLocation lhs, TyTy::TyWithLocation rhs,
	       location_t coercion_locus);

TyTy::BaseType *
try_coercion (HirId id, TyTy::TyWithLocation lhs, TyTy::TyWithLocation rhs,
	      location_t coercion_locus);

TyTy::BaseType *
cast_site (HirId id, TyTy::TyWithLocation from, TyTy::TyWithLocation to,
	   location_t cast_locus);

AssociatedImplTrait *
lookup_associated_impl_block (const TyTy::TypeBoundPredicate &bound,
			      const TyTy::BaseType *binding,
			      bool *ambigious = nullptr);

} // namespace Resolver
} // namespace Rust

#endif // RUST_TYPE_UTIL
