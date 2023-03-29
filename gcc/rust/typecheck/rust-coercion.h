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

#ifndef RUST_COERCION
#define RUST_COERCION

#include "rust-autoderef.h"
#include "rust-hir-type-check.h"

namespace Rust {
namespace Resolver {

class TypeCoercionRules : protected AutoderefCycle
{
public:
  struct CoercionResult
  {
    std::vector<Adjustment> adjustments;
    TyTy::BaseType *tyty;

    bool is_error ()
    {
      return tyty == nullptr || tyty->get_kind () == TyTy::TypeKind::ERROR;
    }

    static CoercionResult get_error () { return CoercionResult{{}, nullptr}; }
  };

  static CoercionResult Coerce (TyTy::BaseType *receiver,
				TyTy::BaseType *expected, Location locus);

  static CoercionResult TryCoerce (TyTy::BaseType *receiver,
				   TyTy::BaseType *expected, Location locus);

  CoercionResult coerce_unsafe_ptr (TyTy::BaseType *receiver,
				    TyTy::PointerType *expected,
				    Mutability mutability);

  CoercionResult coerce_borrowed_pointer (TyTy::BaseType *receiver,
					  TyTy::ReferenceType *expected,
					  Mutability mutability);

  CoercionResult coerce_unsized (TyTy::BaseType *receiver,
				 TyTy::BaseType *expected, bool &unsafe_error);

  static bool coerceable_mutability (Mutability from_mutbl,
				     Mutability to_mutbl);

  void mismatched_mutability_error (Location expr_locus, Location lhs,
				    Location rhs);
  void object_unsafe_error (Location expr_locus, Location lhs, Location rhs);

protected:
  TypeCoercionRules (TyTy::BaseType *expected, Location locus,
		     bool emit_errors);

  bool select (const TyTy::BaseType &autoderefed) override;

  bool do_coercion (TyTy::BaseType *receiver);

private:
  // context info
  Analysis::Mappings *mappings;
  TypeCheckContext *context;

  // search
  TyTy::BaseType *expected;
  Location locus;

  // mutable fields
  CoercionResult try_result;
  bool emit_errors;
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_COERCION
