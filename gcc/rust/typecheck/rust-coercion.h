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

#ifndef RUST_COERCION
#define RUST_COERCION

#include "rust-autoderef.h"
#include "rust-hir-type-check.h"

namespace Rust {
namespace Resolver {

class AutoderefTypeCoercion : protected AutoderefCycle
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

  static CoercionResult Coerce (const TyTy::BaseType *receiver,
				const TyTy::BaseType *expected, Location locus);

protected:
  AutoderefTypeCoercion (const TyTy::BaseType *expected, Location locus);

  bool cycle (const TyTy::BaseType *receiver) override;

  bool select (const TyTy::BaseType &autoderefed) override;

private:
  // context info
  Analysis::Mappings *mappings;
  TypeCheckContext *context;

  // search
  const TyTy::BaseType *expected;
  Location locus;

  // mutable fields
  CoercionResult try_result;
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_COERCION
