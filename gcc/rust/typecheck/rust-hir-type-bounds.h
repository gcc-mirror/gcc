// Copyright (C) 2021-2024 Free Software Foundation, Inc.

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

#ifndef RUST_HIR_TYPE_BOUNDS_H
#define RUST_HIR_TYPE_BOUNDS_H

#include "rust-hir-type-check-base.h"
#include "rust-hir-full.h"
#include "rust-tyty.h"

namespace Rust {
namespace Resolver {

class TypeBoundsProbe : public TypeCheckBase
{
public:
  static std::vector<std::pair<TraitReference *, HIR::ImplBlock *>>
  Probe (const TyTy::BaseType *receiver);

  static bool is_bound_satisfied_for_type (TyTy::BaseType *receiver,
					   TraitReference *ref);

private:
  void scan ();
  void assemble_sized_builtin ();
  void assemble_builtin_candidate (Analysis::RustLangItem::ItemType item);

private:
  TypeBoundsProbe (const TyTy::BaseType *receiver);

  const TyTy::BaseType *receiver;
  std::vector<std::pair<TraitReference *, HIR::ImplBlock *>> trait_references;
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_HIR_TYPE_BOUNDS_H
