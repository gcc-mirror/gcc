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

#ifndef RUST_HIR_TYPE_BOUNDS_H
#define RUST_HIR_TYPE_BOUNDS_H

#include "rust-hir-type-check-base.h"
#include "rust-hir-full.h"
#include "rust-tyty.h"

namespace Rust {
namespace Resolver {

class TypeBoundsProbe : public TypeCheckBase
{
  using Rust::Resolver::TypeCheckBase::visit;

public:
  static std::vector<TraitReference *> Probe (const TyTy::BaseType *receiver)
  {
    TypeBoundsProbe probe (receiver);
    probe.scan ();
    return probe.trait_references;
  }

private:
  void scan ();

private:
  TypeBoundsProbe (const TyTy::BaseType *receiver)
    : TypeCheckBase (), receiver (receiver)
  {}

  const TyTy::BaseType *receiver;
  std::vector<TraitReference *> trait_references;
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_HIR_TYPE_BOUNDS_H
