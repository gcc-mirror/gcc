// Copyright (C) 2020-2021 Free Software Foundation, Inc.

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

#ifndef RUST_HIR_TYPE_CHECK_PATTERN
#define RUST_HIR_TYPE_CHECK_PATTERN

#include "rust-hir-type-check-base.h"
#include "rust-hir-full.h"

namespace Rust {
namespace Resolver {

class TypeCheckPattern : public TypeCheckBase
{
  using Rust::Resolver::TypeCheckBase::visit;

public:
  static TyTy::BaseType *Resolve (HIR::Pattern *pattern)
  {
    TypeCheckPattern resolver;
    pattern->accept_vis (resolver);

    // FIXME need to check how we do mappings here
    if (resolver.infered == nullptr)
      return new TyTy::ErrorType (1);

    return resolver.infered;
  }

  void visit (HIR::PathInExpression &pattern) override;

  void visit (HIR::StructPattern &pattern) override;

  void visit (HIR::TupleStructPattern &pattern) override;

private:
  TypeCheckPattern () : TypeCheckBase (), infered (nullptr) {}

  TyTy::BaseType *infered;
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_HIR_TYPE_CHECK_PATTERN
