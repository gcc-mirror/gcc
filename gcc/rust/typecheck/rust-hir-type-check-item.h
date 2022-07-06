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

#ifndef RUST_HIR_TYPE_CHECK_ITEM
#define RUST_HIR_TYPE_CHECK_ITEM

#include "rust-hir-type-check-base.h"
#include "rust-hir-full.h"
#include "rust-hir-type-check-implitem.h"
#include "rust-hir-type-check-type.h"
#include "rust-hir-type-check-stmt.h"
#include "rust-hir-trait-resolve.h"
#include "rust-tyty-visitor.h"

namespace Rust {
namespace Resolver {

class TypeCheckItem : public TypeCheckBase
{
  using Rust::Resolver::TypeCheckBase::visit;

public:
  static void Resolve (HIR::Item *item);

  void visit (HIR::ImplBlock &impl_block) override;
  void visit (HIR::Function &function) override;
  void visit (HIR::Module &module) override;
  void visit (HIR::Trait &trait) override;

private:
  TypeCheckItem () : TypeCheckBase () {}
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_HIR_TYPE_CHECK_ITEM
