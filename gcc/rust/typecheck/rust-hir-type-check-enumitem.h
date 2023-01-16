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

#ifndef RUST_HIR_TYPE_CHECK_ENUMITEM
#define RUST_HIR_TYPE_CHECK_ENUMITEM

#include "rust-hir-type-check-base.h"
#include "rust-hir-full.h"

namespace Rust {
namespace Resolver {

class TypeCheckEnumItem : public TypeCheckBase
{
public:
  static TyTy::VariantDef *Resolve (HIR::EnumItem *item,
				    int64_t last_discriminant);

protected:
  void visit (HIR::EnumItem &item);
  void visit (HIR::EnumItemDiscriminant &item);
  void visit (HIR::EnumItemTuple &item);
  void visit (HIR::EnumItemStruct &item);

private:
  TypeCheckEnumItem (int64_t last_discriminant);

  TyTy::VariantDef *variant;
  int64_t last_discriminant;
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_HIR_TYPE_CHECK_ENUMITEM
