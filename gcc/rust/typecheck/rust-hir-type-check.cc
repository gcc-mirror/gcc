// Copyright (C) 2020 Free Software Foundation, Inc.

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

#include "rust-hir-type-check.h"
#include "rust-hir-full.h"
#include "rust-hir-type-check-toplevel.h"
#include "rust-hir-type-check-item.h"
#include "rust-hir-type-check-expr.h"

namespace Rust {
namespace Resolver {

void
TypeResolution::Resolve (HIR::Crate &crate)
{
  for (auto it = crate.items.begin (); it != crate.items.end (); it++)
    TypeCheckTopLevel::Resolve (it->get ());

  for (auto it = crate.items.begin (); it != crate.items.end (); it++)
    TypeCheckItem::Resolve (it->get ());
}

// RUST_HIR_TYPE_CHECK_EXPR
void
TypeCheckExpr::visit (HIR::BlockExpr &expr)
{
  expr.iterate_stmts ([&] (HIR::Stmt *s) mutable -> bool {
    TypeCheckStmt::Resolve (s);
    return true;
  });
}

} // namespace Resolver
} // namespace Rust
