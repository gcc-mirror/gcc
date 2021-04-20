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

#include "rust-hir-const-fold-ctx.h"
#include "rust-hir-const-fold.h"

namespace Rust {
namespace ConstFold {

static Context *context = nullptr;

Context::Context (::Backend *backend) : backend (backend) {}

void
Context::init (::Backend *backend)
{
  rust_assert (context == nullptr);
  context = new Context (backend);
}

Context *
Context::get ()
{
  rust_assert (context != nullptr);
  return context;
}

bool
Context::lookup_const (HirId id, Bexpression **expr)
{
  auto it = ctx.find (id);
  if (it == ctx.end ())
    return false;

  *expr = it->second;
  return true;
}

void
Context::insert_const (HirId id, Bexpression *expr)
{
  rust_assert (ctx.find (id) == ctx.end ());
  ctx[id] = expr;
}

// rust-hir-const-fold.h

void
ConstFoldItem::visit (HIR::ConstantItem &item)
{
  auto folded_expr = ConstFoldExpr::fold (item.get_expr ());
  if (folded_expr == nullptr)
    return;

  folded = folded_expr;
}

} // namespace ConstFold
} // namespace Rust
