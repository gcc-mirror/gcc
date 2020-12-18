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

#ifndef RUST_TYTY_CALL
#define RUST_TYTY_CALL

#include "rust-diagnostics.h"
#include "rust-hir-full.h"
#include "rust-tyty-visitor.h"
#include "rust-tyty.h"

namespace Rust {
namespace TyTy {

class TypeCheckCallExpr : private TyVisitor
{
public:
  static TyBase *go (TyBase *ref, HIR::CallExpr &call)
  {
    TypeCheckCallExpr checker (call);
    ref->accept_vis (checker);
    return checker.resolved;
  }
  ~TypeCheckCallExpr () {}

  void visit (FnType &type) override;

private:
  TypeCheckCallExpr (HIR::CallExpr &c) : resolved (nullptr), call (c) {}

  TyBase *resolved;
  HIR::CallExpr &call;
};

} // namespace TyTy
} // namespace Rust

#endif // RUST_TYTY_CALL
