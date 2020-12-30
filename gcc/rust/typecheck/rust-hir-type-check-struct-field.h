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

#ifndef RUST_HIR_TYPE_CHECK_STRUCT_FIELD
#define RUST_HIR_TYPE_CHECK_STRUCT_FIELD

#include "rust-hir-type-check-base.h"
#include "rust-hir-full.h"
#include "rust-hir-type-check-type.h"
#include "rust-tyty.h"

namespace Rust {
namespace Resolver {

class TypeCheckStructExpr : public TypeCheckBase
{
public:
  static TyTy::TyBase *Resolve (HIR::StructExprStructFields *expr)
  {
    TypeCheckStructExpr resolver;
    expr->accept_vis (resolver);
    rust_assert (resolver.resolved != nullptr);
    return resolver.resolved;
  }

  void visit (HIR::StructExprStructFields &struct_expr);

  void visit (HIR::PathInExpression &path);

  void visit (HIR::StructExprFieldIdentifierValue &field);

private:
  TypeCheckStructExpr ()
    : TypeCheckBase (), resolved (nullptr), struct_path_resolved (nullptr)
  {}

  TyTy::TyBase *resolved;
  TyTy::ADTType *struct_path_resolved;
  TyTy::TyBase *resolved_field;
  std::set<std::string> fields_assigned;
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_HIR_TYPE_CHECK_STRUCT_FIELD
