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
  using Rust::Resolver::TypeCheckBase::visit;

public:
  static TyTy::BaseType *Resolve (HIR::StructExprStructFields *expr)
  {
    TypeCheckStructExpr resolver (expr);
    expr->accept_vis (resolver);
    return resolver.resolved;
  }

  void visit (HIR::StructExprStructFields &struct_expr) override;

  void visit (HIR::StructExprFieldIdentifierValue &field) override;

  void visit (HIR::StructExprFieldIndexValue &field) override;

  void visit (HIR::StructExprFieldIdentifier &field) override;

private:
  TypeCheckStructExpr (HIR::Expr *e)
    : TypeCheckBase (),
      resolved (new TyTy::ErrorType (e->get_mappings ().get_hirid ())),
      struct_path_resolved (nullptr)
  {}

  // result
  TyTy::BaseType *resolved;

  // internal state:
  TyTy::ADTType *struct_path_resolved;
  TyTy::BaseType *resolved_field_value_expr;
  std::set<std::string> fields_assigned;
  std::map<size_t, HIR::StructExprField *> adtFieldIndexToField;
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_HIR_TYPE_CHECK_STRUCT_FIELD
