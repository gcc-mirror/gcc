// Copyright (C) 2020-2025 Free Software Foundation, Inc.

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

struct Error;

namespace Resolver {

class TypeCheckStructExpr : public TypeCheckBase
{
public:
  static TyTy::BaseType *Resolve (HIR::StructExprStructFields &expr);

  // Helper for making any errors
  static Error
  make_missing_field_error (location_t locus,
			    const std::vector<std::string> &missing_field_names,
			    const std::string &struct_name);

protected:
  void resolve (HIR::StructExprStructFields &struct_expr);

  bool visit (HIR::StructExprFieldIdentifierValue &field);
  bool visit (HIR::StructExprFieldIndexValue &field);
  bool visit (HIR::StructExprFieldIdentifier &field);

private:
  TypeCheckStructExpr (HIR::Expr &e);

  // result
  TyTy::BaseType *resolved;

  // internal state:
  TyTy::ADTType *struct_path_resolved;
  TyTy::VariantDef *variant;
  TyTy::BaseType *resolved_field_value_expr;
  std::set<std::string> fields_assigned;
  std::map<size_t, HIR::StructExprField *> adtFieldIndexToField;
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_HIR_TYPE_CHECK_STRUCT_FIELD
