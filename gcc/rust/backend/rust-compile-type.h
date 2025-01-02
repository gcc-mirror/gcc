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

#ifndef RUST_COMPILE_TYPE
#define RUST_COMPILE_TYPE

#include "rust-compile-context.h"

namespace Rust {
namespace Compile {

class TyTyResolveCompile : protected TyTy::TyConstVisitor
{
public:
  static tree compile (Context *ctx, const TyTy::BaseType *ty,
		       bool trait_object_mode = false);

  static tree get_implicit_enumeral_node_type (Context *ctx);

  static tree get_unit_type (Context *ctx);

  void visit (const TyTy::InferType &) override;
  void visit (const TyTy::ADTType &) override;
  void visit (const TyTy::TupleType &) override;
  void visit (const TyTy::FnType &) override;
  void visit (const TyTy::FnPtr &) override;
  void visit (const TyTy::ArrayType &) override;
  void visit (const TyTy::SliceType &) override;
  void visit (const TyTy::BoolType &) override;
  void visit (const TyTy::IntType &) override;
  void visit (const TyTy::UintType &) override;
  void visit (const TyTy::FloatType &) override;
  void visit (const TyTy::USizeType &) override;
  void visit (const TyTy::ISizeType &) override;
  void visit (const TyTy::ErrorType &) override;
  void visit (const TyTy::CharType &) override;
  void visit (const TyTy::ReferenceType &) override;
  void visit (const TyTy::PointerType &) override;
  void visit (const TyTy::ParamType &) override;
  void visit (const TyTy::StrType &) override;
  void visit (const TyTy::NeverType &) override;
  void visit (const TyTy::PlaceholderType &) override;
  void visit (const TyTy::ProjectionType &) override;
  void visit (const TyTy::DynamicObjectType &) override;
  void visit (const TyTy::ClosureType &) override;

public:
  static hashval_t type_hasher (tree type);

protected:
  tree create_slice_type_record (const TyTy::SliceType &type);
  tree create_str_type_record (const TyTy::StrType &type);
  tree create_dyn_obj_record (const TyTy::DynamicObjectType &type);

private:
  TyTyResolveCompile (Context *ctx, bool trait_object_mode);

  Context *ctx;
  bool trait_object_mode;
  tree translated;
};

} // namespace Compile
} // namespace Rust

#endif // RUST_COMPILE_TYPE
