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

#ifndef RUST_HIR_TYPE_CHECK_STMT
#define RUST_HIR_TYPE_CHECK_STMT

#include "rust-hir-type-check-base.h"
#include "rust-hir-visitor.h"

namespace Rust {
namespace Resolver {

class TypeCheckStmt : private TypeCheckBase, private HIR::HIRStmtVisitor
{
public:
  static TyTy::BaseType *Resolve (HIR::Stmt &stmt);

  void visit (HIR::ExprStmt &stmt) override;
  void visit (HIR::EmptyStmt &stmt) override;
  void visit (HIR::ExternBlock &extern_block) override;
  void visit (HIR::ConstantItem &constant) override;
  void visit (HIR::LetStmt &stmt) override;
  void visit (HIR::TupleStruct &struct_decl) override;
  void visit (HIR::Enum &enum_decl) override;
  void visit (HIR::StructStruct &struct_decl) override;
  void visit (HIR::Union &union_decl) override;
  void visit (HIR::Function &function) override;
  void visit (HIR::Module &module) override;
  void visit (HIR::TypeAlias &type_alias) override;
  void visit (HIR::StaticItem &static_item) override;
  void visit (HIR::Trait &trait) override;
  void visit (HIR::ImplBlock &impl) override;
  void visit (HIR::TypePath &path) override;
  void visit (HIR::QualifiedPathInType &path) override;

  // FIXME
  // this seems like it should not be part of this visitor
  void visit (HIR::TypePathSegmentFunction &segment) override
  {
    rust_unreachable ();
  }

  // nothing to do for these
  void visit (HIR::ExternCrate &crate) override {}
  void visit (HIR::UseDeclaration &use_decl) override {}

  // nothing to do for these as they are taken care of by the
  // hir-type-check-enumitem.h
  void visit (HIR::EnumItemTuple &) override {}
  void visit (HIR::EnumItemStruct &) override {}
  void visit (HIR::EnumItem &) override {}
  void visit (HIR::EnumItemDiscriminant &) override {}

private:
  TypeCheckStmt () : TypeCheckBase (), infered (nullptr) {}

  TyTy::BaseType *infered;
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_HIR_TYPE_CHECK_STMT
