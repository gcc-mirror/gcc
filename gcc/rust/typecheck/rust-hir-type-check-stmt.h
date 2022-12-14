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

#ifndef RUST_HIR_TYPE_CHECK_STMT
#define RUST_HIR_TYPE_CHECK_STMT

#include "rust-hir-type-check-base.h"

namespace Rust {
namespace Resolver {

class TypeCheckStmt : private TypeCheckBase, private HIR::HIRStmtVisitor
{
public:
  static TyTy::BaseType *Resolve (HIR::Stmt *stmt);

  void visit (HIR::ExprStmtWithBlock &stmt) override;
  void visit (HIR::ExprStmtWithoutBlock &stmt) override;
  void visit (HIR::EmptyStmt &stmt) override;
  void visit (HIR::ExternBlock &extern_block) override;
  void visit (HIR::ConstantItem &constant) override;
  void visit (HIR::LetStmt &stmt) override;
  void visit (HIR::TupleStruct &struct_decl) override;
  void visit (HIR::Enum &enum_decl) override;
  void visit (HIR::StructStruct &struct_decl) override;
  void visit (HIR::Union &union_decl) override;
  void visit (HIR::Function &function) override;

  void visit (HIR::EnumItemTuple &) override
  { /* TODO? */
  }
  void visit (HIR::EnumItemStruct &) override
  { /* TODO? */
  }
  void visit (HIR::EnumItem &item) override
  { /* TODO? */
  }
  void visit (HIR::EnumItemDiscriminant &) override
  { /* TODO? */
  }
  void visit (HIR::TypePathSegmentFunction &segment) override
  { /* TODO? */
  }
  void visit (HIR::TypePath &path) override
  { /* TODO? */
  }
  void visit (HIR::QualifiedPathInType &path) override
  { /* TODO? */
  }
  void visit (HIR::Module &module) override
  { /* TODO? */
  }
  void visit (HIR::ExternCrate &crate) override
  { /* TODO? */
  }
  void visit (HIR::UseDeclaration &use_decl) override
  { /* TODO? */
  }
  void visit (HIR::TypeAlias &type_alias) override
  { /* TODO? */
  }
  void visit (HIR::StaticItem &static_item) override
  { /* TODO? */
  }
  void visit (HIR::Trait &trait) override
  { /* TODO? */
  }
  void visit (HIR::ImplBlock &impl) override
  { /* TODO? */
  }

private:
  TypeCheckStmt () : TypeCheckBase (), infered (nullptr) {}

  TyTy::BaseType *infered;
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_HIR_TYPE_CHECK_STMT
