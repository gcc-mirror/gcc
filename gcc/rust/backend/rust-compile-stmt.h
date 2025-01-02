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

#ifndef RUST_COMPILE_STMT
#define RUST_COMPILE_STMT

#include "rust-compile-base.h"
#include "rust-hir-visitor.h"

namespace Rust {
namespace Compile {

class CompileStmt : private HIRCompileBase, protected HIR::HIRStmtVisitor
{
public:
  static tree Compile (HIR::Stmt *stmt, Context *ctx);

  void visit (HIR::ExprStmt &stmt) override;
  void visit (HIR::LetStmt &stmt) override;

  // Empty visit for unused Stmt HIR nodes.
  void visit (HIR::TupleStruct &) override {}
  void visit (HIR::EnumItem &) override {}
  void visit (HIR::EnumItemTuple &) override {}
  void visit (HIR::EnumItemStruct &) override {}
  void visit (HIR::EnumItemDiscriminant &) override {}
  void visit (HIR::TypePathSegmentFunction &) override {}
  void visit (HIR::TypePath &) override {}
  void visit (HIR::QualifiedPathInType &) override {}
  void visit (HIR::Module &) override {}
  void visit (HIR::ExternCrate &) override {}
  void visit (HIR::UseDeclaration &) override {}
  void visit (HIR::Function &) override {}
  void visit (HIR::TypeAlias &) override {}
  void visit (HIR::StructStruct &) override {}
  void visit (HIR::Enum &) override {}
  void visit (HIR::Union &) override {}
  void visit (HIR::ConstantItem &) override {}
  void visit (HIR::StaticItem &) override {}
  void visit (HIR::Trait &) override {}
  void visit (HIR::ImplBlock &) override {}
  void visit (HIR::ExternBlock &) override {}
  void visit (HIR::EmptyStmt &) override {}

private:
  CompileStmt (Context *ctx);

  tree translated;
};

} // namespace Compile
} // namespace Rust

#endif // RUST_COMPILE_STMT
