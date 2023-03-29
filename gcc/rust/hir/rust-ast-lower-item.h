// Copyright (C) 2020-2023 Free Software Foundation, Inc.

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

#ifndef RUST_AST_LOWER_ITEM
#define RUST_AST_LOWER_ITEM

#include "rust-diagnostics.h"

#include "rust-ast-lower.h"
#include "rust-ast-lower-base.h"
#include "rust-ast-lower-enumitem.h"
#include "rust-ast-lower-type.h"
#include "rust-ast-lower-implitem.h"
#include "rust-ast-lower-stmt.h"
#include "rust-ast-lower-expr.h"
#include "rust-ast-lower-pattern.h"
#include "rust-ast-lower-block.h"
#include "rust-ast-lower-extern.h"
#include "rust-hir-full-decls.h"

namespace Rust {
namespace HIR {

class ASTLoweringItem : public ASTLoweringBase
{
  using Rust::HIR::ASTLoweringBase::visit;

public:
  static HIR::Item *translate (AST::Item *item);

  void visit (AST::Module &module) override;
  void visit (AST::TypeAlias &alias) override;
  void visit (AST::TupleStruct &struct_decl) override;
  void visit (AST::StructStruct &struct_decl) override;
  void visit (AST::Enum &enum_decl) override;
  void visit (AST::Union &union_decl) override;
  void visit (AST::StaticItem &var) override;
  void visit (AST::ConstantItem &constant) override;
  void visit (AST::Function &function) override;
  void visit (AST::InherentImpl &impl_block) override;
  void visit (AST::Trait &trait) override;
  void visit (AST::TraitImpl &impl_block) override;
  void visit (AST::ExternBlock &extern_block) override;

private:
  ASTLoweringItem () : translated (nullptr) {}

  HIR::Item *translated;
};

class ASTLoweringSimplePath : public ASTLoweringBase
{
public:
  static HIR::SimplePath translate (const AST::SimplePath &path);

  HIR::SimplePathSegment lower (const AST::SimplePathSegment &segment);
  HIR::SimplePath lower (const AST::SimplePath &path);
};

} // namespace HIR
} // namespace Rust

#endif // RUST_AST_LOWER_ITEM
