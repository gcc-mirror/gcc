// Copyright (C) 2020-2024 Free Software Foundation, Inc.

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

#ifndef RUST_AST_LOWER_STMT
#define RUST_AST_LOWER_STMT

#include "rust-ast-lower-base.h"

namespace Rust {
namespace HIR {

class ASTLoweringStmt : public ASTLoweringBase
{
  using Rust::HIR::ASTLoweringBase::visit;

public:
  static HIR::Stmt *translate (AST::Stmt *stmt, bool *terminated);

  void visit (AST::ExprStmt &stmt) override;
  void visit (AST::ConstantItem &constant) override;
  void visit (AST::LetStmt &stmt) override;
  void visit (AST::TupleStruct &struct_decl) override;
  void visit (AST::StructStruct &struct_decl) override;
  void visit (AST::Union &union_decl) override;
  void visit (AST::Enum &enum_decl) override;
  void visit (AST::EmptyStmt &empty) override;
  void visit (AST::Function &function) override;
  void visit (AST::ExternBlock &extern_block) override;
  void visit (AST::MacroRulesDefinition &extern_block) override;
  void visit (AST::Trait &trait) override;
  void visit (AST::InherentImpl &impl_block) override;
  void visit (AST::TraitImpl &impl_block) override;

private:
  ASTLoweringStmt () : translated (nullptr), terminated (false) {}

  HIR::Stmt *translated;
  bool terminated;
};

} // namespace HIR
} // namespace Rust

#endif // RUST_AST_LOWER_PATTERN
