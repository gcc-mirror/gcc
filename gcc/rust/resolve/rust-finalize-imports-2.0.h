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

#include "rust-ast.h"
#include "rust-expr.h"
#include "rust-name-resolution-context.h"
#include "rust-toplevel-name-resolver-2.0.h"
#include "rust-early-name-resolver-2.0.h"

namespace Rust {
namespace Resolver2_0 {

class GlobbingVisitor : public AST::DefaultASTVisitor
{
  using AST::DefaultASTVisitor::visit;

public:
  GlobbingVisitor (NameResolutionContext &ctx) : ctx (ctx) {}

  void go (AST::Module *module);
  void visit (AST::Module &module) override;
  void visit (AST::MacroRulesDefinition &macro) override;
  void visit (AST::Function &function) override;
  void visit (AST::StaticItem &static_item) override;
  void visit (AST::StructStruct &struct_item) override;
  void visit (AST::TupleStruct &tuple_struct) override;
  void visit (AST::Enum &enum_item) override;
  void visit (AST::Union &union_item) override;
  void visit (AST::ConstantItem &const_item) override;
  void visit (AST::ExternCrate &crate) override;
  void visit (AST::UseDeclaration &use) override;

private:
  NameResolutionContext &ctx;
};

} // namespace Resolver2_0
} // namespace Rust
