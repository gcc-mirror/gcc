
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

#ifndef RUST_AST_DEFAULT_RESOLVER_H
#define RUST_AST_DEFAULT_RESOLVER_H

#include "rust-ast-visitor.h"
#include "rust-name-resolution-context.h"

namespace Rust {
namespace Resolver2_0 {

/**
 * The `DefaultResolver` is a base visitor for all passes of our name resolution
 * algorithm: `TopLevel`, `Easy` and `Late`. It does not do a lot, apart from
 * visiting each node's subnodes - a block's statements, a function call's
 * arguments...
 */
class DefaultResolver : public AST::DefaultASTVisitor
{
public:
  using AST::DefaultASTVisitor::visit;

  virtual ~DefaultResolver () {}

  // First, our lexical scope expressions - these visit their sub nodes, always
  // these nodes create new scopes and ribs - they are often used to declare new
  // variables, such as a for loop's iterator, or a function's arguments
  void visit (AST::BlockExpr &) override;
  void visit (AST::Module &) override;
  void visit (AST::Function &) override;
  void visit (AST::ForLoopExpr &expr) override;
  void visit (AST::Trait &) override;
  void visit (AST::InherentImpl &) override;
  void visit (AST::TraitImpl &) override;

  // type dec nodes, which visit their fields or variants by default
  void visit (AST::StructStruct &) override;
  void visit (AST::TupleStruct &) override;
  void visit (AST::Enum &) override;
  void visit (AST::Union &) override;
  void visit (AST::TypeAlias &) override;

  // Visitors that visit their expression node(s)
  void visit (AST::ClosureExprInner &) override;
  void visit (AST::ClosureExprInnerTyped &) override;
  void visit (AST::MatchExpr &) override;

  // Leaf visitors, which do nothing by default
  void visit (AST::ConstantItem &) override;
  void visit (AST::StaticItem &) override;

protected:
  DefaultResolver (NameResolutionContext &ctx) : ctx (ctx) {}

  NameResolutionContext &ctx;
};

} // namespace Resolver2_0
} // namespace Rust

#endif // RUST_AST_DEFAULT_RESOLVER_H
