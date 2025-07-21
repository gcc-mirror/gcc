// Copyright (C) 2025 Free Software Foundation, Inc.

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

#ifndef RUST_EXPRESSION_YEAST
#define RUST_EXPRESSION_YEAST

#include "rust-ast-visitor.h"
#include "rust-ast.h"
#include "rust-desugar-question-mark.h"

namespace Rust {
namespace AST {

// This visitor takes care of all the expression desugars: try-blocks,
// error-propagation, etc.
class ExpressionYeast : public AST::DefaultASTVisitor
{
  using AST::DefaultASTVisitor::visit;

public:
  void go (AST::Crate &);

private:
  // Dispatch to the proper desugar
  void dispatch (std::unique_ptr<Expr> &expr);

  void visit (AST::ExprStmt &) override;
  void visit (AST::CallExpr &) override;
  void visit (AST::LetStmt &) override;
  void visit (AST::BlockExpr &) override;
};

} // namespace AST
} // namespace Rust

#endif // ! RUST_EXPRESSION_YEAST
