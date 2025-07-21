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

#include "rust-expression-yeast.h"
#include "rust-ast-visitor.h"
#include "rust-desugar-question-mark.h"
#include "rust-ast-full.h"

namespace Rust {
namespace AST {

void
ExpressionYeast::go (AST::Crate &crate)
{
  DefaultASTVisitor::visit (crate);
}

void
ExpressionYeast::dispatch (std::unique_ptr<Expr> &expr)
{
  switch (expr->get_expr_kind ())
    {
      // TODO: Handle try-blocks
    case Expr::Kind::ErrorPropagation:
      DesugarQuestionMark::go (expr);
      break;

    default:
      break;
    }
}

void
ExpressionYeast::visit (ExprStmt &stmt)
{
  dispatch (stmt.get_expr_ptr ());

  DefaultASTVisitor::visit (stmt);
}

void
ExpressionYeast::visit (CallExpr &call)
{
  dispatch (call.get_function_expr_ptr ());

  for (auto &arg : call.get_params ())
    dispatch (arg);

  DefaultASTVisitor::visit (call);
}

void
ExpressionYeast::visit (BlockExpr &block)
{
  for (auto &stmt : block.get_statements ())
    DefaultASTVisitor::visit (stmt);

  if (block.has_tail_expr ())
    dispatch (block.get_tail_expr_ptr ());
}

void
ExpressionYeast::visit (LetStmt &stmt)
{
  if (stmt.has_init_expr ())
    dispatch (stmt.get_init_expr_ptr ());

  DefaultASTVisitor::visit (stmt);
}

} // namespace AST
} // namespace Rust
