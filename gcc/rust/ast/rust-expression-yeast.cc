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
#include "rust-desugar-try-block.h"
#include "rust-desugar-for-loops.h"
#include "rust-ast-full.h"
#include "rust-expr.h"
#include "rust-stmt.h"

namespace Rust {
namespace AST {

void
ExpressionYeast::go (AST::Crate &crate)
{
  DefaultASTVisitor::visit (crate);
}

void
ExpressionYeast::dispatch_loops (std::unique_ptr<Expr> &loop_expr)
{
  auto &loop = static_cast<BaseLoopExpr &> (*loop_expr.get ());

  switch (loop.get_loop_kind ())
    {
    case BaseLoopExpr::Kind::For:
      DesugarForLoops::go (loop_expr);
      break;
    case BaseLoopExpr::Kind::Loop:
    case BaseLoopExpr::Kind::While:
    case BaseLoopExpr::Kind::WhileLet:
      break;
    }
}

void
ExpressionYeast::dispatch (std::unique_ptr<Expr> &expr)
{
  switch (expr->get_expr_kind ())
    {
    case Expr::Kind::ErrorPropagation:
      DesugarQuestionMark::go (expr);
      break;
    case Expr::Kind::Try:
      DesugarTryBlock::go (expr);
      break;
    case Expr::Kind::Loop:
      dispatch_loops (expr);
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
    if (stmt->get_stmt_kind () == Stmt::Kind::Expr)
      dispatch (static_cast<ExprStmt &> (*stmt).get_expr_ptr ());

  if (block.has_tail_expr ())
    dispatch (block.get_tail_expr_ptr ());

  DefaultASTVisitor::visit (block);
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
