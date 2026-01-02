// Copyright (C) 2025-2026 Free Software Foundation, Inc.

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
#include "rust-desugar-while-let.h"
#include "rust-expr.h"

namespace Rust {
namespace AST {

void
ExpressionYeast::go (AST::Crate &crate)
{
  PointerVisitor::visit (crate);
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
    case BaseLoopExpr::Kind::WhileLet:
      DesugarWhileLet::go (loop_expr);
      break;
    default:
      break;
    }
}

void
ExpressionYeast::reseat (std::unique_ptr<Expr> &expr)
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

  visit (expr);
}

} // namespace AST
} // namespace Rust
