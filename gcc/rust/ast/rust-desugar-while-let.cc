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

#include "rust-desugar-while-let.h"
#include "rust-ast.h"
#include "rust-hir-map.h"
#include "rust-path.h"
#include "rust-pattern.h"
#include "rust-stmt.h"
#include "rust-expr.h"
#include "rust-ast-builder.h"

namespace Rust {
namespace AST {

DesugarWhileLet::DesugarWhileLet () {}

MatchCase
DesugarWhileLet::DesugarCtx::make_break_arm ()
{
  auto arm = builder.match_arm (builder.wildcard ());

  auto break_expr
    = std::unique_ptr<Expr> (new BreakExpr (tl::nullopt, nullptr, {}, loc));

  return MatchCase (std::move (arm), std::move (break_expr));
}

MatchCase
DesugarWhileLet::DesugarCtx::make_continue_arm (
  std::unique_ptr<Pattern> &&pattern, std::unique_ptr<BlockExpr> &&body)
{
  auto arm = builder.match_arm (std::move (pattern));

  return MatchCase (std::move (arm), std::move (body));
}

std::unique_ptr<Expr>
DesugarWhileLet::desugar (WhileLetLoopExpr &expr)
{
  rust_assert (expr.get_patterns ().size () == 1);

  auto pattern = expr.get_patterns ()[0]->clone_pattern ();
  auto body = expr.get_loop_block ().clone_block_expr ();
  auto scrutinee = expr.get_scrutinee_expr ().clone_expr ();

  auto ctx = DesugarCtx (expr.get_locus ());

  // _ => break,
  auto break_arm = ctx.make_break_arm ();

  // <pattern> => <body>,
  auto continue_arm
    = ctx.make_continue_arm (std::move (pattern), std::move (body));

  // match <scrutinee> {
  //     <continue_arm>
  //     <break_arm>
  // }
  auto match_expr
    = ctx.builder.match (std::move (scrutinee),
			 {std::move (continue_arm), std::move (break_arm)});

  auto loop_stmts = std::vector<std::unique_ptr<Stmt>> ();
  loop_stmts.emplace_back (ctx.builder.statementify (std::move (match_expr)));

  // loop {
  //     <match_expr>
  // }
  return ctx.builder.loop (std::move (loop_stmts));
}

void
DesugarWhileLet::go (std::unique_ptr<Expr> &ptr)
{
  rust_assert (ptr->get_expr_kind () == Expr::Kind::Loop);

  auto &loop = static_cast<BaseLoopExpr &> (*ptr);

  rust_assert (loop.get_loop_kind () == BaseLoopExpr::Kind::WhileLet);

  auto &while_let = static_cast<WhileLetLoopExpr &> (loop);
  auto desugared = DesugarWhileLet ().desugar (while_let);

  ptr = std::move (desugared);
}

} // namespace AST
} // namespace Rust
