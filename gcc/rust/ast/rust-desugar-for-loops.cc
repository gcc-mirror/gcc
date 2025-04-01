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

#include "rust-desugar-for-loops.h"
#include "rust-ast-visitor.h"
#include "rust-ast.h"
#include "rust-hir-map.h"
#include "rust-path.h"
#include "rust-pattern.h"
#include "rust-stmt.h"
#include "rust-expr.h"
#include "rust-ast-builder.h"

namespace Rust {
namespace AST {

DesugarForLoops::DesugarForLoops () {}

void
DesugarForLoops::go (AST::Crate &crate)
{
  DefaultASTVisitor::visit (crate);
}

static void
replace_for_loop (std::unique_ptr<Expr> &for_loop,
		  std::unique_ptr<Expr> &&expanded)
{
  for_loop = std::move (expanded);
}

MatchArm
DesugarForLoops::DesugarCtx::make_match_arm (std::unique_ptr<Pattern> &&path)
{
  auto patterns = std::vector<std::unique_ptr<Pattern>> ();
  patterns.emplace_back (std::move (path));

  return MatchArm (std::move (patterns), loc);
}

MatchCase
DesugarForLoops::DesugarCtx::make_break_arm ()
{
  auto arm = make_match_arm (std::unique_ptr<Pattern> (new PathInExpression (
    builder.path_in_expression (LangItem::Kind::OPTION_NONE))));

  auto break_expr
    = std::unique_ptr<Expr> (new BreakExpr (tl::nullopt, nullptr, {}, loc));

  return MatchCase (std::move (arm), std::move (break_expr));
}

MatchCase
DesugarForLoops::DesugarCtx::make_continue_arm ()
{
  auto val = builder.identifier_pattern (DesugarCtx::continue_pattern_id);

  auto patterns = std::vector<std::unique_ptr<Pattern>> ();
  patterns.emplace_back (std::move (val));

  auto pattern_item = std::unique_ptr<TupleStructItems> (
    new TupleStructItemsNoRange (std::move (patterns)));
  auto pattern = std::unique_ptr<Pattern> (new TupleStructPattern (
    builder.path_in_expression (LangItem::Kind::OPTION_SOME),
    std::move (pattern_item)));

  auto val_arm = make_match_arm (std::move (pattern));

  auto next = builder.identifier (DesugarCtx::next_value_id);

  auto assignment = std::unique_ptr<Expr> (
    new AssignmentExpr (std::move (next),
			builder.identifier (DesugarCtx::continue_pattern_id),
			{}, loc));

  return MatchCase (std::move (val_arm), std::move (assignment));
}

std::unique_ptr<Stmt>
DesugarForLoops::DesugarCtx::statementify (std::unique_ptr<Expr> &&expr)
{
  return std::unique_ptr<Stmt> (new ExprStmt (std::move (expr), loc, true));
}

std::unique_ptr<Expr>
DesugarForLoops::desugar (AST::ForLoopExpr &expr)
{
  auto ctx = DesugarCtx (expr.get_locus ());

  auto into_iter = std::make_unique<PathInExpression> (
    ctx.builder.path_in_expression (LangItem::Kind::INTOITER_INTOITER));
  auto next = std::make_unique<PathInExpression> (
    ctx.builder.path_in_expression (LangItem::Kind::ITERATOR_NEXT));

  // IntoIterator::into_iter(<head>)
  auto into_iter_call
    = ctx.builder.call (std::move (into_iter),
			expr.get_iterator_expr ().clone_expr ());

  // Iterator::next(iter)
  auto next_call = ctx.builder.call (
    std::move (next),
    ctx.builder.ref (ctx.builder.identifier (DesugarCtx::iter_id), true));

  // None => break,
  auto break_arm = ctx.make_break_arm ();
  // Some(val) => __next = val; },
  auto continue_arm = ctx.make_continue_arm ();

  // match <next_call> {
  //     <continue_arm>
  //     <break_arm>
  // }
  auto match_next
    = ctx.builder.match (std::move (next_call),
			 {std::move (continue_arm), std::move (break_arm)});

  // let mut __next;
  auto let_next = ctx.builder.let (
    ctx.builder.identifier_pattern (DesugarCtx::next_value_id, true));
  // let <pattern> = __next;
  auto let_pat
    = ctx.builder.let (expr.get_pattern ().clone_pattern (), nullptr,
		       ctx.builder.identifier (DesugarCtx::next_value_id));

  auto loop_stmts = std::vector<std::unique_ptr<Stmt>> ();
  loop_stmts.emplace_back (std::move (let_next));
  loop_stmts.emplace_back (ctx.statementify (std::move (match_next)));
  loop_stmts.emplace_back (std::move (let_pat));
  loop_stmts.emplace_back (
    ctx.statementify (expr.get_loop_block ().clone_expr ()));

  // loop {
  //     <let_next>;
  //     <match_next>;
  //     <let_pat>;
  //
  //     <body>;
  // }
  auto loop = ctx.builder.loop (std::move (loop_stmts));

  auto mut_iter_pattern
    = ctx.builder.identifier_pattern (DesugarCtx::iter_id, true);
  auto match_iter
    = ctx.builder.match (std::move (into_iter_call),
			 {ctx.builder.match_case (std::move (mut_iter_pattern),
						  std::move (loop))});

  auto let_result
    = ctx.builder.let (ctx.builder.identifier_pattern (DesugarCtx::result_id),
		       nullptr, std::move (match_iter));
  auto result_return = ctx.builder.identifier (DesugarCtx::result_id);

  return ctx.builder.block (std::move (let_result), std::move (result_return));
}

void
DesugarForLoops::maybe_desugar_expr (std::unique_ptr<Expr> &expr)
{
  if (expr->get_expr_kind () == AST::Expr::Kind::Loop)
    {
      auto &loop = static_cast<AST::BaseLoopExpr &> (*expr);

      if (loop.get_loop_kind () == AST::BaseLoopExpr::Kind::For)
	{
	  auto &for_loop = static_cast<AST::ForLoopExpr &> (loop);

	  auto desugared = desugar (for_loop);

	  replace_for_loop (expr, std::move (desugared));
	}
    }
}

void
DesugarForLoops::visit (AST::BlockExpr &block)
{
  for (auto &stmt : block.get_statements ())
    if (stmt->get_stmt_kind () == AST::Stmt::Kind::Expr)
      maybe_desugar_expr (static_cast<AST::ExprStmt &> (*stmt).get_expr_ptr ());

  if (block.has_tail_expr ())
    maybe_desugar_expr (block.get_tail_expr_ptr ());

  DefaultASTVisitor::visit (block);
}

} // namespace AST
} // namespace Rust
