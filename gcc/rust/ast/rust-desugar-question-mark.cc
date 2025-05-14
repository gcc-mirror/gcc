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

#include "rust-desugar-question-mark.h"
#include "rust-ast-builder.h"
#include "rust-ast-visitor.h"

namespace Rust {
namespace AST {

DesugarQuestionMark::DesugarQuestionMark () {}

void
DesugarQuestionMark::go (AST::Crate &crate)
{
  DesugarQuestionMark::visit (crate);
}

void
DesugarQuestionMark::visit (ExprStmt &stmt)
{
  if (stmt.get_expr ().get_expr_kind () == Expr::Kind::ErrorPropagation)
    desugar_and_replace (stmt.get_expr_ptr ());

  DefaultASTVisitor::visit (stmt);
}

void
DesugarQuestionMark::visit (CallExpr &call)
{
  if (call.get_function_expr ().get_expr_kind ()
      == Expr::Kind::ErrorPropagation)
    desugar_and_replace (call.get_function_expr_ptr ());

  for (auto &arg : call.get_params ())
    if (arg->get_expr_kind () == Expr::Kind::ErrorPropagation)
      desugar_and_replace (arg);

  DefaultASTVisitor::visit (call);
}

void
DesugarQuestionMark::visit (LetStmt &stmt)
{
  if (stmt.has_init_expr ()
      && stmt.get_init_expr ().get_expr_kind () == Expr::Kind::ErrorPropagation)
    desugar_and_replace (stmt.get_init_expr_ptr ());

  DefaultASTVisitor::visit (stmt);
}

MatchArm
make_match_arm (std::unique_ptr<Pattern> &&pattern)
{
  auto loc = pattern->get_locus ();

  auto patterns = std::vector<std::unique_ptr<Pattern>> ();
  patterns.emplace_back (std::move (pattern));

  return MatchArm (std::move (patterns), loc);
}

MatchCase
ok_case (Builder &builder)
{
  auto val = builder.identifier_pattern ("val");

  auto patterns = std::vector<std::unique_ptr<Pattern>> ();
  patterns.emplace_back (std::move (val));

  auto pattern_item = std::unique_ptr<TupleStructItems> (
    new TupleStructItemsNoRange (std::move (patterns)));
  auto pattern = std::unique_ptr<Pattern> (new TupleStructPattern (
    builder.path_in_expression (LangItem::Kind::RESULT_OK),
    std::move (pattern_item)));

  auto arm = make_match_arm (std::move (pattern));

  auto ret_val = builder.identifier ("val");

  return MatchCase (std::move (arm), std::move (ret_val));
}

MatchCase
err_case (Builder &builder)
{
  auto val = builder.identifier_pattern ("err");

  auto patterns = std::vector<std::unique_ptr<Pattern>> ();
  patterns.emplace_back (std::move (val));

  auto pattern_item = std::unique_ptr<TupleStructItems> (
    new TupleStructItemsNoRange (std::move (patterns)));
  auto pattern = std::unique_ptr<Pattern> (new TupleStructPattern (
    builder.path_in_expression (LangItem::Kind::RESULT_ERR),
    std::move (pattern_item)));

  auto arm = make_match_arm (std::move (pattern));

  auto try_from_err = std::make_unique<PathInExpression> (
    builder.path_in_expression (LangItem::Kind::TRY_FROM_ERROR));
  auto from_from = std::make_unique<PathInExpression> (
    builder.path_in_expression (LangItem::Kind::FROM_FROM));

  auto early_return = builder.return_expr (
    builder.call (std::move (try_from_err),
		  builder.call (std::move (from_from),
				builder.identifier ("err"))));

  return MatchCase (std::move (arm), std::move (early_return));
}

std::unique_ptr<Expr>
DesugarQuestionMark::desugar (ErrorPropagationExpr &expr)
{
  auto builder = Builder (expr.get_locus ());

  // Try::into_result(<expr>)
  auto try_into = std::make_unique<PathInExpression> (
    builder.path_in_expression (LangItem::Kind::TRY_INTO_RESULT));
  auto call = builder.call (std::move (try_into),
			    expr.get_propagating_expr ().clone_expr ());

  // Ok(val) => val,
  auto ok_match_case = ok_case (builder);
  // Err(err) => return Try::from_error(From::from(err)),
  auto err_match_case = err_case (builder);

  auto cases = std::vector<MatchCase> ();
  cases.emplace_back (ok_match_case);
  cases.emplace_back (err_match_case);

  // match <call> {
  //     <ok_arm>
  //     <err_arm>
  // }
  return std::unique_ptr<MatchExpr> (new MatchExpr (std::move (call),
						    std::move (cases), {}, {},
						    expr.get_locus ()));
}

void
DesugarQuestionMark::desugar_and_replace (std::unique_ptr<Expr> &ptr)
{
  auto original = static_cast<ErrorPropagationExpr &> (*ptr);
  auto desugared = desugar (original);

  ptr = std::move (desugared);
}

} // namespace AST
} // namespace Rust
