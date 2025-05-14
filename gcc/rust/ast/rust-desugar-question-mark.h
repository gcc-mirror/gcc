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

#ifndef RUST_DESUGAR_QUESTION_MARK
#define RUST_DESUGAR_QUESTION_MARK

#include "rust-ast-visitor.h"
#include "rust-expr.h"
#include "rust-stmt.h"

namespace Rust {
namespace AST {

// NOTE: One more complexity compare to desugaring for-loops is that we need to
// desugar every possible expression... should we do that during lowering
// instead? but would it get resolved and expanded etc? Not sure...

// The goal of this desugar is to go from this:
//
// ```
// <expr>?
// ```
//
// to this:
//
// ```
// match Try::into_result(<expr>) {
//   Ok(val) => val,
//   Err(err) => return Try::from_err(From::from(err))
// }
// ```
//
// We use lang items for almost everything, so the actual desugared code looks
// more like this:
//
// ```
// match #[lang = "into_result"](<expr>) {
//   #[lang = "Ok"](val) => val,
//   #[lang = "Err"](err) => {
//     return #[lang = "from_error"](#[lang ="from"](err))
//   }
// }
// ```
class DesugarQuestionMark : public DefaultASTVisitor
{
  using DefaultASTVisitor::visit;

public:
  DesugarQuestionMark ();
  void go (AST::Crate &);

private:
  void desugar_and_replace (std::unique_ptr<Expr> &ptr);
  std::unique_ptr<Expr> desugar (ErrorPropagationExpr &);

  void visit (AST::ExprStmt &) override;
  void visit (AST::CallExpr &) override;
  void visit (AST::LetStmt &) override;
};

} // namespace AST
} // namespace Rust

#endif // ! RUST_DESUGAR_QUESTION_MARK
