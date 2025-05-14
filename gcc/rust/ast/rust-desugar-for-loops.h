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

#ifndef RUST_DESUGAR_FOR_LOOPS_H
#define RUST_DESUGAR_FOR_LOOPS_H

#include "rust-ast-builder.h"
#include "rust-ast-visitor.h"
#include "rust-expr.h"

namespace Rust {
namespace AST {

// Desugar for-loops into a set of other AST nodes. The desugar is of the
// following form:
//
// ```
// for <pat> in <head> <body>
// ```
//
// becomes:
//
// ```
// {
//     let result = match ::std::iter::IntoIterator::into_iter(<head>) {
//         mut iter => {
//             loop {
//                 let mut __next;
//                 match ::std::iter::Iterator::next(&mut iter) {
//                     ::std::option::Option::Some(val) => __next = val,
//                     ::std::option::Option::None => break
//                 };
//                 let <pat> = __next;
//
//                 <body>;
//             }
//         }
//     };
//     result
// }
// ```
//
// NOTE: In a perfect world, this would be an immutable visitor which would take
// ownership of the AST node and return a new one, instead of mutating this one
// in place. Nevertheless, this isn't Rust, and doing immutable visitors in C++
// sucks, and the world isn't perfect, so we are impure and sad.
//
// NOTE: This class could eventually be removed in favor of
// an HIR desugar. This would avoid mutating the AST and would be cleaner.
// However, it requires multiple changes in the way we do typechecking and name
// resolution, as this desugar creates new bindings. Because of this, these new
// bindings need to be inserted into the name-resolution context outside of the
// name resolution pass, which is difficult. Those bindings are needed because
// of the way the typechecker is currently structured, where it will fetch name
// resolution information in order to typecheck paths - which technically isn't
// necessary.
class DesugarForLoops : public DefaultASTVisitor
{
  using DefaultASTVisitor::visit;

public:
  DesugarForLoops ();
  void go (AST::Crate &);

private:
  struct DesugarCtx
  {
    DesugarCtx (location_t loc) : builder (Builder (loc)), loc (loc) {}

    Builder builder;
    location_t loc;

    MatchArm make_match_arm (std::unique_ptr<Pattern> &&pattern);
    MatchCase make_break_arm ();
    MatchCase make_continue_arm ();
    std::unique_ptr<Stmt> statementify (std::unique_ptr<Expr> &&expr);

    constexpr static const char *continue_pattern_id = "#val";
    constexpr static const char *next_value_id = "#__next";
    constexpr static const char *iter_id = "#iter";
    constexpr static const char *result_id = "#result";
  };

  std::unique_ptr<Expr> desugar (AST::ForLoopExpr &expr);
  void maybe_desugar_expr (std::unique_ptr<Expr> &expr);

  void visit (AST::BlockExpr &) override;
};

} // namespace AST
} // namespace Rust

#endif // ! RUST_DESUGAR_FOR_LOOPS_H
