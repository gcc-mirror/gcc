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

#ifndef RUST_DESUGAR_WHILE_LET_H
#define RUST_DESUGAR_WHILE_LET_H

#include "rust-ast-builder.h"
#include "rust-expr.h"

namespace Rust {
namespace AST {

// Desugar while-let into a set of other AST nodes. The desugar is of the
// following form:
//
// ```
// whilet let <pat> = <expr> <body>
// ```
//
// becomes:
//
// ```
// loop {
//     match <expr> {
//         <pat> => <body>,
//         _ => break
//     }
// }
// ```
class DesugarWhileLet
{
public:
  static void go (std::unique_ptr<Expr> &ptr);

private:
  DesugarWhileLet ();

  struct DesugarCtx
  {
    DesugarCtx (location_t loc) : builder (Builder (loc)), loc (loc) {}

    Builder builder;
    location_t loc;

    MatchCase make_break_arm ();
    MatchCase make_continue_arm (std::unique_ptr<Pattern> &&pattern,
				 std::unique_ptr<BlockExpr> &&body);
  };

  std::unique_ptr<Expr> desugar (WhileLetLoopExpr &expr);
};

} // namespace AST
} // namespace Rust

#endif // ! RUST_DESUGAR_WHILE_LET_H
