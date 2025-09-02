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

#include "rust-desugar-try-block.h"
#include "rust-ast-builder.h"
#include "rust-expr.h"

namespace Rust {
namespace AST {

DesugarTryBlock::DesugarTryBlock () {}

void
DesugarTryBlock::go (std::unique_ptr<Expr> &ptr)
{
  rust_assert (ptr->get_expr_kind () == Expr::Kind::Try);

  auto original = static_cast<TryExpr &> (*ptr);
  auto desugared = DesugarTryBlock ().desugar (original);

  ptr = std::move (desugared);
}

std::unique_ptr<Expr>
DesugarTryBlock::desugar (TryExpr &expr)
{
  auto builder = Builder (expr.get_locus ());
  auto &block = expr.get_block_expr ();

  if (block.has_statements ())
    rust_sorry_at (expr.get_locus (),
		   "cannot desugar try-blocks with statements");

  auto tail_expr = builder.tuple ();

  if (block.has_tail_expr ())
    tail_expr = block.get_tail_expr ().clone_expr ();

  // Wrap in Try::from_ok call
  auto from_ok = builder.path_in_expression (LangItem::Kind::TRY_FROM_OK);
  auto call = builder.call (ptrify (from_ok), std::move (tail_expr));

  return builder.block (std::move (call));
}

} // namespace AST
} // namespace Rust
