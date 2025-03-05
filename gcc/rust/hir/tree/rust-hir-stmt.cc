// Copyright (C) 2020-2024 Free Software Foundation, Inc.

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

#include "rust-hir-stmt.h"
#include "optional.h"
#include "rust-system.h"

namespace Rust {
namespace HIR {

LetStmt::LetStmt (Analysis::NodeMapping mappings,
		  std::unique_ptr<Pattern> variables_pattern,
		  tl::optional<std::unique_ptr<Expr>> init_expr,
		  tl::optional<std::unique_ptr<Expr>> else_expr,
		  tl::optional<std::unique_ptr<Type>> type,
		  AST::AttrVec outer_attrs, location_t locus)
  : Stmt (std::move (mappings)), outer_attrs (std::move (outer_attrs)),
    variables_pattern (std::move (variables_pattern)), type (std::move (type)),
    init_expr (std::move (init_expr)), else_expr (std::move (else_expr)),
    locus (locus)
{}

LetStmt::LetStmt (LetStmt const &other)
  : Stmt (other.mappings), outer_attrs (other.outer_attrs), locus (other.locus)
{
  // guard to prevent null dereference (only required if error state)
  if (other.variables_pattern != nullptr)
    variables_pattern = other.variables_pattern->clone_pattern ();

  // guard to prevent null dereference (always required)
  if (other.has_init_expr ())
    init_expr = other.get_init_expr ().clone_expr ();
  if (other.has_else_expr ())
    else_expr = other.get_else_expr ().clone_expr ();

  if (other.has_type ())
    type = other.get_type ().clone_type ();
  else
    type = tl::nullopt;
}

LetStmt &
LetStmt::operator= (LetStmt const &other)
{
  outer_attrs = other.outer_attrs;
  locus = other.locus;

  // guard to prevent null dereference (only required if error state)
  if (other.variables_pattern != nullptr)
    variables_pattern = other.variables_pattern->clone_pattern ();
  else
    variables_pattern = nullptr;

  // guard to prevent null dereference (always required)
  if (other.has_init_expr ())
    init_expr = other.get_init_expr ().clone_expr ();
  else
    init_expr = nullptr;

  if (other.has_else_expr ())
    else_expr = other.get_else_expr ().clone_expr ();
  else
    else_expr = tl::nullopt;

  if (other.has_type ())
    type = other.get_type ().clone_type ();
  else
    type = tl::nullopt;

  return *this;
}

ExprStmt::ExprStmt (Analysis::NodeMapping mappings, std::unique_ptr<Expr> expr,
		    location_t locus, bool must_be_unit)
  : Stmt (std::move (mappings)), expr (std::move (expr)), locus (locus),
    must_be_unit (must_be_unit)
{}

ExprStmt::ExprStmt (Analysis::NodeMapping mappings, std::unique_ptr<Expr> expr,
		    location_t locus)
  : ExprStmt (std::move (mappings), std::move (expr), locus, false)
{}

ExprStmt::ExprStmt (ExprStmt const &other)
  : Stmt (other), expr (other.expr->clone_expr ()), locus (other.locus)
{}

ExprStmt &
ExprStmt::operator= (ExprStmt const &other)
{
  Stmt::operator= (other);
  expr = other.expr->clone_expr ();
  locus = other.locus;

  return *this;
}

} // namespace HIR
} // namespace Rust
