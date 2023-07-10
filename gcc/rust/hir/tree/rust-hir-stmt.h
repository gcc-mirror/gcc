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

#ifndef RUST_HIR_STATEMENT_H
#define RUST_HIR_STATEMENT_H

#include "rust-hir.h"
#include "rust-hir-path.h"
#include "rust-hir-expr.h"

namespace Rust {
namespace HIR {
// Just a semi-colon, which apparently is a statement.
class EmptyStmt : public Stmt
{
  location_t locus;

public:
  std::string as_string () const override { return std::string (1, ';'); }

  EmptyStmt (Analysis::NodeMapping mappings, location_t locus)
    : Stmt (std::move (mappings)), locus (locus)
  {}

  location_t get_locus () const override final { return locus; }

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRStmtVisitor &vis) override;

  bool is_item () const override final { return false; }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  EmptyStmt *clone_stmt_impl () const override { return new EmptyStmt (*this); }
};

/* Variable assignment let statement - type of "declaration statement" as it
 * introduces new name into scope */
class LetStmt : public Stmt
{
  // bool has_outer_attrs;
  AST::AttrVec outer_attrs;

  std::unique_ptr<Pattern> variables_pattern;

  // bool has_type;
  std::unique_ptr<Type> type;

  // bool has_init_expr;
  std::unique_ptr<Expr> init_expr;

  location_t locus;

public:
  // Returns whether let statement has outer attributes.
  bool has_outer_attrs () const { return !outer_attrs.empty (); }

  // Returns whether let statement has a given return type.
  bool has_type () const { return type != nullptr; }

  // Returns whether let statement has an initialisation expression.
  bool has_init_expr () const { return init_expr != nullptr; }

  std::string as_string () const override;

  LetStmt (Analysis::NodeMapping mappings,
	   std::unique_ptr<Pattern> variables_pattern,
	   std::unique_ptr<Expr> init_expr, std::unique_ptr<Type> type,
	   AST::AttrVec outer_attrs, location_t locus)
    : Stmt (std::move (mappings)), outer_attrs (std::move (outer_attrs)),
      variables_pattern (std::move (variables_pattern)),
      type (std::move (type)), init_expr (std::move (init_expr)), locus (locus)
  {}

  // Copy constructor with clone
  LetStmt (LetStmt const &other)
    : Stmt (other.mappings), outer_attrs (other.outer_attrs),
      locus (other.locus)
  {
    // guard to prevent null dereference (only required if error state)
    if (other.variables_pattern != nullptr)
      variables_pattern = other.variables_pattern->clone_pattern ();

    // guard to prevent null dereference (always required)
    if (other.init_expr != nullptr)
      init_expr = other.init_expr->clone_expr ();
    if (other.type != nullptr)
      type = other.type->clone_type ();
  }

  // Overloaded assignment operator to clone
  LetStmt &operator= (LetStmt const &other)
  {
    outer_attrs = other.outer_attrs;
    locus = other.locus;

    // guard to prevent null dereference (only required if error state)
    if (other.variables_pattern != nullptr)
      variables_pattern = other.variables_pattern->clone_pattern ();
    else
      variables_pattern = nullptr;

    // guard to prevent null dereference (always required)
    if (other.init_expr != nullptr)
      init_expr = other.init_expr->clone_expr ();
    else
      init_expr = nullptr;
    if (other.type != nullptr)
      type = other.type->clone_type ();
    else
      type = nullptr;

    return *this;
  }

  // move constructors
  LetStmt (LetStmt &&other) = default;
  LetStmt &operator= (LetStmt &&other) = default;

  location_t get_locus () const override final { return locus; }

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRStmtVisitor &vis) override;

  const std::vector<AST::Attribute> &get_outer_attrs () const
  {
    return outer_attrs;
  }
  std::vector<AST::Attribute> &get_outer_attrs () { return outer_attrs; }

  std::unique_ptr<HIR::Type> &get_type () { return type; }

  std::unique_ptr<HIR::Expr> &get_init_expr () { return init_expr; }

  std::unique_ptr<HIR::Pattern> &get_pattern () { return variables_pattern; }

  bool is_item () const override final { return false; }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  LetStmt *clone_stmt_impl () const override { return new LetStmt (*this); }
};

/* class for expression statements (statements containing an expression) */
class ExprStmt : public Stmt
{
  std::unique_ptr<Expr> expr;
  location_t locus;
  bool must_be_unit;

public:
  ExprStmt (Analysis::NodeMapping mappings, std::unique_ptr<Expr> expr,
	    location_t locus, bool must_be_unit)
    : Stmt (std::move (mappings)), expr (std::move (expr)), locus (locus),
      must_be_unit (must_be_unit)
  {}

  ExprStmt (Analysis::NodeMapping mappings, std::unique_ptr<Expr> expr,
	    location_t locus)
    : ExprStmt (std::move (mappings), std::move (expr), locus, false)
  {}

  std::string as_string () const override;

  location_t get_locus () const override final { return locus; }

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRStmtVisitor &vis) override;

  bool is_item () const override final { return false; }

  std::unique_ptr<Expr> &get_expr () { return expr; }

  // Copy constructor with clone
  ExprStmt (ExprStmt const &other)
    : Stmt (other), expr (other.expr->clone_expr ()), locus (other.locus)
  {}

  // Overloaded assignment operator to clone
  ExprStmt &operator= (ExprStmt const &other)
  {
    Stmt::operator= (other);
    expr = other.expr->clone_expr ();
    locus = other.locus;

    return *this;
  }

  // move constructors
  ExprStmt (ExprStmt &&other) = default;
  ExprStmt &operator= (ExprStmt &&other) = default;

  bool is_unit_check_needed () const override { return must_be_unit; }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  ExprStmt *clone_stmt_impl () const override { return new ExprStmt (*this); }
};

} // namespace HIR
} // namespace Rust

#endif
