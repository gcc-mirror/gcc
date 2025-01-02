// Copyright (C) 2020-2025 Free Software Foundation, Inc.

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

#ifndef RUST_AST_STATEMENT_H
#define RUST_AST_STATEMENT_H

#include "rust-ast.h"
#include "rust-path.h"
#include "rust-expr.h"
#include <memory>

namespace Rust {
namespace AST {
// Just a semi-colon, which apparently is a statement.
class EmptyStmt : public Stmt
{
  location_t locus;

  // TODO: find another way to store this to save memory?
  bool marked_for_strip = false;

public:
  std::string as_string () const override { return std::string (1, ';'); }

  EmptyStmt (location_t locus) : locus (locus) {}

  location_t get_locus () const override final { return locus; }

  void accept_vis (ASTVisitor &vis) override;

  // Can't think of any invalid invariants, so store boolean.
  void mark_for_strip () override { marked_for_strip = true; }
  bool is_marked_for_strip () const override { return marked_for_strip; }

  bool is_item () const override final { return false; }

  Stmt::Kind get_stmt_kind () final { return Stmt::Kind::Empty; }

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
  std::vector<Attribute> outer_attrs;

  std::unique_ptr<Pattern> variables_pattern;

  // bool has_type;
  std::unique_ptr<Type> type;

  // bool has_init_expr;
  std::unique_ptr<Expr> init_expr;

  location_t locus;

public:
  Type *inferedType;

  // Returns whether let statement has outer attributes.
  bool has_outer_attrs () const { return !outer_attrs.empty (); }

  // Returns whether let statement has a given return type.
  bool has_type () const { return type != nullptr; }

  // Returns whether let statement has an initialisation expression.
  bool has_init_expr () const { return init_expr != nullptr; }

  std::string as_string () const override;

  LetStmt (std::unique_ptr<Pattern> variables_pattern,
	   std::unique_ptr<Expr> init_expr, std::unique_ptr<Type> type,
	   std::vector<Attribute> outer_attrs, location_t locus)
    : outer_attrs (std::move (outer_attrs)),
      variables_pattern (std::move (variables_pattern)),
      type (std::move (type)), init_expr (std::move (init_expr)), locus (locus)
  {}

  // Copy constructor with clone
  LetStmt (LetStmt const &other)
    : outer_attrs (other.outer_attrs), locus (other.locus)
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

  void accept_vis (ASTVisitor &vis) override;

  // Invalid if pattern is null, so base stripping on that.
  void mark_for_strip () override { variables_pattern = nullptr; }
  bool is_marked_for_strip () const override
  {
    return variables_pattern == nullptr;
  }

  // TODO: this mutable getter seems really dodgy. Think up better way.
  std::vector<Attribute> &get_outer_attrs () { return outer_attrs; }
  const std::vector<Attribute> &get_outer_attrs () const { return outer_attrs; }

  // TODO: is this better? Or is a "vis_block" better?
  Expr &get_init_expr ()
  {
    rust_assert (has_init_expr ());
    return *init_expr;
  }

  std::unique_ptr<Expr> &get_init_expr_ptr ()
  {
    rust_assert (has_init_expr ());
    return init_expr;
  }

  Pattern &get_pattern ()
  {
    rust_assert (variables_pattern != nullptr);
    return *variables_pattern;
  }

  Type &get_type ()
  {
    rust_assert (has_type ());
    return *type;
  }

  std::unique_ptr<Type> &get_type_ptr ()
  {
    rust_assert (has_type ());
    return type;
  }

  bool is_item () const override final { return false; }
  Stmt::Kind get_stmt_kind () final { return Stmt::Kind::Let; }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  LetStmt *clone_stmt_impl () const override { return new LetStmt (*this); }
};

// Expression statements (statements containing an expression)
class ExprStmt : public Stmt
{
  std::unique_ptr<Expr> expr;
  location_t locus;
  bool semicolon_followed;

public:
  location_t get_locus () const override final { return locus; }

  bool is_item () const override final { return false; }

  bool is_expr () const override final { return true; }

  // Used for the last statement for statement macros with a trailing
  // semicolon.
  void add_semicolon () override final { semicolon_followed = true; }

  std::string as_string () const override;

  Stmt::Kind get_stmt_kind () final { return Stmt::Kind::Expr; }

  std::vector<LetStmt *> locals;

  ExprStmt (std::unique_ptr<Expr> &&expr, location_t locus,
	    bool semicolon_followed)
    : expr (std::move (expr)), locus (locus),
      semicolon_followed (semicolon_followed)
  {}

  // Copy constructor with clone
  ExprStmt (ExprStmt const &other)
    : locus (other.locus), semicolon_followed (other.semicolon_followed)
  {
    // guard to prevent null dereference (only required if error state)
    if (other.expr != nullptr)
      expr = other.expr->clone_expr ();
  }

  // Overloaded assignment operator to clone
  ExprStmt &operator= (ExprStmt const &other)
  {
    Stmt::operator= (other);

    // guard to prevent null dereference (only required if error state)
    if (other.expr != nullptr)
      expr = other.expr->clone_expr ();
    else
      expr = nullptr;

    locus = other.locus;
    semicolon_followed = other.semicolon_followed;

    return *this;
  }

  // move constructors
  ExprStmt (ExprStmt &&other) = default;
  ExprStmt &operator= (ExprStmt &&other) = default;

  void accept_vis (ASTVisitor &vis) override;

  // Invalid if expr is null, so base stripping on that.
  void mark_for_strip () override { expr = nullptr; }
  bool is_marked_for_strip () const override { return expr == nullptr; }

  // TODO: is this better? Or is a "vis_block" better?
  Expr &get_expr ()
  {
    rust_assert (expr != nullptr);
    return *expr;
  }

  std::unique_ptr<Expr> &get_expr_ptr ()
  {
    rust_assert (expr != nullptr);
    return expr;
  }

  std::unique_ptr<Expr> take_expr ()
  {
    rust_assert (expr != nullptr);
    return std::move (expr);
  }

  bool is_semicolon_followed () const { return semicolon_followed; }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  ExprStmt *clone_stmt_impl () const override { return new ExprStmt (*this); }
};

} // namespace AST
} // namespace Rust

#endif
