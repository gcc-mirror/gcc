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

#ifndef RUST_HIR_STATEMENT_H
#define RUST_HIR_STATEMENT_H

#include "rust-hir.h"
#include "rust-hir-path.h"
#include "rust-hir-expr.h"
#include "rust-system.h"

namespace Rust {
namespace HIR {
/* Base statement abstract class. Note that most "statements" are not allowed in
 * top-level module scope - only a subclass of statements called "items" are. */
class Stmt : public Node, public FullVisitable
{
public:
  using FullVisitable::accept_vis;

  // Unique pointer custom clone function
  std::unique_ptr<Stmt> clone_stmt () const
  {
    return std::unique_ptr<Stmt> (clone_stmt_impl ());
  }

  BaseKind get_hir_kind () override { return STMT; }

  virtual ~Stmt () {}

  virtual std::string as_string () const = 0;

  virtual void accept_vis (HIRStmtVisitor &vis) = 0;

  virtual location_t get_locus () const = 0;

  virtual bool is_unit_check_needed () const { return false; }

  const Analysis::NodeMapping &get_mappings () const { return mappings; }

  virtual bool is_item () const = 0;

protected:
  Stmt (Analysis::NodeMapping mappings) : mappings (std::move (mappings)) {}

  // Clone function implementation as pure virtual method
  virtual Stmt *clone_stmt_impl () const = 0;

  Analysis::NodeMapping mappings;
};

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

  tl::optional<std::unique_ptr<Type>> type;

  tl::optional<std::unique_ptr<Expr>> init_expr;
  tl::optional<std::unique_ptr<Expr>> else_expr;

  location_t locus;

public:
  // Returns whether let statement has outer attributes.
  bool has_outer_attrs () const { return !outer_attrs.empty (); }

  // Returns whether let statement has a given return type.
  bool has_type () const { return type.has_value (); }

  // Returns whether let statement has an initialisation expression.
  bool has_init_expr () const { return init_expr.has_value (); }
  // Returns whether let statement has a diverging else expression.
  bool has_else_expr () const { return else_expr.has_value (); }

  std::string as_string () const override;

  LetStmt (Analysis::NodeMapping mappings,
	   std::unique_ptr<Pattern> variables_pattern,
	   tl::optional<std::unique_ptr<Expr>> init_expr,
	   tl::optional<std::unique_ptr<Expr>> else_expr,
	   tl::optional<std::unique_ptr<Type>> type, AST::AttrVec outer_attrs,
	   location_t locus);

  // Copy constructor with clone
  LetStmt (LetStmt const &other);

  // Overloaded assignment operator to clone
  LetStmt &operator= (LetStmt const &other);

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

  HIR::Type &get_type ()
  {
    rust_assert (*type);
    return *type.value ();
  }

  const HIR::Type &get_type () const
  {
    rust_assert (*type);
    return *type.value ();
  }

  HIR::Expr &get_init_expr ()
  {
    rust_assert (*init_expr);
    return *init_expr.value ();
  }

  const HIR::Expr &get_init_expr () const
  {
    rust_assert (*init_expr);
    return *init_expr.value ();
  }

  HIR::Expr &get_else_expr ()
  {
    rust_assert (*else_expr);
    return *else_expr.value ();
  }

  const HIR::Expr &get_else_expr () const
  {
    rust_assert (*else_expr);
    return *else_expr.value ();
  }

  HIR::Pattern &get_pattern () { return *variables_pattern; }

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
	    location_t locus, bool must_be_unit);

  ExprStmt (Analysis::NodeMapping mappings, std::unique_ptr<Expr> expr,
	    location_t locus);

  std::string as_string () const override;

  location_t get_locus () const override final { return locus; }

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRStmtVisitor &vis) override;

  bool is_item () const override final { return false; }

  Expr &get_expr () { return *expr; }

  // Copy constructor with clone
  ExprStmt (ExprStmt const &other);

  // Overloaded assignment operator to clone
  ExprStmt &operator= (ExprStmt const &other);

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
