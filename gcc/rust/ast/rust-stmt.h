#ifndef RUST_AST_STATEMENT_H
#define RUST_AST_STATEMENT_H

#include "rust-ast.h"
#include "rust-path.h"
#include "rust-expr.h"

namespace Rust {
namespace AST {
// Just a semi-colon, which apparently is a statement.
class EmptyStmt : public Stmt
{
  Location locus;

public:
  std::string as_string () const override { return std::string (1, ';'); }

  EmptyStmt (Location locus) : locus (locus) {}

  Location get_locus () const { return locus; }

  void accept_vis (ASTVisitor &vis) override;

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  EmptyStmt *clone_stmt_impl () const override { return new EmptyStmt (*this); }
};

/* Variable assignment let statement - type of "declaration statement" as it
 * introduces new name into scope */
class LetStmt : public Stmt
{
public:
  // bool has_outer_attrs;
  std::vector<Attribute> outer_attrs;

  std::unique_ptr<Pattern> variables_pattern;

  // bool has_type;
  std::unique_ptr<Type> type;

  // bool has_init_expr;
  std::unique_ptr<Expr> init_expr;

  Location locus;

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
	   std::vector<Attribute> outer_attrs, Location locus)
    : outer_attrs (std::move (outer_attrs)),
      variables_pattern (std::move (variables_pattern)),
      type (std::move (type)), init_expr (std::move (init_expr)), locus (locus)
  {}

  // Copy constructor with clone
  LetStmt (LetStmt const &other)
    : outer_attrs (other.outer_attrs),
      variables_pattern (other.variables_pattern->clone_pattern ()),
      type (other.type->clone_type ()),
      init_expr (other.init_expr->clone_expr ()), locus (other.locus)
  {}

  // Overloaded assignment operator to clone
  LetStmt &operator= (LetStmt const &other)
  {
    variables_pattern = other.variables_pattern->clone_pattern ();
    init_expr = other.init_expr->clone_expr ();
    type = other.type->clone_type ();
    outer_attrs = other.outer_attrs;
    locus = other.locus;

    return *this;
  }

  // move constructors
  LetStmt (LetStmt &&other) = default;
  LetStmt &operator= (LetStmt &&other) = default;

  Location get_locus () const { return locus; }

  void accept_vis (ASTVisitor &vis) override;

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  LetStmt *clone_stmt_impl () const override { return new LetStmt (*this); }
};

/* Abstract base class for expression statements (statements containing an
 * expression) */
class ExprStmt : public Stmt
{
  // TODO: add any useful virtual functions

  Location locus;

public:
  Location get_locus () const { return locus; }

protected:
  ExprStmt (Location locus) : locus (locus) {}
};

/* Statement containing an expression without a block (or, due to technical
 * difficulties, can only be guaranteed to hold an expression). */
class ExprStmtWithoutBlock : public ExprStmt
{
public:
  // TODO: ensure that this works
  std::unique_ptr<ExprWithoutBlock> expr;
  /* HACK: cannot ensure type safety of ExprWithoutBlock due to Pratt parsing,
   * so have to store more general type of Expr. FIXME: fix this issue somehow
   * or redesign AST. */
  // std::unique_ptr<Expr> expr;

  std::string as_string () const override;

  ExprStmtWithoutBlock (std::unique_ptr<ExprWithoutBlock> expr, Location locus)
    : ExprStmt (locus), expr (std::move (expr))
  {}
  /*ExprStmtWithoutBlock (std::unique_ptr<Expr> expr, Location locus)
    : ExprStmt (locus), expr (std::move (expr))
  {}*/

  // Copy constructor with clone
  ExprStmtWithoutBlock (ExprStmtWithoutBlock const &other)
    : ExprStmt (other), expr (other.expr->clone_expr_without_block ())
  {}
  /*ExprStmtWithoutBlock (ExprStmtWithoutBlock const &other)
    : ExprStmt (other), expr (other.expr->clone_expr ())
  {}*/

  // Overloaded assignment operator to clone
  ExprStmtWithoutBlock &operator= (ExprStmtWithoutBlock const &other)
  {
    ExprStmt::operator= (other);
    expr = other.expr->clone_expr_without_block ();
    //expr = other.expr->clone_expr ();

    return *this;
  }

  // move constructors
  ExprStmtWithoutBlock (ExprStmtWithoutBlock &&other) = default;
  ExprStmtWithoutBlock &operator= (ExprStmtWithoutBlock &&other) = default;

  void accept_vis (ASTVisitor &vis) override;

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  ExprStmtWithoutBlock *clone_stmt_impl () const override
  {
    return new ExprStmtWithoutBlock (*this);
  }
};

// Statement containing an expression with a block
class ExprStmtWithBlock : public ExprStmt
{
public:
  std::unique_ptr<ExprWithBlock> expr;

  std::string as_string () const override;

  std::vector<LetStmt *> locals;

  ExprStmtWithBlock (std::unique_ptr<ExprWithBlock> expr, Location locus)
    : ExprStmt (locus), expr (std::move (expr))
  {}

  // Copy constructor with clone
  ExprStmtWithBlock (ExprStmtWithBlock const &other)
    : ExprStmt (other), expr (other.expr->clone_expr_with_block ())
  {}

  // Overloaded assignment operator to clone
  ExprStmtWithBlock &operator= (ExprStmtWithBlock const &other)
  {
    ExprStmt::operator= (other);
    expr = other.expr->clone_expr_with_block ();

    return *this;
  }

  // move constructors
  ExprStmtWithBlock (ExprStmtWithBlock &&other) = default;
  ExprStmtWithBlock &operator= (ExprStmtWithBlock &&other) = default;

  void accept_vis (ASTVisitor &vis) override;

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  ExprStmtWithBlock *clone_stmt_impl () const override
  {
    return new ExprStmtWithBlock (*this);
  }
};

/* Replaced definition of MacroInvocationSemi with forward decl - defined in
 * rust-macro.h */
class MacroInvocationSemi;
} // namespace AST
} // namespace Rust

#endif
