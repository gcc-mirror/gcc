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

#ifndef RUST_HIR_EXPR_H
#define RUST_HIR_EXPR_H

#include "rust-common.h"
#include "rust-ast-full-decls.h"
#include "rust-hir.h"
#include "rust-hir-path.h"
#include "rust-operators.h"

namespace Rust {
namespace HIR {

// Loop label expression HIR node used with break and continue expressions
// TODO: inline?
class LoopLabel /*: public Node*/
{
  Lifetime label; // or type LIFETIME_OR_LABEL

  location_t locus;

  Analysis::NodeMapping mappings;

public:
  std::string as_string () const;

  LoopLabel (Analysis::NodeMapping mapping, Lifetime loop_label,
	     location_t locus)
    : label (std::move (loop_label)), locus (locus), mappings (mapping)
  {}

  // Returns whether the LoopLabel is in an error state.
  bool is_error () const { return label.is_error (); }

  location_t get_locus () const { return locus; }

  Analysis::NodeMapping &get_mappings () { return mappings; }

  Lifetime &get_lifetime () { return label; }
};

// HIR node for an expression with an accompanying block - abstract
class ExprWithBlock : public Expr
{
  // TODO: should this mean that a BlockExpr should be a member variable?
protected:
  ExprWithBlock (Analysis::NodeMapping mappings,
		 AST::AttrVec outer_attrs = AST::AttrVec ())
    : Expr (std::move (mappings), std::move (outer_attrs))
  {}

  // pure virtual clone implementation
  virtual ExprWithBlock *clone_expr_with_block_impl () const = 0;

  // prevent having to define multiple clone expressions
  ExprWithBlock *clone_expr_impl () const override
  {
    return clone_expr_with_block_impl ();
  }

public:
  // Unique pointer custom clone function
  std::unique_ptr<ExprWithBlock> clone_expr_with_block () const
  {
    return std::unique_ptr<ExprWithBlock> (clone_expr_with_block_impl ());
  }

  BlockType get_block_expr_type () const final override
  {
    return BlockType::WITH_BLOCK;
  };
};

// Literals? Or literal base?
class LiteralExpr : public ExprWithoutBlock
{
  Literal literal;
  location_t locus;

public:
  std::string as_string () const override
  {
    return "( " + literal.as_string () + " (" + get_mappings ().as_string ()
	   + "))";
  }

  Literal::LitType get_lit_type () const { return literal.get_lit_type (); }

  LiteralExpr (Analysis::NodeMapping mappings, std::string value_as_string,
	       Literal::LitType type, PrimitiveCoreType type_hint,
	       location_t locus, AST::AttrVec outer_attrs)
    : ExprWithoutBlock (std::move (mappings), std::move (outer_attrs)),
      literal (std::move (value_as_string), type, type_hint), locus (locus)
  {}

  LiteralExpr (Analysis::NodeMapping mappings, Literal literal,
	       location_t locus, AST::AttrVec outer_attrs)
    : ExprWithoutBlock (std::move (mappings), std::move (outer_attrs)),
      literal (std::move (literal)), locus (locus)
  {}

  // Unique pointer custom clone function
  std::unique_ptr<LiteralExpr> clone_literal_expr () const
  {
    return std::unique_ptr<LiteralExpr> (clone_literal_expr_impl ());
  }

  location_t get_locus () const override final { return locus; }

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

  Literal &get_literal () { return literal; }
  const Literal &get_literal () const { return literal; }

  ExprType get_expression_type () const override final { return ExprType::Lit; }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  LiteralExpr *clone_expr_impl () const override
  {
    return new LiteralExpr (*this);
  }

  /* Use covariance to implement clone function as returning this object rather
   * than base */
  LiteralExpr *clone_expr_without_block_impl () const override
  {
    return new LiteralExpr (*this);
  }

  /* not virtual as currently no subclasses of LiteralExpr, but could be in
   * future */
  /*virtual*/ LiteralExpr *clone_literal_expr_impl () const
  {
    return new LiteralExpr (*this);
  }
};

/* Represents an expression using unary or binary operators as HIR node. Can be
 * overloaded. */
class OperatorExpr : public ExprWithoutBlock
{
  // TODO: create binary and unary operator subclasses?
public:
  location_t locus;

protected:
  /* Variable must be protected to allow derived classes to use it as a first
   * class citizen */
  std::unique_ptr<Expr> main_or_left_expr;

  // Constructor (only for initialisation of expr purposes)
  OperatorExpr (Analysis::NodeMapping mappings,
		std::unique_ptr<Expr> main_or_left_expr,
		AST::AttrVec outer_attribs, location_t locus)
    : ExprWithoutBlock (std::move (mappings), std::move (outer_attribs)),
      locus (locus), main_or_left_expr (std::move (main_or_left_expr))
  {}

  // Copy constructor (only for initialisation of expr purposes)
  OperatorExpr (OperatorExpr const &other)
    : ExprWithoutBlock (other), locus (other.locus),
      main_or_left_expr (other.main_or_left_expr->clone_expr ())
  {}

  // Overload assignment operator to deep copy expr
  OperatorExpr &operator= (OperatorExpr const &other)
  {
    ExprWithoutBlock::operator= (other);
    main_or_left_expr = other.main_or_left_expr->clone_expr ();
    locus = other.locus;
    // outer_attrs = other.outer_attrs;

    return *this;
  }

  // move constructors
  OperatorExpr (OperatorExpr &&other) = default;
  OperatorExpr &operator= (OperatorExpr &&other) = default;

public:
  location_t get_locus () const override final { return locus; }

  std::unique_ptr<Expr> &get_expr () { return main_or_left_expr; }

  ExprType get_expression_type () const override final
  {
    return ExprType::Operator;
  }
};

/* Unary prefix & or &mut (or && and &&mut) borrow operator. Cannot be
 * overloaded. */
class BorrowExpr : public OperatorExpr
{
  Mutability mut;

public:
  std::string as_string () const override;

  BorrowExpr (Analysis::NodeMapping mappings,
	      std::unique_ptr<Expr> borrow_lvalue, Mutability mut,
	      AST::AttrVec outer_attribs, location_t locus)
    : OperatorExpr (std::move (mappings), std::move (borrow_lvalue),
		    std::move (outer_attribs), locus),
      mut (mut)
  {}

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

  Mutability get_mut () const { return mut; }
  bool is_mut () const { return mut == Mutability::Mut; }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  BorrowExpr *clone_expr_impl () const override
  {
    return new BorrowExpr (*this);
  }

  /* Use covariance to implement clone function as returning this object rather
   * than base */
  BorrowExpr *clone_expr_without_block_impl () const override
  {
    return new BorrowExpr (*this);
  }
};

// Unary prefix * deference operator
class DereferenceExpr : public OperatorExpr
{
public:
  std::string as_string () const override;

  // Constructor calls OperatorExpr's protected constructor
  DereferenceExpr (Analysis::NodeMapping mappings,
		   std::unique_ptr<Expr> deref_lvalue,
		   AST::AttrVec outer_attribs, location_t locus)
    : OperatorExpr (std::move (mappings), std::move (deref_lvalue),
		    std::move (outer_attribs), locus)
  {}

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  DereferenceExpr *clone_expr_impl () const override
  {
    return new DereferenceExpr (*this);
  }

  /* Use covariance to implement clone function as returning this object rather
   * than base */
  DereferenceExpr *clone_expr_without_block_impl () const override
  {
    return new DereferenceExpr (*this);
  }
};

// Unary postfix ? error propogation operator. Cannot be overloaded.
class ErrorPropagationExpr : public OperatorExpr
{
public:
  std::string as_string () const override;

  // Constructor calls OperatorExpr's protected constructor
  ErrorPropagationExpr (Analysis::NodeMapping mappings,
			std::unique_ptr<Expr> potential_error_value,
			AST::AttrVec outer_attribs, location_t locus)
    : OperatorExpr (std::move (mappings), std::move (potential_error_value),
		    std::move (outer_attribs), locus)
  {}

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  ErrorPropagationExpr *clone_expr_impl () const override
  {
    return new ErrorPropagationExpr (*this);
  }

  /* Use covariance to implement clone function as returning this object rather
   * than base */
  ErrorPropagationExpr *clone_expr_without_block_impl () const override
  {
    return new ErrorPropagationExpr (*this);
  }
};

// Unary prefix - or ! negation or NOT operators.
class NegationExpr : public OperatorExpr
{
public:
  using ExprType = NegationOperator;

private:
  /* Note: overload negation via std::ops::Neg and not via std::ops::Not
   * Negation only works for signed integer and floating-point types, NOT only
   * works for boolean and integer types (via bitwise NOT) */
  ExprType expr_type;

public:
  std::string as_string () const override;

  ExprType get_expr_type () const { return expr_type; }

  // Constructor calls OperatorExpr's protected constructor
  NegationExpr (Analysis::NodeMapping mappings,
		std::unique_ptr<Expr> negated_value, ExprType expr_kind,
		AST::AttrVec outer_attribs, location_t locus)
    : OperatorExpr (std::move (mappings), std::move (negated_value),
		    std::move (outer_attribs), locus),
      expr_type (expr_kind)
  {}

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  NegationExpr *clone_expr_impl () const override
  {
    return new NegationExpr (*this);
  }

  /* Use covariance to implement clone function as returning this object rather
   * than base */
  NegationExpr *clone_expr_without_block_impl () const override
  {
    return new NegationExpr (*this);
  }
};

// Infix binary operators. +, -, *, /, %, &, |, ^, <<, >>
class ArithmeticOrLogicalExpr : public OperatorExpr
{
public:
  using ExprType = ArithmeticOrLogicalOperator;

private:
  // Note: overloading trait specified in comments
  ExprType expr_type;

  std::unique_ptr<Expr> right_expr;

public:
  std::string as_string () const override;

  ExprType get_expr_type () const { return expr_type; }

  // Constructor calls OperatorExpr's protected constructor
  ArithmeticOrLogicalExpr (Analysis::NodeMapping mappings,
			   std::unique_ptr<Expr> left_value,
			   std::unique_ptr<Expr> right_value,
			   ExprType expr_kind, location_t locus)
    : OperatorExpr (std::move (mappings), std::move (left_value),
		    AST::AttrVec (), locus),
      expr_type (expr_kind), right_expr (std::move (right_value))
  {}
  // outer attributes not allowed

  // Copy constructor - probably required due to unique pointer
  ArithmeticOrLogicalExpr (ArithmeticOrLogicalExpr const &other)
    : OperatorExpr (other), expr_type (other.expr_type),
      right_expr (other.right_expr->clone_expr ())
  {}

  // Overload assignment operator
  ArithmeticOrLogicalExpr &operator= (ArithmeticOrLogicalExpr const &other)
  {
    OperatorExpr::operator= (other);
    // main_or_left_expr = other.main_or_left_expr->clone_expr();
    right_expr = other.right_expr->clone_expr ();
    expr_type = other.expr_type;

    return *this;
  }

  // move constructors
  ArithmeticOrLogicalExpr (ArithmeticOrLogicalExpr &&other) = default;
  ArithmeticOrLogicalExpr &operator= (ArithmeticOrLogicalExpr &&other)
    = default;

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

  void visit_lhs (HIRFullVisitor &vis) { main_or_left_expr->accept_vis (vis); }
  void visit_rhs (HIRFullVisitor &vis) { right_expr->accept_vis (vis); }

  std::unique_ptr<Expr> &get_lhs () { return main_or_left_expr; }
  std::unique_ptr<Expr> &get_rhs () { return right_expr; }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  ArithmeticOrLogicalExpr *clone_expr_impl () const override
  {
    return new ArithmeticOrLogicalExpr (*this);
  }

  /* Use covariance to implement clone function as returning this object rather
   * than base */
  ArithmeticOrLogicalExpr *clone_expr_without_block_impl () const override
  {
    return new ArithmeticOrLogicalExpr (*this);
  }
};

// Infix binary comparison operators. ==, !=, <, <=, >, >=
class ComparisonExpr : public OperatorExpr
{
public:
  using ExprType = ComparisonOperator;

private:
  // Note: overloading trait specified in comments
  ExprType expr_type;

  std::unique_ptr<Expr> right_expr;

public:
  std::string as_string () const override;

  ExprType get_expr_type () const { return expr_type; }

  // Constructor requires pointers for polymorphism
  ComparisonExpr (Analysis::NodeMapping mappings,
		  std::unique_ptr<Expr> left_value,
		  std::unique_ptr<Expr> right_value, ExprType comparison_kind,
		  location_t locus)
    : OperatorExpr (std::move (mappings), std::move (left_value),
		    AST::AttrVec (), locus),
      expr_type (comparison_kind), right_expr (std::move (right_value))
  {}
  // outer attributes not allowed

  // Copy constructor also calls OperatorExpr's protected constructor
  ComparisonExpr (ComparisonExpr const &other)
    : OperatorExpr (other), expr_type (other.expr_type),
      right_expr (other.right_expr->clone_expr ())
  {}

  // Overload assignment operator to deep copy
  ComparisonExpr &operator= (ComparisonExpr const &other)
  {
    OperatorExpr::operator= (other);
    // main_or_left_expr = other.main_or_left_expr->clone_expr();
    right_expr = other.right_expr->clone_expr ();
    expr_type = other.expr_type;
    // outer_attrs = other.outer_attrs;

    return *this;
  }

  // move constructors
  ComparisonExpr (ComparisonExpr &&other) = default;
  ComparisonExpr &operator= (ComparisonExpr &&other) = default;

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

  std::unique_ptr<Expr> &get_lhs () { return main_or_left_expr; }
  std::unique_ptr<Expr> &get_rhs () { return right_expr; }

  ExprType get_kind () { return expr_type; }

  /* TODO: implement via a function call to std::cmp::PartialEq::eq(&op1, &op2)
   * maybe? */
protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  ComparisonExpr *clone_expr_impl () const override
  {
    return new ComparisonExpr (*this);
  }

  /* Use covariance to implement clone function as returning this object rather
   * than base */
  ComparisonExpr *clone_expr_without_block_impl () const override
  {
    return new ComparisonExpr (*this);
  }
};

// Infix binary lazy boolean logical operators && and ||.
class LazyBooleanExpr : public OperatorExpr
{
public:
  using ExprType = LazyBooleanOperator;

private:
  ExprType expr_type;

  std::unique_ptr<Expr> right_expr;

public:
  // Constructor calls OperatorExpr's protected constructor
  LazyBooleanExpr (Analysis::NodeMapping mappings,
		   std::unique_ptr<Expr> left_bool_expr,
		   std::unique_ptr<Expr> right_bool_expr, ExprType expr_kind,
		   location_t locus)
    : OperatorExpr (std::move (mappings), std::move (left_bool_expr),
		    AST::AttrVec (), locus),
      expr_type (expr_kind), right_expr (std::move (right_bool_expr))
  {}
  // outer attributes not allowed

  // Copy constructor also calls OperatorExpr's protected constructor
  LazyBooleanExpr (LazyBooleanExpr const &other)
    : OperatorExpr (other), expr_type (other.expr_type),
      right_expr (other.right_expr->clone_expr ())
  {}

  // Overload assignment operator to deep copy
  LazyBooleanExpr &operator= (LazyBooleanExpr const &other)
  {
    OperatorExpr::operator= (other);
    // main_or_left_expr = other.main_or_left_expr->clone_expr();
    right_expr = other.right_expr->clone_expr ();
    expr_type = other.expr_type;

    return *this;
  }

  // move constructors
  LazyBooleanExpr (LazyBooleanExpr &&other) = default;
  LazyBooleanExpr &operator= (LazyBooleanExpr &&other) = default;

  std::string as_string () const override;

  ExprType get_expr_type () const { return expr_type; }

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

  std::unique_ptr<Expr> &get_lhs () { return main_or_left_expr; }
  std::unique_ptr<Expr> &get_rhs () { return right_expr; }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  LazyBooleanExpr *clone_expr_impl () const override
  {
    return new LazyBooleanExpr (*this);
  }

  /* Use covariance to implement clone function as returning this object rather
   * than base */
  LazyBooleanExpr *clone_expr_without_block_impl () const override
  {
    return new LazyBooleanExpr (*this);
  }
};

// Binary infix "as" chir expression.
class TypeCastExpr : public OperatorExpr
{
  std::unique_ptr<Type> type_to_convert_to;

  // Note: only certain type casts allowed, outlined in reference
public:
  std::string as_string () const override;

  // Constructor requires calling protected constructor of OperatorExpr
  TypeCastExpr (Analysis::NodeMapping mappings,
		std::unique_ptr<Expr> expr_to_cast,
		std::unique_ptr<Type> type_to_cast_to, location_t locus)
    : OperatorExpr (std::move (mappings), std::move (expr_to_cast),
		    AST::AttrVec (), locus),
      type_to_convert_to (std::move (type_to_cast_to))
  {}
  // outer attributes not allowed

  // Copy constructor also requires calling protected constructor
  TypeCastExpr (TypeCastExpr const &other)
    : OperatorExpr (other),
      type_to_convert_to (other.type_to_convert_to->clone_type ())
  {}

  // Overload assignment operator to deep copy
  TypeCastExpr &operator= (TypeCastExpr const &other)
  {
    OperatorExpr::operator= (other);
    // main_or_left_expr = other.main_or_left_expr->clone_expr();
    type_to_convert_to = other.type_to_convert_to->clone_type ();

    return *this;
  }

  // move constructors as not supported in c++03
  TypeCastExpr (TypeCastExpr &&other) = default;
  TypeCastExpr &operator= (TypeCastExpr &&other) = default;

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

  // FIXME: isn't it the same as get_expr() from parent?
  std::unique_ptr<Expr> &get_casted_expr () { return main_or_left_expr; }

  std::unique_ptr<Type> &get_type_to_convert_to ()
  {
    return type_to_convert_to;
  }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  TypeCastExpr *clone_expr_impl () const override
  {
    return new TypeCastExpr (*this);
  }

  /* Use covariance to implement clone function as returning this object rather
   * than base */
  TypeCastExpr *clone_expr_without_block_impl () const override
  {
    return new TypeCastExpr (*this);
  }
};

// Binary assignment expression.
class AssignmentExpr : public OperatorExpr
{
  std::unique_ptr<Expr> right_expr;

public:
  std::string as_string () const override;

  // Call OperatorExpr constructor to initialise left_expr
  AssignmentExpr (Analysis::NodeMapping mappings,
		  std::unique_ptr<Expr> value_to_assign_to,
		  std::unique_ptr<Expr> value_to_assign, location_t locus)
    : OperatorExpr (std::move (mappings), std::move (value_to_assign_to),
		    AST::AttrVec (), locus),
      right_expr (std::move (value_to_assign))
  {}
  // outer attributes not allowed

  // Call OperatorExpr constructor in copy constructor, as well as clone
  AssignmentExpr (AssignmentExpr const &other)
    : OperatorExpr (other), right_expr (other.right_expr->clone_expr ())
  {}

  // Overload assignment operator to clone unique_ptr right_expr
  AssignmentExpr &operator= (AssignmentExpr const &other)
  {
    OperatorExpr::operator= (other);
    // main_or_left_expr = other.main_or_left_expr->clone_expr();
    right_expr = other.right_expr->clone_expr ();
    // outer_attrs = other.outer_attrs;

    return *this;
  }

  // move constructors
  AssignmentExpr (AssignmentExpr &&other) = default;
  AssignmentExpr &operator= (AssignmentExpr &&other) = default;

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

  void visit_lhs (HIRFullVisitor &vis) { main_or_left_expr->accept_vis (vis); }
  void visit_rhs (HIRFullVisitor &vis) { right_expr->accept_vis (vis); }

  std::unique_ptr<Expr> &get_lhs () { return main_or_left_expr; }
  std::unique_ptr<Expr> &get_rhs () { return right_expr; }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  AssignmentExpr *clone_expr_impl () const override
  {
    return new AssignmentExpr (*this);
  }

  /* Use covariance to implement clone function as returning this object rather
   * than base */
  AssignmentExpr *clone_expr_without_block_impl () const override
  {
    return new AssignmentExpr (*this);
  }
};

class CompoundAssignmentExpr : public OperatorExpr
{
public:
  using ExprType = ArithmeticOrLogicalOperator;

private:
  // Note: overloading trait specified in comments
  ExprType expr_type;
  std::unique_ptr<Expr> right_expr;

public:
  std::string as_string () const override;

  ExprType get_expr_type () const { return expr_type; }

  // Use pointers in constructor to enable polymorphism
  CompoundAssignmentExpr (Analysis::NodeMapping mappings,
			  std::unique_ptr<Expr> value_to_assign_to,
			  std::unique_ptr<Expr> value_to_assign,
			  ExprType expr_kind, location_t locus)
    : OperatorExpr (std::move (mappings), std::move (value_to_assign_to),
		    AST::AttrVec (), locus),
      expr_type (expr_kind), right_expr (std::move (value_to_assign))
  {}
  // outer attributes not allowed

  // Have clone in copy constructor
  CompoundAssignmentExpr (CompoundAssignmentExpr const &other)
    : OperatorExpr (other), expr_type (other.expr_type),
      right_expr (other.right_expr->clone_expr ())
  {}

  // Overload assignment operator to clone
  CompoundAssignmentExpr &operator= (CompoundAssignmentExpr const &other)
  {
    OperatorExpr::operator= (other);
    // main_or_left_expr = other.main_or_left_expr->clone_expr();
    right_expr = other.right_expr->clone_expr ();
    expr_type = other.expr_type;
    // outer_attrs = other.outer_attrs;

    return *this;
  }

  // move constructors
  CompoundAssignmentExpr (CompoundAssignmentExpr &&other) = default;
  CompoundAssignmentExpr &operator= (CompoundAssignmentExpr &&other) = default;

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

  std::unique_ptr<Expr> &get_lhs () { return main_or_left_expr; }

  std::unique_ptr<Expr> &get_rhs () { return right_expr; }

  void visit_lhs (HIRFullVisitor &vis) { main_or_left_expr->accept_vis (vis); }
  void visit_rhs (HIRFullVisitor &vis) { right_expr->accept_vis (vis); }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  CompoundAssignmentExpr *clone_expr_without_block_impl () const override
  {
    return new CompoundAssignmentExpr (*this);
  }
};

// Expression in parentheses (i.e. like literally just any 3 + (2 * 6))
class GroupedExpr : public ExprWithoutBlock, public WithInnerAttrs
{
  std::unique_ptr<Expr> expr_in_parens;

  location_t locus;

public:
  std::string as_string () const override;

  GroupedExpr (Analysis::NodeMapping mappings,
	       std::unique_ptr<Expr> parenthesised_expr,
	       AST::AttrVec inner_attribs, AST::AttrVec outer_attribs,
	       location_t locus)
    : ExprWithoutBlock (std::move (mappings), std::move (outer_attribs)),
      WithInnerAttrs (std::move (inner_attribs)),
      expr_in_parens (std::move (parenthesised_expr)), locus (locus)
  {}

  // Copy constructor includes clone for expr_in_parens
  GroupedExpr (GroupedExpr const &other)
    : ExprWithoutBlock (other), WithInnerAttrs (other.inner_attrs),
      expr_in_parens (other.expr_in_parens->clone_expr ()), locus (other.locus)
  {}

  // Overloaded assignment operator to clone expr_in_parens
  GroupedExpr &operator= (GroupedExpr const &other)
  {
    ExprWithoutBlock::operator= (other);
    inner_attrs = other.inner_attrs;
    expr_in_parens = other.expr_in_parens->clone_expr ();
    locus = other.locus;
    // outer_attrs = other.outer_attrs;

    return *this;
  }

  // move constructors
  GroupedExpr (GroupedExpr &&other) = default;
  GroupedExpr &operator= (GroupedExpr &&other) = default;

  location_t get_locus () const override final { return locus; }

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

  std::unique_ptr<Expr> &get_expr_in_parens () { return expr_in_parens; }

  ExprType get_expression_type () const override final
  {
    return ExprType::Grouped;
  }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  GroupedExpr *clone_expr_impl () const override
  {
    return new GroupedExpr (*this);
  }

  /* Use covariance to implement clone function as returning this object rather
   * than base */
  GroupedExpr *clone_expr_without_block_impl () const override
  {
    return new GroupedExpr (*this);
  }
};

// Base array initialisation internal element representation thing (abstract)
// aka ArrayElements
class ArrayElems : public FullVisitable
{
public:
  enum ArrayExprType
  {
    VALUES,
    COPIED,
  };

  ArrayElems (Analysis::NodeMapping mappings) : mappings (mappings){};

  virtual ~ArrayElems () {}

  // Unique pointer custom clone ArrayElems function
  std::unique_ptr<ArrayElems> clone_array_elems () const
  {
    return std::unique_ptr<ArrayElems> (clone_array_elems_impl ());
  }

  virtual std::string as_string () const = 0;

  virtual void accept_vis (HIRFullVisitor &vis) = 0;

  virtual ArrayExprType get_array_expr_type () const = 0;

  Analysis::NodeMapping &get_mappings () { return mappings; }

protected:
  // pure virtual clone implementation
  virtual ArrayElems *clone_array_elems_impl () const = 0;

  Analysis::NodeMapping mappings;
};

// Value array elements
class ArrayElemsValues : public ArrayElems
{
  std::vector<std::unique_ptr<Expr> > values;

  // TODO: should this store location data?

public:
  ArrayElemsValues (Analysis::NodeMapping mappings,
		    std::vector<std::unique_ptr<Expr> > elems)
    : ArrayElems (mappings), values (std::move (elems))
  {}

  // copy constructor with vector clone
  ArrayElemsValues (ArrayElemsValues const &other) : ArrayElems (other)
  {
    values.reserve (other.values.size ());
    for (const auto &e : other.values)
      values.push_back (e->clone_expr ());
  }

  // overloaded assignment operator with vector clone
  ArrayElemsValues &operator= (ArrayElemsValues const &other)
  {
    values.reserve (other.values.size ());
    for (const auto &e : other.values)
      values.push_back (e->clone_expr ());

    return *this;
  }

  // move constructors
  ArrayElemsValues (ArrayElemsValues &&other) = default;
  ArrayElemsValues &operator= (ArrayElemsValues &&other) = default;

  std::string as_string () const override;

  void accept_vis (HIRFullVisitor &vis) override;

  size_t get_num_elements () const { return values.size (); }

  std::vector<std::unique_ptr<Expr> > &get_values () { return values; }

  ArrayElems::ArrayExprType get_array_expr_type () const override final
  {
    return ArrayElems::ArrayExprType::VALUES;
  }

protected:
  ArrayElemsValues *clone_array_elems_impl () const override
  {
    return new ArrayElemsValues (*this);
  }
};

// Copied array element and number of copies
class ArrayElemsCopied : public ArrayElems
{
  std::unique_ptr<Expr> elem_to_copy;
  std::unique_ptr<Expr> num_copies;

public:
  // Constructor requires pointers for polymorphism
  ArrayElemsCopied (Analysis::NodeMapping mappings,
		    std::unique_ptr<Expr> copied_elem,
		    std::unique_ptr<Expr> copy_amount)
    : ArrayElems (mappings), elem_to_copy (std::move (copied_elem)),
      num_copies (std::move (copy_amount))
  {}

  // Copy constructor required due to unique_ptr - uses custom clone
  ArrayElemsCopied (ArrayElemsCopied const &other)
    : ArrayElems (other), elem_to_copy (other.elem_to_copy->clone_expr ()),
      num_copies (other.num_copies->clone_expr ())
  {}

  // Overloaded assignment operator for deep copying
  ArrayElemsCopied &operator= (ArrayElemsCopied const &other)
  {
    elem_to_copy = other.elem_to_copy->clone_expr ();
    num_copies = other.num_copies->clone_expr ();

    return *this;
  }

  // move constructors
  ArrayElemsCopied (ArrayElemsCopied &&other) = default;
  ArrayElemsCopied &operator= (ArrayElemsCopied &&other) = default;

  std::string as_string () const override;

  void accept_vis (HIRFullVisitor &vis) override;

  std::unique_ptr<Expr> &get_elem_to_copy () { return elem_to_copy; }

  std::unique_ptr<Expr> &get_num_copies_expr () { return num_copies; }

  ArrayElems::ArrayExprType get_array_expr_type () const override final
  {
    return ArrayElems::ArrayExprType::COPIED;
  }

protected:
  ArrayElemsCopied *clone_array_elems_impl () const override
  {
    return new ArrayElemsCopied (*this);
  }
};

// Array definition-ish expression
class ArrayExpr : public ExprWithoutBlock, public WithInnerAttrs
{
  std::unique_ptr<ArrayElems> internal_elements;

  location_t locus;

public:
  std::string as_string () const override;

  // Returns whether array expr has array elems or if it is just empty.
  bool has_array_elems () const { return internal_elements != nullptr; }

  // Constructor requires ArrayElems pointer
  ArrayExpr (Analysis::NodeMapping mappings,
	     std::unique_ptr<ArrayElems> array_elems,
	     AST::AttrVec inner_attribs, AST::AttrVec outer_attribs,
	     location_t locus)
    : ExprWithoutBlock (std::move (mappings), std::move (outer_attribs)),
      WithInnerAttrs (std::move (inner_attribs)),
      internal_elements (std::move (array_elems)), locus (locus)
  {}

  // Copy constructor requires cloning ArrayElems for polymorphism to hold
  ArrayExpr (ArrayExpr const &other)
    : ExprWithoutBlock (other), WithInnerAttrs (other.inner_attrs),
      locus (other.locus)
  {
    if (other.has_array_elems ())
      internal_elements = other.internal_elements->clone_array_elems ();
  }

  // Overload assignment operator to clone internal_elements
  ArrayExpr &operator= (ArrayExpr const &other)
  {
    ExprWithoutBlock::operator= (other);
    inner_attrs = other.inner_attrs;
    if (other.has_array_elems ())
      internal_elements = other.internal_elements->clone_array_elems ();
    locus = other.locus;
    // outer_attrs = other.outer_attrs;

    return *this;
  }

  // move constructors
  ArrayExpr (ArrayExpr &&other) = default;
  ArrayExpr &operator= (ArrayExpr &&other) = default;

  location_t get_locus () const override final { return locus; }

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

  std::unique_ptr<ArrayElems> &get_internal_elements ()
  {
    return internal_elements;
  };

  ExprType get_expression_type () const override final
  {
    return ExprType::Array;
  }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  ArrayExpr *clone_expr_impl () const override { return new ArrayExpr (*this); }

  /* Use covariance to implement clone function as returning this object rather
   * than base */
  ArrayExpr *clone_expr_without_block_impl () const override
  {
    return new ArrayExpr (*this);
  }
};

class ArrayIndexExpr : public ExprWithoutBlock
{
  std::unique_ptr<Expr> array_expr;
  std::unique_ptr<Expr> index_expr;

  location_t locus;

public:
  std::string as_string () const override;

  ArrayIndexExpr (Analysis::NodeMapping mappings,
		  std::unique_ptr<Expr> array_expr,
		  std::unique_ptr<Expr> array_index_expr,
		  AST::AttrVec outer_attribs, location_t locus)
    : ExprWithoutBlock (std::move (mappings), std::move (outer_attribs)),
      array_expr (std::move (array_expr)),
      index_expr (std::move (array_index_expr)), locus (locus)
  {}

  // Copy constructor requires special cloning due to unique_ptr
  ArrayIndexExpr (ArrayIndexExpr const &other)
    : ExprWithoutBlock (other), array_expr (other.array_expr->clone_expr ()),
      index_expr (other.index_expr->clone_expr ()), locus (other.locus)
  {}

  // Overload assignment operator to clone unique_ptrs
  ArrayIndexExpr &operator= (ArrayIndexExpr const &other)
  {
    ExprWithoutBlock::operator= (other);
    array_expr = other.array_expr->clone_expr ();
    index_expr = other.index_expr->clone_expr ();
    // outer_attrs = other.outer_attrs;
    locus = other.locus;

    return *this;
  }

  // move constructors
  ArrayIndexExpr (ArrayIndexExpr &&other) = default;
  ArrayIndexExpr &operator= (ArrayIndexExpr &&other) = default;

  location_t get_locus () const override final { return locus; }

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

  std::unique_ptr<Expr> &get_array_expr () { return array_expr; }
  std::unique_ptr<Expr> &get_index_expr () { return index_expr; }

  ExprType get_expression_type () const override final
  {
    return ExprType::ArrayIndex;
  }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  ArrayIndexExpr *clone_expr_impl () const override
  {
    return new ArrayIndexExpr (*this);
  }

  /* Use covariance to implement clone function as returning this object rather
   * than base */
  ArrayIndexExpr *clone_expr_without_block_impl () const override
  {
    return new ArrayIndexExpr (*this);
  }
};

// HIR representation of a tuple
class TupleExpr : public ExprWithoutBlock, public WithInnerAttrs
{
  std::vector<std::unique_ptr<Expr> > tuple_elems;
  // replaces (inlined version of) TupleElements

  location_t locus;

public:
  std::string as_string () const override;

  TupleExpr (Analysis::NodeMapping mappings,
	     std::vector<std::unique_ptr<Expr> > tuple_elements,
	     AST::AttrVec inner_attribs, AST::AttrVec outer_attribs,
	     location_t locus)
    : ExprWithoutBlock (std::move (mappings), std::move (outer_attribs)),
      WithInnerAttrs (std::move (inner_attribs)),
      tuple_elems (std::move (tuple_elements)), locus (locus)
  {}

  // copy constructor with vector clone
  TupleExpr (TupleExpr const &other)
    : ExprWithoutBlock (other), WithInnerAttrs (other.inner_attrs),
      locus (other.locus)
  {
    tuple_elems.reserve (other.tuple_elems.size ());
    for (const auto &e : other.tuple_elems)
      tuple_elems.push_back (e->clone_expr ());
  }

  // overloaded assignment operator to vector clone
  TupleExpr &operator= (TupleExpr const &other)
  {
    ExprWithoutBlock::operator= (other);
    inner_attrs = other.inner_attrs;
    locus = other.locus;

    tuple_elems.reserve (other.tuple_elems.size ());
    for (const auto &e : other.tuple_elems)
      tuple_elems.push_back (e->clone_expr ());

    return *this;
  }

  // move constructors
  TupleExpr (TupleExpr &&other) = default;
  TupleExpr &operator= (TupleExpr &&other) = default;

  /* Note: syntactically, can disambiguate single-element tuple from parens with
   * comma, i.e. (0,) rather than (0) */

  location_t get_locus () const override final { return locus; }

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

  const std::vector<std::unique_ptr<Expr> > &get_tuple_elems () const
  {
    return tuple_elems;
  }
  std::vector<std::unique_ptr<Expr> > &get_tuple_elems ()
  {
    return tuple_elems;
  }

  bool is_unit () const { return tuple_elems.size () == 0; }

  ExprType get_expression_type () const override final
  {
    return ExprType::Tuple;
  }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  TupleExpr *clone_expr_impl () const override { return new TupleExpr (*this); }

  /* Use covariance to implement clone function as returning this object rather
   * than base */
  TupleExpr *clone_expr_without_block_impl () const override
  {
    return new TupleExpr (*this);
  }
};

class TupleIndexExpr : public ExprWithoutBlock
{
  std::unique_ptr<Expr> tuple_expr;
  TupleIndex tuple_index;
  location_t locus;

public:
  std::string as_string () const override;

  TupleIndex get_tuple_index () const { return tuple_index; }

  TupleIndexExpr (Analysis::NodeMapping mappings,
		  std::unique_ptr<Expr> tuple_expr, TupleIndex index,
		  AST::AttrVec outer_attribs, location_t locus)
    : ExprWithoutBlock (std::move (mappings), std::move (outer_attribs)),
      tuple_expr (std::move (tuple_expr)), tuple_index (index), locus (locus)
  {}

  // Copy constructor requires a clone for tuple_expr
  TupleIndexExpr (TupleIndexExpr const &other)
    : ExprWithoutBlock (other), tuple_expr (other.tuple_expr->clone_expr ()),
      tuple_index (other.tuple_index), locus (other.locus)
  {}

  // Overload assignment operator in order to clone
  TupleIndexExpr &operator= (TupleIndexExpr const &other)
  {
    ExprWithoutBlock::operator= (other);
    tuple_expr = other.tuple_expr->clone_expr ();
    tuple_index = other.tuple_index;
    locus = other.locus;
    // outer_attrs = other.outer_attrs;

    return *this;
  }

  // move constructors
  TupleIndexExpr (TupleIndexExpr &&other) = default;
  TupleIndexExpr &operator= (TupleIndexExpr &&other) = default;

  location_t get_locus () const override final { return locus; }

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

  std::unique_ptr<Expr> &get_tuple_expr () { return tuple_expr; }

  ExprType get_expression_type () const override final
  {
    return ExprType::TupleIdx;
  }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  TupleIndexExpr *clone_expr_impl () const override
  {
    return new TupleIndexExpr (*this);
  }

  /* Use covariance to implement clone function as returning this object rather
   * than base */
  TupleIndexExpr *clone_expr_without_block_impl () const override
  {
    return new TupleIndexExpr (*this);
  }
};

// Base struct/tuple/union value creator HIR node (abstract)
class StructExpr : public ExprWithoutBlock
{
protected:
  PathInExpression struct_name;

  // Protected constructor to allow initialising struct_name
  StructExpr (Analysis::NodeMapping mappings, PathInExpression struct_path,
	      AST::AttrVec outer_attribs)
    : ExprWithoutBlock (std::move (mappings), std::move (outer_attribs)),
      struct_name (std::move (struct_path))
  {}

public:
  PathInExpression &get_struct_name () { return struct_name; }

  std::string as_string () const override;

  ExprType get_expression_type () const override final
  {
    return ExprType::Struct;
  }
};

// Actual HIR node of the struct creator (with no fields). Not abstract!
class StructExprStruct : public StructExpr, public WithInnerAttrs
{
  location_t locus;

public:
  std::string as_string () const override;

  // Constructor has to call protected constructor of base class
  StructExprStruct (Analysis::NodeMapping mappings,
		    PathInExpression struct_path, AST::AttrVec inner_attribs,
		    AST::AttrVec outer_attribs, location_t locus)
    : StructExpr (std::move (mappings), std::move (struct_path),
		  std::move (outer_attribs)),
      WithInnerAttrs (std::move (inner_attribs)), locus (locus)
  {}

  location_t get_locus () const override final { return locus; }

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  StructExprStruct *clone_expr_impl () const override
  {
    return new StructExprStruct (*this);
  }

  /* Use covariance to implement clone function as returning this object rather
   * than base */
  StructExprStruct *clone_expr_without_block_impl () const override
  {
    return new StructExprStruct (*this);
  }
};

/* HIR node representing expression used to fill a struct's fields from another
 * struct */
struct StructBase
{
public:
  std::unique_ptr<Expr> base_struct;

  // TODO: should this store location data?
  StructBase (std::unique_ptr<Expr> base_struct_ptr)
    : base_struct (std::move (base_struct_ptr))
  {}

  // Copy constructor requires clone
  StructBase (StructBase const &other)
  {
    /* HACK: gets around base_struct pointer being null (e.g. if no struct base
     * exists) */
    if (other.base_struct != nullptr)
      other.base_struct->clone_expr ();
  }

  // Destructor
  ~StructBase () = default;

  // Overload assignment operator to clone base_struct
  StructBase &operator= (StructBase const &other)
  {
    base_struct = other.base_struct->clone_expr ();

    return *this;
  }

  // move constructors
  StructBase (StructBase &&other) = default;
  StructBase &operator= (StructBase &&other) = default;

  // Returns a null expr-ed StructBase - error state
  static StructBase error () { return StructBase (nullptr); }

  // Returns whether StructBase is in error state
  bool is_invalid () const { return base_struct == nullptr; }

  std::string as_string () const;

  Expr *get_base () { return base_struct.get (); }
};

/* Base HIR node for a single struct expression field (in struct instance
 * creation) - abstract */
class StructExprField : public FullVisitable
{
public:
  enum StructExprFieldKind
  {
    IDENTIFIER_VALUE,
    IDENTIFIER,
    INDEX_VALUE,
  };

  virtual ~StructExprField () {}

  // Unique pointer custom clone function
  std::unique_ptr<StructExprField> clone_struct_expr_field () const
  {
    return std::unique_ptr<StructExprField> (clone_struct_expr_field_impl ());
  }

  virtual std::string as_string () const = 0;

  virtual void accept_vis (HIRFullVisitor &vis) = 0;
  virtual void accept_vis (HIRExpressionVisitor &vis) = 0;

  Analysis::NodeMapping &get_mappings () { return mappings; }

  location_t get_locus () { return locus; }

  virtual StructExprFieldKind get_kind () const = 0;

protected:
  // pure virtual clone implementation
  virtual StructExprField *clone_struct_expr_field_impl () const = 0;

  StructExprField (Analysis::NodeMapping mapping, location_t locus)
    : mappings (mapping), locus (locus)
  {}

  Analysis::NodeMapping mappings;
  location_t locus;
};

// Identifier-only variant of StructExprField HIR node
class StructExprFieldIdentifier : public StructExprField
{
private:
  Identifier field_name;

  // TODO: should this store location data?
public:
  StructExprFieldIdentifier (Analysis::NodeMapping mapping,
			     Identifier field_identifier, location_t locus)
    : StructExprField (mapping, locus),
      field_name (std::move (field_identifier))
  {}

  std::string as_string () const override { return field_name.as_string (); }

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

  Identifier get_field_name () const { return field_name; }

  StructExprFieldKind get_kind () const override
  {
    return StructExprFieldKind::IDENTIFIER;
  }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  StructExprFieldIdentifier *clone_struct_expr_field_impl () const override
  {
    return new StructExprFieldIdentifier (*this);
  }
};

/* Base HIR node for a single struct expression field with an assigned value -
 * abstract */
class StructExprFieldWithVal : public StructExprField
{
  std::unique_ptr<Expr> value;

protected:
  StructExprFieldWithVal (Analysis::NodeMapping mapping,
			  std::unique_ptr<Expr> field_value, location_t locus)
    : StructExprField (mapping, locus), value (std::move (field_value))
  {}

  // Copy constructor requires clone
  StructExprFieldWithVal (StructExprFieldWithVal const &other)
    : StructExprField (other.mappings, other.locus),
      value (other.value->clone_expr ())
  {}

  // Overload assignment operator to clone unique_ptr
  StructExprFieldWithVal &operator= (StructExprFieldWithVal const &other)
  {
    value = other.value->clone_expr ();
    mappings = other.mappings;
    locus = other.locus;

    return *this;
  }

  // move constructors
  StructExprFieldWithVal (StructExprFieldWithVal &&other) = default;
  StructExprFieldWithVal &operator= (StructExprFieldWithVal &&other) = default;

public:
  std::string as_string () const override;

  std::unique_ptr<Expr> &get_value () { return value; }
};

// Identifier and value variant of StructExprField HIR node
class StructExprFieldIdentifierValue : public StructExprFieldWithVal
{
public:
  Identifier field_name;

  // TODO: should this store location data?

  StructExprFieldIdentifierValue (Analysis::NodeMapping mapping,
				  Identifier field_identifier,
				  std::unique_ptr<Expr> field_value,
				  location_t locus)
    : StructExprFieldWithVal (mapping, std::move (field_value), locus),
      field_name (std::move (field_identifier))
  {}

  std::string as_string () const override;

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

  Identifier get_field_name () const { return field_name; }

  StructExprFieldKind get_kind () const override
  {
    return StructExprFieldKind::IDENTIFIER_VALUE;
  }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  StructExprFieldIdentifierValue *clone_struct_expr_field_impl () const override
  {
    return new StructExprFieldIdentifierValue (*this);
  }
};

// Tuple index and value variant of StructExprField HIR node
class StructExprFieldIndexValue : public StructExprFieldWithVal
{
public:
  TupleIndex index;

  // TODO: should this store location data?

  StructExprFieldIndexValue (Analysis::NodeMapping mapping,
			     TupleIndex tuple_index,
			     std::unique_ptr<Expr> field_value,
			     location_t locus)
    : StructExprFieldWithVal (mapping, std::move (field_value), locus),
      index (tuple_index)
  {}

  std::string as_string () const override;

  TupleIndex get_tuple_index () const { return index; };

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

  StructExprFieldKind get_kind () const override
  {
    return StructExprFieldKind::INDEX_VALUE;
  }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  StructExprFieldIndexValue *clone_struct_expr_field_impl () const override
  {
    return new StructExprFieldIndexValue (*this);
  }
};

// HIR node of a struct creator with fields
class StructExprStructFields : public StructExprStruct
{
public:
  // std::vector<StructExprField> fields;
  std::vector<std::unique_ptr<StructExprField> > fields;

  // bool has_struct_base;
  // FIXME make unique_ptr
  StructBase *struct_base;

  // For unions there is just one field, the index
  // is set when type checking
  int union_index = -1;

  std::string as_string () const override;

  bool has_struct_base () const { return struct_base != nullptr; }

  // Constructor for StructExprStructFields when no struct base is used
  StructExprStructFields (
    Analysis::NodeMapping mappings, PathInExpression struct_path,
    std::vector<std::unique_ptr<StructExprField> > expr_fields,
    location_t locus, StructBase *base_struct,
    AST::AttrVec inner_attribs = AST::AttrVec (),
    AST::AttrVec outer_attribs = AST::AttrVec ())
    : StructExprStruct (std::move (mappings), std::move (struct_path),
			std::move (inner_attribs), std::move (outer_attribs),
			locus),
      fields (std::move (expr_fields)), struct_base (base_struct)
  {}

  // copy constructor with vector clone
  StructExprStructFields (StructExprStructFields const &other)
    : StructExprStruct (other), struct_base (other.struct_base),
      union_index (other.union_index)
  {
    fields.reserve (other.fields.size ());
    for (const auto &e : other.fields)
      fields.push_back (e->clone_struct_expr_field ());
  }

  // overloaded assignment operator with vector clone
  StructExprStructFields &operator= (StructExprStructFields const &other)
  {
    StructExprStruct::operator= (other);
    struct_base = other.struct_base;
    union_index = other.union_index;

    fields.reserve (other.fields.size ());
    for (const auto &e : other.fields)
      fields.push_back (e->clone_struct_expr_field ());

    return *this;
  }

  // move constructors
  StructExprStructFields (StructExprStructFields &&other) = default;
  StructExprStructFields &operator= (StructExprStructFields &&other) = default;

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

  std::vector<std::unique_ptr<StructExprField> > &get_fields ()
  {
    return fields;
  };

  const std::vector<std::unique_ptr<StructExprField> > &get_fields () const
  {
    return fields;
  };

  StructBase *get_struct_base () { return struct_base; }

  void set_fields_as_owner (
    std::vector<std::unique_ptr<StructExprField> > new_fields)
  {
    fields = std::move (new_fields);
  }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  StructExprStructFields *clone_expr_impl () const override
  {
    return new StructExprStructFields (*this);
  }

  /* Use covariance to implement clone function as returning this object rather
   * than base */
  StructExprStructFields *clone_expr_without_block_impl () const override
  {
    return new StructExprStructFields (*this);
  }
};

// HIR node of the functional update struct creator
class StructExprStructBase : public StructExprStruct
{
  StructBase struct_base;

public:
  std::string as_string () const override;

  /*inline StructBase get_struct_base() const {
      return struct_base;
  }*/

  StructExprStructBase (Analysis::NodeMapping mappings,
			PathInExpression struct_path, StructBase base_struct,
			AST::AttrVec inner_attribs, AST::AttrVec outer_attribs,
			location_t locus)
    : StructExprStruct (std::move (mappings), std::move (struct_path),
			std::move (inner_attribs), std::move (outer_attribs),
			locus),
      struct_base (std::move (base_struct))
  {}

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

  StructBase *get_struct_base () { return &struct_base; }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  StructExprStructBase *clone_expr_impl () const override
  {
    return new StructExprStructBase (*this);
  }

  /* Use covariance to implement clone function as returning this object rather
   * than base */
  StructExprStructBase *clone_expr_without_block_impl () const override
  {
    return new StructExprStructBase (*this);
  }
};

// Function call expression HIR node
class CallExpr : public ExprWithoutBlock
{
  std::unique_ptr<Expr> function;
  std::vector<std::unique_ptr<Expr> > params;
  location_t locus;

public:
  std::string as_string () const override;

  CallExpr (Analysis::NodeMapping mappings, std::unique_ptr<Expr> function_expr,
	    std::vector<std::unique_ptr<Expr> > function_params,
	    AST::AttrVec outer_attribs, location_t locus)
    : ExprWithoutBlock (std::move (mappings), std::move (outer_attribs)),
      function (std::move (function_expr)),
      params (std::move (function_params)), locus (locus)
  {}

  // copy constructor requires clone
  CallExpr (CallExpr const &other)
    : ExprWithoutBlock (other), function (other.function->clone_expr ()),
      locus (other.locus)
  /*, params(other.params),*/ {
    params.reserve (other.params.size ());
    for (const auto &e : other.params)
      params.push_back (e->clone_expr ());
  }

  // Overload assignment operator to clone
  CallExpr &operator= (CallExpr const &other)
  {
    ExprWithoutBlock::operator= (other);
    function = other.function->clone_expr ();
    locus = other.locus;
    // params = other.params;
    // outer_attrs = other.outer_attrs;

    params.reserve (other.params.size ());
    for (const auto &e : other.params)
      params.push_back (e->clone_expr ());

    return *this;
  }

  // move constructors
  CallExpr (CallExpr &&other) = default;
  CallExpr &operator= (CallExpr &&other) = default;

  // Returns whether function call has parameters.
  bool has_params () const { return !params.empty (); }

  location_t get_locus () const override final { return locus; }

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

  std::unique_ptr<Expr> &get_fnexpr () { return function; }

  size_t num_params () const { return params.size (); }

  std::vector<std::unique_ptr<Expr> > &get_arguments () { return params; }

  const std::vector<std::unique_ptr<Expr> > &get_arguments () const
  {
    return params;
  }

  ExprType get_expression_type () const override final
  {
    return ExprType::Call;
  }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  CallExpr *clone_expr_impl () const override { return new CallExpr (*this); }

  /* Use covariance to implement clone function as returning this object rather
   * than base */
  CallExpr *clone_expr_without_block_impl () const override
  {
    return new CallExpr (*this);
  }
};

// Method call expression HIR node
class MethodCallExpr : public ExprWithoutBlock
{
  std::unique_ptr<Expr> receiver;
  PathExprSegment method_name;
  std::vector<std::unique_ptr<Expr> > params;
  location_t locus;

public:
  std::string as_string () const override;

  MethodCallExpr (Analysis::NodeMapping mappings,
		  std::unique_ptr<Expr> call_receiver,
		  PathExprSegment method_path,
		  std::vector<std::unique_ptr<Expr> > method_params,
		  AST::AttrVec outer_attribs, location_t locus)
    : ExprWithoutBlock (std::move (mappings), std::move (outer_attribs)),
      receiver (std::move (call_receiver)),
      method_name (std::move (method_path)), params (std::move (method_params)),
      locus (locus)
  {}

  // copy constructor required due to cloning
  MethodCallExpr (MethodCallExpr const &other)
    : ExprWithoutBlock (other), receiver (other.receiver->clone_expr ()),
      method_name (other.method_name), locus (other.locus)
  /*, params(other.params),*/ {
    params.reserve (other.params.size ());
    for (const auto &e : other.params)
      params.push_back (e->clone_expr ());
  }

  // Overload assignment operator to clone receiver object
  MethodCallExpr &operator= (MethodCallExpr const &other)
  {
    ExprWithoutBlock::operator= (other);
    receiver = other.receiver->clone_expr ();
    method_name = other.method_name;
    locus = other.locus;
    // params = other.params;
    // outer_attrs = other.outer_attrs;

    params.reserve (other.params.size ());
    for (const auto &e : other.params)
      params.push_back (e->clone_expr ());

    return *this;
  }

  // move constructors
  MethodCallExpr (MethodCallExpr &&other) = default;
  MethodCallExpr &operator= (MethodCallExpr &&other) = default;

  location_t get_locus () const override final { return locus; }

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

  std::unique_ptr<Expr> &get_receiver () { return receiver; }

  PathExprSegment &get_method_name () { return method_name; };
  const PathExprSegment &get_method_name () const { return method_name; };

  bool has_params () const { return !params.empty (); }
  size_t num_params () const { return params.size (); }

  std::vector<std::unique_ptr<Expr> > &get_arguments () { return params; }

  const std::vector<std::unique_ptr<Expr> > &get_arguments () const
  {
    return params;
  }

  ExprType get_expression_type () const override final
  {
    return ExprType::MethodCall;
  }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  MethodCallExpr *clone_expr_impl () const override
  {
    return new MethodCallExpr (*this);
  }

  /* Use covariance to implement clone function as returning this object rather
   * than base */
  MethodCallExpr *clone_expr_without_block_impl () const override
  {
    return new MethodCallExpr (*this);
  }
};

// aka FieldExpression
// Struct or union field access expression HIR node
class FieldAccessExpr : public ExprWithoutBlock
{
  std::unique_ptr<Expr> receiver;
  Identifier field;

  location_t locus;

public:
  std::string as_string () const override;

  FieldAccessExpr (Analysis::NodeMapping mappings,
		   std::unique_ptr<Expr> field_access_receiver,
		   Identifier field_name, AST::AttrVec outer_attribs,
		   location_t locus)
    : ExprWithoutBlock (std::move (mappings), std::move (outer_attribs)),
      receiver (std::move (field_access_receiver)),
      field (std::move (field_name)), locus (locus)
  {}

  // Copy constructor required due to unique_ptr cloning
  FieldAccessExpr (FieldAccessExpr const &other)
    : ExprWithoutBlock (other), receiver (other.receiver->clone_expr ()),
      field (other.field), locus (other.locus)
  {}

  // Overload assignment operator to clone unique_ptr
  FieldAccessExpr &operator= (FieldAccessExpr const &other)
  {
    ExprWithoutBlock::operator= (other);
    receiver = other.receiver->clone_expr ();
    field = other.field;
    locus = other.locus;
    // outer_attrs = other.outer_attrs;

    return *this;
  }

  // move constructors
  FieldAccessExpr (FieldAccessExpr &&other) = default;
  FieldAccessExpr &operator= (FieldAccessExpr &&other) = default;

  location_t get_locus () const override final { return locus; }

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

  std::unique_ptr<Expr> &get_receiver_expr () { return receiver; }

  Identifier get_field_name () const { return field; }

  ExprType get_expression_type () const override final
  {
    return ExprType::FieldAccess;
  }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  FieldAccessExpr *clone_expr_impl () const override
  {
    return new FieldAccessExpr (*this);
  }

  /* Use covariance to implement clone function as returning this object rather
   * than base */
  FieldAccessExpr *clone_expr_without_block_impl () const override
  {
    return new FieldAccessExpr (*this);
  }
};

// Closure parameter data structure
struct ClosureParam
{
private:
  std::vector<AST::Attribute> outer_attrs;
  std::unique_ptr<Pattern> pattern;
  std::unique_ptr<Type> type;
  location_t locus;

public:
  // Returns whether the type of the parameter has been given.
  bool has_type_given () const { return type != nullptr; }

  // Constructor for closure parameter
  ClosureParam (std::unique_ptr<Pattern> param_pattern, location_t locus,
		std::unique_ptr<Type> param_type = nullptr,
		std::vector<AST::Attribute> outer_attrs = {})
    : outer_attrs (std::move (outer_attrs)),
      pattern (std::move (param_pattern)), type (std::move (param_type)),
      locus (locus)
  {}

  // Copy constructor required due to cloning as a result of unique_ptrs
  ClosureParam (ClosureParam const &other)
    : pattern (other.pattern->clone_pattern ())
  {
    // guard to protect from null pointer dereference
    if (other.pattern != nullptr)
      pattern = other.pattern->clone_pattern ();
    if (other.type != nullptr)
      type = other.type->clone_type ();
  }

  ~ClosureParam () = default;

  // Assignment operator must be overloaded to clone as well
  ClosureParam &operator= (ClosureParam const &other)
  {
    outer_attrs = other.outer_attrs;

    // guard to protect from null pointer dereference
    if (other.pattern != nullptr)
      pattern = other.pattern->clone_pattern ();
    else
      pattern = nullptr;
    if (other.type != nullptr)
      type = other.type->clone_type ();
    else
      type = nullptr;

    return *this;
  }

  // move constructors
  ClosureParam (ClosureParam &&other) = default;
  ClosureParam &operator= (ClosureParam &&other) = default;

  std::string as_string () const;

  const std::vector<AST::Attribute> &get_outer_attrs () const
  {
    return outer_attrs;
  }
  std::vector<AST::Attribute> &get_outer_attrs () { return outer_attrs; }

  std::unique_ptr<Pattern> &get_pattern () { return pattern; }

  std::unique_ptr<Type> &get_type () { return type; }

  location_t get_locus () const { return locus; }
};

// Base closure definition expression HIR node - abstract
class ClosureExpr : public ExprWithoutBlock
{
private:
  bool has_move;
  std::vector<ClosureParam> params;
  location_t locus;
  std::unique_ptr<Type> return_type;
  std::unique_ptr<Expr> expr;

public:
  ClosureExpr (Analysis::NodeMapping mappings,
	       std::vector<ClosureParam> closure_params,
	       std::unique_ptr<Type> closure_return_type,
	       std::unique_ptr<Expr> closure_expr, bool has_move,
	       AST::AttrVec outer_attribs, location_t locus)
    : ExprWithoutBlock (std::move (mappings), std::move (outer_attribs)),
      has_move (has_move), params (std::move (closure_params)), locus (locus),
      return_type (std::move (closure_return_type)),
      expr (std::move (closure_expr))
  {}

  // Copy constructor requires cloning
  ClosureExpr (ClosureExpr const &other)
    : ExprWithoutBlock (other.get_mappings (), other.get_outer_attrs ())
  {
    return_type
      = other.has_return_type () ? other.return_type->clone_type () : nullptr;
    expr = other.expr->clone_expr ();
    params = other.params;
    has_move = other.has_move;
  }

  // Overload assignment operator to clone unique_ptrs
  ClosureExpr &operator= (ClosureExpr const &other)
  {
    mappings = other.mappings;
    return_type
      = other.has_return_type () ? other.return_type->clone_type () : nullptr;
    expr = other.expr->clone_expr ();
    params = other.params;
    has_move = other.has_move;

    return *this;
  }

  // move constructors
  ClosureExpr (ClosureExpr &&other) = default;
  ClosureExpr &operator= (ClosureExpr &&other) = default;

  std::string as_string () const override;

  location_t get_locus () const override final { return locus; }

  ExprType get_expression_type () const override final
  {
    return ExprType::Closure;
  }

  bool get_has_move () const { return has_move; }

  bool has_return_type () const { return return_type != nullptr; }

  std::unique_ptr<Type> &get_return_type () { return return_type; };
  std::unique_ptr<Expr> &get_expr () { return expr; }

  bool has_params () const { return !params.empty (); }
  std::vector<ClosureParam> &get_params () { return params; }

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  ClosureExpr *clone_expr_impl () const override
  {
    return new ClosureExpr (*this);
  }

  /* Use covariance to implement clone function as returning this object rather
   * than base */
  ClosureExpr *clone_expr_without_block_impl () const override
  {
    return new ClosureExpr (*this);
  }
};

// A block HIR node
class BlockExpr : public ExprWithBlock, public WithInnerAttrs
{
  // FIXME this should be private + get/set
public:
  std::vector<std::unique_ptr<Stmt> > statements;
  std::unique_ptr<Expr> expr;
  bool tail_reachable;
  LoopLabel label;
  location_t start_locus;
  location_t end_locus;

  std::string as_string () const override;

  AST::AttrVec get_inner_attrs () const { return inner_attrs; }

  // Returns whether the block contains statements.
  bool has_statements () const { return !statements.empty (); }

  // Returns whether the block contains an expression
  bool has_expr () const { return expr != nullptr; }

  bool is_tail_reachable () const { return tail_reachable; }

  BlockExpr (Analysis::NodeMapping mappings,
	     std::vector<std::unique_ptr<Stmt> > block_statements,
	     std::unique_ptr<Expr> block_expr, bool tail_reachable,
	     AST::AttrVec inner_attribs, AST::AttrVec outer_attribs,
	     LoopLabel label, location_t start_locus, location_t end_locus)
    : ExprWithBlock (std::move (mappings), std::move (outer_attribs)),
      WithInnerAttrs (std::move (inner_attribs)),
      statements (std::move (block_statements)), expr (std::move (block_expr)),
      tail_reachable (tail_reachable), label (std::move (label)),
      start_locus (start_locus), end_locus (end_locus)
  {}

  // Copy constructor with clone
  BlockExpr (BlockExpr const &other)
    : ExprWithBlock (other), /*statements(other.statements),*/
      WithInnerAttrs (other.inner_attrs), label (other.label),
      start_locus (other.start_locus), end_locus (other.end_locus)
  {
    // guard to protect from null pointer dereference
    if (other.expr != nullptr)
      expr = other.expr->clone_expr ();

    statements.reserve (other.statements.size ());
    for (const auto &e : other.statements)
      statements.push_back (e->clone_stmt ());
  }

  // Overloaded assignment operator to clone pointer
  BlockExpr &operator= (BlockExpr const &other)
  {
    ExprWithBlock::operator= (other);
    // statements = other.statements;
    expr = other.expr->clone_expr ();
    inner_attrs = other.inner_attrs;
    start_locus = other.end_locus;
    end_locus = other.end_locus;
    // outer_attrs = other.outer_attrs;

    statements.reserve (other.statements.size ());
    for (const auto &e : other.statements)
      statements.push_back (e->clone_stmt ());

    return *this;
  }

  // move constructors
  BlockExpr (BlockExpr &&other) = default;
  BlockExpr &operator= (BlockExpr &&other) = default;

  // Unique pointer custom clone function
  std::unique_ptr<BlockExpr> clone_block_expr () const
  {
    return std::unique_ptr<BlockExpr> (clone_block_expr_impl ());
  }

  location_t get_locus () const override final { return start_locus; }

  location_t get_start_locus () const { return start_locus; }

  location_t get_end_locus () const { return end_locus; }

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

  bool is_final_stmt (Stmt *stmt) { return statements.back ().get () == stmt; }

  std::unique_ptr<Expr> &get_final_expr () { return expr; }

  std::vector<std::unique_ptr<Stmt> > &get_statements () { return statements; }

  ExprType get_expression_type () const final override
  {
    return ExprType::Block;
  }

  bool has_label () const { return !label.is_error (); }
  LoopLabel &get_label () { return label; }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  BlockExpr *clone_expr_impl () const override
  {
    return clone_block_expr_impl ();
  }

  /* Use covariance to implement clone function as returning this object rather
   * than base */
  BlockExpr *clone_expr_with_block_impl () const override
  {
    return clone_block_expr_impl ();
  }

  /* This is the base method as not an abstract class - not virtual but could be
   * in future if required. */
  /*virtual*/ BlockExpr *clone_block_expr_impl () const
  {
    return new BlockExpr (*this);
  }
};

// HIR node representing continue expression within loops
class ContinueExpr : public ExprWithoutBlock
{
  Lifetime label;
  location_t locus;

public:
  std::string as_string () const override;

  // Returns true if the continue expr has a label.
  bool has_label () const { return !label.is_error (); }

  // Constructor for a ContinueExpr with a label.
  ContinueExpr (Analysis::NodeMapping mappings, location_t locus,
		Lifetime label, AST::AttrVec outer_attribs = AST::AttrVec ())
    : ExprWithoutBlock (std::move (mappings), std::move (outer_attribs)),
      label (std::move (label)), locus (locus)
  {}

  location_t get_locus () const override final { return locus; }

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

  Lifetime &get_label () { return label; }

  ExprType get_expression_type () const final override
  {
    return ExprType::Continue;
  }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  ContinueExpr *clone_expr_impl () const override
  {
    return new ContinueExpr (*this);
  }

  /* Use covariance to implement clone function as returning this object rather
   * than base */
  ContinueExpr *clone_expr_without_block_impl () const override
  {
    return new ContinueExpr (*this);
  }
};

// HIR node representing break expression within loops
class BreakExpr : public ExprWithoutBlock
{
  // bool has_label;
  Lifetime label;

  // bool has_break_expr;
  std::unique_ptr<Expr> break_expr;

  location_t locus;

public:
  std::string as_string () const override;

  // Returns whether the break expression has a label or not.
  bool has_label () const { return !label.is_error (); }

  /* Returns whether the break expression has an expression used in the break or
   * not. */
  bool has_break_expr () const { return break_expr != nullptr; }

  // Constructor for a break expression
  BreakExpr (Analysis::NodeMapping mappings, location_t locus,
	     Lifetime break_label,
	     std::unique_ptr<Expr> expr_in_break = nullptr,
	     AST::AttrVec outer_attribs = AST::AttrVec ())
    : ExprWithoutBlock (std::move (mappings), std::move (outer_attribs)),
      label (std::move (break_label)), break_expr (std::move (expr_in_break)),
      locus (locus)
  {}

  // Copy constructor defined to use clone for unique pointer
  BreakExpr (BreakExpr const &other)
    : ExprWithoutBlock (other), label (other.label), locus (other.locus)
  {
    // guard to protect from null pointer dereference
    if (other.break_expr != nullptr)
      break_expr = other.break_expr->clone_expr ();
  }

  // Overload assignment operator to clone unique pointer
  BreakExpr &operator= (BreakExpr const &other)
  {
    ExprWithoutBlock::operator= (other);
    label = other.label;
    break_expr = other.break_expr->clone_expr ();
    locus = other.locus;
    // outer_attrs = other.outer_attrs;

    return *this;
  }

  // move constructors
  BreakExpr (BreakExpr &&other) = default;
  BreakExpr &operator= (BreakExpr &&other) = default;

  location_t get_locus () const override final { return locus; }

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

  Lifetime &get_label () { return label; }

  std::unique_ptr<Expr> &get_expr () { return break_expr; }

  ExprType get_expression_type () const override final
  {
    return ExprType::Break;
  }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  BreakExpr *clone_expr_impl () const override { return new BreakExpr (*this); }

  /* Use covariance to implement clone function as returning this object rather
   * than base */
  BreakExpr *clone_expr_without_block_impl () const override
  {
    return new BreakExpr (*this);
  }
};

// Base range expression HIR node object - abstract
class RangeExpr : public ExprWithoutBlock
{
  location_t locus;

protected:
  // outer attributes not allowed before range expressions
  RangeExpr (Analysis::NodeMapping mappings, location_t locus)
    : ExprWithoutBlock (std::move (mappings), AST::AttrVec ()), locus (locus)
  {}

public:
  location_t get_locus () const override final { return locus; }

  ExprType get_expression_type () const override final
  {
    return ExprType::Range;
  }
};

// Range from (inclusive) and to (exclusive) expression HIR node object
// aka RangeExpr; constructs a std::ops::Range object
class RangeFromToExpr : public RangeExpr
{
  std::unique_ptr<Expr> from;
  std::unique_ptr<Expr> to;

public:
  std::string as_string () const override;

  RangeFromToExpr (Analysis::NodeMapping mappings,
		   std::unique_ptr<Expr> range_from,
		   std::unique_ptr<Expr> range_to, location_t locus)
    : RangeExpr (std::move (mappings), locus), from (std::move (range_from)),
      to (std::move (range_to))
  {}

  // Copy constructor with cloning
  RangeFromToExpr (RangeFromToExpr const &other)
    : RangeExpr (other), from (other.from->clone_expr ()),
      to (other.to->clone_expr ())
  {}

  // Overload assignment operator to clone unique pointers
  RangeFromToExpr &operator= (RangeFromToExpr const &other)
  {
    RangeExpr::operator= (other);
    from = other.from->clone_expr ();
    to = other.to->clone_expr ();

    return *this;
  }

  // move constructors
  RangeFromToExpr (RangeFromToExpr &&other) = default;
  RangeFromToExpr &operator= (RangeFromToExpr &&other) = default;

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

  std::unique_ptr<Expr> &get_from_expr () { return from; }
  std::unique_ptr<Expr> &get_to_expr () { return to; }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  RangeFromToExpr *clone_expr_impl () const override
  {
    return new RangeFromToExpr (*this);
  }

  /* Use covariance to implement clone function as returning this object rather
   * than base */
  RangeFromToExpr *clone_expr_without_block_impl () const override
  {
    return new RangeFromToExpr (*this);
  }
};

// Range from (inclusive) expression HIR node object
// constructs a std::ops::RangeFrom object
class RangeFromExpr : public RangeExpr
{
  std::unique_ptr<Expr> from;

public:
  std::string as_string () const override;

  RangeFromExpr (Analysis::NodeMapping mappings,
		 std::unique_ptr<Expr> range_from, location_t locus)
    : RangeExpr (std::move (mappings), locus), from (std::move (range_from))
  {}

  // Copy constructor with clone
  RangeFromExpr (RangeFromExpr const &other)
    : RangeExpr (other), from (other.from->clone_expr ())
  {}

  // Overload assignment operator to clone unique_ptr
  RangeFromExpr &operator= (RangeFromExpr const &other)
  {
    RangeExpr::operator= (other);
    from = other.from->clone_expr ();

    return *this;
  }

  // move constructors
  RangeFromExpr (RangeFromExpr &&other) = default;
  RangeFromExpr &operator= (RangeFromExpr &&other) = default;

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

  std::unique_ptr<Expr> &get_from_expr () { return from; }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  RangeFromExpr *clone_expr_impl () const override
  {
    return new RangeFromExpr (*this);
  }

  /* Use covariance to implement clone function as returning this object rather
   * than base */
  RangeFromExpr *clone_expr_without_block_impl () const override
  {
    return new RangeFromExpr (*this);
  }
};

// Range to (exclusive) expression HIR node object
// constructs a std::ops::RangeTo object
class RangeToExpr : public RangeExpr
{
  std::unique_ptr<Expr> to;

public:
  std::string as_string () const override;

  // outer attributes not allowed
  RangeToExpr (Analysis::NodeMapping mappings, std::unique_ptr<Expr> range_to,
	       location_t locus)
    : RangeExpr (std::move (mappings), locus), to (std::move (range_to))
  {}

  // Copy constructor with clone
  RangeToExpr (RangeToExpr const &other)
    : RangeExpr (other), to (other.to->clone_expr ())
  {}

  // Overload assignment operator to clone unique_ptr
  RangeToExpr &operator= (RangeToExpr const &other)
  {
    RangeExpr::operator= (other);
    to = other.to->clone_expr ();

    return *this;
  }

  // move constructors
  RangeToExpr (RangeToExpr &&other) = default;
  RangeToExpr &operator= (RangeToExpr &&other) = default;

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

  std::unique_ptr<Expr> &get_to_expr () { return to; }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  RangeToExpr *clone_expr_impl () const override
  {
    return new RangeToExpr (*this);
  }

  /* Use covariance to implement clone function as returning this object rather
   * than base */
  RangeToExpr *clone_expr_without_block_impl () const override
  {
    return new RangeToExpr (*this);
  }
};

// Full range expression HIR node object
// constructs a std::ops::RangeFull object
class RangeFullExpr : public RangeExpr
{
public:
  std::string as_string () const override;

  RangeFullExpr (Analysis::NodeMapping mappings, location_t locus)
    : RangeExpr (std::move (mappings), locus)
  {}
  // outer attributes not allowed

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  RangeFullExpr *clone_expr_impl () const override
  {
    return new RangeFullExpr (*this);
  }

  /* Use covariance to implement clone function as returning this object rather
   * than base */
  RangeFullExpr *clone_expr_without_block_impl () const override
  {
    return new RangeFullExpr (*this);
  }
};

// Range from (inclusive) and to (inclusive) expression HIR node object
// aka RangeInclusiveExpr; constructs a std::ops::RangeInclusive object
class RangeFromToInclExpr : public RangeExpr
{
  std::unique_ptr<Expr> from;
  std::unique_ptr<Expr> to;

public:
  std::string as_string () const override;

  RangeFromToInclExpr (Analysis::NodeMapping mappings,
		       std::unique_ptr<Expr> range_from,
		       std::unique_ptr<Expr> range_to, location_t locus)
    : RangeExpr (std::move (mappings), locus), from (std::move (range_from)),
      to (std::move (range_to))
  {}
  // outer attributes not allowed

  // Copy constructor with clone
  RangeFromToInclExpr (RangeFromToInclExpr const &other)
    : RangeExpr (other), from (other.from->clone_expr ()),
      to (other.to->clone_expr ())
  {}

  // Overload assignment operator to use clone
  RangeFromToInclExpr &operator= (RangeFromToInclExpr const &other)
  {
    RangeExpr::operator= (other);
    from = other.from->clone_expr ();
    to = other.to->clone_expr ();

    return *this;
  }

  // move constructors
  RangeFromToInclExpr (RangeFromToInclExpr &&other) = default;
  RangeFromToInclExpr &operator= (RangeFromToInclExpr &&other) = default;

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

  std::unique_ptr<Expr> &get_from_expr () { return from; }
  std::unique_ptr<Expr> &get_to_expr () { return to; }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  RangeFromToInclExpr *clone_expr_impl () const override
  {
    return new RangeFromToInclExpr (*this);
  }

  /* Use covariance to implement clone function as returning this object rather
   * than base */
  RangeFromToInclExpr *clone_expr_without_block_impl () const override
  {
    return new RangeFromToInclExpr (*this);
  }
};

// Range to (inclusive) expression HIR node object
// aka RangeToInclusiveExpr; constructs a std::ops::RangeToInclusive object
class RangeToInclExpr : public RangeExpr
{
  std::unique_ptr<Expr> to;

public:
  std::string as_string () const override;

  RangeToInclExpr (Analysis::NodeMapping mappings,
		   std::unique_ptr<Expr> range_to, location_t locus)
    : RangeExpr (std::move (mappings), locus), to (std::move (range_to))
  {}
  // outer attributes not allowed

  // Copy constructor with clone
  RangeToInclExpr (RangeToInclExpr const &other)
    : RangeExpr (other), to (other.to->clone_expr ())
  {}

  // Overload assignment operator to clone pointer
  RangeToInclExpr &operator= (RangeToInclExpr const &other)
  {
    RangeExpr::operator= (other);
    to = other.to->clone_expr ();

    return *this;
  }

  // move constructors
  RangeToInclExpr (RangeToInclExpr &&other) = default;
  RangeToInclExpr &operator= (RangeToInclExpr &&other) = default;

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

  std::unique_ptr<Expr> &get_to_expr () { return to; };

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  RangeToInclExpr *clone_expr_impl () const override
  {
    return new RangeToInclExpr (*this);
  }

  /* Use covariance to implement clone function as returning this object rather
   * than base */
  RangeToInclExpr *clone_expr_without_block_impl () const override
  {
    return new RangeToInclExpr (*this);
  }
};

// Return expression HIR node representation
class ReturnExpr : public ExprWithoutBlock
{
public:
  std::unique_ptr<Expr> return_expr;

  location_t locus;

  std::string as_string () const override;

  /* Returns whether the object has an expression returned (i.e. not void return
   * type). */
  bool has_return_expr () const { return return_expr != nullptr; }

  // Constructor for ReturnExpr.
  ReturnExpr (Analysis::NodeMapping mappings, location_t locus,
	      std::unique_ptr<Expr> returned_expr = nullptr,
	      AST::AttrVec outer_attribs = AST::AttrVec ())
    : ExprWithoutBlock (std::move (mappings), std::move (outer_attribs)),
      return_expr (std::move (returned_expr)), locus (locus)
  {}

  // Copy constructor with clone
  ReturnExpr (ReturnExpr const &other)
    : ExprWithoutBlock (other), locus (other.locus)
  {
    // guard to protect from null pointer dereference
    if (other.return_expr != nullptr)
      return_expr = other.return_expr->clone_expr ();
  }

  // Overloaded assignment operator to clone return_expr pointer
  ReturnExpr &operator= (ReturnExpr const &other)
  {
    ExprWithoutBlock::operator= (other);
    return_expr = other.return_expr->clone_expr ();
    locus = other.locus;
    // outer_attrs = other.outer_attrs;

    return *this;
  }

  // move constructors
  ReturnExpr (ReturnExpr &&other) = default;
  ReturnExpr &operator= (ReturnExpr &&other) = default;

  location_t get_locus () const override final { return locus; }

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

  std::unique_ptr<Expr> &get_expr () { return return_expr; }

  ExprType get_expression_type () const override final
  {
    return ExprType::Return;
  }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  ReturnExpr *clone_expr_impl () const override
  {
    return new ReturnExpr (*this);
  }

  /* Use covariance to implement clone function as returning this object rather
   * than base */
  ReturnExpr *clone_expr_without_block_impl () const override
  {
    return new ReturnExpr (*this);
  }
};

// An unsafe block HIR node
class UnsafeBlockExpr : public ExprWithBlock
{
  // Or just have it extend BlockExpr
  std::unique_ptr<BlockExpr> expr;
  location_t locus;

public:
  std::string as_string () const override;

  UnsafeBlockExpr (Analysis::NodeMapping mappings,
		   std::unique_ptr<BlockExpr> block_expr,
		   AST::AttrVec outer_attribs, location_t locus)
    : ExprWithBlock (std::move (mappings), std::move (outer_attribs)),
      expr (std::move (block_expr)), locus (locus)
  {}

  // Copy constructor with clone
  UnsafeBlockExpr (UnsafeBlockExpr const &other)
    : ExprWithBlock (other), expr (other.expr->clone_block_expr ()),
      locus (other.locus)
  {}

  // Overloaded assignment operator to clone
  UnsafeBlockExpr &operator= (UnsafeBlockExpr const &other)
  {
    ExprWithBlock::operator= (other);
    expr = other.expr->clone_block_expr ();
    locus = other.locus;
    // outer_attrs = other.outer_attrs;

    return *this;
  }

  // move constructors
  UnsafeBlockExpr (UnsafeBlockExpr &&other) = default;
  UnsafeBlockExpr &operator= (UnsafeBlockExpr &&other) = default;

  location_t get_locus () const override final { return locus; }

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

  std::unique_ptr<BlockExpr> &get_block_expr () { return expr; }

  ExprType get_expression_type () const override final
  {
    return ExprType::UnsafeBlock;
  }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  UnsafeBlockExpr *clone_expr_impl () const override
  {
    return new UnsafeBlockExpr (*this);
  }

  /* Use covariance to implement clone function as returning this object rather
   * than base */
  UnsafeBlockExpr *clone_expr_with_block_impl () const override
  {
    return new UnsafeBlockExpr (*this);
  }
};

// Base loop expression HIR node - aka LoopExpr
class BaseLoopExpr : public ExprWithBlock
{
protected:
  LoopLabel loop_label;
  std::unique_ptr<BlockExpr> loop_block;

private:
  location_t locus;

protected:
  // Constructor for BaseLoopExpr
  BaseLoopExpr (Analysis::NodeMapping mappings,
		std::unique_ptr<BlockExpr> loop_block, location_t locus,
		LoopLabel loop_label,
		AST::AttrVec outer_attribs = AST::AttrVec ())
    : ExprWithBlock (std::move (mappings), std::move (outer_attribs)),
      loop_label (std::move (loop_label)), loop_block (std::move (loop_block)),
      locus (locus)
  {}

  // Copy constructor for BaseLoopExpr with clone
  BaseLoopExpr (BaseLoopExpr const &other)
    : ExprWithBlock (other), loop_label (other.loop_label),
      loop_block (other.loop_block->clone_block_expr ()), locus (other.locus)
  {}

  // Overloaded assignment operator to clone
  BaseLoopExpr &operator= (BaseLoopExpr const &other)
  {
    ExprWithBlock::operator= (other);
    loop_block = other.loop_block->clone_block_expr ();
    loop_label = other.loop_label;
    locus = other.locus;
    // outer_attrs = other.outer_attrs;

    return *this;
  }

  // move constructors
  BaseLoopExpr (BaseLoopExpr &&other) = default;
  BaseLoopExpr &operator= (BaseLoopExpr &&other) = default;

  ExprType get_expression_type () const final override
  {
    return ExprType::BaseLoop;
  }

public:
  bool has_loop_label () const { return !loop_label.is_error (); }

  location_t get_locus () const override final { return locus; }

  std::unique_ptr<HIR::BlockExpr> &get_loop_block () { return loop_block; };

  LoopLabel &get_loop_label () { return loop_label; }
};

// 'Loop' expression (i.e. the infinite loop) HIR node
class LoopExpr : public BaseLoopExpr
{
public:
  std::string as_string () const override;

  // Constructor for LoopExpr
  LoopExpr (Analysis::NodeMapping mappings,
	    std::unique_ptr<BlockExpr> loop_block, location_t locus,
	    LoopLabel loop_label, AST::AttrVec outer_attribs = AST::AttrVec ())
    : BaseLoopExpr (std::move (mappings), std::move (loop_block), locus,
		    std::move (loop_label), std::move (outer_attribs))
  {}

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  LoopExpr *clone_expr_impl () const override { return new LoopExpr (*this); }

  /* Use covariance to implement clone function as returning this object rather
   * than base */
  LoopExpr *clone_expr_with_block_impl () const override
  {
    return new LoopExpr (*this);
  }
};

// While loop expression HIR node (predicate loop)
class WhileLoopExpr : public BaseLoopExpr
{
  std::unique_ptr<Expr> condition;

public:
  std::string as_string () const override;

  // Constructor for while loop with loop label
  WhileLoopExpr (Analysis::NodeMapping mappings,
		 std::unique_ptr<Expr> loop_condition,
		 std::unique_ptr<BlockExpr> loop_block, location_t locus,
		 LoopLabel loop_label,
		 AST::AttrVec outer_attribs = AST::AttrVec ())
    : BaseLoopExpr (std::move (mappings), std::move (loop_block), locus,
		    std::move (loop_label), std::move (outer_attribs)),
      condition (std::move (loop_condition))
  {}

  // Copy constructor with clone
  WhileLoopExpr (WhileLoopExpr const &other)
    : BaseLoopExpr (other), condition (other.condition->clone_expr ())
  {}

  // Overloaded assignment operator to clone
  WhileLoopExpr &operator= (WhileLoopExpr const &other)
  {
    BaseLoopExpr::operator= (other);
    condition = other.condition->clone_expr ();
    // loop_block = other.loop_block->clone_block_expr();
    // loop_label = other.loop_label;
    // outer_attrs = other.outer_attrs;

    return *this;
  }

  // move constructors
  WhileLoopExpr (WhileLoopExpr &&other) = default;
  WhileLoopExpr &operator= (WhileLoopExpr &&other) = default;

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

  std::unique_ptr<Expr> &get_predicate_expr () { return condition; }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  WhileLoopExpr *clone_expr_impl () const override
  {
    return new WhileLoopExpr (*this);
  }

  /* Use covariance to implement clone function as returning this object rather
   * than base */
  WhileLoopExpr *clone_expr_with_block_impl () const override
  {
    return new WhileLoopExpr (*this);
  }
};

// While let loop expression HIR node (predicate pattern loop)
class WhileLetLoopExpr : public BaseLoopExpr
{
  // MatchArmPatterns patterns;
  std::vector<std::unique_ptr<Pattern> > match_arm_patterns; // inlined
  std::unique_ptr<Expr> condition;

public:
  std::string as_string () const override;

  // Constructor with a loop label
  WhileLetLoopExpr (Analysis::NodeMapping mappings,
		    std::vector<std::unique_ptr<Pattern> > match_arm_patterns,
		    std::unique_ptr<Expr> condition,
		    std::unique_ptr<BlockExpr> loop_block, location_t locus,
		    LoopLabel loop_label,
		    AST::AttrVec outer_attribs = AST::AttrVec ())
    : BaseLoopExpr (std::move (mappings), std::move (loop_block), locus,
		    std::move (loop_label), std::move (outer_attribs)),
      match_arm_patterns (std::move (match_arm_patterns)),
      condition (std::move (condition))
  {}

  // Copy constructor with clone
  WhileLetLoopExpr (WhileLetLoopExpr const &other)
    : BaseLoopExpr (other),
      /*match_arm_patterns(other.match_arm_patterns),*/ condition (
	other.condition->clone_expr ())
  {
    match_arm_patterns.reserve (other.match_arm_patterns.size ());
    for (const auto &e : other.match_arm_patterns)
      match_arm_patterns.push_back (e->clone_pattern ());
  }

  // Overloaded assignment operator to clone pointers
  WhileLetLoopExpr &operator= (WhileLetLoopExpr const &other)
  {
    BaseLoopExpr::operator= (other);
    // match_arm_patterns = other.match_arm_patterns;
    condition = other.condition->clone_expr ();
    // loop_block = other.loop_block->clone_block_expr();
    // loop_label = other.loop_label;
    // outer_attrs = other.outer_attrs;

    match_arm_patterns.reserve (other.match_arm_patterns.size ());
    for (const auto &e : other.match_arm_patterns)
      match_arm_patterns.push_back (e->clone_pattern ());

    return *this;
  }

  // move constructors
  WhileLetLoopExpr (WhileLetLoopExpr &&other) = default;
  WhileLetLoopExpr &operator= (WhileLetLoopExpr &&other) = default;

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

  std::unique_ptr<Expr> &get_cond () { return condition; }
  std::vector<std::unique_ptr<Pattern> > &get_patterns ()
  {
    return match_arm_patterns;
  }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  WhileLetLoopExpr *clone_expr_impl () const override
  {
    return new WhileLetLoopExpr (*this);
  }

  /* Use covariance to implement clone function as returning this object rather
   * than base */
  WhileLetLoopExpr *clone_expr_with_block_impl () const override
  {
    return new WhileLetLoopExpr (*this);
  }
};

// forward decl for IfExpr
class IfLetExpr;

// Base if expression with no "else" or "if let" HIR node
class IfExpr : public ExprWithBlock
{
  std::unique_ptr<Expr> condition;
  std::unique_ptr<BlockExpr> if_block;

  location_t locus;

public:
  std::string as_string () const override;

  IfExpr (Analysis::NodeMapping mappings, std::unique_ptr<Expr> condition,
	  std::unique_ptr<BlockExpr> if_block, location_t locus)
    : ExprWithBlock (std::move (mappings), AST::AttrVec ()),
      condition (std::move (condition)), if_block (std::move (if_block)),
      locus (locus)
  {}
  // outer attributes are never allowed on IfExprs

  // Copy constructor with clone
  IfExpr (IfExpr const &other)
    : ExprWithBlock (other), condition (other.condition->clone_expr ()),
      if_block (other.if_block->clone_block_expr ()), locus (other.locus)
  {}

  // Overloaded assignment operator to clone expressions
  IfExpr &operator= (IfExpr const &other)
  {
    ExprWithBlock::operator= (other);
    condition = other.condition->clone_expr ();
    if_block = other.if_block->clone_block_expr ();
    locus = other.locus;

    return *this;
  }

  // move constructors
  IfExpr (IfExpr &&other) = default;
  IfExpr &operator= (IfExpr &&other) = default;

  // Unique pointer custom clone function
  std::unique_ptr<IfExpr> clone_if_expr () const
  {
    return std::unique_ptr<IfExpr> (clone_if_expr_impl ());
  }

  /* Note that multiple "else if"s are handled via nested HIRs rather than a
   * vector of else ifs - i.e. not like a switch statement. TODO - is this a
   * better approach? or does it not parse correctly and have downsides? */

  location_t get_locus () const override final { return locus; }

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

  void vis_if_condition (HIRFullVisitor &vis) { condition->accept_vis (vis); }
  void vis_if_block (HIRFullVisitor &vis) { if_block->accept_vis (vis); }

  std::unique_ptr<Expr> &get_if_condition () { return condition; }
  std::unique_ptr<BlockExpr> &get_if_block () { return if_block; }

  ExprType get_expression_type () const final override { return ExprType::If; }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  IfExpr *clone_expr_impl () const override { return new IfExpr (*this); }

  // Base clone function but still concrete as concrete base class
  virtual IfExpr *clone_if_expr_impl () const { return new IfExpr (*this); }

  /* Use covariance to implement clone function as returning this object rather
   * than base */
  IfExpr *clone_expr_with_block_impl () const override
  {
    return new IfExpr (*this);
  }
};

// If expression with an ending "else" expression HIR node (trailing)
class IfExprConseqElse : public IfExpr
{
  std::unique_ptr<ExprWithBlock> else_block;

public:
  std::string as_string () const override;

  IfExprConseqElse (Analysis::NodeMapping mappings,
		    std::unique_ptr<Expr> condition,
		    std::unique_ptr<BlockExpr> if_block,
		    std::unique_ptr<ExprWithBlock> else_block, location_t locus)
    : IfExpr (std::move (mappings), std::move (condition), std::move (if_block),
	      locus),
      else_block (std::move (else_block))
  {}
  // again, outer attributes not allowed

  // Copy constructor with clone
  IfExprConseqElse (IfExprConseqElse const &other)
    : IfExpr (other), else_block (other.else_block->clone_expr_with_block ())
  {}

  // Overloaded assignment operator with cloning
  IfExprConseqElse &operator= (IfExprConseqElse const &other)
  {
    IfExpr::operator= (other);
    // condition = other.condition->clone_expr();
    // if_block = other.if_block->clone_block_expr();
    else_block = other.else_block->clone_expr_with_block ();

    return *this;
  }

  // move constructors
  IfExprConseqElse (IfExprConseqElse &&other) = default;
  IfExprConseqElse &operator= (IfExprConseqElse &&other) = default;

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

  void vis_else_block (HIRFullVisitor &vis) { else_block->accept_vis (vis); }

  std::unique_ptr<ExprWithBlock> &get_else_block () { return else_block; }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  IfExprConseqElse *clone_expr_impl () const override
  {
    return new IfExprConseqElse (*this);
  }

  /* Use covariance to implement clone function as returning this object rather
   * than base */
  IfExprConseqElse *clone_expr_with_block_impl () const override
  {
    return new IfExprConseqElse (*this);
  }

  /* Use covariance to implement clone function as returning this object rather
   * than base */
  IfExprConseqElse *clone_if_expr_impl () const override
  {
    return new IfExprConseqElse (*this);
  }
};

// Basic "if let" expression HIR node with no else
class IfLetExpr : public ExprWithBlock
{
  // MatchArmPatterns patterns;
  std::vector<std::unique_ptr<Pattern> > match_arm_patterns; // inlined
  std::unique_ptr<Expr> value;
  std::unique_ptr<BlockExpr> if_block;

  location_t locus;

public:
  std::string as_string () const override;

  IfLetExpr (Analysis::NodeMapping mappings,
	     std::vector<std::unique_ptr<Pattern> > match_arm_patterns,
	     std::unique_ptr<Expr> value, std::unique_ptr<BlockExpr> if_block,
	     location_t locus)
    : ExprWithBlock (std::move (mappings), AST::AttrVec ()),
      match_arm_patterns (std::move (match_arm_patterns)),
      value (std::move (value)), if_block (std::move (if_block)), locus (locus)
  {}
  // outer attributes not allowed on if let exprs either

  // copy constructor with clone
  IfLetExpr (IfLetExpr const &other)
    : ExprWithBlock (other),
      /*match_arm_patterns(other.match_arm_patterns),*/ value (
	other.value->clone_expr ()),
      if_block (other.if_block->clone_block_expr ()), locus (other.locus)
  {
    match_arm_patterns.reserve (other.match_arm_patterns.size ());
    for (const auto &e : other.match_arm_patterns)
      match_arm_patterns.push_back (e->clone_pattern ());
  }

  // overload assignment operator to clone
  IfLetExpr &operator= (IfLetExpr const &other)
  {
    ExprWithBlock::operator= (other);
    // match_arm_patterns = other.match_arm_patterns;
    value = other.value->clone_expr ();
    if_block = other.if_block->clone_block_expr ();
    locus = other.locus;

    match_arm_patterns.reserve (other.match_arm_patterns.size ());
    for (const auto &e : other.match_arm_patterns)
      match_arm_patterns.push_back (e->clone_pattern ());

    return *this;
  }

  // move constructors
  IfLetExpr (IfLetExpr &&other) = default;
  IfLetExpr &operator= (IfLetExpr &&other) = default;

  // Unique pointer custom clone function
  std::unique_ptr<IfLetExpr> clone_if_let_expr () const
  {
    return std::unique_ptr<IfLetExpr> (clone_if_let_expr_impl ());
  }

  location_t get_locus () const override final { return locus; }

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

  std::unique_ptr<Expr> &get_scrutinee_expr () { return value; }

  std::vector<std::unique_ptr<Pattern> > &get_patterns ()
  {
    return match_arm_patterns;
  }

  std::unique_ptr<BlockExpr> &get_if_block () { return if_block; }

  ExprType get_expression_type () const final override
  {
    return ExprType::IfLet;
  }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  IfLetExpr *clone_expr_impl () const override { return new IfLetExpr (*this); }

  /* Use covariance to implement clone function as returning this object rather
   * than base */
  IfLetExpr *clone_expr_with_block_impl () const override
  {
    return new IfLetExpr (*this);
  }

  // Base clone function but still concrete as concrete base class
  virtual IfLetExpr *clone_if_let_expr_impl () const
  {
    return new IfLetExpr (*this);
  }
};

/* HIR node representing "if let" expression with an "else" expression at the
 * end */
class IfLetExprConseqElse : public IfLetExpr
{
  std::unique_ptr<ExprWithBlock> else_block;

public:
  std::string as_string () const override;

  IfLetExprConseqElse (
    Analysis::NodeMapping mappings,
    std::vector<std::unique_ptr<Pattern> > match_arm_patterns,
    std::unique_ptr<Expr> value, std::unique_ptr<BlockExpr> if_block,
    std::unique_ptr<ExprWithBlock> else_block, location_t locus)
    : IfLetExpr (std::move (mappings), std::move (match_arm_patterns),
		 std::move (value), std::move (if_block), locus),
      else_block (std::move (else_block))
  {}
  // outer attributes not allowed

  // copy constructor with clone
  IfLetExprConseqElse (IfLetExprConseqElse const &other)
    : IfLetExpr (other), else_block (other.else_block->clone_expr_with_block ())
  {}

  // overload assignment operator to clone
  IfLetExprConseqElse &operator= (IfLetExprConseqElse const &other)
  {
    IfLetExpr::operator= (other);
    // match_arm_patterns = other.match_arm_patterns;
    // value = other.value->clone_expr();
    // if_block = other.if_block->clone_block_expr();
    else_block = other.else_block->clone_expr_with_block ();
    // outer_attrs = other.outer_attrs;

    return *this;
  }

  // move constructors
  IfLetExprConseqElse (IfLetExprConseqElse &&other) = default;
  IfLetExprConseqElse &operator= (IfLetExprConseqElse &&other) = default;

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

  void vis_else_block (HIRFullVisitor &vis) { else_block->accept_vis (vis); }

  std::unique_ptr<ExprWithBlock> &get_else_block () { return else_block; }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  IfLetExprConseqElse *clone_expr_impl () const override
  {
    return new IfLetExprConseqElse (*this);
  }

  /* Use covariance to implement clone function as returning this object rather
   * than base */
  IfLetExprConseqElse *clone_expr_with_block_impl () const override
  {
    return new IfLetExprConseqElse (*this);
  }

  /* Use covariance to implement clone function as returning this object rather
   * than base */
  IfLetExprConseqElse *clone_if_let_expr_impl () const override
  {
    return new IfLetExprConseqElse (*this);
  }
};

// Match arm expression
struct MatchArm
{
private:
  AST::AttrVec outer_attrs;
  std::vector<std::unique_ptr<Pattern> > match_arm_patterns;
  std::unique_ptr<Expr> guard_expr;
  location_t locus;

public:
  // Returns whether the MatchArm has a match arm guard expression
  bool has_match_arm_guard () const { return guard_expr != nullptr; }

  // Constructor for match arm with a guard expression
  MatchArm (std::vector<std::unique_ptr<Pattern> > match_arm_patterns,
	    location_t locus, std::unique_ptr<Expr> guard_expr = nullptr,
	    AST::AttrVec outer_attrs = AST::AttrVec ())
    : outer_attrs (std::move (outer_attrs)),
      match_arm_patterns (std::move (match_arm_patterns)),
      guard_expr (std::move (guard_expr)), locus (locus)
  {}

  // Copy constructor with clone
  MatchArm (MatchArm const &other) : outer_attrs (other.outer_attrs)
  {
    // guard to protect from null pointer dereference
    if (other.guard_expr != nullptr)
      guard_expr = other.guard_expr->clone_expr ();

    match_arm_patterns.reserve (other.match_arm_patterns.size ());
    for (const auto &e : other.match_arm_patterns)
      match_arm_patterns.push_back (e->clone_pattern ());

    locus = other.locus;
  }

  ~MatchArm () = default;

  // Overload assignment operator to clone
  MatchArm &operator= (MatchArm const &other)
  {
    outer_attrs = other.outer_attrs;

    if (other.guard_expr != nullptr)
      guard_expr = other.guard_expr->clone_expr ();

    match_arm_patterns.clear ();
    match_arm_patterns.reserve (other.match_arm_patterns.size ());
    for (const auto &e : other.match_arm_patterns)
      match_arm_patterns.push_back (e->clone_pattern ());

    return *this;
  }

  // move constructors
  MatchArm (MatchArm &&other) = default;
  MatchArm &operator= (MatchArm &&other) = default;

  // Returns whether match arm is in an error state.
  bool is_error () const { return match_arm_patterns.empty (); }

  // Creates a match arm in an error state.
  static MatchArm create_error ()
  {
    location_t locus = UNDEF_LOCATION;
    return MatchArm (std::vector<std::unique_ptr<Pattern> > (), locus);
  }

  std::string as_string () const;

  std::vector<std::unique_ptr<Pattern> > &get_patterns ()
  {
    return match_arm_patterns;
  }

  std::unique_ptr<Expr> &get_guard_expr () { return guard_expr; }

  location_t get_locus () const { return locus; }
};

/* A "match case" - a correlated match arm and resulting expression. Not
 * abstract. */
struct MatchCase
{
private:
  Analysis::NodeMapping mappings;
  MatchArm arm;
  std::unique_ptr<Expr> expr;

public:
  MatchCase (Analysis::NodeMapping mappings, MatchArm arm,
	     std::unique_ptr<Expr> expr)
    : mappings (mappings), arm (std::move (arm)), expr (std::move (expr))
  {}

  MatchCase (const MatchCase &other)
    : mappings (other.mappings), arm (other.arm),
      expr (other.expr->clone_expr ())
  {}

  MatchCase &operator= (const MatchCase &other)
  {
    mappings = other.mappings;
    arm = other.arm;
    expr = other.expr->clone_expr ();

    return *this;
  }

  MatchCase (MatchCase &&other) = default;
  MatchCase &operator= (MatchCase &&other) = default;

  ~MatchCase () = default;

  std::string as_string () const;

  Analysis::NodeMapping get_mappings () const { return mappings; }

  MatchArm &get_arm () { return arm; }
  std::unique_ptr<Expr> &get_expr () { return expr; }
};

// Match expression HIR node
class MatchExpr : public ExprWithBlock, public WithInnerAttrs
{
  std::unique_ptr<Expr> branch_value;
  std::vector<MatchCase> match_arms;
  location_t locus;

public:
  std::string as_string () const override;

  bool has_match_arms () const { return !match_arms.empty (); }

  MatchExpr (Analysis::NodeMapping mappings, std::unique_ptr<Expr> branch_value,
	     std::vector<MatchCase> match_arms, AST::AttrVec inner_attrs,
	     AST::AttrVec outer_attrs, location_t locus)
    : ExprWithBlock (std::move (mappings), std::move (outer_attrs)),
      WithInnerAttrs (std::move (inner_attrs)),
      branch_value (std::move (branch_value)),
      match_arms (std::move (match_arms)), locus (locus)
  {}

  // Copy constructor requires clone due to unique_ptr
  MatchExpr (MatchExpr const &other)
    : ExprWithBlock (other), WithInnerAttrs (other.inner_attrs),
      branch_value (other.branch_value->clone_expr ()),
      match_arms (other.match_arms), locus (other.locus)
  {
    /*match_arms.reserve (other.match_arms.size ());
    for (const auto &e : other.match_arms)
      match_arms.push_back (e->clone_match_case ());*/
  }

  // Overloaded assignment operator to clone due to unique_ptr
  MatchExpr &operator= (MatchExpr const &other)
  {
    ExprWithBlock::operator= (other);
    branch_value = other.branch_value->clone_expr ();
    inner_attrs = other.inner_attrs;
    match_arms = other.match_arms;
    // outer_attrs = other.outer_attrs;
    locus = other.locus;

    /*match_arms.reserve (other.match_arms.size ());
    for (const auto &e : other.match_arms)
      match_arms.push_back (e->clone_match_case ());*/

    return *this;
  }

  // move constructors
  MatchExpr (MatchExpr &&other) = default;
  MatchExpr &operator= (MatchExpr &&other) = default;

  location_t get_locus () const override final { return locus; }

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

  std::unique_ptr<Expr> &get_scrutinee_expr () { return branch_value; }
  AST::AttrVec get_inner_attrs () const { return inner_attrs; }
  const std::vector<MatchCase> &get_match_cases () const { return match_arms; }
  std::vector<MatchCase> &get_match_cases () { return match_arms; }

  ExprType get_expression_type () const final override
  {
    return ExprType::Match;
  }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  MatchExpr *clone_expr_impl () const override { return new MatchExpr (*this); }

  /* Use covariance to implement clone function as returning this object rather
   * than base */
  MatchExpr *clone_expr_with_block_impl () const override
  {
    return new MatchExpr (*this);
  }
};

// Await expression HIR node (pseudo-member variable access)
class AwaitExpr : public ExprWithoutBlock
{
  std::unique_ptr<Expr> awaited_expr;
  location_t locus;

public:
  // TODO: ensure outer attributes are actually allowed
  AwaitExpr (Analysis::NodeMapping mappings, std::unique_ptr<Expr> awaited_expr,
	     AST::AttrVec outer_attrs, location_t locus)
    : ExprWithoutBlock (std::move (mappings), std::move (outer_attrs)),
      awaited_expr (std::move (awaited_expr)), locus (locus)
  {}

  // copy constructor with clone
  AwaitExpr (AwaitExpr const &other)
    : ExprWithoutBlock (other),
      awaited_expr (other.awaited_expr->clone_expr ()), locus (other.locus)
  {}

  // overloaded assignment operator with clone
  AwaitExpr &operator= (AwaitExpr const &other)
  {
    ExprWithoutBlock::operator= (other);
    awaited_expr = other.awaited_expr->clone_expr ();
    locus = other.locus;

    return *this;
  }

  // move constructors
  AwaitExpr (AwaitExpr &&other) = default;
  AwaitExpr &operator= (AwaitExpr &&other) = default;

  std::string as_string () const override;

  location_t get_locus () const override final { return locus; }

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

  std::unique_ptr<Expr> &get_awaited_expr () { return awaited_expr; }

  ExprType get_expression_type () const final override
  {
    return ExprType::Await;
  }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  AwaitExpr *clone_expr_without_block_impl () const override
  {
    return new AwaitExpr (*this);
  }
};

// Async block expression HIR node (block expr that evaluates to a future)
class AsyncBlockExpr : public ExprWithBlock
{
  bool has_move;
  std::unique_ptr<BlockExpr> block_expr;
  location_t locus;

public:
  AsyncBlockExpr (Analysis::NodeMapping mappings,
		  std::unique_ptr<BlockExpr> block_expr, bool has_move,
		  AST::AttrVec outer_attrs, location_t locus)
    : ExprWithBlock (std::move (mappings), std::move (outer_attrs)),
      has_move (has_move), block_expr (std::move (block_expr)), locus (locus)
  {}

  // copy constructor with clone
  AsyncBlockExpr (AsyncBlockExpr const &other)
    : ExprWithBlock (other), has_move (other.has_move),
      block_expr (other.block_expr->clone_block_expr ()), locus (other.locus)
  {}

  // overloaded assignment operator to clone
  AsyncBlockExpr &operator= (AsyncBlockExpr const &other)
  {
    ExprWithBlock::operator= (other);
    has_move = other.has_move;
    block_expr = other.block_expr->clone_block_expr ();
    locus = other.locus;

    return *this;
  }

  // move constructors
  AsyncBlockExpr (AsyncBlockExpr &&other) = default;
  AsyncBlockExpr &operator= (AsyncBlockExpr &&other) = default;

  std::string as_string () const override;

  location_t get_locus () const override final { return locus; }

  bool get_has_move () const { return has_move; }
  std::unique_ptr<BlockExpr> &get_block_expr () { return block_expr; }

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

  ExprType get_expression_type () const final override
  {
    return ExprType::AsyncBlock;
  }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  AsyncBlockExpr *clone_expr_with_block_impl () const override
  {
    return new AsyncBlockExpr (*this);
  }
};

// this is a utility helper class for type-checking and code-generation
class OperatorExprMeta
{
public:
  OperatorExprMeta (HIR::CompoundAssignmentExpr &expr)
    : node_mappings (expr.get_mappings ()),
      lvalue_mappings (expr.get_expr ()->get_mappings ()),
      locus (expr.get_locus ())
  {}

  OperatorExprMeta (HIR::ArithmeticOrLogicalExpr &expr)
    : node_mappings (expr.get_mappings ()),
      lvalue_mappings (expr.get_expr ()->get_mappings ()),
      locus (expr.get_locus ())
  {}

  OperatorExprMeta (HIR::NegationExpr &expr)
    : node_mappings (expr.get_mappings ()),
      lvalue_mappings (expr.get_expr ()->get_mappings ()),
      locus (expr.get_locus ())
  {}

  OperatorExprMeta (HIR::DereferenceExpr &expr)
    : node_mappings (expr.get_mappings ()),
      lvalue_mappings (expr.get_expr ()->get_mappings ()),
      locus (expr.get_locus ())
  {}

  OperatorExprMeta (HIR::ArrayIndexExpr &expr)
    : node_mappings (expr.get_mappings ()),
      lvalue_mappings (expr.get_array_expr ()->get_mappings ()),
      locus (expr.get_locus ())
  {}

  const Analysis::NodeMapping &get_mappings () const { return node_mappings; }

  const Analysis::NodeMapping &get_lvalue_mappings () const
  {
    return lvalue_mappings;
  }

  location_t get_locus () const { return locus; }

private:
  const Analysis::NodeMapping node_mappings;
  const Analysis::NodeMapping lvalue_mappings;
  location_t locus;
};

class InlineAsmReg
{
  enum Kind
  {
    X86,
    Arm,
    AArch64,
    RiscV,
    Nvptx,
    PowerPC,
    Hexagon,
    Mips,
    S390x,
    SpirV,
    Wasm,
    Bpf,
    Avr,
    Msp430,
    // Placeholder for invalid register constraints for the current target
    Err,
  };

  // this placeholder is to be removed when the definations
  // of the above enums are made in a later PR/patch.
  std::string placeholder;
};

class InlineAsmRegClass
{
  enum Type
  {
    X86,
    Arm,
    AArch64,
    RiscV,
    Nvptx,
    PowerPC,
    Hexagon,
    Mips,
    S390x,
    SpirV,
    Wasm,
    Bpf,
    Avr,
    Msp430,
    // Placeholder for invalid register constraints for the current target
    Err,
  };

  // this placeholder is to be removed when the definations
  // of the above enums are made in a later PR/patch.
  std::string placeholder;
};

struct InlineAsmRegOrRegClass
{
  enum Type
  {
    Reg,      // links to struct Register
    RegClass, // links to struct RegisterClass
  };

  struct Register
  {
    InlineAsmReg Reg;
  };

  struct RegisterClass
  {
    InlineAsmRegClass RegClass;
  };

  Identifier name;
  location_t locus;
};

// Inline Assembly Node
class InlineAsm : public ExprWithoutBlock
{
  NodeId id;

public:
  std::vector<AST::InlineAsmTemplatePiece> template_;
  std::vector<AST::TupleTemplateStr> template_strs;
  std::vector<AST::InlineAsmOperand> operands;
  AST::InlineAsmOptions options;
  std::vector<location_t> line_spans;
};
} // namespace HIR
} // namespace Rust

#endif
