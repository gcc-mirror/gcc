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

#ifndef RUST_HIR_EXPR_H
#define RUST_HIR_EXPR_H

#include "rust-hir-expr-abstract.h"
#include "rust-hir-literal.h"
#include "rust-common.h"
#include "rust-hir-bound.h"
#include "rust-hir-attrs.h"
#include "rust-expr.h"

namespace Rust {
namespace HIR {

// Loop label expression HIR node used with break and continue expressions
// TODO: inline?
class LoopLabel /*: public Node*/
{
  Lifetime label; // of type LIFETIME_OR_LABEL

  location_t locus;

  Analysis::NodeMapping mappings;

public:
  std::string as_string () const;

  LoopLabel (Analysis::NodeMapping mapping, Lifetime loop_label,
	     location_t locus);

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
		 AST::AttrVec outer_attrs = AST::AttrVec ());

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
  bool negative_number = false;

public:
  std::string as_string () const override
  {
    return "( " + literal.as_string () + " (" + get_mappings ().as_string ()
	   + "))";
  }

  Literal::LitType get_lit_type () const { return literal.get_lit_type (); }

  LiteralExpr (Analysis::NodeMapping mappings, std::string value_as_string,
	       Literal::LitType type, PrimitiveCoreType type_hint,
	       location_t locus, AST::AttrVec outer_attrs);

  LiteralExpr (Analysis::NodeMapping mappings, Literal literal,
	       location_t locus, AST::AttrVec outer_attrs);

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

  bool is_negative () const { return negative_number; }
  void set_negative ()
  {
    rust_assert (get_lit_type () == Literal::LitType::INT
		 || get_lit_type () == Literal::LitType::FLOAT);
    negative_number = true;
  }

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
private:
  location_t locus;

protected:
  /* Variable must be protected to allow derived classes to use it as a first
   * class citizen */
  std::unique_ptr<Expr> main_or_left_expr;

  // Constructor (only for initialisation of expr purposes)
  OperatorExpr (Analysis::NodeMapping mappings,
		std::unique_ptr<Expr> main_or_left_expr,
		AST::AttrVec outer_attribs, location_t locus);

  // Copy constructor (only for initialisation of expr purposes)
  OperatorExpr (OperatorExpr const &other);

  // Overload assignment operator to deep copy expr
  OperatorExpr &operator= (OperatorExpr const &other);

  // move constructors
  OperatorExpr (OperatorExpr &&other) = default;
  OperatorExpr &operator= (OperatorExpr &&other) = default;

public:
  location_t get_locus () const override final { return locus; }

  Expr &get_expr () { return *main_or_left_expr; }

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
  bool raw;

public:
  std::string as_string () const override;

  BorrowExpr (Analysis::NodeMapping mappings,
	      std::unique_ptr<Expr> borrow_lvalue, Mutability mut, bool raw,
	      AST::AttrVec outer_attribs, location_t locus);

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

  Mutability get_mut () const { return mut; }
  bool is_mut () const { return mut == Mutability::Mut; }
  bool is_raw_borrow () const { return raw; }

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
		   AST::AttrVec outer_attribs, location_t locus);

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
			AST::AttrVec outer_attribs, location_t locus);

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
		AST::AttrVec outer_attribs, location_t locus);

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
			   ExprType expr_kind, location_t locus);
  // outer attributes not allowed

  // Copy constructor - probably required due to unique pointer
  ArithmeticOrLogicalExpr (ArithmeticOrLogicalExpr const &other);

  // Overload assignment operator
  ArithmeticOrLogicalExpr &operator= (ArithmeticOrLogicalExpr const &other);

  // move constructors
  ArithmeticOrLogicalExpr (ArithmeticOrLogicalExpr &&other) = default;
  ArithmeticOrLogicalExpr &operator= (ArithmeticOrLogicalExpr &&other)
    = default;

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

  void visit_lhs (HIRFullVisitor &vis) { main_or_left_expr->accept_vis (vis); }
  void visit_rhs (HIRFullVisitor &vis) { right_expr->accept_vis (vis); }

  Expr &get_lhs () { return *main_or_left_expr; }
  Expr &get_rhs () { return *right_expr; }

  std::string get_operator_str () const;

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
		  location_t locus);
  // outer attributes not allowed

  // Copy constructor also calls OperatorExpr's protected constructor
  ComparisonExpr (ComparisonExpr const &other);

  // Overload assignment operator to deep copy
  ComparisonExpr &operator= (ComparisonExpr const &other);

  // move constructors
  ComparisonExpr (ComparisonExpr &&other) = default;
  ComparisonExpr &operator= (ComparisonExpr &&other) = default;

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

  Expr &get_lhs () { return *main_or_left_expr; }
  Expr &get_rhs () { return *right_expr; }

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
		   location_t locus);
  // outer attributes not allowed

  // Copy constructor also calls OperatorExpr's protected constructor
  LazyBooleanExpr (LazyBooleanExpr const &other);

  // Overload assignment operator to deep copy
  LazyBooleanExpr &operator= (LazyBooleanExpr const &other);

  // move constructors
  LazyBooleanExpr (LazyBooleanExpr &&other) = default;
  LazyBooleanExpr &operator= (LazyBooleanExpr &&other) = default;

  std::string as_string () const override;

  ExprType get_expr_type () const { return expr_type; }

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

  Expr &get_lhs () { return *main_or_left_expr; }
  Expr &get_rhs () { return *right_expr; }

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
		std::unique_ptr<Type> type_to_cast_to, location_t locus);
  // outer attributes not allowed

  // Copy constructor also requires calling protected constructor
  TypeCastExpr (TypeCastExpr const &other);

  // Overload assignment operator to deep copy
  TypeCastExpr &operator= (TypeCastExpr const &other);

  // move constructors as not supported in c++03
  TypeCastExpr (TypeCastExpr &&other) = default;
  TypeCastExpr &operator= (TypeCastExpr &&other) = default;

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

  // FIXME: isn't it the same as get_expr() from parent?
  Expr &get_casted_expr () { return *main_or_left_expr; }

  Type &get_type_to_convert_to () { return *type_to_convert_to; }

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
		  std::unique_ptr<Expr> value_to_assign, location_t locus);
  // outer attributes not allowed

  // Call OperatorExpr constructor in copy constructor, as well as clone
  AssignmentExpr (AssignmentExpr const &other);

  // Overload assignment operator to clone unique_ptr right_expr
  AssignmentExpr &operator= (AssignmentExpr const &other);

  // move constructors
  AssignmentExpr (AssignmentExpr &&other) = default;
  AssignmentExpr &operator= (AssignmentExpr &&other) = default;

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

  void visit_lhs (HIRFullVisitor &vis) { main_or_left_expr->accept_vis (vis); }
  void visit_rhs (HIRFullVisitor &vis) { right_expr->accept_vis (vis); }

  Expr &get_lhs () { return *main_or_left_expr; }
  Expr &get_rhs () { return *right_expr; }

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
			  ExprType expr_kind, location_t locus);
  // outer attributes not allowed

  // Have clone in copy constructor
  CompoundAssignmentExpr (CompoundAssignmentExpr const &other);

  // Overload assignment operator to clone
  CompoundAssignmentExpr &operator= (CompoundAssignmentExpr const &other);

  // move constructors
  CompoundAssignmentExpr (CompoundAssignmentExpr &&other) = default;
  CompoundAssignmentExpr &operator= (CompoundAssignmentExpr &&other) = default;

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

  Expr &get_lhs () { return *main_or_left_expr; }

  Expr &get_rhs () { return *right_expr; }

  void visit_lhs (HIRFullVisitor &vis) { main_or_left_expr->accept_vis (vis); }
  void visit_rhs (HIRFullVisitor &vis) { right_expr->accept_vis (vis); }

  std::string get_operator_str () const;

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
	       location_t locus);

  // Copy constructor includes clone for expr_in_parens
  GroupedExpr (GroupedExpr const &other);

  // Overloaded assignment operator to clone expr_in_parens
  GroupedExpr &operator= (GroupedExpr const &other);

  // move constructors
  GroupedExpr (GroupedExpr &&other) = default;
  GroupedExpr &operator= (GroupedExpr &&other) = default;

  location_t get_locus () const override final { return locus; }

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

  Expr &get_expr_in_parens () { return *expr_in_parens; }

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
  std::vector<std::unique_ptr<Expr>> values;

  // TODO: should this store location data?

public:
  ArrayElemsValues (Analysis::NodeMapping mappings,
		    std::vector<std::unique_ptr<Expr>> elems);

  // copy constructor with vector clone
  ArrayElemsValues (ArrayElemsValues const &other);

  // overloaded assignment operator with vector clone
  ArrayElemsValues &operator= (ArrayElemsValues const &other);

  // move constructors
  ArrayElemsValues (ArrayElemsValues &&other) = default;
  ArrayElemsValues &operator= (ArrayElemsValues &&other) = default;

  std::string as_string () const override;

  void accept_vis (HIRFullVisitor &vis) override;

  size_t get_num_elements () const { return values.size (); }

  std::vector<std::unique_ptr<Expr>> &get_values () { return values; }

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
		    std::unique_ptr<Expr> copy_amount);

  // Copy constructor required due to unique_ptr - uses custom clone
  ArrayElemsCopied (ArrayElemsCopied const &other);

  // Overloaded assignment operator for deep copying
  ArrayElemsCopied &operator= (ArrayElemsCopied const &other);

  // move constructors
  ArrayElemsCopied (ArrayElemsCopied &&other) = default;
  ArrayElemsCopied &operator= (ArrayElemsCopied &&other) = default;

  std::string as_string () const override;

  void accept_vis (HIRFullVisitor &vis) override;

  Expr &get_elem_to_copy () { return *elem_to_copy; }

  Expr &get_num_copies_expr () { return *num_copies; }

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
	     location_t locus);

  // Copy constructor requires cloning ArrayElems for polymorphism to hold
  ArrayExpr (ArrayExpr const &other);

  // Overload assignment operator to clone internal_elements
  ArrayExpr &operator= (ArrayExpr const &other);

  // move constructors
  ArrayExpr (ArrayExpr &&other) = default;
  ArrayExpr &operator= (ArrayExpr &&other) = default;

  location_t get_locus () const override final { return locus; }

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

  ArrayElems &get_internal_elements () { return *internal_elements; };

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
		  AST::AttrVec outer_attribs, location_t locus);

  // Copy constructor requires special cloning due to unique_ptr
  ArrayIndexExpr (ArrayIndexExpr const &other);

  // Overload assignment operator to clone unique_ptrs
  ArrayIndexExpr &operator= (ArrayIndexExpr const &other);

  // move constructors
  ArrayIndexExpr (ArrayIndexExpr &&other) = default;
  ArrayIndexExpr &operator= (ArrayIndexExpr &&other) = default;

  location_t get_locus () const override final { return locus; }

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

  Expr &get_array_expr () { return *array_expr; }
  Expr &get_index_expr () { return *index_expr; }

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
  std::vector<std::unique_ptr<Expr>> tuple_elems;
  // replaces (inlined version of) TupleElements

  location_t locus;

public:
  std::string as_string () const override;

  TupleExpr (Analysis::NodeMapping mappings,
	     std::vector<std::unique_ptr<Expr>> tuple_elements,
	     AST::AttrVec inner_attribs, AST::AttrVec outer_attribs,
	     location_t locus);

  // copy constructor with vector clone
  TupleExpr (TupleExpr const &other);

  // overloaded assignment operator to vector clone
  TupleExpr &operator= (TupleExpr const &other);

  // move constructors
  TupleExpr (TupleExpr &&other) = default;
  TupleExpr &operator= (TupleExpr &&other) = default;

  /* Note: syntactically, can disambiguate single-element tuple from parens with
   * comma, i.e. (0,) rather than (0) */

  location_t get_locus () const override final { return locus; }

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

  const std::vector<std::unique_ptr<Expr>> &get_tuple_elems () const
  {
    return tuple_elems;
  }
  std::vector<std::unique_ptr<Expr>> &get_tuple_elems () { return tuple_elems; }

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
		  AST::AttrVec outer_attribs, location_t locus);

  // Copy constructor requires a clone for tuple_expr
  TupleIndexExpr (TupleIndexExpr const &other);

  // Overload assignment operator in order to clone
  TupleIndexExpr &operator= (TupleIndexExpr const &other);

  // move constructors
  TupleIndexExpr (TupleIndexExpr &&other) = default;
  TupleIndexExpr &operator= (TupleIndexExpr &&other) = default;

  location_t get_locus () const override final { return locus; }

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

  Expr &get_tuple_expr () { return *tuple_expr; }

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
	      AST::AttrVec outer_attribs);

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
		    AST::AttrVec outer_attribs, location_t locus);

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
private:
  std::unique_ptr<Expr> base_struct;

public:
  // TODO: should this store location data?
  StructBase (std::unique_ptr<Expr> base_struct_ptr);

  // Copy constructor requires clone
  StructBase (StructBase const &other);

  // Destructor
  ~StructBase () = default;

  // Overload assignment operator to clone base_struct
  StructBase &operator= (StructBase const &other);

  // move constructors
  StructBase (StructBase &&other) = default;
  StructBase &operator= (StructBase &&other) = default;

  // Returns a null expr-ed StructBase - error state
  static StructBase error () { return StructBase (nullptr); }

  // Returns whether StructBase is in error state
  bool is_invalid () const { return base_struct == nullptr; }

  std::string as_string () const;

  Expr &get_base () { return *base_struct; }
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

  StructExprField (Analysis::NodeMapping mapping, location_t locus);

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
			     Identifier field_identifier, location_t locus);

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
			  std::unique_ptr<Expr> field_value, location_t locus);

  // Copy constructor requires clone
  StructExprFieldWithVal (StructExprFieldWithVal const &other);

  // Overload assignment operator to clone unique_ptr
  StructExprFieldWithVal &operator= (StructExprFieldWithVal const &other);

  // move constructors
  StructExprFieldWithVal (StructExprFieldWithVal &&other) = default;
  StructExprFieldWithVal &operator= (StructExprFieldWithVal &&other) = default;

public:
  std::string as_string () const override;

  Expr &get_value () { return *value; }
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
				  location_t locus);

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
			     location_t locus);

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
  // std::vector<StructExprField> fields;
  std::vector<std::unique_ptr<StructExprField>> fields;
  tl::optional<std::unique_ptr<StructBase>> struct_base;

public:
  // For unions there is just one field, the index
  // is set when type checking
  int union_index = -1;

  std::string as_string () const override;

  bool has_struct_base () const { return struct_base.has_value (); }

  // Constructor for StructExprStructFields when no struct base is used
  StructExprStructFields (
    Analysis::NodeMapping mappings, PathInExpression struct_path,
    std::vector<std::unique_ptr<StructExprField>> expr_fields, location_t locus,
    tl::optional<std::unique_ptr<StructBase>> base_struct,
    AST::AttrVec inner_attribs, AST::AttrVec outer_attribs);

  // copy constructor with vector clone
  StructExprStructFields (StructExprStructFields const &other);

  // overloaded assignment operator with vector clone
  StructExprStructFields &operator= (StructExprStructFields const &other);

  // move constructors
  StructExprStructFields (StructExprStructFields &&other) = default;
  StructExprStructFields &operator= (StructExprStructFields &&other) = default;

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

  std::vector<std::unique_ptr<StructExprField>> &get_fields ()
  {
    return fields;
  };

  const std::vector<std::unique_ptr<StructExprField>> &get_fields () const
  {
    return fields;
  };

  StructBase &get_struct_base () { return *struct_base.value (); }

  void
  set_fields_as_owner (std::vector<std::unique_ptr<StructExprField>> new_fields)
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
  StructExprStructBase (Analysis::NodeMapping mappings,
			PathInExpression struct_path, StructBase base_struct,
			AST::AttrVec inner_attribs, AST::AttrVec outer_attribs,
			location_t locus);

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

  StructBase &get_struct_base () { return struct_base; }

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
  std::vector<std::unique_ptr<Expr>> params;
  location_t locus;

public:
  std::string as_string () const override;

  CallExpr (Analysis::NodeMapping mappings, std::unique_ptr<Expr> function_expr,
	    std::vector<std::unique_ptr<Expr>> function_params,
	    AST::AttrVec outer_attribs, location_t locus);

  // copy constructor requires clone
  CallExpr (CallExpr const &other);

  // Overload assignment operator to clone
  CallExpr &operator= (CallExpr const &other);

  // move constructors
  CallExpr (CallExpr &&other) = default;
  CallExpr &operator= (CallExpr &&other) = default;

  // Returns whether function call has parameters.
  bool has_params () const { return !params.empty (); }

  location_t get_locus () const override final { return locus; }

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

  bool has_fnexpr () const { return function != nullptr; }
  Expr &get_fnexpr () { return *function; }

  size_t num_params () const { return params.size (); }

  std::vector<std::unique_ptr<Expr>> &get_arguments () { return params; }

  const std::vector<std::unique_ptr<Expr>> &get_arguments () const
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
  std::vector<std::unique_ptr<Expr>> params;
  location_t locus;

public:
  std::string as_string () const override;

  MethodCallExpr (Analysis::NodeMapping mappings,
		  std::unique_ptr<Expr> call_receiver,
		  PathExprSegment method_path,
		  std::vector<std::unique_ptr<Expr>> method_params,
		  AST::AttrVec outer_attribs, location_t locus);

  // copy constructor required due to cloning
  MethodCallExpr (MethodCallExpr const &other);

  // Overload assignment operator to clone receiver object
  MethodCallExpr &operator= (MethodCallExpr const &other);

  // move constructors
  MethodCallExpr (MethodCallExpr &&other) = default;
  MethodCallExpr &operator= (MethodCallExpr &&other) = default;

  location_t get_locus () const override final { return locus; }

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

  Expr &get_receiver () { return *receiver; }

  PathExprSegment &get_method_name () { return method_name; };
  const PathExprSegment &get_method_name () const { return method_name; };

  bool has_params () const { return !params.empty (); }
  size_t num_params () const { return params.size (); }

  std::vector<std::unique_ptr<Expr>> &get_arguments () { return params; }

  const std::vector<std::unique_ptr<Expr>> &get_arguments () const
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
		   location_t locus);

  // Copy constructor required due to unique_ptr cloning
  FieldAccessExpr (FieldAccessExpr const &other);

  // Overload assignment operator to clone unique_ptr
  FieldAccessExpr &operator= (FieldAccessExpr const &other);

  // move constructors
  FieldAccessExpr (FieldAccessExpr &&other) = default;
  FieldAccessExpr &operator= (FieldAccessExpr &&other) = default;

  location_t get_locus () const override final { return locus; }

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

  Expr &get_receiver_expr () { return *receiver; }

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
		std::vector<AST::Attribute> outer_attrs = {});

  // Copy constructor required due to cloning as a result of unique_ptrs
  ClosureParam (ClosureParam const &other);

  ~ClosureParam () = default;

  // Assignment operator must be overloaded to clone as well
  ClosureParam &operator= (ClosureParam const &other);

  // move constructors
  ClosureParam (ClosureParam &&other) = default;
  ClosureParam &operator= (ClosureParam &&other) = default;

  std::string as_string () const;

  const std::vector<AST::Attribute> &get_outer_attrs () const
  {
    return outer_attrs;
  }
  std::vector<AST::Attribute> &get_outer_attrs () { return outer_attrs; }

  Pattern &get_pattern () { return *pattern; }

  Type &get_type () { return *type; }

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
	       AST::AttrVec outer_attribs, location_t locus);

  // Copy constructor requires cloning
  ClosureExpr (ClosureExpr const &other);

  // Overload assignment operator to clone unique_ptrs
  ClosureExpr &operator= (ClosureExpr const &other);

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

  Type &get_return_type () { return *return_type; };
  Expr &get_expr () { return *expr; }

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
  std::vector<std::unique_ptr<Stmt>> statements;
  std::unique_ptr<Expr> expr;
  bool tail_reachable;
  tl::optional<LoopLabel> label;
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
	     std::vector<std::unique_ptr<Stmt>> block_statements,
	     std::unique_ptr<Expr> block_expr, bool tail_reachable,
	     AST::AttrVec inner_attribs, AST::AttrVec outer_attribs,
	     tl::optional<LoopLabel> label, location_t start_locus,
	     location_t end_locus);

  // Copy constructor with clone
  BlockExpr (BlockExpr const &other);

  // Overloaded assignment operator to clone pointer
  BlockExpr &operator= (BlockExpr const &other);

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

  bool has_final_expr () { return expr != nullptr; }
  Expr &get_final_expr () { return *expr; }

  std::vector<std::unique_ptr<Stmt>> &get_statements () { return statements; }

  ExprType get_expression_type () const final override
  {
    return ExprType::Block;
  }

  bool has_label () const { return label.has_value (); }
  LoopLabel &get_label () { return label.value (); }

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
  tl::optional<Lifetime> label;
  location_t locus;

public:
  std::string as_string () const override;

  // Returns true if the continue expr has a label.
  bool has_label () const { return label.has_value (); }

  // Constructor for a ContinueExpr with a label.
  ContinueExpr (Analysis::NodeMapping mappings, location_t locus,
		tl::optional<Lifetime> label,
		AST::AttrVec outer_attribs = AST::AttrVec ());

  location_t get_locus () const override final { return locus; }

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

  Lifetime &get_label () { return label.value (); }
  const Lifetime &get_label () const { return label.value (); }

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
  tl::optional<Lifetime> label;

  // bool has_break_expr;
  std::unique_ptr<Expr> break_expr;

  location_t locus;

public:
  std::string as_string () const override;

  // Returns whether the break expression has a label or not.
  bool has_label () const { return label.has_value (); }

  /* Returns whether the break expression has an expression used in the break or
   * not. */
  bool has_break_expr () const { return break_expr != nullptr; }

  // Constructor for a break expression
  BreakExpr (Analysis::NodeMapping mappings, location_t locus,
	     tl::optional<Lifetime> break_label,
	     std::unique_ptr<Expr> expr_in_break = nullptr,
	     AST::AttrVec outer_attribs = AST::AttrVec ());

  // Copy constructor defined to use clone for unique pointer
  BreakExpr (BreakExpr const &other);

  // Overload assignment operator to clone unique pointer
  BreakExpr &operator= (BreakExpr const &other);

  // move constructors
  BreakExpr (BreakExpr &&other) = default;
  BreakExpr &operator= (BreakExpr &&other) = default;

  location_t get_locus () const override final { return locus; }

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

  Lifetime &get_label () { return label.value (); }
  const Lifetime &get_label () const { return label.value (); }

  Expr &get_expr () { return *break_expr; }

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
  RangeExpr (Analysis::NodeMapping mappings, location_t locus);

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
		   std::unique_ptr<Expr> range_to, location_t locus);

  // Copy constructor with cloning
  RangeFromToExpr (RangeFromToExpr const &other);

  // Overload assignment operator to clone unique pointers
  RangeFromToExpr &operator= (RangeFromToExpr const &other);

  // move constructors
  RangeFromToExpr (RangeFromToExpr &&other) = default;
  RangeFromToExpr &operator= (RangeFromToExpr &&other) = default;

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

  Expr &get_from_expr () { return *from; }
  Expr &get_to_expr () { return *to; }

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
		 std::unique_ptr<Expr> range_from, location_t locus);

  // Copy constructor with clone
  RangeFromExpr (RangeFromExpr const &other);

  // Overload assignment operator to clone unique_ptr
  RangeFromExpr &operator= (RangeFromExpr const &other);

  // move constructors
  RangeFromExpr (RangeFromExpr &&other) = default;
  RangeFromExpr &operator= (RangeFromExpr &&other) = default;

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

  Expr &get_from_expr () { return *from; }

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
	       location_t locus);

  // Copy constructor with clone
  RangeToExpr (RangeToExpr const &other);

  // Overload assignment operator to clone unique_ptr
  RangeToExpr &operator= (RangeToExpr const &other);

  // move constructors
  RangeToExpr (RangeToExpr &&other) = default;
  RangeToExpr &operator= (RangeToExpr &&other) = default;

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

  Expr &get_to_expr () { return *to; }

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

  RangeFullExpr (Analysis::NodeMapping mappings, location_t locus);
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
		       std::unique_ptr<Expr> range_to, location_t locus);
  // outer attributes not allowed

  // Copy constructor with clone
  RangeFromToInclExpr (RangeFromToInclExpr const &other);

  // Overload assignment operator to use clone
  RangeFromToInclExpr &operator= (RangeFromToInclExpr const &other);

  // move constructors
  RangeFromToInclExpr (RangeFromToInclExpr &&other) = default;
  RangeFromToInclExpr &operator= (RangeFromToInclExpr &&other) = default;

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

  Expr &get_from_expr () { return *from; }
  Expr &get_to_expr () { return *to; }

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
		   std::unique_ptr<Expr> range_to, location_t locus);
  // outer attributes not allowed

  // Copy constructor with clone
  RangeToInclExpr (RangeToInclExpr const &other);

  // Overload assignment operator to clone pointer
  RangeToInclExpr &operator= (RangeToInclExpr const &other);

  // move constructors
  RangeToInclExpr (RangeToInclExpr &&other) = default;
  RangeToInclExpr &operator= (RangeToInclExpr &&other) = default;

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

  Expr &get_to_expr () { return *to; };

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
	      AST::AttrVec outer_attribs = AST::AttrVec ());

  // Copy constructor with clone
  ReturnExpr (ReturnExpr const &other);

  // Overloaded assignment operator to clone return_expr pointer
  ReturnExpr &operator= (ReturnExpr const &other);

  // move constructors
  ReturnExpr (ReturnExpr &&other) = default;
  ReturnExpr &operator= (ReturnExpr &&other) = default;

  location_t get_locus () const override final { return locus; }

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

  bool has_expr () { return return_expr != nullptr; }
  Expr &get_expr () { return *return_expr; }

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
		   AST::AttrVec outer_attribs, location_t locus);

  // Copy constructor with clone
  UnsafeBlockExpr (UnsafeBlockExpr const &other);

  // Overloaded assignment operator to clone
  UnsafeBlockExpr &operator= (UnsafeBlockExpr const &other);

  // move constructors
  UnsafeBlockExpr (UnsafeBlockExpr &&other) = default;
  UnsafeBlockExpr &operator= (UnsafeBlockExpr &&other) = default;

  location_t get_locus () const override final { return locus; }

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

  BlockExpr &get_block_expr () { return *expr; }

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
  tl::optional<LoopLabel> loop_label;
  std::unique_ptr<BlockExpr> loop_block;

private:
  location_t locus;

protected:
  // Constructor for BaseLoopExpr
  BaseLoopExpr (Analysis::NodeMapping mappings,
		std::unique_ptr<BlockExpr> loop_block, location_t locus,
		tl::optional<LoopLabel> loop_label,
		AST::AttrVec outer_attribs = AST::AttrVec ());

  // Copy constructor for BaseLoopExpr with clone
  BaseLoopExpr (BaseLoopExpr const &other);

  // Overloaded assignment operator to clone
  BaseLoopExpr &operator= (BaseLoopExpr const &other);

  // move constructors
  BaseLoopExpr (BaseLoopExpr &&other) = default;
  BaseLoopExpr &operator= (BaseLoopExpr &&other) = default;

  ExprType get_expression_type () const final override
  {
    return ExprType::BaseLoop;
  }

public:
  bool has_loop_label () const { return loop_label.has_value (); }

  location_t get_locus () const override final { return locus; }

  HIR::BlockExpr &get_loop_block () { return *loop_block; };

  LoopLabel &get_loop_label () { return loop_label.value (); }
  const LoopLabel &get_loop_label () const { return loop_label.value (); }
};

// 'Loop' expression (i.e. the infinite loop) HIR node
class LoopExpr : public BaseLoopExpr
{
public:
  std::string as_string () const override;

  // Constructor for LoopExpr
  LoopExpr (Analysis::NodeMapping mappings,
	    std::unique_ptr<BlockExpr> loop_block, location_t locus,
	    tl::optional<LoopLabel> loop_label,
	    AST::AttrVec outer_attribs = AST::AttrVec ());

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
		 tl::optional<LoopLabel> loop_label,
		 AST::AttrVec outer_attribs = AST::AttrVec ());

  // Copy constructor with clone
  WhileLoopExpr (WhileLoopExpr const &other);

  // Overloaded assignment operator to clone
  WhileLoopExpr &operator= (WhileLoopExpr const &other);

  // move constructors
  WhileLoopExpr (WhileLoopExpr &&other) = default;
  WhileLoopExpr &operator= (WhileLoopExpr &&other) = default;

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

  Expr &get_predicate_expr () { return *condition; }

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
  std::vector<std::unique_ptr<Pattern>> match_arm_patterns; // inlined
  std::unique_ptr<Expr> condition;

public:
  std::string as_string () const override;

  // Constructor with a loop label
  WhileLetLoopExpr (Analysis::NodeMapping mappings,
		    std::vector<std::unique_ptr<Pattern>> match_arm_patterns,
		    std::unique_ptr<Expr> condition,
		    std::unique_ptr<BlockExpr> loop_block, location_t locus,
		    tl::optional<LoopLabel> loop_label,
		    AST::AttrVec outer_attribs = AST::AttrVec ());

  // Copy constructor with clone
  WhileLetLoopExpr (WhileLetLoopExpr const &other);

  // Overloaded assignment operator to clone pointers
  WhileLetLoopExpr &operator= (WhileLetLoopExpr const &other);

  // move constructors
  WhileLetLoopExpr (WhileLetLoopExpr &&other) = default;
  WhileLetLoopExpr &operator= (WhileLetLoopExpr &&other) = default;

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

  Expr &get_cond () { return *condition; }
  std::vector<std::unique_ptr<Pattern>> &get_patterns ()
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

// Base if expression with no "else" or "if let" HIR node
class IfExpr : public ExprWithBlock
{
  std::unique_ptr<Expr> condition;
  std::unique_ptr<BlockExpr> if_block;

  location_t locus;

public:
  std::string as_string () const override;

  IfExpr (Analysis::NodeMapping mappings, std::unique_ptr<Expr> condition,
	  std::unique_ptr<BlockExpr> if_block, location_t locus);
  // outer attributes are never allowed on IfExprs

  // Copy constructor with clone
  IfExpr (IfExpr const &other);

  // Overloaded assignment operator to clone expressions
  IfExpr &operator= (IfExpr const &other);

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

  Expr &get_if_condition () { return *condition; }
  BlockExpr &get_if_block () { return *if_block; }

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
		    std::unique_ptr<ExprWithBlock> else_block,
		    location_t locus);
  // again, outer attributes not allowed

  // Copy constructor with clone
  IfExprConseqElse (IfExprConseqElse const &other);

  // Overloaded assignment operator with cloning
  IfExprConseqElse &operator= (IfExprConseqElse const &other);

  // move constructors
  IfExprConseqElse (IfExprConseqElse &&other) = default;
  IfExprConseqElse &operator= (IfExprConseqElse &&other) = default;

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

  void vis_else_block (HIRFullVisitor &vis) { else_block->accept_vis (vis); }

  ExprWithBlock &get_else_block () { return *else_block; }

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

// Match arm expression
struct MatchArm
{
private:
  AST::AttrVec outer_attrs;
  std::vector<std::unique_ptr<Pattern>> match_arm_patterns;
  std::unique_ptr<Expr> guard_expr;
  location_t locus;

public:
  // Returns whether the MatchArm has a match arm guard expression
  bool has_match_arm_guard () const { return guard_expr != nullptr; }

  // Constructor for match arm with a guard expression
  MatchArm (std::vector<std::unique_ptr<Pattern>> match_arm_patterns,
	    location_t locus, std::unique_ptr<Expr> guard_expr = nullptr,
	    AST::AttrVec outer_attrs = AST::AttrVec ());

  // Copy constructor with clone
  MatchArm (MatchArm const &other);

  ~MatchArm () = default;

  // Overload assignment operator to clone
  MatchArm &operator= (MatchArm const &other);

  // move constructors
  MatchArm (MatchArm &&other) = default;
  MatchArm &operator= (MatchArm &&other) = default;

  // Returns whether match arm is in an error state.
  bool is_error () const { return match_arm_patterns.empty (); }

  // Creates a match arm in an error state.
  static MatchArm create_error ()
  {
    location_t locus = UNDEF_LOCATION;
    return MatchArm (std::vector<std::unique_ptr<Pattern>> (), locus);
  }

  std::string as_string () const;

  std::vector<std::unique_ptr<Pattern>> &get_patterns ()
  {
    return match_arm_patterns;
  }

  Expr &get_guard_expr () { return *guard_expr; }

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
	     std::unique_ptr<Expr> expr);

  MatchCase (const MatchCase &other);

  MatchCase &operator= (const MatchCase &other);

  MatchCase (MatchCase &&other) = default;
  MatchCase &operator= (MatchCase &&other) = default;

  ~MatchCase () = default;

  std::string as_string () const;

  Analysis::NodeMapping get_mappings () const { return mappings; }

  MatchArm &get_arm () { return arm; }
  Expr &get_expr () { return *expr; }
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
	     AST::AttrVec outer_attrs, location_t locus);

  // Copy constructor requires clone due to unique_ptr
  MatchExpr (MatchExpr const &other);

  // Overloaded assignment operator to clone due to unique_ptr
  MatchExpr &operator= (MatchExpr const &other);

  // move constructors
  MatchExpr (MatchExpr &&other) = default;
  MatchExpr &operator= (MatchExpr &&other) = default;

  location_t get_locus () const override final { return locus; }

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

  Expr &get_scrutinee_expr () { return *branch_value; }
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
	     AST::AttrVec outer_attrs, location_t locus);

  // copy constructor with clone
  AwaitExpr (AwaitExpr const &other);

  // overloaded assignment operator with clone
  AwaitExpr &operator= (AwaitExpr const &other);

  // move constructors
  AwaitExpr (AwaitExpr &&other) = default;
  AwaitExpr &operator= (AwaitExpr &&other) = default;

  std::string as_string () const override;

  location_t get_locus () const override final { return locus; }

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;

  Expr &get_awaited_expr () { return *awaited_expr; }

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
		  AST::AttrVec outer_attrs, location_t locus);

  // copy constructor with clone
  AsyncBlockExpr (AsyncBlockExpr const &other);

  // overloaded assignment operator to clone
  AsyncBlockExpr &operator= (AsyncBlockExpr const &other);

  // move constructors
  AsyncBlockExpr (AsyncBlockExpr &&other) = default;
  AsyncBlockExpr &operator= (AsyncBlockExpr &&other) = default;

  std::string as_string () const override;

  location_t get_locus () const override final { return locus; }

  bool get_has_move () const { return has_move; }
  BlockExpr &get_block_expr () { return *block_expr; }

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
  OperatorExprMeta (HIR::CompoundAssignmentExpr &expr);

  OperatorExprMeta (HIR::ArithmeticOrLogicalExpr &expr);

  OperatorExprMeta (HIR::NegationExpr &expr);

  OperatorExprMeta (HIR::DereferenceExpr &expr);

  OperatorExprMeta (HIR::ArrayIndexExpr &expr);

  OperatorExprMeta (HIR::ComparisonExpr &expr);

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

struct AnonConst
{
  NodeId id;
  std::unique_ptr<Expr> expr;

  AnonConst (NodeId id, std::unique_ptr<Expr> expr);

  AnonConst (const AnonConst &other);

  AnonConst operator= (const AnonConst &other);
};

class InlineAsmOperand
{
public:
  struct In
  {
    tl::optional<struct AST::InlineAsmRegOrRegClass> reg;
    std::unique_ptr<Expr> expr;

    In (const tl::optional<struct AST::InlineAsmRegOrRegClass> &reg,
	std::unique_ptr<Expr> expr);

    In (const struct In &other);

    In operator= (const struct In &other);
  };

  struct Out
  {
    tl::optional<struct AST::InlineAsmRegOrRegClass> reg;
    bool late;
    std::unique_ptr<Expr> expr; // can be null

    Out (tl::optional<struct AST::InlineAsmRegOrRegClass> &reg, bool late,
	 std::unique_ptr<Expr> expr);

    Out (const struct Out &other);

    Out operator= (const struct Out &other);
  };

  struct InOut
  {
    tl::optional<struct AST::InlineAsmRegOrRegClass> reg;
    bool late;
    std::unique_ptr<Expr> expr; // this can't be null

    InOut (tl::optional<struct AST::InlineAsmRegOrRegClass> &reg, bool late,
	   std::unique_ptr<Expr> expr);

    InOut (const struct InOut &other);

    InOut operator= (const struct InOut &other);
  };

  struct SplitInOut
  {
    tl::optional<struct AST::InlineAsmRegOrRegClass> reg;
    bool late;
    std::unique_ptr<Expr> in_expr;
    std::unique_ptr<Expr> out_expr; // could be null

    SplitInOut (tl::optional<struct AST::InlineAsmRegOrRegClass> &reg,
		bool late, std::unique_ptr<Expr> in_expr,
		std::unique_ptr<Expr> out_expr);

    SplitInOut (const struct SplitInOut &other);

    SplitInOut operator= (const struct SplitInOut &other);
  };

  struct Const
  {
    AnonConst anon_const;
  };

  struct Sym
  {
    std::unique_ptr<Expr> expr;

    Sym (std::unique_ptr<Expr> expr);

    Sym (const struct Sym &other);

    Sym operator= (const struct Sym &other);
  };

  struct Label
  {
    std::string label_name;
    std::unique_ptr<Expr> expr;

    Label (tl::optional<std::string> label_name, std::unique_ptr<Expr> expr);

    Label (const struct Label &other);

    Label operator= (const struct Label &other);
  };

private:
  using RegisterType = AST::InlineAsmOperand::RegisterType;
  AST::InlineAsmOperand::RegisterType register_type;

  tl::optional<struct In> in;
  tl::optional<struct Out> out;
  tl::optional<struct InOut> in_out;
  tl::optional<struct SplitInOut> split_in_out;
  tl::optional<struct Const> cnst;
  tl::optional<struct Sym> sym;
  tl::optional<struct Label> label;

public:
  InlineAsmOperand (const InlineAsmOperand &other)
    : register_type (other.register_type), in (other.in), out (other.out),
      in_out (other.in_out), split_in_out (other.split_in_out),
      cnst (other.cnst), sym (other.sym)
  {}

  InlineAsmOperand (const struct In &reg)
    : register_type (RegisterType::In), in (reg)
  {}

  InlineAsmOperand (const struct Out &reg)
    : register_type (RegisterType::Out), out (reg)
  {}
  InlineAsmOperand (const struct InOut &reg)
    : register_type (RegisterType::InOut), in_out (reg)
  {}
  InlineAsmOperand (const struct SplitInOut &reg)
    : register_type (RegisterType::SplitInOut), split_in_out (reg)
  {}
  InlineAsmOperand (const struct Const &reg)
    : register_type (RegisterType::Const), cnst (reg)
  {}
  InlineAsmOperand (const struct Sym &reg)
    : register_type (RegisterType::Sym), sym (reg)
  {}
  InlineAsmOperand (const struct Label &reg)
    : register_type (RegisterType::Label), label (reg)
  {}

  RegisterType get_register_type () const { return register_type; }

  // Potentially unsafe without get_register_type() check
  struct In get_in () const { return in.value (); }
  struct Out get_out () const { return out.value (); }
  struct InOut get_in_out () const { return in_out.value (); }
  struct SplitInOut get_split_in_out () const { return split_in_out.value (); }
  struct Const get_const () const { return cnst.value (); }
  struct Sym get_sym () const { return sym.value (); }
  struct Label get_label () const { return label.value (); }
};

// Inline Assembly Node
class InlineAsm : public ExprWithoutBlock
{
  NodeId id;
  location_t locus;

public:
  bool is_global_asm;

  std::vector<AST::InlineAsmTemplatePiece> template_;
  std::vector<AST::TupleTemplateStr> template_strs;
  std::vector<HIR::InlineAsmOperand> operands;
  std::vector<AST::TupleClobber> clobber_abi;
  std::set<AST::InlineAsmOption> options;

  std::vector<location_t> line_spans;

  void accept_vis (HIRExpressionVisitor &vis) override;

  void accept_vis (HIRFullVisitor &vis) override;

  std::string as_string () const override { return "InlineAsm HIR Node"; }

  location_t get_locus () const override { return locus; }

  InlineAsm *clone_expr_without_block_impl () const override
  {
    return new InlineAsm (*this);
  }

  ExprType get_expression_type () const final override
  {
    return ExprType::InlineAsm;
  }
  std::vector<AST::InlineAsmTemplatePiece> get_template_ ()
  {
    return template_;
  }

  std::vector<AST::TupleTemplateStr> get_template_strs ()
  {
    return template_strs;
  }

  std::vector<HIR::InlineAsmOperand> get_operands () { return operands; }

  std::vector<AST::TupleClobber> get_clobber_abi () { return clobber_abi; }

  std::set<AST::InlineAsmOption> get_options () { return options; }

  bool is_simple_asm ()
  {
    // INFO: A simple asm is an asm that does not have any operands
    return this->operands.size () == 0;
  }

  bool is_inline_asm ()
  {
    // INFO: An inline asm is asm!, which is the opposite of a global_asm()
    return !this->is_global_asm;
  }

  InlineAsm (location_t locus, bool is_global_asm,
	     std::vector<AST::InlineAsmTemplatePiece> template_,
	     std::vector<AST::TupleTemplateStr> template_strs,
	     std::vector<HIR::InlineAsmOperand> operands,
	     std::vector<AST::TupleClobber> clobber_abi,
	     std::set<AST::InlineAsmOption> options,
	     Analysis::NodeMapping mappings,
	     AST::AttrVec outer_attribs = AST::AttrVec ());
};

} // namespace HIR
} // namespace Rust

#endif
