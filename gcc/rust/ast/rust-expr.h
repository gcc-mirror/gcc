#ifndef RUST_AST_EXPR_H
#define RUST_AST_EXPR_H

#include "rust-ast.h"
#include "rust-path.h"
#include "rust-macro.h"
#include "rust-operators.h"

namespace Rust {
namespace AST {
/* TODO: if GCC moves to C++17 or allows boost, replace some boolean
 * "has_whatever" pairs with
 * optional types (std::optional or boost::optional)? */

// Loop label expression AST node used with break and continue expressions
// TODO: inline?
class LoopLabel /*: public Node*/
{
  Lifetime label; // or type LIFETIME_OR_LABEL
  location_t locus;

  NodeId node_id;

public:
  std::string as_string () const;

  LoopLabel (Lifetime loop_label, location_t locus = UNDEF_LOCATION)
    : label (std::move (loop_label)), locus (locus),
      node_id (Analysis::Mappings::get ()->get_next_node_id ())
  {}

  // Returns whether the LoopLabel is in an error state.
  bool is_error () const { return label.is_error (); }

  // Creates an error state LoopLabel.
  static LoopLabel error () { return LoopLabel (Lifetime::error ()); }

  location_t get_locus () const { return locus; }

  Lifetime &get_lifetime () { return label; }

  NodeId get_node_id () const { return node_id; }
};

// AST node for an expression with an accompanying block - abstract
class ExprWithBlock : public Expr
{
protected:
  // pure virtual clone implementation
  virtual ExprWithBlock *clone_expr_with_block_impl () const = 0;

  // prevent having to define multiple clone expressions
  ExprWithBlock *clone_expr_impl () const final override
  {
    return clone_expr_with_block_impl ();
  }

  bool is_expr_without_block () const final override { return false; };

public:
  // Unique pointer custom clone function
  std::unique_ptr<ExprWithBlock> clone_expr_with_block () const
  {
    return std::unique_ptr<ExprWithBlock> (clone_expr_with_block_impl ());
  }
};

// Literals? Or literal base?
class LiteralExpr : public ExprWithoutBlock
{
  std::vector<Attribute> outer_attrs;
  Literal literal;
  location_t locus;

public:
  std::string as_string () const override { return literal.as_string (); }

  Literal::LitType get_lit_type () const { return literal.get_lit_type (); }

  LiteralExpr (std::string value_as_string, Literal::LitType type,
	       PrimitiveCoreType type_hint, std::vector<Attribute> outer_attrs,
	       location_t locus)
    : outer_attrs (std::move (outer_attrs)),
      literal (std::move (value_as_string), type, type_hint), locus (locus)
  {}

  LiteralExpr (Literal literal, std::vector<Attribute> outer_attrs,
	       location_t locus)
    : outer_attrs (std::move (outer_attrs)), literal (std::move (literal)),
      locus (locus)
  {}

  // Unique pointer custom clone function
  std::unique_ptr<LiteralExpr> clone_literal_expr () const
  {
    return std::unique_ptr<LiteralExpr> (clone_literal_expr_impl ());
  }

  location_t get_locus () const override final { return locus; }

  bool is_literal () const override final { return true; }

  Literal get_literal () const { return literal; }

  void accept_vis (ASTVisitor &vis) override;

  // Invalid if literal is in error state, so base stripping on that.
  void mark_for_strip () override { literal = Literal::create_error (); }
  bool is_marked_for_strip () const override { return literal.is_error (); }

  const std::vector<Attribute> &get_outer_attrs () const { return outer_attrs; }
  std::vector<Attribute> &get_outer_attrs () override { return outer_attrs; }

  void set_outer_attrs (std::vector<Attribute> new_attrs) override
  {
    outer_attrs = std::move (new_attrs);
  }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  LiteralExpr *clone_expr_without_block_impl () const final override
  {
    return clone_literal_expr_impl ();
  }

  /* not virtual as currently no subclasses of LiteralExpr, but could be in
   * future */
  /*virtual*/ LiteralExpr *clone_literal_expr_impl () const
  {
    return new LiteralExpr (*this);
  }
};

// Literal expression attribute body (non-macro attribute)
class AttrInputLiteral : public AttrInput
{
  LiteralExpr literal_expr;

public:
  AttrInputLiteral (LiteralExpr lit_expr) : literal_expr (std::move (lit_expr))
  {}

  std::string as_string () const override
  {
    return " = " + literal_expr.as_string ();
  }

  void accept_vis (ASTVisitor &vis) override;

  /* this can never be a cfg predicate - cfg and cfg_attr require a token-tree
   * cfg */
  bool check_cfg_predicate (const Session &) const override { return false; }

  bool is_meta_item () const override { return false; }

  LiteralExpr &get_literal () { return literal_expr; }

  AttrInputType get_attr_input_type () const final override
  {
    return AttrInput::AttrInputType::LITERAL;
  }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  AttrInputLiteral *clone_attr_input_impl () const override
  {
    return new AttrInputLiteral (*this);
  }
};

// Like an AttrInputLiteral, but stores a MacroInvocation
class AttrInputMacro : public AttrInput
{
  std::unique_ptr<MacroInvocation> macro;

public:
  AttrInputMacro (std::unique_ptr<MacroInvocation> macro)
    : macro (std::move (macro))
  {}

  AttrInputMacro (const AttrInputMacro &oth);

  AttrInputMacro (AttrInputMacro &&oth) : macro (std::move (oth.macro)) {}

  void operator= (const AttrInputMacro &oth);

  void operator= (AttrInputMacro &&oth) { macro = std::move (oth.macro); }

  std::string as_string () const override;

  void accept_vis (ASTVisitor &vis) override;

  // assuming this can't be a cfg predicate
  bool check_cfg_predicate (const Session &) const override { return false; }

  // assuming this is like AttrInputLiteral
  bool is_meta_item () const override { return false; }

  std::unique_ptr<MacroInvocation> &get_macro () { return macro; }

  AttrInputType get_attr_input_type () const final override
  {
    return AttrInput::AttrInputType::MACRO;
  }

protected:
  AttrInputMacro *clone_attr_input_impl () const override
  {
    return new AttrInputMacro (*this);
  }
};

/* literal expr only meta item inner - TODO possibly replace with inheritance of
 * LiteralExpr itself? */
class MetaItemLitExpr : public MetaItemInner
{
  LiteralExpr lit_expr;

public:
  MetaItemLitExpr (LiteralExpr lit_expr) : lit_expr (std::move (lit_expr)) {}

  std::string as_string () const override { return lit_expr.as_string (); }

  location_t get_locus () const override { return lit_expr.get_locus (); }

  LiteralExpr get_literal () const { return lit_expr; }

  LiteralExpr &get_literal () { return lit_expr; }

  void accept_vis (ASTVisitor &vis) override;

  bool check_cfg_predicate (const Session &session) const override;

  MetaItemInner::Kind get_kind () override
  {
    return MetaItemInner::Kind::LitExpr;
  }

protected:
  // Use covariance to implement clone function as returning this type
  MetaItemLitExpr *clone_meta_item_inner_impl () const override
  {
    return new MetaItemLitExpr (*this);
  }
};

// more generic meta item "path = lit" form
class MetaItemPathLit : public MetaItem
{
  SimplePath path;
  LiteralExpr lit;

public:
  MetaItemPathLit (SimplePath path, LiteralExpr lit_expr)
    : path (std::move (path)), lit (std::move (lit_expr))
  {}

  SimplePath get_path () const { return path; }

  SimplePath &get_path () { return path; }

  LiteralExpr get_literal () const { return lit; }

  LiteralExpr &get_literal () { return lit; }

  std::string as_string () const override
  {
    return path.as_string () + " = " + lit.as_string ();
  }

  MetaItem::ItemKind get_item_kind () const override
  {
    return MetaItem::ItemKind::PathLit;
  }

  // There are two Locations in MetaItemPathLit (path and lit_expr),
  //  we have no idea use which of them, just simply return UNKNOWN_LOCATION
  //  now.
  // Maybe we will figure out when we really need the location in the future.
  location_t get_locus () const override { return UNKNOWN_LOCATION; }

  void accept_vis (ASTVisitor &vis) override;

  bool check_cfg_predicate (const Session &session) const override;
  /* TODO: return true if "ident" is defined and value of it is "lit", return
   * false otherwise */

  Attribute to_attribute () const override;

protected:
  // Use covariance to implement clone function as returning this type
  MetaItemPathLit *clone_meta_item_inner_impl () const override
  {
    return new MetaItemPathLit (*this);
  }
};

/* Represents an expression using unary or binary operators as AST node. Can be
 * overloaded. */
class OperatorExpr : public ExprWithoutBlock
{
  // TODO: create binary and unary operator subclasses?
public:
  location_t locus;

protected:
  /* Variables must be protected to allow derived classes to use them as first
   * class citizens */
  std::vector<Attribute> outer_attrs;
  std::unique_ptr<Expr> main_or_left_expr;

  // Constructor (only for initialisation of expr purposes)
  OperatorExpr (std::unique_ptr<Expr> main_or_left_expr,
		std::vector<Attribute> outer_attribs, location_t locus)
    : locus (locus), outer_attrs (std::move (outer_attribs)),
      main_or_left_expr (std::move (main_or_left_expr))
  {}

  // Copy constructor (only for initialisation of expr purposes)
  OperatorExpr (OperatorExpr const &other)
    : locus (other.locus), outer_attrs (other.outer_attrs)
  {
    // guard to prevent null dereference (only required if error state)
    if (other.main_or_left_expr != nullptr)
      main_or_left_expr = other.main_or_left_expr->clone_expr ();
  }

  // Overload assignment operator to deep copy expr
  OperatorExpr &operator= (OperatorExpr const &other)
  {
    ExprWithoutBlock::operator= (other);
    locus = other.locus;
    outer_attrs = other.outer_attrs;

    // guard to prevent null dereference (only required if error state)
    if (other.main_or_left_expr != nullptr)
      main_or_left_expr = other.main_or_left_expr->clone_expr ();
    else
      main_or_left_expr = nullptr;

    return *this;
  }

  // move constructors
  OperatorExpr (OperatorExpr &&other) = default;
  OperatorExpr &operator= (OperatorExpr &&other) = default;

public:
  location_t get_locus () const override final { return locus; }

  // Invalid if expr is null, so base stripping on that.
  void mark_for_strip () override { main_or_left_expr = nullptr; }
  bool is_marked_for_strip () const override
  {
    return main_or_left_expr == nullptr;
  }

  const std::vector<Attribute> &get_outer_attrs () const { return outer_attrs; }
  std::vector<Attribute> &get_outer_attrs () override { return outer_attrs; }

  void set_outer_attrs (std::vector<Attribute> new_attrs) override
  {
    outer_attrs = std::move (new_attrs);
  }
};

/* Unary prefix & or &mut (or && and &&mut) borrow operator. Cannot be
 * overloaded. */
class BorrowExpr : public OperatorExpr
{
  bool is_mut;
  bool double_borrow;

public:
  std::string as_string () const override;

  BorrowExpr (std::unique_ptr<Expr> borrow_lvalue, bool is_mut_borrow,
	      bool is_double_borrow, std::vector<Attribute> outer_attribs,
	      location_t locus)
    : OperatorExpr (std::move (borrow_lvalue), std::move (outer_attribs),
		    locus),
      is_mut (is_mut_borrow), double_borrow (is_double_borrow)
  {}

  void accept_vis (ASTVisitor &vis) override;

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<Expr> &get_borrowed_expr ()
  {
    rust_assert (main_or_left_expr != nullptr);
    return main_or_left_expr;
  }

  bool get_is_mut () const { return is_mut; }

  bool get_is_double_borrow () const { return double_borrow; }

protected:
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
  DereferenceExpr (std::unique_ptr<Expr> deref_lvalue,
		   std::vector<Attribute> outer_attribs, location_t locus)
    : OperatorExpr (std::move (deref_lvalue), std::move (outer_attribs), locus)
  {}

  void accept_vis (ASTVisitor &vis) override;

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<Expr> &get_dereferenced_expr ()
  {
    rust_assert (main_or_left_expr != nullptr);
    return main_or_left_expr;
  }

protected:
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
  ErrorPropagationExpr (std::unique_ptr<Expr> potential_error_value,
			std::vector<Attribute> outer_attribs, location_t locus)
    : OperatorExpr (std::move (potential_error_value),
		    std::move (outer_attribs), locus)
  {}

  void accept_vis (ASTVisitor &vis) override;

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<Expr> &get_propagating_expr ()
  {
    rust_assert (main_or_left_expr != nullptr);
    return main_or_left_expr;
  }

protected:
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
  NegationExpr (std::unique_ptr<Expr> negated_value, ExprType expr_kind,
		std::vector<Attribute> outer_attribs, location_t locus)
    : OperatorExpr (std::move (negated_value), std::move (outer_attribs),
		    locus),
      expr_type (expr_kind)
  {}

  void accept_vis (ASTVisitor &vis) override;

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<Expr> &get_negated_expr ()
  {
    rust_assert (main_or_left_expr != nullptr);
    return main_or_left_expr;
  }

protected:
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
  ArithmeticOrLogicalExpr (std::unique_ptr<Expr> left_value,
			   std::unique_ptr<Expr> right_value,
			   ExprType expr_kind, location_t locus)
    : OperatorExpr (std::move (left_value), std::vector<Attribute> (), locus),
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

  void accept_vis (ASTVisitor &vis) override;

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<Expr> &get_left_expr ()
  {
    rust_assert (main_or_left_expr != nullptr);
    return main_or_left_expr;
  }

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<Expr> &get_right_expr ()
  {
    rust_assert (right_expr != nullptr);
    return right_expr;
  }

  void visit_lhs (ASTVisitor &vis) { main_or_left_expr->accept_vis (vis); }
  void visit_rhs (ASTVisitor &vis) { right_expr->accept_vis (vis); }

protected:
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
  ComparisonExpr (std::unique_ptr<Expr> left_value,
		  std::unique_ptr<Expr> right_value, ExprType comparison_kind,
		  location_t locus)
    : OperatorExpr (std::move (left_value), std::vector<Attribute> (), locus),
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

  void accept_vis (ASTVisitor &vis) override;

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<Expr> &get_left_expr ()
  {
    rust_assert (main_or_left_expr != nullptr);
    return main_or_left_expr;
  }

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<Expr> &get_right_expr ()
  {
    rust_assert (right_expr != nullptr);
    return right_expr;
  }

  ExprType get_kind () { return expr_type; }

  /* TODO: implement via a function call to std::cmp::PartialEq::eq(&op1, &op2)
   * maybe? */
protected:
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
  LazyBooleanExpr (std::unique_ptr<Expr> left_bool_expr,
		   std::unique_ptr<Expr> right_bool_expr, ExprType expr_kind,
		   location_t locus)
    : OperatorExpr (std::move (left_bool_expr), std::vector<Attribute> (),
		    locus),
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

  void accept_vis (ASTVisitor &vis) override;

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<Expr> &get_left_expr ()
  {
    rust_assert (main_or_left_expr != nullptr);
    return main_or_left_expr;
  }

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<Expr> &get_right_expr ()
  {
    rust_assert (right_expr != nullptr);
    return right_expr;
  }

  ExprType get_kind () { return expr_type; }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  LazyBooleanExpr *clone_expr_without_block_impl () const override
  {
    return new LazyBooleanExpr (*this);
  }
};

// Binary infix "as" cast expression.
class TypeCastExpr : public OperatorExpr
{
  std::unique_ptr<TypeNoBounds> type_to_convert_to;

  // Note: only certain type casts allowed, outlined in reference
public:
  std::string as_string () const override;

  // Constructor requires calling protected constructor of OperatorExpr
  TypeCastExpr (std::unique_ptr<Expr> expr_to_cast,
		std::unique_ptr<TypeNoBounds> type_to_cast_to, location_t locus)
    : OperatorExpr (std::move (expr_to_cast), std::vector<Attribute> (), locus),
      type_to_convert_to (std::move (type_to_cast_to))
  {}
  // outer attributes not allowed

  // Copy constructor also requires calling protected constructor
  TypeCastExpr (TypeCastExpr const &other)
    : OperatorExpr (other),
      type_to_convert_to (other.type_to_convert_to->clone_type_no_bounds ())
  {}

  // Overload assignment operator to deep copy
  TypeCastExpr &operator= (TypeCastExpr const &other)
  {
    OperatorExpr::operator= (other);
    // main_or_left_expr = other.main_or_left_expr->clone_expr();
    type_to_convert_to = other.type_to_convert_to->clone_type_no_bounds ();

    return *this;
  }

  // move constructors
  TypeCastExpr (TypeCastExpr &&other) = default;
  TypeCastExpr &operator= (TypeCastExpr &&other) = default;

  void accept_vis (ASTVisitor &vis) override;

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<Expr> &get_casted_expr ()
  {
    rust_assert (main_or_left_expr != nullptr);
    return main_or_left_expr;
  }

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<TypeNoBounds> &get_type_to_cast_to ()
  {
    rust_assert (type_to_convert_to != nullptr);
    return type_to_convert_to;
  }

protected:
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
  AssignmentExpr (std::unique_ptr<Expr> value_to_assign_to,
		  std::unique_ptr<Expr> value_to_assign,
		  std::vector<Attribute> outer_attribs, location_t locus)
    : OperatorExpr (std::move (value_to_assign_to), std::move (outer_attribs),
		    locus),
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

  void accept_vis (ASTVisitor &vis) override;

  void visit_lhs (ASTVisitor &vis) { main_or_left_expr->accept_vis (vis); }
  void visit_rhs (ASTVisitor &vis) { right_expr->accept_vis (vis); }

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<Expr> &get_left_expr ()
  {
    rust_assert (main_or_left_expr != nullptr);
    return main_or_left_expr;
  }

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<Expr> &get_right_expr ()
  {
    rust_assert (right_expr != nullptr);
    return right_expr;
  }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  AssignmentExpr *clone_expr_without_block_impl () const override
  {
    return new AssignmentExpr (*this);
  }
};

/* Binary infix compound assignment (arithmetic or logic then assignment)
 * expressions. */
class CompoundAssignmentExpr : public OperatorExpr
{
public:
  using ExprType = CompoundAssignmentOperator;

private:
  // Note: overloading trait specified in comments
  ExprType expr_type;
  std::unique_ptr<Expr> right_expr;

public:
  std::string as_string () const override;

  ExprType get_expr_type () const { return expr_type; }

  // Use pointers in constructor to enable polymorphism
  CompoundAssignmentExpr (std::unique_ptr<Expr> value_to_assign_to,
			  std::unique_ptr<Expr> value_to_assign,
			  ExprType expr_kind, location_t locus)
    : OperatorExpr (std::move (value_to_assign_to), std::vector<Attribute> (),
		    locus),
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

  void accept_vis (ASTVisitor &vis) override;

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<Expr> &get_left_expr ()
  {
    rust_assert (main_or_left_expr != nullptr);
    return main_or_left_expr;
  }

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<Expr> &get_right_expr ()
  {
    rust_assert (right_expr != nullptr);
    return right_expr;
  }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  CompoundAssignmentExpr *clone_expr_without_block_impl () const override
  {
    return new CompoundAssignmentExpr (*this);
  }
};

// Expression in parentheses (i.e. like literally just any 3 + (2 * 6))
class GroupedExpr : public ExprWithoutBlock
{
  std::vector<Attribute> outer_attrs;
  std::vector<Attribute> inner_attrs;
  std::unique_ptr<Expr> expr_in_parens;
  location_t locus;

public:
  std::string as_string () const override;

  const std::vector<Attribute> &get_inner_attrs () const { return inner_attrs; }
  std::vector<Attribute> &get_inner_attrs () { return inner_attrs; }

  const std::vector<Attribute> &get_outer_attrs () const { return outer_attrs; }
  std::vector<Attribute> &get_outer_attrs () override { return outer_attrs; }

  void set_outer_attrs (std::vector<Attribute> new_attrs) override
  {
    outer_attrs = std::move (new_attrs);
  }

  GroupedExpr (std::unique_ptr<Expr> parenthesised_expr,
	       std::vector<Attribute> inner_attribs,
	       std::vector<Attribute> outer_attribs, location_t locus)
    : outer_attrs (std::move (outer_attribs)),
      inner_attrs (std::move (inner_attribs)),
      expr_in_parens (std::move (parenthesised_expr)), locus (locus)
  {}

  // Copy constructor includes clone for expr_in_parens
  GroupedExpr (GroupedExpr const &other)
    : ExprWithoutBlock (other), outer_attrs (other.outer_attrs),
      inner_attrs (other.inner_attrs), locus (other.locus)
  {
    // guard to prevent null dereference (only required if error state)
    if (other.expr_in_parens != nullptr)
      expr_in_parens = other.expr_in_parens->clone_expr ();
  }

  // Overloaded assignment operator to clone expr_in_parens
  GroupedExpr &operator= (GroupedExpr const &other)
  {
    ExprWithoutBlock::operator= (other);
    inner_attrs = other.inner_attrs;
    locus = other.locus;
    outer_attrs = other.outer_attrs;

    // guard to prevent null dereference (only required if error state)
    if (other.expr_in_parens != nullptr)
      expr_in_parens = other.expr_in_parens->clone_expr ();
    else
      expr_in_parens = nullptr;

    return *this;
  }

  // move constructors
  GroupedExpr (GroupedExpr &&other) = default;
  GroupedExpr &operator= (GroupedExpr &&other) = default;

  location_t get_locus () const override final { return locus; }

  void accept_vis (ASTVisitor &vis) override;

  // Invalid if inner expr is null, so base stripping on that.
  void mark_for_strip () override { expr_in_parens = nullptr; }
  bool is_marked_for_strip () const override
  {
    return expr_in_parens == nullptr;
  }

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<Expr> &get_expr_in_parens ()
  {
    rust_assert (expr_in_parens != nullptr);
    return expr_in_parens;
  }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  GroupedExpr *clone_expr_without_block_impl () const override
  {
    return new GroupedExpr (*this);
  }
};

// Base array initialisation internal element representation thing (abstract)
// aka ArrayElements
class ArrayElems
{
public:
  virtual ~ArrayElems () {}

  // Unique pointer custom clone ArrayElems function
  std::unique_ptr<ArrayElems> clone_array_elems () const
  {
    return std::unique_ptr<ArrayElems> (clone_array_elems_impl ());
  }

  virtual std::string as_string () const = 0;

  virtual void accept_vis (ASTVisitor &vis) = 0;

  NodeId get_node_id () const { return node_id; }

protected:
  ArrayElems () : node_id (Analysis::Mappings::get ()->get_next_node_id ()) {}

  // pure virtual clone implementation
  virtual ArrayElems *clone_array_elems_impl () const = 0;

  NodeId node_id;
};

// Value array elements
class ArrayElemsValues : public ArrayElems
{
  std::vector<std::unique_ptr<Expr> > values;
  location_t locus;

public:
  ArrayElemsValues (std::vector<std::unique_ptr<Expr> > elems, location_t locus)
    : ArrayElems (), values (std::move (elems)), locus (locus)
  {}

  // copy constructor with vector clone
  ArrayElemsValues (ArrayElemsValues const &other)
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

  void accept_vis (ASTVisitor &vis) override;

  // TODO: this mutable getter seems really dodgy. Think up better way.
  const std::vector<std::unique_ptr<Expr> > &get_values () const
  {
    return values;
  }
  std::vector<std::unique_ptr<Expr> > &get_values () { return values; }

  size_t get_num_values () const { return values.size (); }

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
  location_t locus;

public:
  // Constructor requires pointers for polymorphism
  ArrayElemsCopied (std::unique_ptr<Expr> copied_elem,
		    std::unique_ptr<Expr> copy_amount, location_t locus)
    : ArrayElems (), elem_to_copy (std::move (copied_elem)),
      num_copies (std::move (copy_amount)), locus (locus)
  {}

  // Copy constructor required due to unique_ptr - uses custom clone
  ArrayElemsCopied (ArrayElemsCopied const &other)
    : elem_to_copy (other.elem_to_copy->clone_expr ()),
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

  void accept_vis (ASTVisitor &vis) override;

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<Expr> &get_elem_to_copy ()
  {
    rust_assert (elem_to_copy != nullptr);
    return elem_to_copy;
  }

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<Expr> &get_num_copies ()
  {
    rust_assert (num_copies != nullptr);
    return num_copies;
  }

protected:
  ArrayElemsCopied *clone_array_elems_impl () const override
  {
    return new ArrayElemsCopied (*this);
  }
};

// Array definition-ish expression
class ArrayExpr : public ExprWithoutBlock
{
  std::vector<Attribute> outer_attrs;
  std::vector<Attribute> inner_attrs;
  std::unique_ptr<ArrayElems> internal_elements;
  location_t locus;

  // TODO: find another way to store this to save memory?
  bool marked_for_strip = false;

public:
  std::string as_string () const override;

  const std::vector<Attribute> &get_inner_attrs () const { return inner_attrs; }
  std::vector<Attribute> &get_inner_attrs () { return inner_attrs; }

  const std::vector<Attribute> &get_outer_attrs () const { return outer_attrs; }
  std::vector<Attribute> &get_outer_attrs () override { return outer_attrs; }

  void set_outer_attrs (std::vector<Attribute> new_attrs) override
  {
    outer_attrs = std::move (new_attrs);
  }

  // Constructor requires ArrayElems pointer
  ArrayExpr (std::unique_ptr<ArrayElems> array_elems,
	     std::vector<Attribute> inner_attribs,
	     std::vector<Attribute> outer_attribs, location_t locus)
    : outer_attrs (std::move (outer_attribs)),
      inner_attrs (std::move (inner_attribs)),
      internal_elements (std::move (array_elems)), locus (locus)
  {
    rust_assert (internal_elements != nullptr);
  }

  // Copy constructor requires cloning ArrayElems for polymorphism to hold
  ArrayExpr (ArrayExpr const &other)
    : ExprWithoutBlock (other), outer_attrs (other.outer_attrs),
      inner_attrs (other.inner_attrs), locus (other.locus),
      marked_for_strip (other.marked_for_strip)
  {
    internal_elements = other.internal_elements->clone_array_elems ();
    rust_assert (internal_elements != nullptr);
  }

  // Overload assignment operator to clone internal_elements
  ArrayExpr &operator= (ArrayExpr const &other)
  {
    ExprWithoutBlock::operator= (other);
    inner_attrs = other.inner_attrs;
    locus = other.locus;
    marked_for_strip = other.marked_for_strip;
    outer_attrs = other.outer_attrs;

    internal_elements = other.internal_elements->clone_array_elems ();

    rust_assert (internal_elements != nullptr);
    return *this;
  }

  // move constructors
  ArrayExpr (ArrayExpr &&other) = default;
  ArrayExpr &operator= (ArrayExpr &&other) = default;

  location_t get_locus () const override final { return locus; }

  void accept_vis (ASTVisitor &vis) override;

  // Can't think of any invalid invariants, so store boolean.
  void mark_for_strip () override { marked_for_strip = true; }
  bool is_marked_for_strip () const override { return marked_for_strip; }

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<ArrayElems> &get_array_elems ()
  {
    rust_assert (internal_elements != nullptr);
    return internal_elements;
  }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  ArrayExpr *clone_expr_without_block_impl () const override
  {
    return new ArrayExpr (*this);
  }
};

// Aka IndexExpr (also applies to slices)
/* Apparently a[b] is equivalent to *std::ops::Index::index(&a, b) or
 * *std::ops::Index::index_mut(&mut a, b) */
/* Also apparently deref operations on a will be repeatedly applied to find an
 * implementation */
class ArrayIndexExpr : public ExprWithoutBlock
{
  std::vector<Attribute> outer_attrs;
  std::unique_ptr<Expr> array_expr;
  std::unique_ptr<Expr> index_expr;
  location_t locus;

public:
  std::string as_string () const override;

  ArrayIndexExpr (std::unique_ptr<Expr> array_expr,
		  std::unique_ptr<Expr> array_index_expr,
		  std::vector<Attribute> outer_attribs, location_t locus)
    : outer_attrs (std::move (outer_attribs)),
      array_expr (std::move (array_expr)),
      index_expr (std::move (array_index_expr)), locus (locus)
  {}

  // Copy constructor requires special cloning due to unique_ptr
  ArrayIndexExpr (ArrayIndexExpr const &other)
    : ExprWithoutBlock (other), outer_attrs (other.outer_attrs),
      locus (other.locus)
  {
    // guard to prevent null dereference (only required if error state)
    if (other.array_expr != nullptr)
      array_expr = other.array_expr->clone_expr ();
    if (other.index_expr != nullptr)
      index_expr = other.index_expr->clone_expr ();
  }

  // Overload assignment operator to clone unique_ptrs
  ArrayIndexExpr &operator= (ArrayIndexExpr const &other)
  {
    ExprWithoutBlock::operator= (other);
    outer_attrs = other.outer_attrs;
    locus = other.locus;

    // guard to prevent null dereference (only required if error state)
    if (other.array_expr != nullptr)
      array_expr = other.array_expr->clone_expr ();
    else
      array_expr = nullptr;
    if (other.index_expr != nullptr)
      index_expr = other.index_expr->clone_expr ();
    else
      index_expr = nullptr;

    return *this;
  }

  // move constructors
  ArrayIndexExpr (ArrayIndexExpr &&other) = default;
  ArrayIndexExpr &operator= (ArrayIndexExpr &&other) = default;

  location_t get_locus () const override final { return locus; }

  void accept_vis (ASTVisitor &vis) override;

  // Invalid if either expr is null, so base stripping on that.
  void mark_for_strip () override
  {
    array_expr = nullptr;
    index_expr = nullptr;
  }
  bool is_marked_for_strip () const override
  {
    return array_expr == nullptr && index_expr == nullptr;
  }

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<Expr> &get_array_expr ()
  {
    rust_assert (array_expr != nullptr);
    return array_expr;
  }

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<Expr> &get_index_expr ()
  {
    rust_assert (index_expr != nullptr);
    return index_expr;
  }

  const std::vector<Attribute> &get_outer_attrs () const { return outer_attrs; }
  std::vector<Attribute> &get_outer_attrs () override { return outer_attrs; }

  void set_outer_attrs (std::vector<Attribute> new_attrs) override
  {
    outer_attrs = std::move (new_attrs);
  }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  ArrayIndexExpr *clone_expr_without_block_impl () const override
  {
    return new ArrayIndexExpr (*this);
  }
};

// AST representation of a tuple
class TupleExpr : public ExprWithoutBlock
{
  std::vector<Attribute> outer_attrs;
  std::vector<Attribute> inner_attrs;
  std::vector<std::unique_ptr<Expr> > tuple_elems;
  location_t locus;

  // TODO: find another way to store this to save memory?
  bool marked_for_strip = false;

public:
  std::string as_string () const override;

  const std::vector<Attribute> &get_inner_attrs () const { return inner_attrs; }
  std::vector<Attribute> &get_inner_attrs () { return inner_attrs; }

  const std::vector<Attribute> &get_outer_attrs () const { return outer_attrs; }
  std::vector<Attribute> &get_outer_attrs () override { return outer_attrs; }

  void set_outer_attrs (std::vector<Attribute> new_attrs) override
  {
    outer_attrs = std::move (new_attrs);
  }

  TupleExpr (std::vector<std::unique_ptr<Expr> > tuple_elements,
	     std::vector<Attribute> inner_attribs,
	     std::vector<Attribute> outer_attribs, location_t locus)
    : outer_attrs (std::move (outer_attribs)),
      inner_attrs (std::move (inner_attribs)),
      tuple_elems (std::move (tuple_elements)), locus (locus)
  {}

  // copy constructor with vector clone
  TupleExpr (TupleExpr const &other)
    : ExprWithoutBlock (other), outer_attrs (other.outer_attrs),
      inner_attrs (other.inner_attrs), locus (other.locus),
      marked_for_strip (other.marked_for_strip)
  {
    tuple_elems.reserve (other.tuple_elems.size ());
    for (const auto &e : other.tuple_elems)
      tuple_elems.push_back (e->clone_expr ());
  }

  // overloaded assignment operator to vector clone
  TupleExpr &operator= (TupleExpr const &other)
  {
    ExprWithoutBlock::operator= (other);
    outer_attrs = other.outer_attrs;
    inner_attrs = other.inner_attrs;
    locus = other.locus;
    marked_for_strip = other.marked_for_strip;

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

  void accept_vis (ASTVisitor &vis) override;

  // Can't think of any invalid invariants, so store boolean.
  void mark_for_strip () override { marked_for_strip = true; }
  bool is_marked_for_strip () const override { return marked_for_strip; }

  // TODO: this mutable getter seems really dodgy. Think up better way.
  const std::vector<std::unique_ptr<Expr> > &get_tuple_elems () const
  {
    return tuple_elems;
  }
  std::vector<std::unique_ptr<Expr> > &get_tuple_elems ()
  {
    return tuple_elems;
  }

  bool is_unit () const { return tuple_elems.size () == 0; }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  TupleExpr *clone_expr_without_block_impl () const override
  {
    return new TupleExpr (*this);
  }
};

// aka TupleIndexingExpr
// AST representation of a tuple indexing expression
class TupleIndexExpr : public ExprWithoutBlock
{
  std::vector<Attribute> outer_attrs;
  std::unique_ptr<Expr> tuple_expr;
  // TupleIndex is a decimal int literal with no underscores or suffix
  TupleIndex tuple_index;

  location_t locus;

  // i.e. pair.0

public:
  std::string as_string () const override;

  TupleIndex get_tuple_index () const { return tuple_index; }

  TupleIndexExpr (std::unique_ptr<Expr> tuple_expr, TupleIndex index,
		  std::vector<Attribute> outer_attribs, location_t locus)
    : outer_attrs (std::move (outer_attribs)),
      tuple_expr (std::move (tuple_expr)), tuple_index (index), locus (locus)
  {}

  // Copy constructor requires a clone for tuple_expr
  TupleIndexExpr (TupleIndexExpr const &other)
    : ExprWithoutBlock (other), outer_attrs (other.outer_attrs),
      tuple_index (other.tuple_index), locus (other.locus)
  {
    // guard to prevent null dereference (only required if error state)
    if (other.tuple_expr != nullptr)
      tuple_expr = other.tuple_expr->clone_expr ();
  }

  // Overload assignment operator in order to clone
  TupleIndexExpr &operator= (TupleIndexExpr const &other)
  {
    ExprWithoutBlock::operator= (other);
    tuple_index = other.tuple_index;
    locus = other.locus;
    outer_attrs = other.outer_attrs;

    // guard to prevent null dereference (only required if error state)
    if (other.tuple_expr != nullptr)
      tuple_expr = other.tuple_expr->clone_expr ();
    else
      tuple_expr = nullptr;

    return *this;
  }

  // move constructors
  TupleIndexExpr (TupleIndexExpr &&other) = default;
  TupleIndexExpr &operator= (TupleIndexExpr &&other) = default;

  location_t get_locus () const override final { return locus; }

  void accept_vis (ASTVisitor &vis) override;

  // Invalid if tuple expr is null, so base stripping on that.
  void mark_for_strip () override { tuple_expr = nullptr; }
  bool is_marked_for_strip () const override { return tuple_expr == nullptr; }

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<Expr> &get_tuple_expr ()
  {
    rust_assert (tuple_expr != nullptr);
    return tuple_expr;
  }

  const std::vector<Attribute> &get_outer_attrs () const { return outer_attrs; }
  std::vector<Attribute> &get_outer_attrs () override { return outer_attrs; }

  void set_outer_attrs (std::vector<Attribute> new_attrs) override
  {
    outer_attrs = std::move (new_attrs);
  }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  TupleIndexExpr *clone_expr_without_block_impl () const override
  {
    return new TupleIndexExpr (*this);
  }
};

// Base struct/tuple/union value creator AST node (abstract)
class StructExpr : public ExprWithoutBlock
{
  std::vector<Attribute> outer_attrs;
  PathInExpression struct_name;

protected:
  // Protected constructor to allow initialising struct_name
  StructExpr (PathInExpression struct_path,
	      std::vector<Attribute> outer_attribs)
    : outer_attrs (std::move (outer_attribs)),
      struct_name (std::move (struct_path))
  {}

public:
  const PathInExpression &get_struct_name () const { return struct_name; }
  PathInExpression &get_struct_name () { return struct_name; }

  std::string as_string () const override;

  // Invalid if path is empty, so base stripping on that.
  void mark_for_strip () override
  {
    struct_name = PathInExpression::create_error ();
  }
  bool is_marked_for_strip () const override { return struct_name.is_error (); }

  const std::vector<Attribute> &get_outer_attrs () const { return outer_attrs; }
  std::vector<Attribute> &get_outer_attrs () override { return outer_attrs; }

  void set_outer_attrs (std::vector<Attribute> new_attrs) override
  {
    outer_attrs = std::move (new_attrs);
  }
};

// Actual AST node of the struct creator (with no fields). Not abstract!
class StructExprStruct : public StructExpr
{
  std::vector<Attribute> inner_attrs;

  location_t locus;

public:
  std::string as_string () const override;

  const std::vector<Attribute> &get_inner_attrs () const { return inner_attrs; }
  std::vector<Attribute> &get_inner_attrs () { return inner_attrs; }

  // Constructor has to call protected constructor of base class
  StructExprStruct (PathInExpression struct_path,
		    std::vector<Attribute> inner_attribs,
		    std::vector<Attribute> outer_attribs, location_t locus)
    : StructExpr (std::move (struct_path), std::move (outer_attribs)),
      inner_attrs (std::move (inner_attribs)), locus (locus)
  {}

  location_t get_locus () const override final { return locus; }

  void accept_vis (ASTVisitor &vis) override;

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  StructExprStruct *clone_expr_without_block_impl () const override
  {
    return new StructExprStruct (*this);
  }
};

/* AST node representing expression used to fill a struct's fields from another
 * struct */
struct StructBase
{
private:
  std::unique_ptr<Expr> base_struct;
  location_t locus;

public:
  StructBase (std::unique_ptr<Expr> base_struct_ptr, location_t locus)
    : base_struct (std::move (base_struct_ptr)), locus (locus)
  {}

  // Copy constructor requires clone
  StructBase (StructBase const &other)
  {
    /* HACK: gets around base_struct pointer being null (e.g. if no struct base
     * exists) */
    if (other.base_struct != nullptr)
      base_struct = other.base_struct->clone_expr ();
  }

  // Destructor
  ~StructBase () = default;

  // Overload assignment operator to clone base_struct
  StructBase &operator= (StructBase const &other)
  {
    // prevent null pointer dereference
    if (other.base_struct != nullptr)
      base_struct = other.base_struct->clone_expr ();
    else
      base_struct = nullptr;

    return *this;
  }

  // move constructors
  StructBase (StructBase &&other) = default;
  StructBase &operator= (StructBase &&other) = default;

  // Returns a null expr-ed StructBase - error state
  static StructBase error () { return StructBase (nullptr, UNDEF_LOCATION); }

  // Returns whether StructBase is in error state
  bool is_invalid () const { return base_struct == nullptr; }

  std::string as_string () const;

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<Expr> &get_base_struct ()
  {
    rust_assert (base_struct != nullptr);
    return base_struct;
  }
};

/* Base AST node for a single struct expression field (in struct instance
 * creation) - abstract */
class StructExprField
{
public:
  virtual ~StructExprField () {}

  // Unique pointer custom clone function
  std::unique_ptr<StructExprField> clone_struct_expr_field () const
  {
    return std::unique_ptr<StructExprField> (clone_struct_expr_field_impl ());
  }

  virtual std::string as_string () const = 0;

  virtual void accept_vis (ASTVisitor &vis) = 0;

  virtual location_t get_locus () const = 0;

  NodeId get_node_id () const { return node_id; }

protected:
  // pure virtual clone implementation
  virtual StructExprField *clone_struct_expr_field_impl () const = 0;

  StructExprField () : node_id (Analysis::Mappings::get ()->get_next_node_id ())
  {}

  NodeId node_id;
};

// Identifier-only variant of StructExprField AST node
class StructExprFieldIdentifier : public StructExprField
{
  Identifier field_name;
  location_t locus;

public:
  StructExprFieldIdentifier (Identifier field_identifier, location_t locus)
    : StructExprField (), field_name (std::move (field_identifier)),
      locus (locus)
  {}

  std::string as_string () const override { return field_name.as_string (); }

  location_t get_locus () const override final { return locus; }

  void accept_vis (ASTVisitor &vis) override;

  Identifier get_field_name () const { return field_name; }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  StructExprFieldIdentifier *clone_struct_expr_field_impl () const override
  {
    return new StructExprFieldIdentifier (*this);
  }
};

/* Base AST node for a single struct expression field with an assigned value -
 * abstract */
class StructExprFieldWithVal : public StructExprField
{
  std::unique_ptr<Expr> value;

protected:
  StructExprFieldWithVal (std::unique_ptr<Expr> field_value)
    : StructExprField (), value (std::move (field_value))
  {}

  // Copy constructor requires clone
  StructExprFieldWithVal (StructExprFieldWithVal const &other)
    : value (other.value->clone_expr ())
  {}

  // Overload assignment operator to clone unique_ptr
  StructExprFieldWithVal &operator= (StructExprFieldWithVal const &other)
  {
    value = other.value->clone_expr ();

    return *this;
  }

  // move constructors
  StructExprFieldWithVal (StructExprFieldWithVal &&other) = default;
  StructExprFieldWithVal &operator= (StructExprFieldWithVal &&other) = default;

public:
  std::string as_string () const override;

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<Expr> &get_value ()
  {
    rust_assert (value != nullptr);
    return value;
  }
};

// Identifier and value variant of StructExprField AST node
class StructExprFieldIdentifierValue : public StructExprFieldWithVal
{
  Identifier field_name;
  location_t locus;

public:
  StructExprFieldIdentifierValue (Identifier field_identifier,
				  std::unique_ptr<Expr> field_value,
				  location_t locus)
    : StructExprFieldWithVal (std::move (field_value)),
      field_name (std::move (field_identifier)), locus (locus)
  {}

  std::string as_string () const override;

  void accept_vis (ASTVisitor &vis) override;

  std::string get_field_name () const { return field_name.as_string (); }

  location_t get_locus () const override final { return locus; }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  StructExprFieldIdentifierValue *clone_struct_expr_field_impl () const override
  {
    return new StructExprFieldIdentifierValue (*this);
  }
};

// Tuple index and value variant of StructExprField AST node
class StructExprFieldIndexValue : public StructExprFieldWithVal
{
  TupleIndex index;
  location_t locus;

public:
  StructExprFieldIndexValue (TupleIndex tuple_index,
			     std::unique_ptr<Expr> field_value,
			     location_t locus)
    : StructExprFieldWithVal (std::move (field_value)), index (tuple_index),
      locus (locus)
  {}

  std::string as_string () const override;

  void accept_vis (ASTVisitor &vis) override;

  TupleIndex get_index () const { return index; }

  location_t get_locus () const override final { return locus; }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  StructExprFieldIndexValue *clone_struct_expr_field_impl () const override
  {
    return new StructExprFieldIndexValue (*this);
  }
};

// AST node of a struct creator with fields
class StructExprStructFields : public StructExprStruct
{
  // std::vector<StructExprField> fields;
  std::vector<std::unique_ptr<StructExprField> > fields;

  // bool has_struct_base;
  StructBase struct_base;

public:
  std::string as_string () const override;

  bool has_struct_base () const { return !struct_base.is_invalid (); }

  // Constructor for StructExprStructFields when no struct base is used
  StructExprStructFields (
    PathInExpression struct_path,
    std::vector<std::unique_ptr<StructExprField> > expr_fields,
    location_t locus, StructBase base_struct = StructBase::error (),
    std::vector<Attribute> inner_attribs = std::vector<Attribute> (),
    std::vector<Attribute> outer_attribs = std::vector<Attribute> ())
    : StructExprStruct (std::move (struct_path), std::move (inner_attribs),
			std::move (outer_attribs), locus),
      fields (std::move (expr_fields)), struct_base (std::move (base_struct))
  {}

  // copy constructor with vector clone
  StructExprStructFields (StructExprStructFields const &other)
    : StructExprStruct (other), struct_base (other.struct_base)
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

    fields.reserve (other.fields.size ());
    for (const auto &e : other.fields)
      fields.push_back (e->clone_struct_expr_field ());

    return *this;
  }

  // move constructors
  StructExprStructFields (StructExprStructFields &&other) = default;
  StructExprStructFields &operator= (StructExprStructFields &&other) = default;

  void accept_vis (ASTVisitor &vis) override;

  // TODO: this mutable getter seems really dodgy. Think up better way.
  std::vector<std::unique_ptr<StructExprField> > &get_fields ()
  {
    return fields;
  }
  const std::vector<std::unique_ptr<StructExprField> > &get_fields () const
  {
    return fields;
  }

  StructBase &get_struct_base () { return struct_base; }
  const StructBase &get_struct_base () const { return struct_base; }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  StructExprStructFields *clone_expr_without_block_impl () const override
  {
    return new StructExprStructFields (*this);
  }
};

// AST node of the functional update struct creator
/* TODO: remove and replace with StructExprStructFields, except with empty
 * vector of fields? */
class StructExprStructBase : public StructExprStruct
{
  StructBase struct_base;

public:
  std::string as_string () const override;

  StructExprStructBase (PathInExpression struct_path, StructBase base_struct,
			std::vector<Attribute> inner_attribs,
			std::vector<Attribute> outer_attribs, location_t locus)
    : StructExprStruct (std::move (struct_path), std::move (inner_attribs),
			std::move (outer_attribs), locus),
      struct_base (std::move (base_struct))
  {}

  void accept_vis (ASTVisitor &vis) override;

  StructBase &get_struct_base () { return struct_base; }
  const StructBase &get_struct_base () const { return struct_base; }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  StructExprStructBase *clone_expr_without_block_impl () const override
  {
    return new StructExprStructBase (*this);
  }
};

// Forward decl for Function - used in CallExpr
class Function;

// Function call expression AST node
class CallExpr : public ExprWithoutBlock
{
  std::vector<Attribute> outer_attrs;
  std::unique_ptr<Expr> function;
  std::vector<std::unique_ptr<Expr> > params;
  location_t locus;

public:
  Function *fndeclRef;

  std::string as_string () const override;

  CallExpr (std::unique_ptr<Expr> function_expr,
	    std::vector<std::unique_ptr<Expr> > function_params,
	    std::vector<Attribute> outer_attribs, location_t locus)
    : outer_attrs (std::move (outer_attribs)),
      function (std::move (function_expr)),
      params (std::move (function_params)), locus (locus)
  {}

  // copy constructor requires clone
  CallExpr (CallExpr const &other)
    : ExprWithoutBlock (other), outer_attrs (other.outer_attrs),
      locus (other.locus)
  {
    // guard to prevent null dereference (only required if error state)
    if (other.function != nullptr)
      function = other.function->clone_expr ();

    params.reserve (other.params.size ());
    for (const auto &e : other.params)
      params.push_back (e->clone_expr ());
  }

  // Overload assignment operator to clone
  CallExpr &operator= (CallExpr const &other)
  {
    ExprWithoutBlock::operator= (other);
    locus = other.locus;
    outer_attrs = other.outer_attrs;

    // guard to prevent null dereference (only required if error state)
    if (other.function != nullptr)
      function = other.function->clone_expr ();
    else
      function = nullptr;

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

  void accept_vis (ASTVisitor &vis) override;

  // Invalid if function expr is null, so base stripping on that.
  void mark_for_strip () override { function = nullptr; }
  bool is_marked_for_strip () const override { return function == nullptr; }

  // TODO: this mutable getter seems really dodgy. Think up better way.
  const std::vector<std::unique_ptr<Expr> > &get_params () const
  {
    return params;
  }
  std::vector<std::unique_ptr<Expr> > &get_params () { return params; }

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<Expr> &get_function_expr ()
  {
    rust_assert (function != nullptr);
    return function;
  }

  const std::vector<Attribute> &get_outer_attrs () const { return outer_attrs; }
  std::vector<Attribute> &get_outer_attrs () override { return outer_attrs; }

  void set_outer_attrs (std::vector<Attribute> new_attrs) override
  {
    outer_attrs = std::move (new_attrs);
  }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  CallExpr *clone_expr_without_block_impl () const override
  {
    return new CallExpr (*this);
  }
};

// Method call expression AST node
class MethodCallExpr : public ExprWithoutBlock
{
  std::vector<Attribute> outer_attrs;
  std::unique_ptr<Expr> receiver;
  PathExprSegment method_name;
  std::vector<std::unique_ptr<Expr> > params;
  location_t locus;

public:
  std::string as_string () const override;

  MethodCallExpr (std::unique_ptr<Expr> call_receiver,
		  PathExprSegment method_path,
		  std::vector<std::unique_ptr<Expr> > method_params,
		  std::vector<Attribute> outer_attribs, location_t locus)
    : outer_attrs (std::move (outer_attribs)),
      receiver (std::move (call_receiver)),
      method_name (std::move (method_path)), params (std::move (method_params)),
      locus (locus)
  {}

  // copy constructor required due to cloning
  MethodCallExpr (MethodCallExpr const &other)
    : ExprWithoutBlock (other), outer_attrs (other.outer_attrs),
      method_name (other.method_name), locus (other.locus)
  {
    // guard to prevent null dereference (only required if error state)
    if (other.receiver != nullptr)
      receiver = other.receiver->clone_expr ();

    params.reserve (other.params.size ());
    for (const auto &e : other.params)
      params.push_back (e->clone_expr ());
  }

  // Overload assignment operator to clone receiver object
  MethodCallExpr &operator= (MethodCallExpr const &other)
  {
    ExprWithoutBlock::operator= (other);
    method_name = other.method_name;
    locus = other.locus;
    outer_attrs = other.outer_attrs;

    // guard to prevent null dereference (only required if error state)
    if (other.receiver != nullptr)
      receiver = other.receiver->clone_expr ();
    else
      receiver = nullptr;

    params.reserve (other.params.size ());
    for (const auto &e : other.params)
      params.push_back (e->clone_expr ());

    return *this;
  }

  // move constructors
  MethodCallExpr (MethodCallExpr &&other) = default;
  MethodCallExpr &operator= (MethodCallExpr &&other) = default;

  location_t get_locus () const override final { return locus; }

  void accept_vis (ASTVisitor &vis) override;

  // Invalid if receiver expr is null, so base stripping on that.
  void mark_for_strip () override { receiver = nullptr; }
  bool is_marked_for_strip () const override { return receiver == nullptr; }

  // TODO: this mutable getter seems really dodgy. Think up better way.
  const std::vector<std::unique_ptr<Expr> > &get_params () const
  {
    return params;
  }
  std::vector<std::unique_ptr<Expr> > &get_params () { return params; }

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<Expr> &get_receiver_expr ()
  {
    rust_assert (receiver != nullptr);
    return receiver;
  }

  const PathExprSegment &get_method_name () const { return method_name; }
  PathExprSegment &get_method_name () { return method_name; }

  const std::vector<Attribute> &get_outer_attrs () const { return outer_attrs; }
  std::vector<Attribute> &get_outer_attrs () override { return outer_attrs; }

  void set_outer_attrs (std::vector<Attribute> new_attrs) override
  {
    outer_attrs = std::move (new_attrs);
  }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  MethodCallExpr *clone_expr_without_block_impl () const override
  {
    return new MethodCallExpr (*this);
  }
};

// aka FieldExpression
// Struct or union field access expression AST node
class FieldAccessExpr : public ExprWithoutBlock
{
  std::vector<Attribute> outer_attrs;
  std::unique_ptr<Expr> receiver;
  Identifier field;
  location_t locus;

public:
  std::string as_string () const override;

  FieldAccessExpr (std::unique_ptr<Expr> field_access_receiver,
		   Identifier field_name, std::vector<Attribute> outer_attribs,
		   location_t locus)
    : outer_attrs (std::move (outer_attribs)),
      receiver (std::move (field_access_receiver)),
      field (std::move (field_name)), locus (locus)
  {}

  // Copy constructor required due to unique_ptr cloning
  FieldAccessExpr (FieldAccessExpr const &other)
    : ExprWithoutBlock (other), outer_attrs (other.outer_attrs),
      field (other.field), locus (other.locus)
  {
    // guard to prevent null dereference (only required if error state)
    if (other.receiver != nullptr)
      receiver = other.receiver->clone_expr ();
  }

  // Overload assignment operator to clone unique_ptr
  FieldAccessExpr &operator= (FieldAccessExpr const &other)
  {
    ExprWithoutBlock::operator= (other);
    field = other.field;
    locus = other.locus;
    outer_attrs = other.outer_attrs;

    // guard to prevent null dereference (only required if error state)
    if (other.receiver != nullptr)
      receiver = other.receiver->clone_expr ();
    else
      receiver = nullptr;

    return *this;
  }

  // move constructors
  FieldAccessExpr (FieldAccessExpr &&other) = default;
  FieldAccessExpr &operator= (FieldAccessExpr &&other) = default;

  location_t get_locus () const override final { return locus; }

  void accept_vis (ASTVisitor &vis) override;

  // Invalid if receiver expr is null, so base stripping on that.
  void mark_for_strip () override { receiver = nullptr; }
  bool is_marked_for_strip () const override { return receiver == nullptr; }

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<Expr> &get_receiver_expr ()
  {
    rust_assert (receiver != nullptr);
    return receiver;
  }

  Identifier get_field_name () const { return field; }

  const std::vector<Attribute> &get_outer_attrs () const { return outer_attrs; }
  std::vector<Attribute> &get_outer_attrs () override { return outer_attrs; }

  void set_outer_attrs (std::vector<Attribute> new_attrs) override
  {
    outer_attrs = std::move (new_attrs);
  }

protected:
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
  std::vector<Attribute> outer_attrs;
  std::unique_ptr<Pattern> pattern;
  std::unique_ptr<Type> type;
  location_t locus;

public:
  // Returns whether the type of the parameter has been given.
  bool has_type_given () const { return type != nullptr; }

  bool has_outer_attrs () const { return !outer_attrs.empty (); }

  // Constructor for closure parameter
  ClosureParam (std::unique_ptr<Pattern> param_pattern, location_t locus,
		std::unique_ptr<Type> param_type = nullptr,
		std::vector<Attribute> outer_attrs = {})
    : outer_attrs (std::move (outer_attrs)),
      pattern (std::move (param_pattern)), type (std::move (param_type)),
      locus (locus)
  {}

  // Copy constructor required due to cloning as a result of unique_ptrs
  ClosureParam (ClosureParam const &other) : outer_attrs (other.outer_attrs)
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

  // Returns whether closure parameter is in an error state.
  bool is_error () const { return pattern == nullptr; }

  // Creates an error state closure parameter.
  static ClosureParam create_error ()
  {
    return ClosureParam (nullptr, UNDEF_LOCATION);
  }

  std::string as_string () const;

  const std::vector<Attribute> &get_outer_attrs () const { return outer_attrs; }
  std::vector<Attribute> &get_outer_attrs () { return outer_attrs; }

  std::unique_ptr<Pattern> &get_pattern ()
  {
    rust_assert (pattern != nullptr);
    return pattern;
  }

  std::unique_ptr<Type> &get_type ()
  {
    rust_assert (has_type_given ());
    return type;
  }

  location_t get_locus () const { return locus; }
};

// Base closure definition expression AST node - abstract
class ClosureExpr : public ExprWithoutBlock
{
  std::vector<Attribute> outer_attrs;
  bool has_move;
  std::vector<ClosureParam> params; // may be empty
  location_t locus;

protected:
  ClosureExpr (std::vector<ClosureParam> closure_params, bool has_move,
	       std::vector<Attribute> outer_attribs, location_t locus)
    : outer_attrs (std::move (outer_attribs)), has_move (has_move),
      params (std::move (closure_params)), locus (locus)
  {}

public:
  std::string as_string () const override;

  location_t get_locus () const override final { return locus; }

  // TODO: this mutable getter seems really dodgy. Think up better way.
  const std::vector<ClosureParam> &get_params () const { return params; }
  std::vector<ClosureParam> &get_params () { return params; }

  const std::vector<Attribute> &get_outer_attrs () const { return outer_attrs; }
  std::vector<Attribute> &get_outer_attrs () override { return outer_attrs; }

  void set_outer_attrs (std::vector<Attribute> new_attrs) override
  {
    outer_attrs = std::move (new_attrs);
  }

  bool get_has_move () const { return has_move; }
};

// Represents a non-type-specified closure expression AST node
class ClosureExprInner : public ClosureExpr
{
  std::unique_ptr<Expr> closure_inner;

public:
  std::string as_string () const override;

  // Constructor for a ClosureExprInner
  ClosureExprInner (std::unique_ptr<Expr> closure_inner_expr,
		    std::vector<ClosureParam> closure_params, location_t locus,
		    bool is_move = false,
		    std::vector<Attribute> outer_attribs
		    = std::vector<Attribute> ())
    : ClosureExpr (std::move (closure_params), is_move,
		   std::move (outer_attribs), locus),
      closure_inner (std::move (closure_inner_expr))
  {}

  // Copy constructor must be defined to allow copying via cloning of unique_ptr
  ClosureExprInner (ClosureExprInner const &other) : ClosureExpr (other)
  {
    // guard to prevent null dereference (only required if error state)
    if (other.closure_inner != nullptr)
      closure_inner = other.closure_inner->clone_expr ();
  }

  // Overload assignment operator to clone closure_inner
  ClosureExprInner &operator= (ClosureExprInner const &other)
  {
    ClosureExpr::operator= (other);
    // params = other.params;
    // has_move = other.has_move;
    // outer_attrs = other.outer_attrs;

    // guard to prevent null dereference (only required if error state)
    if (other.closure_inner != nullptr)
      closure_inner = other.closure_inner->clone_expr ();
    else
      closure_inner = nullptr;

    return *this;
  }

  // move constructors
  ClosureExprInner (ClosureExprInner &&other) = default;
  ClosureExprInner &operator= (ClosureExprInner &&other) = default;

  void accept_vis (ASTVisitor &vis) override;

  // Invalid if inner expr is null, so base stripping on that.
  void mark_for_strip () override { closure_inner = nullptr; }
  bool is_marked_for_strip () const override
  {
    return closure_inner == nullptr;
  }

  std::unique_ptr<Expr> &get_definition_expr ()
  {
    rust_assert (closure_inner != nullptr);
    return closure_inner;
  }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  ClosureExprInner *clone_expr_without_block_impl () const override
  {
    return new ClosureExprInner (*this);
  }
};

// A block AST node
class BlockExpr : public ExprWithBlock
{
  std::vector<Attribute> outer_attrs;
  std::vector<Attribute> inner_attrs;
  std::vector<std::unique_ptr<Stmt> > statements;
  std::unique_ptr<Expr> expr;
  LoopLabel label;
  location_t start_locus;
  location_t end_locus;
  bool marked_for_strip = false;

public:
  std::string as_string () const override;

  // Returns whether the block contains statements.
  bool has_statements () const { return !statements.empty (); }

  // Returns whether the block contains a final expression.
  bool has_tail_expr () const { return expr != nullptr; }

  BlockExpr (std::vector<std::unique_ptr<Stmt> > block_statements,
	     std::unique_ptr<Expr> block_expr,
	     std::vector<Attribute> inner_attribs,
	     std::vector<Attribute> outer_attribs, LoopLabel label,
	     location_t start_locus, location_t end_locus)
    : outer_attrs (std::move (outer_attribs)),
      inner_attrs (std::move (inner_attribs)),
      statements (std::move (block_statements)), expr (std::move (block_expr)),
      label (std::move (label)), start_locus (start_locus),
      end_locus (end_locus)
  {}

  // Copy constructor with clone
  BlockExpr (BlockExpr const &other)
    : ExprWithBlock (other), outer_attrs (other.outer_attrs),
      inner_attrs (other.inner_attrs), label (other.label),
      start_locus (other.start_locus), end_locus (other.end_locus),
      marked_for_strip (other.marked_for_strip)
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
    inner_attrs = other.inner_attrs;
    start_locus = other.start_locus;
    end_locus = other.end_locus;
    marked_for_strip = other.marked_for_strip;
    outer_attrs = other.outer_attrs;

    // guard to protect from null pointer dereference
    if (other.expr != nullptr)
      expr = other.expr->clone_expr ();
    else
      expr = nullptr;

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

  void accept_vis (ASTVisitor &vis) override;

  // Can be completely empty, so have to have a separate flag.
  void mark_for_strip () override { marked_for_strip = true; }
  bool is_marked_for_strip () const override { return marked_for_strip; }

  size_t num_statements () const { return statements.size (); }

  // TODO: this mutable getter seems really dodgy. Think up better way.
  const std::vector<Attribute> &get_inner_attrs () const { return inner_attrs; }
  std::vector<Attribute> &get_inner_attrs () { return inner_attrs; }

  const std::vector<std::unique_ptr<Stmt> > &get_statements () const
  {
    return statements;
  }
  std::vector<std::unique_ptr<Stmt> > &get_statements () { return statements; }

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<Expr> &get_tail_expr ()
  {
    rust_assert (has_tail_expr ());
    return expr;
  }

  std::unique_ptr<Expr> take_tail_expr ()
  {
    rust_assert (has_tail_expr ());
    return std::move (expr);
  }

  void set_tail_expr (std::unique_ptr<Expr> expr)
  {
    this->expr = std::move (expr);
  }

  // Removes the tail expression from the block.
  void strip_tail_expr () { expr = nullptr; }
  // Normalizes a trailing statement without a semicolon to a tail expression.
  void normalize_tail_expr ();

  void try_convert_last_stmt ();

  const std::vector<Attribute> &get_outer_attrs () const { return outer_attrs; }
  std::vector<Attribute> &get_outer_attrs () override { return outer_attrs; }

  void set_outer_attrs (std::vector<Attribute> new_attrs) override
  {
    outer_attrs = std::move (new_attrs);
  }

  bool has_label () { return !label.is_error (); }
  LoopLabel &get_label () { return label; }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  BlockExpr *clone_expr_with_block_impl () const final override
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

// Represents a type-specified closure expression AST node
class ClosureExprInnerTyped : public ClosureExpr
{
  // TODO: spec says typenobounds
  std::unique_ptr<Type> return_type;
  std::unique_ptr<BlockExpr>
    expr; // only used because may be polymorphic in future

public:
  std::string as_string () const override;

  // Constructor potentially with a move
  ClosureExprInnerTyped (std::unique_ptr<Type> closure_return_type,
			 std::unique_ptr<BlockExpr> closure_expr,
			 std::vector<ClosureParam> closure_params,
			 location_t locus, bool is_move = false,
			 std::vector<Attribute> outer_attribs
			 = std::vector<Attribute> ())
    : ClosureExpr (std::move (closure_params), is_move,
		   std::move (outer_attribs), locus),
      return_type (std::move (closure_return_type)),
      expr (std::move (closure_expr))
  {}

  // Copy constructor requires cloning
  ClosureExprInnerTyped (ClosureExprInnerTyped const &other)
    : ClosureExpr (other)
  {
    // guard to prevent null dereference (only required if error state)
    if (other.expr != nullptr)
      expr = other.expr->clone_block_expr ();
    if (other.return_type != nullptr)
      return_type = other.return_type->clone_type ();
  }

  // Overload assignment operator to clone unique_ptrs
  ClosureExprInnerTyped &operator= (ClosureExprInnerTyped const &other)
  {
    ClosureExpr::operator= (other);
    // params = other.params;
    // has_move = other.has_move;
    // outer_attrs = other.outer_attrs;

    // guard to prevent null dereference (only required if error state)
    if (other.expr != nullptr)
      expr = other.expr->clone_block_expr ();
    else
      expr = nullptr;
    if (other.return_type != nullptr)
      return_type = other.return_type->clone_type ();
    else
      return_type = nullptr;

    return *this;
  }

  // move constructors
  ClosureExprInnerTyped (ClosureExprInnerTyped &&other) = default;
  ClosureExprInnerTyped &operator= (ClosureExprInnerTyped &&other) = default;

  void accept_vis (ASTVisitor &vis) override;

  /* Invalid if inner expr is null, so base stripping on that. Technically,
   * type should also not be null. */
  void mark_for_strip () override { expr = nullptr; }
  bool is_marked_for_strip () const override { return expr == nullptr; }

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<BlockExpr> &get_definition_block ()
  {
    rust_assert (expr != nullptr);
    return expr;
  }

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<Type> &get_return_type ()
  {
    rust_assert (return_type != nullptr);
    return return_type;
  }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  ClosureExprInnerTyped *clone_expr_without_block_impl () const override
  {
    return new ClosureExprInnerTyped (*this);
  }
};

// AST node representing continue expression within loops
class ContinueExpr : public ExprWithoutBlock
{
  std::vector<Attribute> outer_attrs;
  Lifetime label;
  location_t locus;

  // TODO: find another way to store this to save memory?
  bool marked_for_strip = false;

public:
  std::string as_string () const override;

  // Returns true if the continue expr has a label.
  bool has_label () const { return !label.is_error (); }

  // Constructor for a ContinueExpr with a label.
  ContinueExpr (Lifetime label, std::vector<Attribute> outer_attribs,
		location_t locus)
    : outer_attrs (std::move (outer_attribs)), label (std::move (label)),
      locus (locus)
  {}

  location_t get_locus () const override final { return locus; }

  void accept_vis (ASTVisitor &vis) override;

  // Can't think of any invalid invariants, so store boolean.
  void mark_for_strip () override { marked_for_strip = true; }
  bool is_marked_for_strip () const override { return marked_for_strip; }

  const std::vector<Attribute> &get_outer_attrs () const { return outer_attrs; }
  std::vector<Attribute> &get_outer_attrs () override { return outer_attrs; }

  void set_outer_attrs (std::vector<Attribute> new_attrs) override
  {
    outer_attrs = std::move (new_attrs);
  }

  Lifetime &get_label () { return label; }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  ContinueExpr *clone_expr_without_block_impl () const override
  {
    return new ContinueExpr (*this);
  }
};
// TODO: merge "break" and "continue"? Or even merge in "return"?

// AST node representing break expression within loops
class BreakExpr : public ExprWithoutBlock
{
  std::vector<Attribute> outer_attrs;
  LoopLabel label;
  std::unique_ptr<Expr> break_expr;
  location_t locus;

  // TODO: find another way to store this to save memory?
  bool marked_for_strip = false;

public:
  std::string as_string () const override;

  // Returns whether the break expression has a label or not.
  bool has_label () const { return !label.is_error (); }

  /* Returns whether the break expression has an expression used in the break or
   * not. */
  bool has_break_expr () const { return break_expr != nullptr; }

  // Constructor for a break expression
  BreakExpr (LoopLabel break_label, std::unique_ptr<Expr> expr_in_break,
	     std::vector<Attribute> outer_attribs, location_t locus)
    : outer_attrs (std::move (outer_attribs)), label (std::move (break_label)),
      break_expr (std::move (expr_in_break)), locus (locus)
  {}

  // Copy constructor defined to use clone for unique pointer
  BreakExpr (BreakExpr const &other)
    : ExprWithoutBlock (other), outer_attrs (other.outer_attrs),
      label (other.label), locus (other.locus),
      marked_for_strip (other.marked_for_strip)
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
    locus = other.locus;
    marked_for_strip = other.marked_for_strip;
    outer_attrs = other.outer_attrs;

    // guard to protect from null pointer dereference
    if (other.break_expr != nullptr)
      break_expr = other.break_expr->clone_expr ();
    else
      break_expr = nullptr;

    return *this;
  }

  // move constructors
  BreakExpr (BreakExpr &&other) = default;
  BreakExpr &operator= (BreakExpr &&other) = default;

  location_t get_locus () const override final { return locus; }

  void accept_vis (ASTVisitor &vis) override;

  // Can't think of any invalid invariants, so store boolean.
  void mark_for_strip () override { marked_for_strip = true; }
  bool is_marked_for_strip () const override { return marked_for_strip; }

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<Expr> &get_break_expr ()
  {
    rust_assert (has_break_expr ());
    return break_expr;
  }

  const std::vector<Attribute> &get_outer_attrs () const { return outer_attrs; }
  std::vector<Attribute> &get_outer_attrs () override { return outer_attrs; }

  void set_outer_attrs (std::vector<Attribute> new_attrs) override
  {
    outer_attrs = std::move (new_attrs);
  }

  LoopLabel &get_label () { return label; }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  BreakExpr *clone_expr_without_block_impl () const override
  {
    return new BreakExpr (*this);
  }
};

// Base range expression AST node object - abstract
class RangeExpr : public ExprWithoutBlock
{
  location_t locus;

protected:
  // outer attributes not allowed before range expressions
  RangeExpr (location_t locus) : locus (locus) {}

public:
  location_t get_locus () const override final { return locus; }

  std::vector<Attribute> &get_outer_attrs () override final
  {
    // RangeExpr cannot have any outer attributes
    rust_assert (false);
  }

  // should never be called - error if called
  void set_outer_attrs (std::vector<Attribute> /* new_attrs */) override
  {
    rust_assert (false);
  }
};

// Range from (inclusive) and to (exclusive) expression AST node object
// aka RangeExpr; constructs a std::ops::Range object
class RangeFromToExpr : public RangeExpr
{
  std::unique_ptr<Expr> from;
  std::unique_ptr<Expr> to;

public:
  std::string as_string () const override;

  RangeFromToExpr (std::unique_ptr<Expr> range_from,
		   std::unique_ptr<Expr> range_to, location_t locus)
    : RangeExpr (locus), from (std::move (range_from)),
      to (std::move (range_to))
  {}

  // Copy constructor with cloning
  RangeFromToExpr (RangeFromToExpr const &other) : RangeExpr (other)
  {
    // guard to prevent null dereference (only required if error state)
    if (other.from != nullptr)
      from = other.from->clone_expr ();
    if (other.to != nullptr)
      to = other.to->clone_expr ();
  }

  // Overload assignment operator to clone unique pointers
  RangeFromToExpr &operator= (RangeFromToExpr const &other)
  {
    RangeExpr::operator= (other);

    // guard to prevent null dereference (only required if error state)
    if (other.from != nullptr)
      from = other.from->clone_expr ();
    else
      from = nullptr;
    if (other.to != nullptr)
      to = other.to->clone_expr ();
    else
      to = nullptr;

    return *this;
  }

  // move constructors
  RangeFromToExpr (RangeFromToExpr &&other) = default;
  RangeFromToExpr &operator= (RangeFromToExpr &&other) = default;

  void accept_vis (ASTVisitor &vis) override;

  // Invalid if either expr is null, so base stripping on that.
  void mark_for_strip () override
  {
    from = nullptr;
    to = nullptr;
  }
  bool is_marked_for_strip () const override
  {
    return from == nullptr && to == nullptr;
  }

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<Expr> &get_from_expr ()
  {
    rust_assert (from != nullptr);
    return from;
  }

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<Expr> &get_to_expr ()
  {
    rust_assert (to != nullptr);
    return to;
  }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  RangeFromToExpr *clone_expr_without_block_impl () const override
  {
    return new RangeFromToExpr (*this);
  }
};

// Range from (inclusive) expression AST node object
// constructs a std::ops::RangeFrom object
class RangeFromExpr : public RangeExpr
{
  std::unique_ptr<Expr> from;

public:
  std::string as_string () const override;

  RangeFromExpr (std::unique_ptr<Expr> range_from, location_t locus)
    : RangeExpr (locus), from (std::move (range_from))
  {}

  // Copy constructor with clone
  RangeFromExpr (RangeFromExpr const &other) : RangeExpr (other)
  {
    // guard to prevent null dereference (only required if error state)
    if (other.from != nullptr)
      from = other.from->clone_expr ();
  }

  // Overload assignment operator to clone unique_ptr
  RangeFromExpr &operator= (RangeFromExpr const &other)
  {
    RangeExpr::operator= (other);

    // guard to prevent null dereference (only required if error state)
    if (other.from != nullptr)
      from = other.from->clone_expr ();
    else
      from = nullptr;

    return *this;
  }

  // move constructors
  RangeFromExpr (RangeFromExpr &&other) = default;
  RangeFromExpr &operator= (RangeFromExpr &&other) = default;

  void accept_vis (ASTVisitor &vis) override;

  // Invalid if expr is null, so base stripping on that.
  void mark_for_strip () override { from = nullptr; }
  bool is_marked_for_strip () const override { return from == nullptr; }

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<Expr> &get_from_expr ()
  {
    rust_assert (from != nullptr);
    return from;
  }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  RangeFromExpr *clone_expr_without_block_impl () const override
  {
    return new RangeFromExpr (*this);
  }
};

// Range to (exclusive) expression AST node object
// constructs a std::ops::RangeTo object
class RangeToExpr : public RangeExpr
{
  std::unique_ptr<Expr> to;

public:
  std::string as_string () const override;

  // outer attributes not allowed
  RangeToExpr (std::unique_ptr<Expr> range_to, location_t locus)
    : RangeExpr (locus), to (std::move (range_to))
  {}

  // Copy constructor with clone
  RangeToExpr (RangeToExpr const &other) : RangeExpr (other)
  {
    // guard to prevent null dereference (only required if error state)
    if (other.to != nullptr)
      to = other.to->clone_expr ();
  }

  // Overload assignment operator to clone unique_ptr
  RangeToExpr &operator= (RangeToExpr const &other)
  {
    RangeExpr::operator= (other);

    // guard to prevent null dereference (only required if error state)
    if (other.to != nullptr)
      to = other.to->clone_expr ();
    else
      to = nullptr;

    return *this;
  }

  // move constructors
  RangeToExpr (RangeToExpr &&other) = default;
  RangeToExpr &operator= (RangeToExpr &&other) = default;

  void accept_vis (ASTVisitor &vis) override;

  // Invalid if expr is null, so base stripping on that.
  void mark_for_strip () override { to = nullptr; }
  bool is_marked_for_strip () const override { return to == nullptr; }

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<Expr> &get_to_expr ()
  {
    rust_assert (to != nullptr);
    return to;
  }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  RangeToExpr *clone_expr_without_block_impl () const override
  {
    return new RangeToExpr (*this);
  }
};

// Full range expression AST node object
// constructs a std::ops::RangeFull object
class RangeFullExpr : public RangeExpr
{
  // TODO: find another way to store this to save memory?
  bool marked_for_strip = false;

public:
  std::string as_string () const override;

  RangeFullExpr (location_t locus) : RangeExpr (locus) {}
  // outer attributes not allowed

  void accept_vis (ASTVisitor &vis) override;

  // Can't think of any invalid invariants, so store boolean.
  void mark_for_strip () override { marked_for_strip = true; }
  bool is_marked_for_strip () const override { return marked_for_strip; }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  RangeFullExpr *clone_expr_without_block_impl () const override
  {
    return new RangeFullExpr (*this);
  }
};

// Range from (inclusive) and to (inclusive) expression AST node object
// aka RangeInclusiveExpr; constructs a std::ops::RangeInclusive object
class RangeFromToInclExpr : public RangeExpr
{
  std::unique_ptr<Expr> from;
  std::unique_ptr<Expr> to;

public:
  std::string as_string () const override;

  RangeFromToInclExpr (std::unique_ptr<Expr> range_from,
		       std::unique_ptr<Expr> range_to, location_t locus)
    : RangeExpr (locus), from (std::move (range_from)),
      to (std::move (range_to))
  {}
  // outer attributes not allowed

  // Copy constructor with clone
  RangeFromToInclExpr (RangeFromToInclExpr const &other) : RangeExpr (other)
  {
    // guard to prevent null dereference (only required if error state)
    if (other.from != nullptr)
      from = other.from->clone_expr ();
    if (other.to != nullptr)
      to = other.to->clone_expr ();
  }

  // Overload assignment operator to use clone
  RangeFromToInclExpr &operator= (RangeFromToInclExpr const &other)
  {
    RangeExpr::operator= (other);

    // guard to prevent null dereference (only required if error state)
    if (other.from != nullptr)
      from = other.from->clone_expr ();
    else
      from = nullptr;
    if (other.to != nullptr)
      to = other.to->clone_expr ();
    else
      to = nullptr;

    return *this;
  }

  // move constructors
  RangeFromToInclExpr (RangeFromToInclExpr &&other) = default;
  RangeFromToInclExpr &operator= (RangeFromToInclExpr &&other) = default;

  void accept_vis (ASTVisitor &vis) override;

  // Invalid if either expr is null, so base stripping on that.
  void mark_for_strip () override
  {
    from = nullptr;
    to = nullptr;
  }
  bool is_marked_for_strip () const override
  {
    return from == nullptr && to == nullptr;
  }

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<Expr> &get_from_expr ()
  {
    rust_assert (from != nullptr);
    return from;
  }

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<Expr> &get_to_expr ()
  {
    rust_assert (to != nullptr);
    return to;
  }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  RangeFromToInclExpr *clone_expr_without_block_impl () const override
  {
    return new RangeFromToInclExpr (*this);
  }
};

// Range to (inclusive) expression AST node object
// aka RangeToInclusiveExpr; constructs a std::ops::RangeToInclusive object
class RangeToInclExpr : public RangeExpr
{
  std::unique_ptr<Expr> to;

public:
  std::string as_string () const override;

  RangeToInclExpr (std::unique_ptr<Expr> range_to, location_t locus)
    : RangeExpr (locus), to (std::move (range_to))
  {}
  // outer attributes not allowed

  // Copy constructor with clone
  RangeToInclExpr (RangeToInclExpr const &other) : RangeExpr (other)
  {
    // guard to prevent null dereference (only required if error state)
    if (other.to != nullptr)
      to = other.to->clone_expr ();
  }

  // Overload assignment operator to clone pointer
  RangeToInclExpr &operator= (RangeToInclExpr const &other)
  {
    RangeExpr::operator= (other);

    // guard to prevent null dereference (only required if error state)
    if (other.to != nullptr)
      to = other.to->clone_expr ();
    else
      to = nullptr;

    return *this;
  }

  // move constructors
  RangeToInclExpr (RangeToInclExpr &&other) = default;
  RangeToInclExpr &operator= (RangeToInclExpr &&other) = default;

  void accept_vis (ASTVisitor &vis) override;

  // Invalid if expr is null, so base stripping on that.
  void mark_for_strip () override { to = nullptr; }
  bool is_marked_for_strip () const override { return to == nullptr; }

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<Expr> &get_to_expr ()
  {
    rust_assert (to != nullptr);
    return to;
  }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  RangeToInclExpr *clone_expr_without_block_impl () const override
  {
    return new RangeToInclExpr (*this);
  }
};

// Return expression AST node representation
class ReturnExpr : public ExprWithoutBlock
{
  std::vector<Attribute> outer_attrs;
  std::unique_ptr<Expr> return_expr;
  location_t locus;

  // TODO: find another way to store this to save memory?
  bool marked_for_strip = false;

public:
  std::string as_string () const override;

  /* Returns whether the object has an expression returned (i.e. not void return
   * type). */
  bool has_returned_expr () const { return return_expr != nullptr; }

  // Constructor for ReturnExpr.
  ReturnExpr (std::unique_ptr<Expr> returned_expr,
	      std::vector<Attribute> outer_attribs, location_t locus)
    : outer_attrs (std::move (outer_attribs)),
      return_expr (std::move (returned_expr)), locus (locus)
  {}

  // Copy constructor with clone
  ReturnExpr (ReturnExpr const &other)
    : ExprWithoutBlock (other), outer_attrs (other.outer_attrs),
      locus (other.locus), marked_for_strip (other.marked_for_strip)
  {
    // guard to protect from null pointer dereference
    if (other.return_expr != nullptr)
      return_expr = other.return_expr->clone_expr ();
  }

  // Overloaded assignment operator to clone return_expr pointer
  ReturnExpr &operator= (ReturnExpr const &other)
  {
    ExprWithoutBlock::operator= (other);
    locus = other.locus;
    marked_for_strip = other.marked_for_strip;
    outer_attrs = other.outer_attrs;

    // guard to protect from null pointer dereference
    if (other.return_expr != nullptr)
      return_expr = other.return_expr->clone_expr ();
    else
      return_expr = nullptr;

    return *this;
  }

  // move constructors
  ReturnExpr (ReturnExpr &&other) = default;
  ReturnExpr &operator= (ReturnExpr &&other) = default;

  location_t get_locus () const override final { return locus; }

  void accept_vis (ASTVisitor &vis) override;

  // Can't think of any invalid invariants, so store boolean.
  void mark_for_strip () override { marked_for_strip = true; }
  bool is_marked_for_strip () const override { return marked_for_strip; }

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<Expr> &get_returned_expr ()
  {
    rust_assert (return_expr != nullptr);
    return return_expr;
  }

  const std::vector<Attribute> &get_outer_attrs () const { return outer_attrs; }
  std::vector<Attribute> &get_outer_attrs () override { return outer_attrs; }

  void set_outer_attrs (std::vector<Attribute> new_attrs) override
  {
    outer_attrs = std::move (new_attrs);
  }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  ReturnExpr *clone_expr_without_block_impl () const override
  {
    return new ReturnExpr (*this);
  }
};

// Forward decl - defined in rust-macro.h
class MacroInvocation;

// An unsafe block AST node
class UnsafeBlockExpr : public ExprWithBlock
{
  std::vector<Attribute> outer_attrs;
  // Or just have it extend BlockExpr
  std::unique_ptr<BlockExpr> expr;
  location_t locus;

public:
  std::string as_string () const override;

  UnsafeBlockExpr (std::unique_ptr<BlockExpr> block_expr,
		   std::vector<Attribute> outer_attribs, location_t locus)
    : outer_attrs (std::move (outer_attribs)), expr (std::move (block_expr)),
      locus (locus)
  {}

  // Copy constructor with clone
  UnsafeBlockExpr (UnsafeBlockExpr const &other)
    : ExprWithBlock (other), outer_attrs (other.outer_attrs),
      locus (other.locus)
  {
    // guard to prevent null dereference (only required if error state)
    if (other.expr != nullptr)
      expr = other.expr->clone_block_expr ();
  }

  // Overloaded assignment operator to clone
  UnsafeBlockExpr &operator= (UnsafeBlockExpr const &other)
  {
    ExprWithBlock::operator= (other);
    locus = other.locus;
    outer_attrs = other.outer_attrs;

    // guard to prevent null dereference (only required if error state)
    if (other.expr != nullptr)
      expr = other.expr->clone_block_expr ();
    else
      expr = nullptr;

    return *this;
  }

  // move constructors
  UnsafeBlockExpr (UnsafeBlockExpr &&other) = default;
  UnsafeBlockExpr &operator= (UnsafeBlockExpr &&other) = default;

  location_t get_locus () const override final { return locus; }

  void accept_vis (ASTVisitor &vis) override;

  // Invalid if block is null, so base stripping on that.
  void mark_for_strip () override { expr = nullptr; }
  bool is_marked_for_strip () const override { return expr == nullptr; }

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<BlockExpr> &get_block_expr ()
  {
    rust_assert (expr != nullptr);
    return expr;
  }

  const std::vector<Attribute> &get_outer_attrs () const { return outer_attrs; }
  std::vector<Attribute> &get_outer_attrs () override { return outer_attrs; }

  void set_outer_attrs (std::vector<Attribute> new_attrs) override
  {
    outer_attrs = std::move (new_attrs);
  }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  UnsafeBlockExpr *clone_expr_with_block_impl () const override
  {
    return new UnsafeBlockExpr (*this);
  }
};

// Base loop expression AST node - aka LoopExpr
class BaseLoopExpr : public ExprWithBlock
{
protected:
  // protected to allow subclasses better use of them
  std::vector<Attribute> outer_attrs;
  LoopLabel loop_label;
  std::unique_ptr<BlockExpr> loop_block;

private:
  location_t locus;

protected:
  // Constructor for BaseLoopExpr
  BaseLoopExpr (std::unique_ptr<BlockExpr> loop_block, location_t locus,
		LoopLabel loop_label = LoopLabel::error (),
		std::vector<Attribute> outer_attribs
		= std::vector<Attribute> ())
    : outer_attrs (std::move (outer_attribs)),
      loop_label (std::move (loop_label)), loop_block (std::move (loop_block)),
      locus (locus)
  {}

  // Copy constructor for BaseLoopExpr with clone
  BaseLoopExpr (BaseLoopExpr const &other)
    : ExprWithBlock (other), outer_attrs (other.outer_attrs),
      loop_label (other.loop_label), locus (other.locus)
  {
    // guard to prevent null dereference (only required if error state)
    if (other.loop_block != nullptr)
      loop_block = other.loop_block->clone_block_expr ();
  }

  // Overloaded assignment operator to clone
  BaseLoopExpr &operator= (BaseLoopExpr const &other)
  {
    ExprWithBlock::operator= (other);
    loop_label = other.loop_label;
    locus = other.locus;
    outer_attrs = other.outer_attrs;

    // guard to prevent null dereference (only required if error state)
    if (other.loop_block != nullptr)
      loop_block = other.loop_block->clone_block_expr ();
    else
      loop_block = nullptr;

    return *this;
  }

  // move constructors
  BaseLoopExpr (BaseLoopExpr &&other) = default;
  BaseLoopExpr &operator= (BaseLoopExpr &&other) = default;

public:
  bool has_loop_label () const { return !loop_label.is_error (); }

  LoopLabel &get_loop_label () { return loop_label; }

  location_t get_locus () const override final { return locus; }

  // Invalid if loop block is null, so base stripping on that.
  void mark_for_strip () override { loop_block = nullptr; }
  bool is_marked_for_strip () const override { return loop_block == nullptr; }

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<BlockExpr> &get_loop_block ()
  {
    rust_assert (loop_block != nullptr);
    return loop_block;
  }

  const std::vector<Attribute> &get_outer_attrs () const { return outer_attrs; }
  std::vector<Attribute> &get_outer_attrs () override { return outer_attrs; }

  void set_outer_attrs (std::vector<Attribute> new_attrs) override
  {
    outer_attrs = std::move (new_attrs);
  }
};

// 'Loop' expression (i.e. the infinite loop) AST node
class LoopExpr : public BaseLoopExpr
{
public:
  std::string as_string () const override;

  // Constructor for LoopExpr
  LoopExpr (std::unique_ptr<BlockExpr> loop_block, location_t locus,
	    LoopLabel loop_label = LoopLabel::error (),
	    std::vector<Attribute> outer_attribs = std::vector<Attribute> ())
    : BaseLoopExpr (std::move (loop_block), locus, std::move (loop_label),
		    std::move (outer_attribs))
  {}

  void accept_vis (ASTVisitor &vis) override;

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  LoopExpr *clone_expr_with_block_impl () const override
  {
    return new LoopExpr (*this);
  }
};

// While loop expression AST node (predicate loop)
class WhileLoopExpr : public BaseLoopExpr
{
  std::unique_ptr<Expr> condition;

public:
  std::string as_string () const override;

  // Constructor for while loop with loop label
  WhileLoopExpr (std::unique_ptr<Expr> loop_condition,
		 std::unique_ptr<BlockExpr> loop_block, location_t locus,
		 LoopLabel loop_label = LoopLabel::error (),
		 std::vector<Attribute> outer_attribs
		 = std::vector<Attribute> ())
    : BaseLoopExpr (std::move (loop_block), locus, std::move (loop_label),
		    std::move (outer_attribs)),
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

  void accept_vis (ASTVisitor &vis) override;

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<Expr> &get_predicate_expr ()
  {
    rust_assert (condition != nullptr);
    return condition;
  }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  WhileLoopExpr *clone_expr_with_block_impl () const override
  {
    return new WhileLoopExpr (*this);
  }
};

// While let loop expression AST node (predicate pattern loop)
class WhileLetLoopExpr : public BaseLoopExpr
{
  // MatchArmPatterns patterns;
  std::vector<std::unique_ptr<Pattern> > match_arm_patterns; // inlined
  std::unique_ptr<Expr> scrutinee;

public:
  std::string as_string () const override;

  // Constructor with a loop label
  WhileLetLoopExpr (std::vector<std::unique_ptr<Pattern> > match_arm_patterns,
		    std::unique_ptr<Expr> scrutinee,
		    std::unique_ptr<BlockExpr> loop_block, location_t locus,
		    LoopLabel loop_label = LoopLabel::error (),
		    std::vector<Attribute> outer_attribs
		    = std::vector<Attribute> ())
    : BaseLoopExpr (std::move (loop_block), locus, std::move (loop_label),
		    std::move (outer_attribs)),
      match_arm_patterns (std::move (match_arm_patterns)),
      scrutinee (std::move (scrutinee))
  {}

  // Copy constructor with clone
  WhileLetLoopExpr (WhileLetLoopExpr const &other)
    : BaseLoopExpr (other),
      /*match_arm_patterns(other.match_arm_patterns),*/ scrutinee (
	other.scrutinee->clone_expr ())
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
    scrutinee = other.scrutinee->clone_expr ();
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

  void accept_vis (ASTVisitor &vis) override;

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<Expr> &get_scrutinee_expr ()
  {
    rust_assert (scrutinee != nullptr);
    return scrutinee;
  }

  // TODO: this mutable getter seems really dodgy. Think up better way.
  const std::vector<std::unique_ptr<Pattern> > &get_patterns () const
  {
    return match_arm_patterns;
  }
  std::vector<std::unique_ptr<Pattern> > &get_patterns ()
  {
    return match_arm_patterns;
  }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  WhileLetLoopExpr *clone_expr_with_block_impl () const override
  {
    return new WhileLetLoopExpr (*this);
  }
};

// For loop expression AST node (iterator loop)
class ForLoopExpr : public BaseLoopExpr
{
  std::unique_ptr<Pattern> pattern;
  std::unique_ptr<Expr> iterator_expr;

public:
  std::string as_string () const override;

  // Constructor with loop label
  ForLoopExpr (std::unique_ptr<Pattern> loop_pattern,
	       std::unique_ptr<Expr> iterator_expr,
	       std::unique_ptr<BlockExpr> loop_body, location_t locus,
	       LoopLabel loop_label = LoopLabel::error (),
	       std::vector<Attribute> outer_attribs = std::vector<Attribute> ())
    : BaseLoopExpr (std::move (loop_body), locus, std::move (loop_label),
		    std::move (outer_attribs)),
      pattern (std::move (loop_pattern)),
      iterator_expr (std::move (iterator_expr))
  {}

  // Copy constructor with clone
  ForLoopExpr (ForLoopExpr const &other)
    : BaseLoopExpr (other), pattern (other.pattern->clone_pattern ()),
      iterator_expr (other.iterator_expr->clone_expr ())
  {}

  // Overloaded assignment operator to clone
  ForLoopExpr &operator= (ForLoopExpr const &other)
  {
    BaseLoopExpr::operator= (other);
    pattern = other.pattern->clone_pattern ();
    iterator_expr = other.iterator_expr->clone_expr ();
    /*loop_block = other.loop_block->clone_block_expr();
    loop_label = other.loop_label;
    outer_attrs = other.outer_attrs;*/

    return *this;
  }

  // move constructors
  ForLoopExpr (ForLoopExpr &&other) = default;
  ForLoopExpr &operator= (ForLoopExpr &&other) = default;

  void accept_vis (ASTVisitor &vis) override;

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<Expr> &get_iterator_expr ()
  {
    rust_assert (iterator_expr != nullptr);
    return iterator_expr;
  }

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<Pattern> &get_pattern ()
  {
    rust_assert (pattern != nullptr);
    return pattern;
  }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  ForLoopExpr *clone_expr_with_block_impl () const override
  {
    return new ForLoopExpr (*this);
  }
};

// forward decl for IfExpr
class IfLetExpr;

// Base if expression with no "else" or "if let" AST node
class IfExpr : public ExprWithBlock
{
  std::vector<Attribute> outer_attrs;
  std::unique_ptr<Expr> condition;
  std::unique_ptr<BlockExpr> if_block;
  location_t locus;

public:
  std::string as_string () const override;

  IfExpr (std::unique_ptr<Expr> condition, std::unique_ptr<BlockExpr> if_block,
	  std::vector<Attribute> outer_attrs, location_t locus)
    : outer_attrs (std::move (outer_attrs)), condition (std::move (condition)),
      if_block (std::move (if_block)), locus (locus)
  {}
  // outer attributes are never allowed on IfExprs

  // Copy constructor with clone
  IfExpr (IfExpr const &other)
    : ExprWithBlock (other), outer_attrs (other.outer_attrs),
      locus (other.locus)
  {
    // guard to prevent null dereference (only required if error state)
    if (other.condition != nullptr)
      condition = other.condition->clone_expr ();
    if (other.if_block != nullptr)
      if_block = other.if_block->clone_block_expr ();
  }

  // Overloaded assignment operator to clone expressions
  IfExpr &operator= (IfExpr const &other)
  {
    ExprWithBlock::operator= (other);
    outer_attrs = other.outer_attrs;
    locus = other.locus;

    // guard to prevent null dereference (only required if error state)
    if (other.condition != nullptr)
      condition = other.condition->clone_expr ();
    else
      condition = nullptr;
    if (other.if_block != nullptr)
      if_block = other.if_block->clone_block_expr ();
    else
      if_block = nullptr;

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

  /* Note that multiple "else if"s are handled via nested ASTs rather than a
   * vector of else ifs - i.e. not like a switch statement. TODO - is this a
   * better approach? or does it not parse correctly and have downsides? */

  location_t get_locus () const override final { return locus; }

  void accept_vis (ASTVisitor &vis) override;

  void vis_if_condition (ASTVisitor &vis) { condition->accept_vis (vis); }
  void vis_if_block (ASTVisitor &vis) { if_block->accept_vis (vis); }

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<Expr> &get_condition_expr ()
  {
    rust_assert (condition != nullptr);
    return condition;
  }

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<BlockExpr> &get_if_block ()
  {
    rust_assert (if_block != nullptr);
    return if_block;
  }

  // Invalid if if block or condition is null, so base stripping on that.
  void mark_for_strip () override
  {
    if_block = nullptr;
    condition = nullptr;
  }
  bool is_marked_for_strip () const override
  {
    return if_block == nullptr && condition == nullptr;
  }

  void set_outer_attrs (std::vector<Attribute> new_attrs) override
  {
    outer_attrs = std::move (new_attrs);
  }

  // TODO: this mutable getter seems really dodgy. Think up better way.
  const std::vector<Attribute> &get_outer_attrs () const { return outer_attrs; }
  std::vector<Attribute> &get_outer_attrs () override { return outer_attrs; }

protected:
  // Base clone function but still concrete as concrete base class
  virtual IfExpr *clone_if_expr_impl () const { return new IfExpr (*this); }

  /* Use covariance to implement clone function as returning this object rather
   * than base */
  IfExpr *clone_expr_with_block_impl () const final override
  {
    return clone_if_expr_impl ();
  }
};

// If expression with an ending "else" expression AST node (trailing)
class IfExprConseqElse : public IfExpr
{
  std::unique_ptr<ExprWithBlock> else_block;

public:
  std::string as_string () const override;

  IfExprConseqElse (std::unique_ptr<Expr> condition,
		    std::unique_ptr<BlockExpr> if_block,
		    std::unique_ptr<ExprWithBlock> else_block,
		    std::vector<Attribute> outer_attrs, location_t locus)
    : IfExpr (std::move (condition), std::move (if_block),
	      std::move (outer_attrs), locus),
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

  void accept_vis (ASTVisitor &vis) override;

  void vis_else_block (ASTVisitor &vis) { else_block->accept_vis (vis); }

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<ExprWithBlock> &get_else_block ()
  {
    rust_assert (else_block != nullptr);
    return else_block;
  }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  IfExprConseqElse *clone_if_expr_impl () const override
  {
    return new IfExprConseqElse (*this);
  }
};

// Basic "if let" expression AST node with no else
class IfLetExpr : public ExprWithBlock
{
  std::vector<Attribute> outer_attrs;
  std::vector<std::unique_ptr<Pattern> > match_arm_patterns; // inlined
  std::unique_ptr<Expr> value;
  std::unique_ptr<BlockExpr> if_block;
  location_t locus;

public:
  std::string as_string () const override;

  IfLetExpr (std::vector<std::unique_ptr<Pattern> > match_arm_patterns,
	     std::unique_ptr<Expr> value, std::unique_ptr<BlockExpr> if_block,
	     std::vector<Attribute> outer_attrs, location_t locus)
    : outer_attrs (std::move (outer_attrs)),
      match_arm_patterns (std::move (match_arm_patterns)),
      value (std::move (value)), if_block (std::move (if_block)), locus (locus)
  {}

  // copy constructor with clone
  IfLetExpr (IfLetExpr const &other)
    : ExprWithBlock (other), outer_attrs (other.outer_attrs),
      locus (other.locus)
  {
    // guard to prevent null dereference (only required if error state)
    if (other.value != nullptr)
      value = other.value->clone_expr ();
    if (other.if_block != nullptr)
      if_block = other.if_block->clone_block_expr ();

    match_arm_patterns.reserve (other.match_arm_patterns.size ());
    for (const auto &e : other.match_arm_patterns)
      match_arm_patterns.push_back (e->clone_pattern ());
  }

  // overload assignment operator to clone
  IfLetExpr &operator= (IfLetExpr const &other)
  {
    ExprWithBlock::operator= (other);
    outer_attrs = other.outer_attrs;
    locus = other.locus;

    // guard to prevent null dereference (only required if error state)
    if (other.value != nullptr)
      value = other.value->clone_expr ();
    else
      value = nullptr;
    if (other.if_block != nullptr)
      if_block = other.if_block->clone_block_expr ();
    else
      if_block = nullptr;

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

  void accept_vis (ASTVisitor &vis) override;

  // Invalid if block or value is null, so base stripping on that.
  void mark_for_strip () override
  {
    if_block = nullptr;
    value = nullptr;
  }
  bool is_marked_for_strip () const override
  {
    return if_block == nullptr && value == nullptr;
  }

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<Expr> &get_value_expr ()
  {
    rust_assert (value != nullptr);
    return value;
  }

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<BlockExpr> &get_if_block ()
  {
    rust_assert (if_block != nullptr);
    return if_block;
  }

  // TODO: this mutable getter seems really dodgy. Think up better way.
  const std::vector<std::unique_ptr<Pattern> > &get_patterns () const
  {
    return match_arm_patterns;
  }
  std::vector<std::unique_ptr<Pattern> > &get_patterns ()
  {
    return match_arm_patterns;
  }

  void set_outer_attrs (std::vector<Attribute> new_attrs) override
  {
    outer_attrs = std::move (new_attrs);
  }

  // TODO: this mutable getter seems really dodgy. Think up better way.
  const std::vector<Attribute> &get_outer_attrs () const { return outer_attrs; }
  std::vector<Attribute> &get_outer_attrs () override { return outer_attrs; }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base (or rather this or any derived object) */
  IfLetExpr *clone_expr_with_block_impl () const final override
  {
    return clone_if_let_expr_impl ();
  }

  // Base clone function but still concrete as concrete base class
  virtual IfLetExpr *clone_if_let_expr_impl () const
  {
    return new IfLetExpr (*this);
  }
};

/* AST node representing "if let" expression with an "else" expression at the
 * end */
class IfLetExprConseqElse : public IfLetExpr
{
  std::unique_ptr<ExprWithBlock> else_block;

public:
  std::string as_string () const override;

  IfLetExprConseqElse (
    std::vector<std::unique_ptr<Pattern> > match_arm_patterns,
    std::unique_ptr<Expr> value, std::unique_ptr<BlockExpr> if_block,
    std::unique_ptr<ExprWithBlock> else_block,
    std::vector<Attribute> outer_attrs, location_t locus)
    : IfLetExpr (std::move (match_arm_patterns), std::move (value),
		 std::move (if_block), std::move (outer_attrs), locus),
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

  void accept_vis (ASTVisitor &vis) override;

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<ExprWithBlock> &get_else_block ()
  {
    rust_assert (else_block != nullptr);
    return else_block;
  }

protected:
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
  std::vector<Attribute> outer_attrs;
  // MatchArmPatterns patterns;
  std::vector<std::unique_ptr<Pattern> > match_arm_patterns; // inlined

  // bool has_match_arm_guard;
  // inlined from MatchArmGuard
  std::unique_ptr<Expr> guard_expr;

  location_t locus;

public:
  // Returns whether the MatchArm has a match arm guard expression
  bool has_match_arm_guard () const { return guard_expr != nullptr; }

  // Constructor for match arm with a guard expression
  MatchArm (std::vector<std::unique_ptr<Pattern> > match_arm_patterns,
	    location_t locus, std::unique_ptr<Expr> guard_expr = nullptr,
	    std::vector<Attribute> outer_attrs = std::vector<Attribute> ())
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
    else
      guard_expr = nullptr;

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

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<Expr> &get_guard_expr ()
  {
    rust_assert (has_match_arm_guard ());
    return guard_expr;
  }

  // TODO: this mutable getter seems really dodgy. Think up better way.
  const std::vector<Attribute> &get_outer_attrs () const { return outer_attrs; }
  std::vector<Attribute> &get_outer_attrs () { return outer_attrs; }

  const std::vector<std::unique_ptr<Pattern> > &get_patterns () const
  {
    return match_arm_patterns;
  }
  std::vector<std::unique_ptr<Pattern> > &get_patterns ()
  {
    return match_arm_patterns;
  }

  location_t get_locus () const { return locus; }
};

/* A "match case" - a correlated match arm and resulting expression. Not
 * abstract. */
struct MatchCase
{
private:
  MatchArm arm;
  std::unique_ptr<Expr> expr;
  NodeId node_id;

  /* TODO: does whether trailing comma exists need to be stored? currently
   * assuming it is only syntactical and has no effect on meaning. */

public:
  MatchCase (MatchArm arm, std::unique_ptr<Expr> expr)
    : arm (std::move (arm)), expr (std::move (expr)),
      node_id (Analysis::Mappings::get ()->get_next_node_id ())
  {}

  MatchCase (const MatchCase &other)
    : arm (other.arm), expr (other.expr->clone_expr ()), node_id (other.node_id)
  {}

  MatchCase &operator= (const MatchCase &other)
  {
    arm = other.arm;
    expr = other.expr->clone_expr ();
    node_id = other.node_id;

    return *this;
  }

  MatchCase (MatchCase &&other) = default;
  MatchCase &operator= (MatchCase &&other) = default;

  ~MatchCase () = default;

  std::string as_string () const;

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<Expr> &get_expr ()
  {
    rust_assert (expr != nullptr);
    return expr;
  }

  // TODO: is this better? Or is a "vis_block" better?
  MatchArm &get_arm ()
  {
    rust_assert (!arm.is_error ());
    return arm;
  }

  NodeId get_node_id () const { return node_id; }
};

// Match expression AST node
class MatchExpr : public ExprWithBlock
{
  std::vector<Attribute> outer_attrs;
  std::unique_ptr<Expr> branch_value;
  std::vector<Attribute> inner_attrs;
  std::vector<MatchCase> match_arms;
  location_t locus;

public:
  std::string as_string () const override;

  // Returns whether the match expression has any match arms.
  bool has_match_arms () const { return !match_arms.empty (); }

  MatchExpr (std::unique_ptr<Expr> branch_value,
	     std::vector<MatchCase> match_arms,
	     std::vector<Attribute> inner_attrs,
	     std::vector<Attribute> outer_attrs, location_t locus)
    : outer_attrs (std::move (outer_attrs)),
      branch_value (std::move (branch_value)),
      inner_attrs (std::move (inner_attrs)),
      match_arms (std::move (match_arms)), locus (locus)
  {}

  // Copy constructor requires clone due to unique_ptr
  MatchExpr (MatchExpr const &other)
    : ExprWithBlock (other), outer_attrs (other.outer_attrs),
      inner_attrs (other.inner_attrs), match_arms (other.match_arms),
      locus (other.locus)
  {
    // guard to prevent null dereference (only required if error state)
    if (other.branch_value != nullptr)
      branch_value = other.branch_value->clone_expr ();
  }

  // Overloaded assignment operator to clone due to unique_ptr
  MatchExpr &operator= (MatchExpr const &other)
  {
    ExprWithBlock::operator= (other);
    inner_attrs = other.inner_attrs;
    match_arms = other.match_arms;
    outer_attrs = other.outer_attrs;
    locus = other.locus;

    // guard to prevent null dereference (only required if error state)
    if (other.branch_value != nullptr)
      branch_value = other.branch_value->clone_expr ();
    else
      branch_value = nullptr;

    return *this;
  }

  // move constructors
  MatchExpr (MatchExpr &&other) = default;
  MatchExpr &operator= (MatchExpr &&other) = default;

  location_t get_locus () const override final { return locus; }

  void accept_vis (ASTVisitor &vis) override;

  // Invalid if branch value is null, so base stripping on that.
  void mark_for_strip () override { branch_value = nullptr; }
  bool is_marked_for_strip () const override { return branch_value == nullptr; }

  // TODO: this mutable getter seems really dodgy. Think up better way.
  const std::vector<Attribute> &get_inner_attrs () const { return inner_attrs; }
  std::vector<Attribute> &get_inner_attrs () { return inner_attrs; }

  const std::vector<Attribute> &get_outer_attrs () const { return outer_attrs; }
  std::vector<Attribute> &get_outer_attrs () override { return outer_attrs; }

  void set_outer_attrs (std::vector<Attribute> new_attrs) override
  {
    outer_attrs = std::move (new_attrs);
  }

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<Expr> &get_scrutinee_expr ()
  {
    rust_assert (branch_value != nullptr);
    return branch_value;
  }

  const std::vector<MatchCase> &get_match_cases () const { return match_arms; }
  std::vector<MatchCase> &get_match_cases () { return match_arms; }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  MatchExpr *clone_expr_with_block_impl () const override
  {
    return new MatchExpr (*this);
  }
};

// Await expression AST node (pseudo-member variable access)
class AwaitExpr : public ExprWithoutBlock
{
  std::vector<Attribute> outer_attrs;
  std::unique_ptr<Expr> awaited_expr;
  location_t locus;

public:
  // TODO: ensure outer attributes are actually allowed
  AwaitExpr (std::unique_ptr<Expr> awaited_expr,
	     std::vector<Attribute> outer_attrs, location_t locus)
    : outer_attrs (std::move (outer_attrs)),
      awaited_expr (std::move (awaited_expr)), locus (locus)
  {}

  // copy constructor with clone
  AwaitExpr (AwaitExpr const &other)
    : ExprWithoutBlock (other), outer_attrs (other.outer_attrs),
      locus (other.locus)
  {
    // guard to prevent null dereference (only required if error state)
    if (other.awaited_expr != nullptr)
      awaited_expr = other.awaited_expr->clone_expr ();
  }

  // overloaded assignment operator with clone
  AwaitExpr &operator= (AwaitExpr const &other)
  {
    ExprWithoutBlock::operator= (other);
    outer_attrs = other.outer_attrs;
    locus = other.locus;

    // guard to prevent null dereference (only required if error state)
    if (other.awaited_expr != nullptr)
      awaited_expr = other.awaited_expr->clone_expr ();
    else
      awaited_expr = nullptr;

    return *this;
  }

  // move constructors
  AwaitExpr (AwaitExpr &&other) = default;
  AwaitExpr &operator= (AwaitExpr &&other) = default;

  std::string as_string () const override;

  location_t get_locus () const override final { return locus; }

  void accept_vis (ASTVisitor &vis) override;

  // Invalid if awaited expr is null, so base stripping on that.
  void mark_for_strip () override { awaited_expr = nullptr; }
  bool is_marked_for_strip () const override { return awaited_expr == nullptr; }

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<Expr> &get_awaited_expr ()
  {
    rust_assert (awaited_expr != nullptr);
    return awaited_expr;
  }

  const std::vector<Attribute> &get_outer_attrs () const { return outer_attrs; }
  std::vector<Attribute> &get_outer_attrs () override { return outer_attrs; }

  void set_outer_attrs (std::vector<Attribute> new_attrs) override
  {
    outer_attrs = std::move (new_attrs);
  }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  AwaitExpr *clone_expr_without_block_impl () const override
  {
    return new AwaitExpr (*this);
  }
};

// Async block expression AST node (block expr that evaluates to a future)
class AsyncBlockExpr : public ExprWithBlock
{
  // TODO: should this extend BlockExpr rather than be a composite of it?
  std::vector<Attribute> outer_attrs;
  bool has_move;
  std::unique_ptr<BlockExpr> block_expr;
  location_t locus;

public:
  AsyncBlockExpr (std::unique_ptr<BlockExpr> block_expr, bool has_move,
		  std::vector<Attribute> outer_attrs, location_t locus)
    : outer_attrs (std::move (outer_attrs)), has_move (has_move),
      block_expr (std::move (block_expr)), locus (locus)
  {}

  // copy constructor with clone
  AsyncBlockExpr (AsyncBlockExpr const &other)
    : ExprWithBlock (other), outer_attrs (other.outer_attrs),
      has_move (other.has_move), locus (other.locus)
  {
    // guard to prevent null dereference (only required if error state)
    if (other.block_expr != nullptr)
      block_expr = other.block_expr->clone_block_expr ();
  }

  // overloaded assignment operator to clone
  AsyncBlockExpr &operator= (AsyncBlockExpr const &other)
  {
    ExprWithBlock::operator= (other);
    outer_attrs = other.outer_attrs;
    has_move = other.has_move;
    locus = other.locus;

    // guard to prevent null dereference (only required if error state)
    if (other.block_expr != nullptr)
      block_expr = other.block_expr->clone_block_expr ();
    else
      block_expr = nullptr;

    return *this;
  }

  // move constructors
  AsyncBlockExpr (AsyncBlockExpr &&other) = default;
  AsyncBlockExpr &operator= (AsyncBlockExpr &&other) = default;

  std::string as_string () const override;

  bool get_has_move () { return has_move; }
  location_t get_locus () const override final { return locus; }

  void accept_vis (ASTVisitor &vis) override;

  // Invalid if block is null, so base stripping on that.
  void mark_for_strip () override { block_expr = nullptr; }
  bool is_marked_for_strip () const override { return block_expr == nullptr; }

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<BlockExpr> &get_block_expr ()
  {
    rust_assert (block_expr != nullptr);
    return block_expr;
  }

  const std::vector<Attribute> &get_outer_attrs () const { return outer_attrs; }
  std::vector<Attribute> &get_outer_attrs () override { return outer_attrs; }

  void set_outer_attrs (std::vector<Attribute> new_attrs) override
  {
    outer_attrs = std::move (new_attrs);
  }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  AsyncBlockExpr *clone_expr_with_block_impl () const override
  {
    return new AsyncBlockExpr (*this);
  }
};

// Inline-assembly specific options
enum class InlineAsmOptions
{
  PURE = 1 << 0,
  NOMEM = 1 << 1,
  READONLY = 1 << 2,
  PRESERVES_FLAGS = 1 << 3,
  NORETURN = 1 << 4,
  NOSTACK = 1 << 5,
  ATT_SYNTAX = 1 << 6,
  RAW = 1 << 7,
  MAY_UNWIND = 1 << 8,
};

struct AnonConst
{
  NodeId id;
  std::unique_ptr<Expr> value;
};

struct InlineAsmRegOrRegClass
{
  enum Type
  {
    Reg,
    RegClass,
  };

  struct Reg
  {
    std::string Symbol;
  };

  struct RegClass
  {
    std::string Symbol;
  };

  Identifier name;
  location_t locus;
};

struct InlineAsmOperand
{
  enum RegisterType
  {
    In,
    Out,
    InOut,
    SplitInOut,
    Const,
    Sym,
  };

  struct In
  {
    InlineAsmRegOrRegClass reg;
    std::unique_ptr<Expr> expr;
  };

  struct Out
  {
    InlineAsmRegOrRegClass reg;
    bool late;
    std::unique_ptr<Expr> expr; // can be null
  };

  struct InOut
  {
    InlineAsmRegOrRegClass reg;
    bool late;
    std::unique_ptr<Expr> expr; // this can't be null
  };

  struct SplitInOut
  {
    InlineAsmRegOrRegClass reg;
    bool late;
    std::unique_ptr<Expr> in_expr;
    std::unique_ptr<Expr> out_expr; // could be null
  };

  struct Const
  {
    AnonConst anon_const;
  };

  struct Sym
  {
    std::unique_ptr<Expr> sym;
  };
  location_t locus;
};

struct InlineAsmPlaceHolder
{
  size_t operand_idx;
  char modifier; // can be null
  location_t locus;
};

struct InlineAsmTemplatePiece
{
  bool is_placeholder;
  union
  {
    std::string string;
    InlineAsmPlaceHolder placeholder;
  };
};

struct TupleClobber
{
  // as gccrs still doesn't contain a symbol class I have put them as strings
  std::string symbol;
  location_t loc;
};

struct TupleTemplateStr
{
  // as gccrs still doesn't contain a symbol class I have put them as strings
  std::string symbol;
  std::string optional_symbol;
  location_t loc;
};

// Inline Assembly Node
class InlineAsm : public ExprWithoutBlock
{
public:
  std::vector<InlineAsmTemplatePiece> template_;
  std::vector<TupleTemplateStr> template_strs;
  std::vector<InlineAsmOperand> operands;
  TupleClobber clobber_abi;
  InlineAsmOptions options;
  std::vector<location_t> line_spans;
};

} // namespace AST
} // namespace Rust

#endif
