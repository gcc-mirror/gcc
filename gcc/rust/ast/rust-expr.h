#ifndef RUST_AST_EXPR_H
#define RUST_AST_EXPR_H

#include "rust-ast.h"
#include "rust-path.h"

namespace Rust {
namespace AST {
/* TODO: if GCC moves to C++17 or allows boost, replace some boolean
 * "has_whatever" pairs with
 * optional types (std::optional or boost::optional)? */

// forward decls: defined in rust-path.h, rust-type.h, rust-pattern.h, and
// rust-stmt.h
/*class PathInExpression;
class QualifiedPathInExpression;
class PathExprSegment;*/ // decls no longer required as "rust-path.h" is included
/*class Type;
class TypeNoBounds;
class Lifetime;
class Pattern;
class Statement;*/ // decls no longer required as definitions moved to rust-ast.h

// Decl as definition moved to rust-ast.h
class ExprWithoutBlock;
class Function;

// AST node for an expression with an accompanying block - abstract
class ExprWithBlock : public Expr
{
  // TODO: should this mean that a BlockExpr should be a member variable?
protected:
  ExprWithBlock (::std::vector<Attribute> outer_attrs
		 = ::std::vector<Attribute> ())
    : Expr (::std::move (outer_attrs))
  {}

  // pure virtual clone implementation
  virtual ExprWithBlock *clone_expr_with_block_impl () const = 0;

  // prevent having to define multiple clone expressions
  virtual ExprWithBlock *clone_expr_impl () const OVERRIDE
  {
    return clone_expr_with_block_impl ();
  }

public:
  // Unique pointer custom clone function
  ::std::unique_ptr<ExprWithBlock> clone_expr_with_block () const
  {
    return ::std::unique_ptr<ExprWithBlock> (clone_expr_with_block_impl ());
  }
};

// Literals? Or literal base?
class LiteralExpr : public ExprWithoutBlock
{
  /*public:
    enum LitType {
	CHAR,
	STRING,
	RAW_STRING,
	BYTE,
	BYTE_STRING,
	RAW_BYTE_STRING,
	INT,
	FLOAT,
	BOOL
    };

  private:
    // TODO: maybe make subclasses of each type of literal with their typed
  values (or
    // generics)
    ::std::string value_as_string;
    LitType type;*/
  // moved to Literal

public:
  Literal literal;

  Location locus;

  ::std::string as_string () const { return literal.as_string (); }

  inline Literal::LitType get_lit_type () const
  {
    return literal.get_lit_type ();
  }

  LiteralExpr (::std::string value_as_string, Literal::LitType type,
	       Location locus,
	       ::std::vector<Attribute> outer_attrs
	       = ::std::vector<Attribute> ())
    : ExprWithoutBlock (::std::move (outer_attrs)),
      literal (::std::move (value_as_string), type), locus (locus)
  {}

  LiteralExpr (Literal literal, Location locus,
	       ::std::vector<Attribute> outer_attrs
	       = ::std::vector<Attribute> ())
    : ExprWithoutBlock (::std::move (outer_attrs)),
      literal (::std::move (literal)), locus (locus)
  {}

  // Unique pointer custom clone function
  ::std::unique_ptr<LiteralExpr> clone_literal_expr () const
  {
    return ::std::unique_ptr<LiteralExpr> (clone_literal_expr_impl ());
  }

  Location get_locus () const { return locus; }

  Location get_locus_slow () const OVERRIDE { return get_locus (); }

  virtual void accept_vis (ASTVisitor &vis) OVERRIDE;

protected:
  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual LiteralExpr *clone_expr_impl () const OVERRIDE
  {
    return new LiteralExpr (*this);
  }

  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual LiteralExpr *clone_expr_without_block_impl () const OVERRIDE
  {
    return new LiteralExpr (*this);
  }

  // not virtual as currently no subclasses of LiteralExpr, but could be in
  // future
  /*virtual*/ LiteralExpr *clone_literal_expr_impl () const
  {
    return new LiteralExpr (*this);
  }
};

// Literal expression attribute body (non-macro attribute)
class AttrInputLiteral : public AttrInput
{
  // Literal expression WITHOUT SUFFIX
  // LiteralExpr* literal_expr;
  //::std::unique_ptr<LiteralExpr> literal_expr;
  LiteralExpr
    literal_expr; // as not using polymorphic behaviour, doesn't require pointer
  // TODO: will require pointer if LiteralExpr is changed to have subclassing

  // TODO: should this store location data?

public:
  AttrInputLiteral (LiteralExpr lit_expr)
    : literal_expr (::std::move (lit_expr))
  {}
  /*~AttrInputLiteral() {
      delete literal_expr;
  }*/

  ::std::string as_string () const { return " = " + literal_expr.as_string (); }

  virtual void accept_vis (ASTVisitor &vis) OVERRIDE;

  // this can never be a cfg predicate - cfg and cfg_attr require a token-tree
  // cfg
  virtual bool
  check_cfg_predicate (const Session &session ATTRIBUTE_UNUSED) const OVERRIDE
  {
    // TODO: ensure this is true
    // DEBUG
    fprintf (stderr, "check_cfg_predicate call went to AttrInputLiteral - "
		     "should not happen?\n");

    return false;
  }

protected:
  // Use covariance to implement clone function as returning an AttrInputLiteral
  // object
  virtual AttrInputLiteral *clone_attr_input_impl () const OVERRIDE
  {
    return new AttrInputLiteral (*this);
  }
};

// literal expr only meta item inner - TODO possibly replace with inheritance of
// LiteralExpr itself?
class MetaItemLitExpr : public MetaItemInner
{
  LiteralExpr lit_expr;

public:
  MetaItemLitExpr (LiteralExpr lit_expr) : lit_expr (::std::move (lit_expr)) {}

  ::std::string as_string () const OVERRIDE { return lit_expr.as_string (); }

  virtual void accept_vis (ASTVisitor &vis) OVERRIDE;

  virtual bool check_cfg_predicate (const Session &session) const OVERRIDE;

protected:
  // Use covariance to implement clone function as returning this type
  virtual MetaItemLitExpr *clone_meta_item_inner_impl () const OVERRIDE
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
    : path (::std::move (path)), lit (::std::move (lit_expr))
  {}

  ::std::string as_string () const OVERRIDE
  {
    return path.as_string () + " = " + lit.as_string ();
  }

  virtual void accept_vis (ASTVisitor &vis) OVERRIDE;

  virtual bool check_cfg_predicate (const Session &session) const OVERRIDE;
  // TODO: return true if "ident" is defined and value of it is "lit", return
  // false otherwise

protected:
  // Use covariance to implement clone function as returning this type
  virtual MetaItemPathLit *clone_meta_item_inner_impl () const OVERRIDE
  {
    return new MetaItemPathLit (*this);
  }
};

// AST node for a non-qualified path expression - FIXME: should this be
// inheritance instead?
/*class PathExprNonQual : public PathExpr {
    PathInExpression path;

  public:
    ::std::string as_string() const {
	return path.as_string();
    }

    PathExprNonQual(PathInExpression path, ::std::vector<Attribute>
outer_attribs) : PathExpr(::std::move(outer_attribs)), path(::std::move(path))
{}

  protected:
    // Use covariance to implement clone function as returning this object
rather than base virtual PathExprNonQual* clone_expr_impl() const OVERRIDE {
	return new PathExprNonQual(*this);
    }

    // Use covariance to implement clone function as returning this object
rather than base virtual PathExprNonQual* clone_expr_without_block_impl() const
OVERRIDE { return new PathExprNonQual(*this);
    }
};*/
// converted to inheritance

// AST node for a qualified path expression - FIXME: should this be inheritance
// instead?
/*class PathExprQual : public PathExpr {
    QualifiedPathInExpression path;

  public:
    ::std::string as_string() const {
	return path.as_string();
    }

    PathExprQual(QualifiedPathInExpression path, ::std::vector<Attribute>
outer_attribs) : PathExpr(::std::move(outer_attribs)), path(::std::move(path))
{}

  protected:
    // Use covariance to implement clone function as returning this object
rather than base virtual PathExprQual* clone_expr_impl() const OVERRIDE { return
new PathExprQual(*this);
    }

    // Use covariance to implement clone function as returning this object
rather than base virtual PathExprQual* clone_expr_without_block_impl() const
OVERRIDE { return new PathExprQual(*this);
    }
};*/
// replaced with inheritance

// Represents an expression using unary or binary operators as AST node. Can be
// overloaded.
class OperatorExpr : public ExprWithoutBlock
{
  // TODO: create binary and unary operator subclasses?
public:
  Location locus;

protected:
  // Variable must be protected to allow derived classes to use it as a first
  // class citizen Expr* main_or_left_expr;
  ::std::unique_ptr<Expr> main_or_left_expr;

  // Constructor (only for initialisation of expr purposes)
  OperatorExpr (::std::unique_ptr<Expr> main_or_left_expr,
		::std::vector<Attribute> outer_attribs, Location locus)
    : ExprWithoutBlock (::std::move (outer_attribs)), locus (locus),
      main_or_left_expr (::std::move (main_or_left_expr))
  {}

  // Copy constructor (only for initialisation of expr purposes)
  OperatorExpr (OperatorExpr const &other)
    : ExprWithoutBlock (other),
      locus (
	other
	  .locus) /*, main_or_left_expr(other.main_or_left_expr->clone_expr())*/
  {
    // DEBUG: moved main_or_left_expr into body - move back later

    if (other.main_or_left_expr == NULL)
      {
	fprintf (stderr, "other operator expr's main_or_left_expr is null!\n");
      }

    fprintf (stderr, "called operator expr copy constructor - about to clone "
		     "main_or_left_expr\n");
    main_or_left_expr = other.main_or_left_expr->clone_expr ();
    fprintf (stderr, "successfully cloned main_or_left_expr\n");
    // this occurred successfully, so something else must be the issue
  }

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
  /*virtual ~OperatorExpr() {
      delete main_or_left_expr;
  }*/

  Location get_locus () const { return locus; }

  Location get_locus_slow () const OVERRIDE { return get_locus (); }
};

// Unary prefix & or &mut (or && and &&mut) borrow operator. Cannot be
// overloaded.
class BorrowExpr : public OperatorExpr
{
  bool is_mut;
  bool double_borrow;

public:
  ::std::string as_string () const;

  BorrowExpr (::std::unique_ptr<Expr> borrow_lvalue, bool is_mut_borrow,
	      bool is_double_borrow, ::std::vector<Attribute> outer_attribs,
	      Location locus)
    : OperatorExpr (::std::move (borrow_lvalue), ::std::move (outer_attribs),
		    locus),
      is_mut (is_mut_borrow), double_borrow (is_double_borrow)
  {}

  // Copy constructor - define here if required

  // Destructor - define here if required

  // Overload assignment operator here if required

  // Move semantics here if required

  virtual void accept_vis (ASTVisitor &vis) OVERRIDE;

protected:
  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual BorrowExpr *clone_expr_impl () const OVERRIDE
  {
    return new BorrowExpr (*this);
  }

  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual BorrowExpr *clone_expr_without_block_impl () const OVERRIDE
  {
    // DEBUG
    fprintf (stderr, "called clone_expr_without_block_impl() on borrowexpr\n");

    return new BorrowExpr (*this);
  }
};

// Unary prefix * deference operator
class DereferenceExpr : public OperatorExpr
{
public:
  ::std::string as_string () const;

  // Constructor calls OperatorExpr's protected constructor
  DereferenceExpr (::std::unique_ptr<Expr> deref_lvalue,
		   ::std::vector<Attribute> outer_attribs, Location locus)
    : OperatorExpr (::std::move (deref_lvalue), ::std::move (outer_attribs),
		    locus)
  {}

  // Copy constructor - define here if required

  // Destructor - define here if required

  // Overload assignment operator here if required

  // Move semantics here if required

  virtual void accept_vis (ASTVisitor &vis) OVERRIDE;

protected:
  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual DereferenceExpr *clone_expr_impl () const OVERRIDE
  {
    return new DereferenceExpr (*this);
  }

  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual DereferenceExpr *clone_expr_without_block_impl () const OVERRIDE
  {
    // DEBUG
    fprintf (stderr,
	     "called clone_expr_without_block_impl() on dereferenceexpr\n");

    return new DereferenceExpr (*this);
  }
};

// Unary postfix ? error propogation operator. Cannot be overloaded.
class ErrorPropagationExpr : public OperatorExpr
{
public:
  ::std::string as_string () const;

  // Constructor calls OperatorExpr's protected constructor
  ErrorPropagationExpr (::std::unique_ptr<Expr> potential_error_value,
			::std::vector<Attribute> outer_attribs, Location locus)
    : OperatorExpr (::std::move (potential_error_value),
		    ::std::move (outer_attribs), locus)
  {}

  // Copy constructor - define here if required

  // Destructor - define here if required

  // Overload assignment operator here if required

  // Move semantics here if required

  virtual void accept_vis (ASTVisitor &vis) OVERRIDE;

protected:
  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual ErrorPropagationExpr *clone_expr_impl () const OVERRIDE
  {
    return new ErrorPropagationExpr (*this);
  }

  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual ErrorPropagationExpr *clone_expr_without_block_impl () const OVERRIDE
  {
    // DEBUG
    fprintf (
      stderr,
      "called clone_expr_without_block_impl() on errorpropagationexpr\n");

    return new ErrorPropagationExpr (*this);
  }
};

// Unary prefix - or ! negation or NOT operators.
class NegationExpr : public OperatorExpr
{
public:
  enum NegationType
  {
    NEGATE,
    NOT
  };

private:
  // Note: overload negation via std::ops::Neg and not via std::ops::Not
  // Negation only works for signed integer and floating-point types, NOT only
  // works for boolean and integer types (via bitwise NOT)
  NegationType negation_type;

public:
  ::std::string as_string () const;

  inline NegationType get_negation_type () const { return negation_type; }

  // Constructor calls OperatorExpr's protected constructor
  NegationExpr (::std::unique_ptr<Expr> negated_value,
		NegationType negation_kind,
		::std::vector<Attribute> outer_attribs, Location locus)
    : OperatorExpr (::std::move (negated_value), ::std::move (outer_attribs),
		    locus),
      negation_type (negation_kind)
  {}

  // Copy constructor - define here if required

  // Destructor - define here if required

  // Overload assignment operator here if required

  // Move semantics here if required

  virtual void accept_vis (ASTVisitor &vis) OVERRIDE;

protected:
  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual NegationExpr *clone_expr_impl () const OVERRIDE
  {
    return new NegationExpr (*this);
  }

  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual NegationExpr *clone_expr_without_block_impl () const OVERRIDE
  {
    // DEBUG
    fprintf (stderr,
	     "called clone_expr_without_block_impl() on negationexpr\n");

    return new NegationExpr (*this);
  }
};

// Infix binary operators. +, -, *, /, %, &, |, ^, <<, >>
class ArithmeticOrLogicalExpr : public OperatorExpr
{
public:
  enum ExprType
  {
    ADD,	 // std::ops::Add
    SUBTRACT,	 // std::ops::Sub
    MULTIPLY,	 // std::ops::Mul
    DIVIDE,	 // std::ops::Div
    MODULUS,	 // std::ops::Rem
    BITWISE_AND, // std::ops::BitAnd
    BITWISE_OR,	 // std::ops::BitOr
    BITWISE_XOR, // std::ops::BitXor
    LEFT_SHIFT,	 // std::ops::Shl
    RIGHT_SHIFT	 // std::ops::Shr
  };

  // Note: overloading trait specified in comments
  ExprType expr_type;

  // Expr* right_expr;
  ::std::unique_ptr<Expr> right_expr;

public:
  /*~ArithmeticOrLogicalExpr() {
      delete right_expr;
  }*/

  ::std::string as_string () const;

  inline ExprType get_expr_type () const { return expr_type; }

  // Constructor calls OperatorExpr's protected constructor
  ArithmeticOrLogicalExpr (::std::unique_ptr<Expr> left_value,
			   ::std::unique_ptr<Expr> right_value,
			   ExprType expr_kind, Location locus)
    : OperatorExpr (::std::move (left_value), ::std::vector<Attribute> (),
		    locus),
      expr_type (expr_kind), right_expr (::std::move (right_value))
  {}
  // outer attributes not allowed

  // Copy constructor - probably required due to unique pointer
  ArithmeticOrLogicalExpr (ArithmeticOrLogicalExpr const &other)
    : OperatorExpr (other), expr_type (other.expr_type),
      right_expr (other.right_expr->clone_expr ())
  {}

  // Destructor - define here if required

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

  virtual void accept_vis (ASTVisitor &vis) OVERRIDE;

  Expr *get_lhs () { return main_or_left_expr.get (); }

  void visit_lhs (ASTVisitor &vis) { main_or_left_expr->accept_vis (vis); }

  void visit_rhs (ASTVisitor &vis) { right_expr->accept_vis (vis); }

protected:
  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual ArithmeticOrLogicalExpr *clone_expr_impl () const OVERRIDE
  {
    return new ArithmeticOrLogicalExpr (*this);
  }

  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual ArithmeticOrLogicalExpr *
  clone_expr_without_block_impl () const OVERRIDE
  {
    // DEBUG
    fprintf (
      stderr,
      "called clone_expr_without_block_impl() on arithmeticorlogicalexpr\n");

    return new ArithmeticOrLogicalExpr (*this);
  }
};

// Infix binary comparison operators. ==, !=, <, <=, >, >=
class ComparisonExpr : public OperatorExpr
{
public:
  enum ExprType
  {
    EQUAL,	      // std::cmp::PartialEq::eq
    NOT_EQUAL,	      // std::cmp::PartialEq::ne
    GREATER_THAN,     // std::cmp::PartialEq::gt
    LESS_THAN,	      // std::cmp::PartialEq::lt
    GREATER_OR_EQUAL, // std::cmp::PartialEq::ge
    LESS_OR_EQUAL     // std::cmp::PartialEq::le
  };

private:
  // Note: overloading trait specified in comments
  ExprType expr_type;

  // Expr* right_expr;
  ::std::unique_ptr<Expr> right_expr;

public:
  /*~ComparisonExpr() {
      delete right_expr;
  }*/

  ::std::string as_string () const;

  inline ExprType get_expr_type () const { return expr_type; }

  // Constructor requires pointers for polymorphism
  ComparisonExpr (::std::unique_ptr<Expr> left_value,
		  ::std::unique_ptr<Expr> right_value, ExprType comparison_kind,
		  Location locus)
    : OperatorExpr (::std::move (left_value), ::std::vector<Attribute> (),
		    locus),
      expr_type (comparison_kind), right_expr (::std::move (right_value))
  {}
  // outer attributes not allowed

  // Copy constructor also calls OperatorExpr's protected constructor
  ComparisonExpr (ComparisonExpr const &other)
    : OperatorExpr (other), expr_type (other.expr_type),
      right_expr (other.right_expr->clone_expr ())
  {}

  // Destructor - define here if required

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

  virtual void accept_vis (ASTVisitor &vis) OVERRIDE;

  // TODO: implement via a function call to std::cmp::PartialEq::eq(&op1, &op2)
  // maybe?
protected:
  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual ComparisonExpr *clone_expr_impl () const OVERRIDE
  {
    return new ComparisonExpr (*this);
  }

  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual ComparisonExpr *clone_expr_without_block_impl () const OVERRIDE
  {
    // DEBUG
    fprintf (stderr,
	     "called clone_expr_without_block_impl() on comparisonexpr\n");

    return new ComparisonExpr (*this);
  }
};

// Infix binary lazy boolean logical operators && and ||.
class LazyBooleanExpr : public OperatorExpr
{
public:
  enum ExprType
  {
    LOGICAL_OR,
    LOGICAL_AND
  };

private:
  ExprType expr_type;

  // Expr* right_expr;
  ::std::unique_ptr<Expr> right_expr;

public:
  /*~LazyBooleanExpr() {
      delete right_expr;
  }*/

  // Constructor calls OperatorExpr's protected constructor
  LazyBooleanExpr (::std::unique_ptr<Expr> left_bool_expr,
		   ::std::unique_ptr<Expr> right_bool_expr, ExprType expr_kind,
		   Location locus)
    : OperatorExpr (::std::move (left_bool_expr), ::std::vector<Attribute> (),
		    locus),
      expr_type (expr_kind), right_expr (::std::move (right_bool_expr))
  {}
  // outer attributes not allowed

  // Copy constructor also calls OperatorExpr's protected constructor
  LazyBooleanExpr (LazyBooleanExpr const &other)
    : OperatorExpr (other), expr_type (other.expr_type),
      right_expr (other.right_expr->clone_expr ())
  {}

  // Destructor - define here if required

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

  ::std::string as_string () const;

  inline ExprType get_expr_type () const { return expr_type; }

  virtual void accept_vis (ASTVisitor &vis) OVERRIDE;

protected:
  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual LazyBooleanExpr *clone_expr_impl () const OVERRIDE
  {
    return new LazyBooleanExpr (*this);
  }

  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual LazyBooleanExpr *clone_expr_without_block_impl () const OVERRIDE
  {
    // DEBUG
    fprintf (stderr,
	     "called clone_expr_without_block_impl() on lazybooleanexpr\n");

    return new LazyBooleanExpr (*this);
  }
};

// Binary infix "as" cast expression.
class TypeCastExpr : public OperatorExpr
{
  // TypeNoBounds type_to_convert_to;
  ::std::unique_ptr<TypeNoBounds> type_to_convert_to;

  // Note: only certain type casts allowed, outlined in reference
public:
  ::std::string as_string () const;

  // Constructor requires calling protected constructor of OperatorExpr
  TypeCastExpr (::std::unique_ptr<Expr> expr_to_cast,
		::std::unique_ptr<TypeNoBounds> type_to_cast_to, Location locus)
    : OperatorExpr (::std::move (expr_to_cast), ::std::vector<Attribute> (),
		    locus),
      type_to_convert_to (::std::move (type_to_cast_to))
  {}
  // outer attributes not allowed

  // Copy constructor also requires calling protected constructor
  TypeCastExpr (TypeCastExpr const &other)
    : OperatorExpr (other),
      type_to_convert_to (other.type_to_convert_to->clone_type_no_bounds ())
  {}

  // Destructor - define here if required

  // Overload assignment operator to deep copy
  TypeCastExpr &operator= (TypeCastExpr const &other)
  {
    OperatorExpr::operator= (other);
    // main_or_left_expr = other.main_or_left_expr->clone_expr();
    type_to_convert_to = other.type_to_convert_to->clone_type_no_bounds ();

    return *this;
  }

  // move constructors as not supported in c++03
  TypeCastExpr (TypeCastExpr &&other) = default;
  TypeCastExpr &operator= (TypeCastExpr &&other) = default;

  virtual void accept_vis (ASTVisitor &vis) OVERRIDE;

protected:
  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual TypeCastExpr *clone_expr_impl () const OVERRIDE
  {
    return new TypeCastExpr (*this);
  }

  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual TypeCastExpr *clone_expr_without_block_impl () const OVERRIDE
  {
    // DEBUG
    fprintf (stderr,
	     "called clone_expr_without_block_impl() on typecastexpr\n");

    return new TypeCastExpr (*this);
  }
};

// Binary assignment expression.
class AssignmentExpr : public OperatorExpr
{
public:
  // Expr* right_expr;
  ::std::unique_ptr<Expr> right_expr;

  /*~AssignmentExpr() {
      delete right_expr;
  }*/

  ::std::string as_string () const;

  // Call OperatorExpr constructor to initialise left_expr
  AssignmentExpr (::std::unique_ptr<Expr> value_to_assign_to,
		  ::std::unique_ptr<Expr> value_to_assign, Location locus)
    : OperatorExpr (::std::move (value_to_assign_to),
		    ::std::vector<Attribute> (), locus),
      right_expr (::std::move (value_to_assign))
  {}
  // outer attributes not allowed

  // Call OperatorExpr constructor in copy constructor, as well as clone
  AssignmentExpr (AssignmentExpr const &other)
    : OperatorExpr (other) /*, right_expr(other.right_expr->clone_expr())*/
  {
    // DEBUG: moved cloning right expr into body
    fprintf (stderr, "assignment expr copy constructor successfully cloned "
		     "base operator expr\n");
    if (other.right_expr == NULL)
      {
	fprintf (stderr, "other expr's right expr (in assignment) is null!!!");
      }
    fprintf (stderr, "test other's right expr as string: %s\n",
	     other.right_expr->as_string ().c_str ());
    // apparently, despite not being null, cloning still fails
    right_expr = other.right_expr->clone_expr ();
    fprintf (
      stderr,
      "assignment expr copy constructor successfully cloned right expr\n");

    // DEBUG
    fprintf (stderr, "assignment expr copy constructor called successfully\n");
  }

  // Destructor - define here if required

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

  virtual void accept_vis (ASTVisitor &vis) OVERRIDE;

  void visit_lhs (ASTVisitor &vis) { main_or_left_expr->accept_vis (vis); }

  void visit_rhs (ASTVisitor &vis) { right_expr->accept_vis (vis); }

protected:
  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual AssignmentExpr *clone_expr_impl () const OVERRIDE
  {
    return new AssignmentExpr (*this);
  }

  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual AssignmentExpr *clone_expr_without_block_impl () const OVERRIDE
  {
    // DEBUG
    fprintf (stderr,
	     "called clone_expr_without_block_impl() on assignmentexpr\n");

    return new AssignmentExpr (*this);
  }
};

// Binary infix compound assignment (arithmetic or logic then assignment)
// expressions.
class CompoundAssignmentExpr : public OperatorExpr
{
public:
  enum ExprType
  {
    ADD,	 // std::ops::AddAssign
    SUBTRACT,	 // std::ops::SubAssign
    MULTIPLY,	 // std::ops::MulAssign
    DIVIDE,	 // std::ops::DivAssign
    MODULUS,	 // std::ops::RemAssign
    BITWISE_AND, // std::ops::BitAndAssign
    BITWISE_OR,	 // std::ops::BitOrAssign
    BITWISE_XOR, // std::ops::BitXorAssign
    LEFT_SHIFT,	 // std::ops::ShlAssign
    RIGHT_SHIFT	 // std::ops::ShrAssign
  };

private:
  // Note: overloading trait specified in comments
  ExprType expr_type;

  // Expr* right_expr;
  ::std::unique_ptr<Expr> right_expr;

public:
  /*~CompoundAssignmentExpr() {
      delete right_expr;
  }*/

  ::std::string as_string () const;

  inline ExprType get_expr_type () const { return expr_type; }

  // Use pointers in constructor to enable polymorphism
  CompoundAssignmentExpr (::std::unique_ptr<Expr> value_to_assign_to,
			  ::std::unique_ptr<Expr> value_to_assign,
			  ExprType expr_kind, Location locus)
    : OperatorExpr (::std::move (value_to_assign_to),
		    ::std::vector<Attribute> (), locus),
      expr_type (expr_kind), right_expr (::std::move (value_to_assign))
  {}
  // outer attributes not allowed

  // Have clone in copy constructor
  CompoundAssignmentExpr (CompoundAssignmentExpr const &other)
    : OperatorExpr (other), expr_type (other.expr_type),
      right_expr (other.right_expr->clone_expr ())
  {}

  // Destructor - define here if required

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

  virtual void accept_vis (ASTVisitor &vis) OVERRIDE;

protected:
  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual CompoundAssignmentExpr *clone_expr_impl () const OVERRIDE
  {
    return new CompoundAssignmentExpr (*this);
  }

  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual CompoundAssignmentExpr *
  clone_expr_without_block_impl () const OVERRIDE
  {
    // DEBUG
    fprintf (
      stderr,
      "called clone_expr_without_block_impl() on compoundassignmentexpr\n");

    return new CompoundAssignmentExpr (*this);
  }
};

// Expression in parentheses (i.e. like literally just any 3 + (2 * 6))
class GroupedExpr : public ExprWithoutBlock
{
  ::std::vector<Attribute> inner_attrs;
  // Expr* expr_in_parens;
  ::std::unique_ptr<Expr> expr_in_parens;

  Location locus;

public:
  /*~GroupedExpr() {
      delete expr_in_parens;
  }*/

  ::std::string as_string () const;

  inline ::std::vector<Attribute> get_inner_attrs () const
  {
    return inner_attrs;
  }

  GroupedExpr (::std::unique_ptr<Expr> parenthesised_expr,
	       ::std::vector<Attribute> inner_attribs,
	       ::std::vector<Attribute> outer_attribs, Location locus)
    : ExprWithoutBlock (::std::move (outer_attribs)),
      inner_attrs (::std::move (inner_attribs)),
      expr_in_parens (::std::move (parenthesised_expr)), locus (locus)
  {}

  // Copy constructor includes clone for expr_in_parens
  GroupedExpr (GroupedExpr const &other)
    : ExprWithoutBlock (other), inner_attrs (other.inner_attrs),
      expr_in_parens (other.expr_in_parens->clone_expr ()), locus (other.locus)
  {}

  // Destructor - define here if required

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

  Location get_locus () const { return locus; }

  Location get_locus_slow () const OVERRIDE { return get_locus (); }

  virtual void accept_vis (ASTVisitor &vis) OVERRIDE;

protected:
  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual GroupedExpr *clone_expr_impl () const OVERRIDE
  {
    return new GroupedExpr (*this);
  }

  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual GroupedExpr *clone_expr_without_block_impl () const OVERRIDE
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
  ::std::unique_ptr<ArrayElems> clone_array_elems () const
  {
    return ::std::unique_ptr<ArrayElems> (clone_array_elems_impl ());
  }

  virtual ::std::string as_string () const = 0;

  virtual void accept_vis (ASTVisitor &vis) = 0;

protected:
  // pure virtual clone implementation
  virtual ArrayElems *clone_array_elems_impl () const = 0;
};

// Value array elements
class ArrayElemsValues : public ArrayElems
{
  //::std::vector<Expr> values;
  ::std::vector< ::std::unique_ptr<Expr> > values;

  // TODO: should this store location data?

public:
  /*inline ::std::vector< ::std::unique_ptr<Expr> > get_values() const {
      return values;
  }*/

  ArrayElemsValues (::std::vector< ::std::unique_ptr<Expr> > elems)
    : values (::std::move (elems))
  {}

  // copy constructor with vector clone
  ArrayElemsValues (ArrayElemsValues const &other)
  {
    // crappy vector unique pointer clone - TODO is there a better way of doing
    // this?
    values.reserve (other.values.size ());

    for (const auto &e : other.values)
      {
	values.push_back (e->clone_expr ());
      }
  }

  // overloaded assignment operator with vector clone
  ArrayElemsValues &operator= (ArrayElemsValues const &other)
  {
    // crappy vector unique pointer clone - TODO is there a better way of doing
    // this?
    values.reserve (other.values.size ());

    for (const auto &e : other.values)
      {
	values.push_back (e->clone_expr ());
      }

    return *this;
  }

  // move constructors
  ArrayElemsValues (ArrayElemsValues &&other) = default;
  ArrayElemsValues &operator= (ArrayElemsValues &&other) = default;

  ::std::string as_string () const;

  virtual void accept_vis (ASTVisitor &vis) OVERRIDE;

protected:
  virtual ArrayElemsValues *clone_array_elems_impl () const OVERRIDE
  {
    return new ArrayElemsValues (*this);
  }
};

// Copied array element and number of copies
class ArrayElemsCopied : public ArrayElems
{
  // Expr* elem_to_copy;
  ::std::unique_ptr<Expr> elem_to_copy;
  // Expr* num_copies;
  ::std::unique_ptr<Expr> num_copies;

  // TODO: should this store location data?

public:
  /*~ArrayElemsCopied() {
      delete num_copies;
      delete elem_to_copy;
  }*/

  // Constructor requires pointers for polymorphism
  ArrayElemsCopied (::std::unique_ptr<Expr> copied_elem,
		    ::std::unique_ptr<Expr> copy_amount)
    : elem_to_copy (::std::move (copied_elem)),
      num_copies (::std::move (copy_amount))
  {}

  // Copy constructor required due to unique_ptr - uses custom clone
  ArrayElemsCopied (ArrayElemsCopied const &other)
    : elem_to_copy (other.elem_to_copy->clone_expr ()),
      num_copies (other.num_copies->clone_expr ())
  {}

  // Destructor - define here if required

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

  ::std::string as_string () const;

  virtual void accept_vis (ASTVisitor &vis) OVERRIDE;

protected:
  virtual ArrayElemsCopied *clone_array_elems_impl () const OVERRIDE
  {
    return new ArrayElemsCopied (*this);
  }
};

// Array definition-ish expression
class ArrayExpr : public ExprWithoutBlock
{
  ::std::vector<Attribute> inner_attrs;
  // ArrayElems internal_elements;
  ::std::unique_ptr<ArrayElems> internal_elements;

  Location locus;

public:
  ::std::string as_string () const;

  inline ::std::vector<Attribute> get_inner_attrs () const
  {
    return inner_attrs;
  }

  // Returns whether array expr has array elems or if it is just empty.
  inline bool has_array_elems () const { return internal_elements != NULL; }

  // Constructor requires ArrayElems pointer
  ArrayExpr (::std::unique_ptr<ArrayElems> array_elems,
	     ::std::vector<Attribute> inner_attribs,
	     ::std::vector<Attribute> outer_attribs, Location locus)
    : ExprWithoutBlock (::std::move (outer_attribs)),
      inner_attrs (::std::move (inner_attribs)),
      internal_elements (::std::move (array_elems)), locus (locus)
  {}

  // Copy constructor requires cloning ArrayElems for polymorphism to hold
  ArrayExpr (ArrayExpr const &other)
    : ExprWithoutBlock (other), inner_attrs (other.inner_attrs),
      locus (other.locus)
  {
    if (other.has_array_elems ())
      {
	internal_elements = other.internal_elements->clone_array_elems ();
      }
  }

  // Destructor - define here if required

  // Overload assignment operator to clone internal_elements
  ArrayExpr &operator= (ArrayExpr const &other)
  {
    ExprWithoutBlock::operator= (other);
    inner_attrs = other.inner_attrs;
    if (other.has_array_elems ())
      {
	internal_elements = other.internal_elements->clone_array_elems ();
      }
    locus = other.locus;
    // outer_attrs = other.outer_attrs;

    return *this;
  }

  // move constructors
  ArrayExpr (ArrayExpr &&other) = default;
  ArrayExpr &operator= (ArrayExpr &&other) = default;

  Location get_locus () const { return locus; }

  Location get_locus_slow () const OVERRIDE { return get_locus (); }

  virtual void accept_vis (ASTVisitor &vis) OVERRIDE;

protected:
  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual ArrayExpr *clone_expr_impl () const OVERRIDE
  {
    return new ArrayExpr (*this);
  }

  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual ArrayExpr *clone_expr_without_block_impl () const OVERRIDE
  {
    return new ArrayExpr (*this);
  }
};

// Aka IndexExpr (also applies to slices)
// Apparently a[b] is equivalent to *std::ops::Index::index(&a, b) or
// *std::ops::Index::index_mut(&mut a, b)
// Also apparently deref operations on a will be repeatedly applied to find an
// implementation
class ArrayIndexExpr : public ExprWithoutBlock
{
  /*Expr* array_expr;
  Expr* index_expr;*/
  ::std::unique_ptr<Expr> array_expr;
  ::std::unique_ptr<Expr> index_expr;

  Location locus;

public:
  /*~ArrayIndexExpr() {
      delete index_expr;
      delete array_expr;
  }*/

  ::std::string as_string () const;

  ArrayIndexExpr (::std::unique_ptr<Expr> array_expr,
		  ::std::unique_ptr<Expr> array_index_expr,
		  ::std::vector<Attribute> outer_attribs, Location locus)
    : ExprWithoutBlock (::std::move (outer_attribs)),
      array_expr (::std::move (array_expr)),
      index_expr (::std::move (array_index_expr)), locus (locus)
  {}

  // Copy constructor requires special cloning due to unique_ptr
  ArrayIndexExpr (ArrayIndexExpr const &other)
    : ExprWithoutBlock (other), array_expr (other.array_expr->clone_expr ()),
      index_expr (other.index_expr->clone_expr ()), locus (other.locus)
  {}

  // Destructor - define here if required

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

  Location get_locus () const { return locus; }

  Location get_locus_slow () const OVERRIDE { return get_locus (); }

  virtual void accept_vis (ASTVisitor &vis) OVERRIDE;

protected:
  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual ArrayIndexExpr *clone_expr_impl () const OVERRIDE
  {
    return new ArrayIndexExpr (*this);
  }

  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual ArrayIndexExpr *clone_expr_without_block_impl () const OVERRIDE
  {
    return new ArrayIndexExpr (*this);
  }
};

// AST representation of a tuple
class TupleExpr : public ExprWithoutBlock
{
  ::std::vector<Attribute> inner_attrs;

  //::std::vector<Expr> tuple_elems;
  ::std::vector< ::std::unique_ptr<Expr> > tuple_elems;
  // replaces (inlined version of) TupleElements

  Location locus;

public:
  ::std::string as_string () const;

  inline ::std::vector<Attribute> get_inner_attrs () const
  {
    return inner_attrs;
  }

  TupleExpr (::std::vector< ::std::unique_ptr<Expr> > tuple_elements,
	     ::std::vector<Attribute> inner_attribs,
	     ::std::vector<Attribute> outer_attribs, Location locus)
    : ExprWithoutBlock (::std::move (outer_attribs)),
      inner_attrs (::std::move (inner_attribs)),
      tuple_elems (::std::move (tuple_elements)), locus (locus)
  {}

  // copy constructor with vector clone
  TupleExpr (TupleExpr const &other)
    : ExprWithoutBlock (other), inner_attrs (other.inner_attrs),
      locus (other.locus)
  {
    // crappy vector unique pointer clone - TODO is there a better way of doing
    // this?
    tuple_elems.reserve (other.tuple_elems.size ());

    for (const auto &e : other.tuple_elems)
      {
	tuple_elems.push_back (e->clone_expr ());
      }
  }

  // overloaded assignment operator to vector clone
  TupleExpr &operator= (TupleExpr const &other)
  {
    ExprWithoutBlock::operator= (other);
    inner_attrs = other.inner_attrs;
    locus = other.locus;

    // crappy vector unique pointer clone - TODO is there a better way of doing
    // this?
    tuple_elems.reserve (other.tuple_elems.size ());

    for (const auto &e : other.tuple_elems)
      {
	tuple_elems.push_back (e->clone_expr ());
      }

    return *this;
  }

  // move constructors
  TupleExpr (TupleExpr &&other) = default;
  TupleExpr &operator= (TupleExpr &&other) = default;

  // Note: syntactically, can disambiguate single-element tuple from parens with
  // comma, i.e. (0,) rather than (0)

  Location get_locus () const { return locus; }

  Location get_locus_slow () const OVERRIDE { return get_locus (); }

  virtual void accept_vis (ASTVisitor &vis) OVERRIDE;

protected:
  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual TupleExpr *clone_expr_impl () const OVERRIDE
  {
    return new TupleExpr (*this);
  }

  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual TupleExpr *clone_expr_without_block_impl () const OVERRIDE
  {
    return new TupleExpr (*this);
  }
};

// aka TupleIndexingExpr
// AST representation of a tuple indexing expression
class TupleIndexExpr : public ExprWithoutBlock
{
  // Expr* tuple_expr;
  ::std::unique_ptr<Expr> tuple_expr;
  // TupleIndex is a decimal int literal with no underscores or suffix
  TupleIndex tuple_index;

  Location locus;

  // i.e. pair.0

public:
  /*~TupleIndexExpr() {
      delete tuple_expr;
  }*/

  ::std::string as_string () const;

  inline TupleIndex get_tuple_index () const { return tuple_index; }

  TupleIndexExpr (::std::unique_ptr<Expr> tuple_expr, TupleIndex index,
		  ::std::vector<Attribute> outer_attribs, Location locus)
    : ExprWithoutBlock (::std::move (outer_attribs)),
      tuple_expr (::std::move (tuple_expr)), tuple_index (index), locus (locus)
  {}

  // Copy constructor requires a clone for tuple_expr
  TupleIndexExpr (TupleIndexExpr const &other)
    : ExprWithoutBlock (other), tuple_expr (other.tuple_expr->clone_expr ()),
      tuple_index (other.tuple_index), locus (other.locus)
  {}

  // Destructor - define here if required

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

  Location get_locus () const { return locus; }

  Location get_locus_slow () const OVERRIDE { return get_locus (); }

  virtual void accept_vis (ASTVisitor &vis) OVERRIDE;

protected:
  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual TupleIndexExpr *clone_expr_impl () const OVERRIDE
  {
    return new TupleIndexExpr (*this);
  }

  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual TupleIndexExpr *clone_expr_without_block_impl () const OVERRIDE
  {
    return new TupleIndexExpr (*this);
  }
};

// Base struct/tuple/union value creator AST node (abstract)
class StructExpr : public ExprWithoutBlock
{
  PathInExpression struct_name;

protected:
  // Protected constructor to allow initialising struct_name
  StructExpr (PathInExpression struct_path,
	      ::std::vector<Attribute> outer_attribs)
    : ExprWithoutBlock (::std::move (outer_attribs)),
      struct_name (::std::move (struct_path))
  {}

public:
  inline const PathInExpression &get_struct_name () const
  {
    return struct_name;
  }

  virtual ::std::string as_string () const;
};

// Actual AST node of the struct creator (with no fields). Not abstract!
class StructExprStruct : public StructExpr
{
  ::std::vector<Attribute> inner_attrs;

  Location locus;

public:
  ::std::string as_string () const;

  inline ::std::vector<Attribute> get_inner_attrs () const
  {
    return inner_attrs;
  }

  // Constructor has to call protected constructor of base class
  StructExprStruct (PathInExpression struct_path,
		    ::std::vector<Attribute> inner_attribs,
		    ::std::vector<Attribute> outer_attribs, Location locus)
    : StructExpr (::std::move (struct_path), ::std::move (outer_attribs)),
      inner_attrs (::std::move (inner_attribs)), locus (locus)
  {}

  Location get_locus () const { return locus; }

  Location get_locus_slow () const OVERRIDE { return get_locus (); }

  virtual void accept_vis (ASTVisitor &vis) OVERRIDE;

protected:
  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual StructExprStruct *clone_expr_impl () const OVERRIDE
  {
    return new StructExprStruct (*this);
  }

  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual StructExprStruct *clone_expr_without_block_impl () const OVERRIDE
  {
    return new StructExprStruct (*this);
  }
};

// AST node representing expression used to fill a struct's fields from another
// struct
struct StructBase
{
private:
  // Expr* base_struct;
  ::std::unique_ptr<Expr> base_struct;

  // TODO: should this store location data?

public:
  StructBase (::std::unique_ptr<Expr> base_struct_ptr)
    : base_struct (::std::move (base_struct_ptr))
  {}

  // Copy constructor requires clone
  StructBase (StructBase const &other)
  {
    // HACK: gets around base_struct pointer being null (e.g. if no struct base
    // exists)
    if (other.base_struct != NULL)
      {
	other.base_struct->clone_expr ();
      }

    // DEBUG:
    fprintf (stderr, "struct base copy constructor called successfully\n");
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

  /*~StructBase() {
      delete base_struct;
  }*/

  // Returns a null expr-ed StructBase - error state
  static StructBase error () { return StructBase (NULL); }

  // Returns whether StructBase is in error state
  inline bool is_invalid () const { return base_struct == NULL; }

  ::std::string as_string () const;
};

// Base AST node for a single struct expression field (in struct instance
// creation) - abstract
class StructExprField
{
public:
  virtual ~StructExprField () {}

  // Unique pointer custom clone function
  ::std::unique_ptr<StructExprField> clone_struct_expr_field () const
  {
    return ::std::unique_ptr<StructExprField> (clone_struct_expr_field_impl ());
  }

  virtual ::std::string as_string () const = 0;

  virtual void accept_vis (ASTVisitor &vis) = 0;

protected:
  // pure virtual clone implementation
  virtual StructExprField *clone_struct_expr_field_impl () const = 0;
};

// Identifier-only variant of StructExprField AST node
class StructExprFieldIdentifier : public StructExprField
{
  Identifier field_name;

  // TODO: should this store location data?

public:
  StructExprFieldIdentifier (Identifier field_identifier)
    : field_name (::std::move (field_identifier))
  {}

  ::std::string as_string () const { return field_name; }

  virtual void accept_vis (ASTVisitor &vis) OVERRIDE;

protected:
  // Use covariance to implement clone function as returning this rather than
  // base
  virtual StructExprFieldIdentifier *
  clone_struct_expr_field_impl () const OVERRIDE
  {
    return new StructExprFieldIdentifier (*this);
  }
};

// Base AST node for a single struct expression field with an assigned value -
// abstract
class StructExprFieldWithVal : public StructExprField
{
  // Expr* value;
  ::std::unique_ptr<Expr> value;

protected:
  StructExprFieldWithVal (::std::unique_ptr<Expr> field_value)
    : value (::std::move (field_value))
  {}

  // Copy constructor requires clone
  StructExprFieldWithVal (StructExprFieldWithVal const &other)
    : value (other.value->clone_expr ())
  {}

  // Destructor - define here if required

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
  /*~StructExprFieldWithVal() {
      delete value;
  }*/

  ::std::string as_string () const;
};

// Identifier and value variant of StructExprField AST node
class StructExprFieldIdentifierValue : public StructExprFieldWithVal
{
  Identifier field_name;

  // TODO: should this store location data?

public:
  StructExprFieldIdentifierValue (Identifier field_identifier,
				  ::std::unique_ptr<Expr> field_value)
    : StructExprFieldWithVal (::std::move (field_value)),
      field_name (::std::move (field_identifier))
  {}

  // copy constructor, destructor, and overloaded assignment operator should
  // carry through

  ::std::string as_string () const;

  virtual void accept_vis (ASTVisitor &vis) OVERRIDE;

protected:
  // Use covariance to implement clone function as returning this rather than
  // base
  virtual StructExprFieldIdentifierValue *
  clone_struct_expr_field_impl () const OVERRIDE
  {
    return new StructExprFieldIdentifierValue (*this);
  }
};

// Tuple index and value variant of StructExprField AST node
class StructExprFieldIndexValue : public StructExprFieldWithVal
{
  TupleIndex index;

  // TODO: should this store location data?

public:
  StructExprFieldIndexValue (TupleIndex tuple_index,
			     ::std::unique_ptr<Expr> field_value)
    : StructExprFieldWithVal (::std::move (field_value)), index (tuple_index)
  {}

  // copy constructor, destructor, and overloaded assignment operator should
  // carry through

  ::std::string as_string () const;

  virtual void accept_vis (ASTVisitor &vis) OVERRIDE;

protected:
  // Use covariance to implement clone function as returning this rather than
  // base
  virtual StructExprFieldIndexValue *
  clone_struct_expr_field_impl () const OVERRIDE
  {
    return new StructExprFieldIndexValue (*this);
  }
};

// AST node of a struct creator with fields
class StructExprStructFields : public StructExprStruct
{
  //::std::vector<StructExprField> fields;
  ::std::vector< ::std::unique_ptr<StructExprField> > fields;

  // bool has_struct_base;
  StructBase struct_base;

public:
  ::std::string as_string () const;

  inline bool has_struct_base () const { return !struct_base.is_invalid (); }

  /*inline ::std::vector< ::std::unique_ptr<StructExprField> > get_fields()
  const { return fields;
  }*/

  /*inline StructBase get_struct_base() const {
      return has_struct_base ? struct_base : StructBase::error();
  }*/

  // Constructor for StructExprStructFields when no struct base is used
  StructExprStructFields (
    PathInExpression struct_path,
    ::std::vector< ::std::unique_ptr<StructExprField> > expr_fields,
    Location locus, StructBase base_struct = StructBase::error (),
    ::std::vector<Attribute> inner_attribs = ::std::vector<Attribute> (),
    ::std::vector<Attribute> outer_attribs = ::std::vector<Attribute> ())
    : StructExprStruct (::std::move (struct_path), ::std::move (inner_attribs),
			::std::move (outer_attribs), locus),
      fields (::std::move (expr_fields)),
      struct_base (::std::move (base_struct))
  {}

  // copy constructor with vector clone
  StructExprStructFields (StructExprStructFields const &other)
    : StructExprStruct (other), struct_base (other.struct_base)
  {
    // DEBUG
    fprintf (stderr,
	     "got past the initialisation list part of copy constructor\n");

    // crappy vector unique pointer clone - TODO is there a better way of doing
    // this?
    fields.reserve (other.fields.size ());

    // DEBUG
    fprintf (stderr, "reserved space in fields\n");

    for (const auto &e : other.fields)
      {
	// DEBUG
	fprintf (stderr, "about to clone a field\n");

	fields.push_back (e->clone_struct_expr_field ());

	// DEBUG
	fprintf (stderr, "cloned a field successfully\n");
      }

    // DEBUG
    fprintf (stderr, "finished cloning fields\n");
  }

  // overloaded assignment operator with vector clone
  StructExprStructFields &operator= (StructExprStructFields const &other)
  {
    StructExprStruct::operator= (other);
    struct_base = other.struct_base;

    // crappy vector unique pointer clone - TODO is there a better way of doing
    // this?
    fields.reserve (other.fields.size ());

    for (const auto &e : other.fields)
      {
	fields.push_back (e->clone_struct_expr_field ());
      }

    return *this;
  }

  // move constructors
  StructExprStructFields (StructExprStructFields &&other) = default;
  StructExprStructFields &operator= (StructExprStructFields &&other) = default;

  virtual void accept_vis (ASTVisitor &vis) OVERRIDE;

protected:
  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual StructExprStructFields *clone_expr_impl () const OVERRIDE
  {
    return new StructExprStructFields (*this);
  }

  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual StructExprStructFields *
  clone_expr_without_block_impl () const OVERRIDE
  {
    // DEBUG
    fprintf (
      stderr,
      "called structexprstructfields clone expr without block impl - about "
      "to return new structexprstructfields\n");

    // DEBUG - test creation of a base from this
    fprintf (stderr, "about to try to create and allocate structexprstruct \n");
    StructExprStruct *test_DELETE = new StructExprStruct (*this);
    delete test_DELETE;
    fprintf (stderr, "managed to create and allocate structexprstruct \n");
    // very weird: can create and allocate structexpstruct but not
    // structexprstructfields

    // DEBUG - test creation of a non-returned class from this
    fprintf (stderr, "about to try to create and allocate "
		     "structexprstructfields (but not return)\n");
    StructExprStructFields *test_DELETE2 = new StructExprStructFields (*this);
    delete test_DELETE2;
    fprintf (stderr, "managed to create and allocate structexprstructfields "
		     "(if not returned) \n");
    // ok this fails. fair enough.

    return new StructExprStructFields (*this);
  }
};

// AST node of the functional update struct creator
class StructExprStructBase : public StructExprStruct
{
  StructBase struct_base;

public:
  ::std::string as_string () const;

  /*inline StructBase get_struct_base() const {
      return struct_base;
  }*/

  StructExprStructBase (PathInExpression struct_path, StructBase base_struct,
			::std::vector<Attribute> inner_attribs,
			::std::vector<Attribute> outer_attribs, Location locus)
    : StructExprStruct (::std::move (struct_path), ::std::move (inner_attribs),
			::std::move (outer_attribs), locus),
      struct_base (::std::move (base_struct))
  {}

  virtual void accept_vis (ASTVisitor &vis) OVERRIDE;

protected:
  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual StructExprStructBase *clone_expr_impl () const OVERRIDE
  {
    return new StructExprStructBase (*this);
  }

  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual StructExprStructBase *clone_expr_without_block_impl () const OVERRIDE
  {
    return new StructExprStructBase (*this);
  }
};

// AST node of a tuple struct creator
class StructExprTuple : public StructExpr
{
  ::std::vector<Attribute> inner_attrs;
  //::std::vector<Expr> exprs;
  ::std::vector< ::std::unique_ptr<Expr> > exprs;

  Location locus;

public:
  ::std::string as_string () const;

  inline const ::std::vector<Attribute> &get_inner_attrs () const
  {
    return inner_attrs;
  }

  /*inline ::std::vector< ::std::unique_ptr<Expr> > get_exprs() const {
      return exprs;
  }*/

  StructExprTuple (PathInExpression struct_path,
		   ::std::vector< ::std::unique_ptr<Expr> > tuple_exprs,
		   ::std::vector<Attribute> inner_attribs,
		   ::std::vector<Attribute> outer_attribs, Location locus)
    : StructExpr (::std::move (struct_path), ::std::move (outer_attribs)),
      inner_attrs (::std::move (inner_attribs)),
      exprs (::std::move (tuple_exprs)), locus (locus)
  {}

  // copy constructor with vector clone
  StructExprTuple (StructExprTuple const &other)
    : StructExpr (other), inner_attrs (other.inner_attrs), locus (other.locus)
  {
    // crappy vector unique pointer clone - TODO is there a better way of doing
    // this?
    exprs.reserve (other.exprs.size ());

    for (const auto &e : other.exprs)
      {
	exprs.push_back (e->clone_expr ());
      }
  }

  // overloaded assignment operator with vector clone
  StructExprTuple &operator= (StructExprTuple const &other)
  {
    StructExpr::operator= (other);
    inner_attrs = other.inner_attrs;
    locus = other.locus;

    // crappy vector unique pointer clone - TODO is there a better way of doing
    // this?
    exprs.reserve (other.exprs.size ());

    for (const auto &e : other.exprs)
      {
	exprs.push_back (e->clone_expr ());
      }

    return *this;
  }

  // move constructors
  StructExprTuple (StructExprTuple &&other) = default;
  StructExprTuple &operator= (StructExprTuple &&other) = default;

  Location get_locus () const { return locus; }

  Location get_locus_slow () const OVERRIDE { return get_locus (); }

  virtual void accept_vis (ASTVisitor &vis) OVERRIDE;

protected:
  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual StructExprTuple *clone_expr_impl () const OVERRIDE
  {
    return new StructExprTuple (*this);
  }

  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual StructExprTuple *clone_expr_without_block_impl () const OVERRIDE
  {
    return new StructExprTuple (*this);
  }
};

// AST node of a "unit" struct creator (no fields and no braces)
class StructExprUnit : public StructExpr
{
  Location locus;

public:
  ::std::string as_string () const
  {
    return get_struct_name ().as_string ();
    // return struct_name.as_string();
  }

  StructExprUnit (PathInExpression struct_path,
		  ::std::vector<Attribute> outer_attribs, Location locus)
    : StructExpr (::std::move (struct_path), ::std::move (outer_attribs)),
      locus (locus)
  {}

  Location get_locus () const { return locus; }

  Location get_locus_slow () const OVERRIDE { return get_locus (); }

  virtual void accept_vis (ASTVisitor &vis) OVERRIDE;

protected:
  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual StructExprUnit *clone_expr_impl () const OVERRIDE
  {
    return new StructExprUnit (*this);
  }

  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual StructExprUnit *clone_expr_without_block_impl () const OVERRIDE
  {
    return new StructExprUnit (*this);
  }
};

// aka EnumerationVariantExpr
// Base AST node representing creation of an enum variant instance - abstract
class EnumVariantExpr : public ExprWithoutBlock
{
  PathInExpression enum_variant_path;

protected:
  // Protected constructor for initialising enum_variant_path
  EnumVariantExpr (PathInExpression path_to_enum_variant,
		   ::std::vector<Attribute> outer_attribs)
    : ExprWithoutBlock (::std::move (outer_attribs)),
      enum_variant_path (::std::move (path_to_enum_variant))
  {}

public:
  // TODO: maybe remove and have string version gotten here directly
  inline PathInExpression get_enum_variant_path () const
  {
    return enum_variant_path;
  }
};

// Base AST node for a single enum expression field (in enum instance creation)
// - abstract
class EnumExprField
{
public:
  virtual ~EnumExprField () {}

  // Unique pointer custom clone function
  ::std::unique_ptr<EnumExprField> clone_enum_expr_field () const
  {
    return ::std::unique_ptr<EnumExprField> (clone_enum_expr_field_impl ());
  }

  virtual void accept_vis (ASTVisitor &vis) = 0;

protected:
  // Clone function implementation as pure virtual method
  virtual EnumExprField *clone_enum_expr_field_impl () const = 0;
};

// Identifier-only variant of EnumExprField AST node
class EnumExprFieldIdentifier : public EnumExprField
{
  Identifier field_name;

  // TODO: should this store location data?

public:
  EnumExprFieldIdentifier (Identifier field_identifier)
    : field_name (::std::move (field_identifier))
  {}

  virtual void accept_vis (ASTVisitor &vis) OVERRIDE;

protected:
  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual EnumExprFieldIdentifier *clone_enum_expr_field_impl () const OVERRIDE
  {
    return new EnumExprFieldIdentifier (*this);
  }
};

// Base AST node for a single enum expression field with an assigned value -
// abstract
class EnumExprFieldWithVal : public EnumExprField
{
  // Expr* value;
  ::std::unique_ptr<Expr> value;

  // TODO: should this store location data?

protected:
  EnumExprFieldWithVal (::std::unique_ptr<Expr> field_value)
    : value (::std::move (field_value))
  {}

  // Copy constructor must clone unique_ptr value
  EnumExprFieldWithVal (EnumExprFieldWithVal const &other)
    : value (other.value->clone_expr ())
  {}

  // Destructor - define here if required

  // Overload assignment operator to clone
  EnumExprFieldWithVal &operator= (EnumExprFieldWithVal const &other)
  {
    value = other.value->clone_expr ();

    return *this;
  }

  // move constructors
  EnumExprFieldWithVal (EnumExprFieldWithVal &&other) = default;
  EnumExprFieldWithVal &operator= (EnumExprFieldWithVal &&other) = default;
};

// Identifier and value variant of EnumExprField AST node
class EnumExprFieldIdentifierValue : public EnumExprFieldWithVal
{
  Identifier field_name;

  // TODO: should this store location data?

public:
  EnumExprFieldIdentifierValue (Identifier field_name,
				::std::unique_ptr<Expr> field_value)
    : EnumExprFieldWithVal (::std::move (field_value)),
      field_name (::std::move (field_name))
  {}

  // copy constructor, destructor, and assignment operator should not need
  // defining

  virtual void accept_vis (ASTVisitor &vis) OVERRIDE;

protected:
  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual EnumExprFieldIdentifierValue *
  clone_enum_expr_field_impl () const OVERRIDE
  {
    return new EnumExprFieldIdentifierValue (*this);
  }
};

// Tuple index and value variant of EnumExprField AST node
class EnumExprFieldIndexValue : public EnumExprFieldWithVal
{
  TupleIndex index;
  // TODO: implement "with val" as a template with EnumExprField as type param?

  // TODO: should this store location data?

public:
  EnumExprFieldIndexValue (TupleIndex field_index,
			   ::std::unique_ptr<Expr> field_value)
    : EnumExprFieldWithVal (::std::move (field_value)), index (field_index)
  {}

  // copy constructor, destructor, and assignment operator should not need
  // defining

  virtual void accept_vis (ASTVisitor &vis) OVERRIDE;

protected:
  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual EnumExprFieldIndexValue *clone_enum_expr_field_impl () const OVERRIDE
  {
    return new EnumExprFieldIndexValue (*this);
  }
};

// Struct-like syntax enum variant instance creation AST node
class EnumExprStruct : public EnumVariantExpr
{
  //::std::vector<EnumExprField> fields;
  ::std::vector< ::std::unique_ptr<EnumExprField> > fields;

  Location locus;

public:
  ::std::string as_string () const;

  /*inline ::std::vector< ::std::unique_ptr<EnumExprField> > get_fields() const
  { return fields;
  }*/

  EnumExprStruct (
    PathInExpression enum_variant_path,
    ::std::vector< ::std::unique_ptr<EnumExprField> > variant_fields,
    ::std::vector<Attribute> outer_attribs, Location locus)
    : EnumVariantExpr (::std::move (enum_variant_path),
		       ::std::move (outer_attribs)),
      fields (::std::move (variant_fields)), locus (locus)
  {}

  // copy constructor with vector clone
  EnumExprStruct (EnumExprStruct const &other)
    : EnumVariantExpr (other), locus (other.locus)
  {
    // crappy vector unique pointer clone - TODO is there a better way of doing
    // this?
    fields.reserve (other.fields.size ());

    for (const auto &e : other.fields)
      {
	fields.push_back (e->clone_enum_expr_field ());
      }
  }

  // overloaded assignment operator with vector clone
  EnumExprStruct &operator= (EnumExprStruct const &other)
  {
    EnumVariantExpr::operator= (other);
    locus = other.locus;

    // crappy vector unique pointer clone - TODO is there a better way of doing
    // this?
    fields.reserve (other.fields.size ());

    for (const auto &e : other.fields)
      {
	fields.push_back (e->clone_enum_expr_field ());
      }

    return *this;
  }

  // move constructors
  EnumExprStruct (EnumExprStruct &&other) = default;
  EnumExprStruct &operator= (EnumExprStruct &&other) = default;

  Location get_locus () const { return locus; }

  Location get_locus_slow () const OVERRIDE { return get_locus (); }

  virtual void accept_vis (ASTVisitor &vis) OVERRIDE;

protected:
  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual EnumExprStruct *clone_expr_impl () const OVERRIDE
  {
    return new EnumExprStruct (*this);
  }

  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual EnumExprStruct *clone_expr_without_block_impl () const OVERRIDE
  {
    return new EnumExprStruct (*this);
  }
};

// Tuple-like syntax enum variant instance creation AST node
class EnumExprTuple : public EnumVariantExpr
{
  //::std::vector<Expr> values;
  ::std::vector< ::std::unique_ptr<Expr> > values;

  Location locus;

public:
  ::std::string as_string () const;

  /*inline ::std::vector< ::std::unique_ptr<Expr> > get_values() const {
      return values;
  }*/

  EnumExprTuple (PathInExpression enum_variant_path,
		 ::std::vector< ::std::unique_ptr<Expr> > variant_values,
		 ::std::vector<Attribute> outer_attribs, Location locus)
    : EnumVariantExpr (::std::move (enum_variant_path),
		       ::std::move (outer_attribs)),
      values (::std::move (variant_values)), locus (locus)
  {}

  // copy constructor with vector clone
  EnumExprTuple (EnumExprTuple const &other)
    : EnumVariantExpr (other), locus (other.locus)
  {
    // crappy vector unique pointer clone - TODO is there a better way of doing
    // this?
    values.reserve (other.values.size ());

    for (const auto &e : other.values)
      {
	values.push_back (e->clone_expr ());
      }
  }

  // overloaded assignment operator with vector clone
  EnumExprTuple &operator= (EnumExprTuple const &other)
  {
    EnumVariantExpr::operator= (other);
    locus = other.locus;

    // crappy vector unique pointer clone - TODO is there a better way of doing
    // this?
    values.reserve (other.values.size ());

    for (const auto &e : other.values)
      {
	values.push_back (e->clone_expr ());
      }

    return *this;
  }

  // move constructors
  EnumExprTuple (EnumExprTuple &&other) = default;
  EnumExprTuple &operator= (EnumExprTuple &&other) = default;

  Location get_locus () const { return locus; }

  Location get_locus_slow () const OVERRIDE { return get_locus (); }

  virtual void accept_vis (ASTVisitor &vis) OVERRIDE;

protected:
  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual EnumExprTuple *clone_expr_impl () const OVERRIDE
  {
    return new EnumExprTuple (*this);
  }

  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual EnumExprTuple *clone_expr_without_block_impl () const OVERRIDE
  {
    return new EnumExprTuple (*this);
  }
};

// No-field enum variant instance creation AST node
class EnumExprFieldless : public EnumVariantExpr
{
  Location locus;

public:
  ::std::string as_string () const
  {
    // return enum_variant_path.as_string();
    return get_enum_variant_path ().as_string ();
  }

  EnumExprFieldless (PathInExpression enum_variant_path,
		     ::std::vector<Attribute> outer_attribs, Location locus)
    : EnumVariantExpr (::std::move (enum_variant_path),
		       ::std::move (outer_attribs)),
      locus (locus)
  {}

  // copy constructor, destructor, and assignment operator should not need
  // defining

  Location get_locus () const { return locus; }

  Location get_locus_slow () const OVERRIDE { return get_locus (); }

  virtual void accept_vis (ASTVisitor &vis) OVERRIDE;

protected:
  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual EnumExprFieldless *clone_expr_impl () const OVERRIDE
  {
    return new EnumExprFieldless (*this);
  }

  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual EnumExprFieldless *clone_expr_without_block_impl () const OVERRIDE
  {
    return new EnumExprFieldless (*this);
  }
};

// Function call expression AST node
class CallExpr : public ExprWithoutBlock
{
public:
  // Expr* function;
  ::std::unique_ptr<Expr> function;
  //::std::vector<Expr> params; // inlined form of CallParams
  ::std::vector< ::std::unique_ptr<Expr> > params;

  Location locus;

  Function *fndeclRef;

  /*~CallExpr() {
      delete function;
  }*/

  ::std::string as_string () const;

  /*inline ::std::vector< ::std::unique_ptr<Expr> > get_params() const {
      return params;
  }*/

  CallExpr (::std::unique_ptr<Expr> function_expr,
	    ::std::vector< ::std::unique_ptr<Expr> > function_params,
	    ::std::vector<Attribute> outer_attribs, Location locus)
    : ExprWithoutBlock (::std::move (outer_attribs)),
      function (::std::move (function_expr)),
      params (::std::move (function_params)), locus (locus)
  {}

  // copy constructor requires clone
  CallExpr (CallExpr const &other)
    : ExprWithoutBlock (other), function (other.function->clone_expr ()),
      locus (other.locus)
  /*, params(other.params),*/ {
    // crappy vector unique pointer clone - TODO is there a better way of doing
    // this?
    params.reserve (other.params.size ());

    for (const auto &e : other.params)
      {
	params.push_back (e->clone_expr ());
      }
  }

  // Destructor - define here if required

  // Overload assignment operator to clone
  CallExpr &operator= (CallExpr const &other)
  {
    ExprWithoutBlock::operator= (other);
    function = other.function->clone_expr ();
    locus = other.locus;
    // params = other.params;
    // outer_attrs = other.outer_attrs;

    // crappy vector unique pointer clone - TODO is there a better way of doing
    // this?
    params.reserve (other.params.size ());

    for (const auto &e : other.params)
      {
	params.push_back (e->clone_expr ());
      }

    return *this;
  }

  // move constructors
  CallExpr (CallExpr &&other) = default;
  CallExpr &operator= (CallExpr &&other) = default;

  // Returns whether function call has parameters.
  inline bool has_params () const { return !params.empty (); }

  Location get_locus () const { return locus; }

  Location get_locus_slow () const OVERRIDE { return get_locus (); }

  virtual void accept_vis (ASTVisitor &vis) OVERRIDE;

protected:
  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual CallExpr *clone_expr_impl () const OVERRIDE
  {
    return new CallExpr (*this);
  }

  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual CallExpr *clone_expr_without_block_impl () const OVERRIDE
  {
    return new CallExpr (*this);
  }
};

// Method call expression AST node
class MethodCallExpr : public ExprWithoutBlock
{
  // Expr* receiver;
  ::std::unique_ptr<Expr> receiver;
  PathExprSegment method_name;
  //::std::vector<Expr> params; // inlined form of CallParams
  ::std::vector< ::std::unique_ptr<Expr> > params;

  Location locus;

public:
  /*~MethodCallExpr() {
      delete receiver;
  }*/

  ::std::string as_string () const;

  /*inline ::std::vector< ::std::unique_ptr<Expr> > get_params() const {
      return params;
  }*/

  MethodCallExpr (::std::unique_ptr<Expr> call_receiver,
		  PathExprSegment method_path,
		  ::std::vector< ::std::unique_ptr<Expr> > method_params,
		  ::std::vector<Attribute> outer_attribs, Location locus)
    : ExprWithoutBlock (::std::move (outer_attribs)),
      receiver (::std::move (call_receiver)),
      method_name (::std::move (method_path)),
      params (::std::move (method_params)), locus (locus)
  {}

  // copy constructor required due to cloning
  MethodCallExpr (MethodCallExpr const &other)
    : ExprWithoutBlock (other), receiver (other.receiver->clone_expr ()),
      method_name (other.method_name), locus (other.locus)
  /*, params(other.params),*/ {
    // crappy vector unique pointer clone - TODO is there a better way of doing
    // this?
    params.reserve (other.params.size ());

    for (const auto &e : other.params)
      {
	params.push_back (e->clone_expr ());
      }
  }

  // Destructor - define here if required

  // Overload assignment operator to clone receiver object
  MethodCallExpr &operator= (MethodCallExpr const &other)
  {
    ExprWithoutBlock::operator= (other);
    receiver = other.receiver->clone_expr ();
    method_name = other.method_name;
    locus = other.locus;
    // params = other.params;
    // outer_attrs = other.outer_attrs;

    // crappy vector unique pointer clone - TODO is there a better way of doing
    // this?
    params.reserve (other.params.size ());

    for (const auto &e : other.params)
      {
	params.push_back (e->clone_expr ());
      }

    return *this;
  }

  // move constructors
  MethodCallExpr (MethodCallExpr &&other) = default;
  MethodCallExpr &operator= (MethodCallExpr &&other) = default;

  Location get_locus () const { return locus; }

  Location get_locus_slow () const OVERRIDE { return get_locus (); }

  virtual void accept_vis (ASTVisitor &vis) OVERRIDE;

protected:
  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual MethodCallExpr *clone_expr_impl () const OVERRIDE
  {
    return new MethodCallExpr (*this);
  }

  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual MethodCallExpr *clone_expr_without_block_impl () const OVERRIDE
  {
    return new MethodCallExpr (*this);
  }
};

// aka FieldExpression
// Struct or union field access expression AST node
class FieldAccessExpr : public ExprWithoutBlock
{
  // Expr* receiver;
  ::std::unique_ptr<Expr> receiver;
  Identifier field;

  Location locus;

public:
  /*~FieldAccessExpr() {
      delete receiver;
  }*/

  ::std::string as_string () const;

  FieldAccessExpr (::std::unique_ptr<Expr> field_access_receiver,
		   Identifier field_name,
		   ::std::vector<Attribute> outer_attribs, Location locus)
    : ExprWithoutBlock (::std::move (outer_attribs)),
      receiver (::std::move (field_access_receiver)),
      field (::std::move (field_name)), locus (locus)
  {}

  // Copy constructor required due to unique_ptr cloning
  FieldAccessExpr (FieldAccessExpr const &other)
    : ExprWithoutBlock (other), receiver (other.receiver->clone_expr ()),
      field (other.field), locus (other.locus)
  {}

  // Destructor - define here if required

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

  Location get_locus () const { return locus; }

  Location get_locus_slow () const OVERRIDE { return get_locus (); }

  virtual void accept_vis (ASTVisitor &vis) OVERRIDE;

protected:
  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual FieldAccessExpr *clone_expr_impl () const OVERRIDE
  {
    return new FieldAccessExpr (*this);
  }

  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual FieldAccessExpr *clone_expr_without_block_impl () const OVERRIDE
  {
    return new FieldAccessExpr (*this);
  }
};

// Closure parameter data structure
struct ClosureParam
{
private:
  // Pattern pattern;
  ::std::unique_ptr<Pattern> pattern;

  // bool has_type_given;
  // Type type;
  ::std::unique_ptr<Type> type;

  // TODO: should this store location data?

public:
  // Returns whether the type of the parameter has been given.
  inline bool has_type_given () const { return type != NULL; }

  // Constructor for closure parameter
  ClosureParam (::std::unique_ptr<Pattern> param_pattern,
		::std::unique_ptr<Type> param_type = NULL)
    : pattern (::std::move (param_pattern)), type (::std::move (param_type))
  {}

  // Copy constructor required due to cloning as a result of unique_ptrs
  ClosureParam (ClosureParam const &other)
    : pattern (other.pattern->clone_pattern ())
  {
    // guard to protect from null pointer dereference
    if (other.type != NULL)
      {
	type = other.type->clone_type ();
      }
  }

  ~ClosureParam () = default;

  // Assignment operator must be overloaded to clone as well
  ClosureParam &operator= (ClosureParam const &other)
  {
    pattern = other.pattern->clone_pattern ();
    type = other.type->clone_type ();

    return *this;
  }

  // move constructors
  ClosureParam (ClosureParam &&other) = default;
  ClosureParam &operator= (ClosureParam &&other) = default;

  // Returns whether closure parameter is in an error state.
  inline bool is_error () const { return pattern == NULL; }

  // Creates an error state closure parameter.
  static ClosureParam create_error () { return ClosureParam (NULL); }

  ::std::string as_string () const;
};

// Base closure definition expression AST node - abstract
class ClosureExpr : public ExprWithoutBlock
{
  bool has_move;
  ::std::vector<ClosureParam> params; // may be empty
  // also note a double pipe "||" can be used for empty params - does not need a
  // space

  Location locus;

protected:
  ClosureExpr (::std::vector<ClosureParam> closure_params, bool has_move,
	       ::std::vector<Attribute> outer_attribs, Location locus)
    : ExprWithoutBlock (::std::move (outer_attribs)), has_move (has_move),
      params (::std::move (closure_params)), locus (locus)
  {}

  // Copy constructor, destructor, and assignment operator override should not
  // be needed
public:
  virtual ::std::string as_string () const;

  Location get_locus () const { return locus; }

  Location get_locus_slow () const OVERRIDE { return get_locus (); }
};

// Represents a non-type-specified closure expression AST node
class ClosureExprInner : public ClosureExpr
{
  // Expr* closure_inner;
  ::std::unique_ptr<Expr> closure_inner;

public:
  /*~ClosureExprInner() {
      delete closure_inner;
  }*/

  ::std::string as_string () const;

  // Constructor for a ClosureExprInner
  ClosureExprInner (::std::unique_ptr<Expr> closure_inner_expr,
		    ::std::vector<ClosureParam> closure_params, Location locus,
		    bool is_move = false,
		    ::std::vector<Attribute> outer_attribs
		    = ::std::vector<Attribute> ())
    : ClosureExpr (::std::move (closure_params), is_move,
		   ::std::move (outer_attribs), locus),
      closure_inner (::std::move (closure_inner_expr))
  {}

  // Copy constructor must be defined to allow copying via cloning of unique_ptr
  ClosureExprInner (ClosureExprInner const &other)
    : ClosureExpr (other), closure_inner (other.closure_inner->clone_expr ())
  {}
  // TODO: ensure that this actually constructs properly

  // Destructor - define here if required

  // Overload assignment operator to clone closure_inner
  ClosureExprInner &operator= (ClosureExprInner const &other)
  {
    ClosureExpr::operator= (other);
    closure_inner = other.closure_inner->clone_expr ();
    // params = other.params;
    // has_move = other.has_move;
    // outer_attrs = other.outer_attrs;

    return *this;
  }

  // move constructors
  ClosureExprInner (ClosureExprInner &&other) = default;
  ClosureExprInner &operator= (ClosureExprInner &&other) = default;

  virtual void accept_vis (ASTVisitor &vis) OVERRIDE;

protected:
  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual ClosureExprInner *clone_expr_impl () const OVERRIDE
  {
    return new ClosureExprInner (*this);
  }

  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual ClosureExprInner *clone_expr_without_block_impl () const OVERRIDE
  {
    return new ClosureExprInner (*this);
  }
};

// Forward decl BlockExpr for ClosureExprInnerTyped
// class BlockExpr;

// A block AST node
class BlockExpr : public ExprWithBlock
{
public:
  ::std::vector<Attribute> inner_attrs;

  /*bool has_statements;
  Statements statements;*/

  // bool has_statements;
  ::std::vector< ::std::unique_ptr<Stmt> > statements;
  // bool has_expr;
  ::std::unique_ptr<ExprWithoutBlock> expr; // inlined from Statements

  Location locus;

  ::std::string as_string () const;

  // Returns whether the block contains statements.
  inline bool has_statements () const { return !statements.empty (); }

  // Returns whether the block contains an expression
  inline bool has_expr () const { return expr != NULL; }

  BlockExpr (::std::vector< ::std::unique_ptr<Stmt> > block_statements,
	     ::std::unique_ptr<ExprWithoutBlock> block_expr,
	     ::std::vector<Attribute> inner_attribs,
	     ::std::vector<Attribute> outer_attribs, Location locus)
    : ExprWithBlock (::std::move (outer_attribs)),
      inner_attrs (::std::move (inner_attribs)),
      statements (::std::move (block_statements)),
      expr (::std::move (block_expr)), locus (locus)
  {}

  // Copy constructor with clone
  BlockExpr (BlockExpr const &other)
    : ExprWithBlock (other), /*statements(other.statements),*/
      inner_attrs (other.inner_attrs), locus (other.locus)
  {
    // guard to protect from null pointer dereference
    if (other.expr != NULL)
      {
	expr = other.expr->clone_expr_without_block ();
      }

    // crappy vector unique pointer clone - TODO is there a better way of doing
    // this?
    statements.reserve (other.statements.size ());

    for (const auto &e : other.statements)
      {
	statements.push_back (e->clone_stmt ());
      }
  }

  // Destructor - define here if required

  // Overloaded assignment operator to clone pointer
  BlockExpr &operator= (BlockExpr const &other)
  {
    ExprWithBlock::operator= (other);
    // statements = other.statements;
    expr = other.expr->clone_expr_without_block ();
    inner_attrs = other.inner_attrs;
    locus = other.locus;
    // outer_attrs = other.outer_attrs;

    // crappy vector unique pointer clone - TODO is there a better way of doing
    // this?
    statements.reserve (other.statements.size ());

    for (const auto &e : other.statements)
      {
	statements.push_back (e->clone_stmt ());
      }

    return *this;
  }

  // move constructors
  BlockExpr (BlockExpr &&other) = default;
  BlockExpr &operator= (BlockExpr &&other) = default;

  // Unique pointer custom clone function
  ::std::unique_ptr<BlockExpr> clone_block_expr () const
  {
    return ::std::unique_ptr<BlockExpr> (clone_block_expr_impl ());
  }

  Location get_locus () const { return locus; }

  Location get_locus_slow () const OVERRIDE { return get_locus (); }

  virtual void accept_vis (ASTVisitor &vis) OVERRIDE;

protected:
  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual BlockExpr *clone_expr_impl () const OVERRIDE
  {
    return new BlockExpr (*this);
  }

  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual BlockExpr *clone_expr_with_block_impl () const OVERRIDE
  {
    return new BlockExpr (*this);
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
  // Type return_type;
  ::std::unique_ptr<Type> return_type;
  // BlockExpr* expr;
  ::std::unique_ptr<BlockExpr>
    expr; // only used because may be polymorphic in future

public:
  /*~ClosureExprInnerTyped() {
      delete expr;
  }*/

  ::std::string as_string () const;

  // Constructor potentially with a move
  ClosureExprInnerTyped (::std::unique_ptr<Type> closure_return_type,
			 ::std::unique_ptr<BlockExpr> closure_expr,
			 ::std::vector<ClosureParam> closure_params,
			 Location locus, bool is_move = false,
			 ::std::vector<Attribute> outer_attribs
			 = ::std::vector<Attribute> ())
    : ClosureExpr (::std::move (closure_params), is_move,
		   ::std::move (outer_attribs), locus),
      return_type (::std::move (closure_return_type)),
      expr (::std::move (closure_expr))
  {}

  // Copy constructor requires cloning
  ClosureExprInnerTyped (ClosureExprInnerTyped const &other)
    : ClosureExpr (other), return_type (other.return_type->clone_type ()),
      expr (other.expr->clone_block_expr ())
  {}

  // Destructor - define here if required

  // Overload assignment operator to clone unique_ptrs
  ClosureExprInnerTyped &operator= (ClosureExprInnerTyped const &other)
  {
    ClosureExpr::operator= (other);
    return_type = other.return_type->clone_type ();
    expr = other.expr->clone_block_expr ();
    // params = other.params;
    // has_move = other.has_move;
    // outer_attrs = other.outer_attrs;

    return *this;
  }

  // move constructors
  ClosureExprInnerTyped (ClosureExprInnerTyped &&other) = default;
  ClosureExprInnerTyped &operator= (ClosureExprInnerTyped &&other) = default;

  virtual void accept_vis (ASTVisitor &vis) OVERRIDE;

protected:
  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual ClosureExprInnerTyped *clone_expr_impl () const OVERRIDE
  {
    return new ClosureExprInnerTyped (*this);
  }

  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual ClosureExprInnerTyped *clone_expr_without_block_impl () const OVERRIDE
  {
    return new ClosureExprInnerTyped (*this);
  }
};

// AST node representing continue expression within loops
class ContinueExpr : public ExprWithoutBlock
{
  // bool has_label;
  Lifetime label;

  Location locus;

public:
  ::std::string as_string () const;

  // Returns true if the continue expr has a label.
  inline bool has_label () const { return !label.is_error (); }

  // Constructor for a ContinueExpr with a label.
  ContinueExpr (Location locus, Lifetime label = Lifetime::error (),
		::std::vector<Attribute> outer_attribs
		= ::std::vector<Attribute> ())
    : ExprWithoutBlock (::std::move (outer_attribs)),
      label (::std::move (label)), locus (locus)
  {}

  // copy constructor, destructor, and assignment operator should not need
  // defining

  Location get_locus () const { return locus; }

  Location get_locus_slow () const OVERRIDE { return get_locus (); }

  virtual void accept_vis (ASTVisitor &vis) OVERRIDE;

protected:
  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual ContinueExpr *clone_expr_impl () const OVERRIDE
  {
    return new ContinueExpr (*this);
  }

  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual ContinueExpr *clone_expr_without_block_impl () const OVERRIDE
  {
    return new ContinueExpr (*this);
  }
};
// TODO: merge "break" and "continue"? Or even merge in "return"?

// AST node representing break expression within loops
class BreakExpr : public ExprWithoutBlock
{
  // bool has_label;
  Lifetime label;

  // bool has_break_expr;
  // Expr* break_expr; // may be uninitialised
  ::std::unique_ptr<Expr> break_expr;

  Location locus;

public:
  /*~BreakExpr() {
      if (has_break_expr) {
	  delete break_expr;
      }
  }*/

  ::std::string as_string () const;

  // Returns whether the break expression has a label or not.
  inline bool has_label () const { return !label.is_error (); }

  // Returns whether the break expression has an expression used in the break or
  // not.
  inline bool has_break_expr () const { return break_expr != NULL; }

  // Constructor for a break expression
  BreakExpr (Location locus, Lifetime break_label = Lifetime::error (),
	     ::std::unique_ptr<Expr> expr_in_break = NULL,
	     ::std::vector<Attribute> outer_attribs
	     = ::std::vector<Attribute> ())
    : ExprWithoutBlock (::std::move (outer_attribs)),
      label (::std::move (break_label)),
      break_expr (::std::move (expr_in_break)), locus (locus)
  {}

  // Copy constructor defined to use clone for unique pointer
  BreakExpr (BreakExpr const &other)
    : ExprWithoutBlock (other), label (other.label), locus (other.locus)
  {
    // guard to protect from null pointer dereference
    if (other.break_expr != NULL)
      {
	break_expr = other.break_expr->clone_expr ();
      }
  }

  // Destructor - define here if required

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

  Location get_locus () const { return locus; }

  Location get_locus_slow () const OVERRIDE { return get_locus (); }

  virtual void accept_vis (ASTVisitor &vis) OVERRIDE;

protected:
  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual BreakExpr *clone_expr_impl () const OVERRIDE
  {
    return new BreakExpr (*this);
  }

  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual BreakExpr *clone_expr_without_block_impl () const OVERRIDE
  {
    return new BreakExpr (*this);
  }
};

// Base range expression AST node object - abstract
class RangeExpr : public ExprWithoutBlock
{
  Location locus;

protected:
  // outer attributes not allowed before range expressions
  RangeExpr (Location locus)
    : ExprWithoutBlock (::std::vector<Attribute> ()), locus (locus)
  {}

public:
  Location get_locus () const { return locus; }

  Location get_locus_slow () const OVERRIDE { return get_locus (); }
};

// Range from (inclusive) and to (exclusive) expression AST node object
// aka RangeExpr; constructs a std::ops::Range object
class RangeFromToExpr : public RangeExpr
{
  /*Expr* from;
  Expr* to;*/
  ::std::unique_ptr<Expr> from;
  ::std::unique_ptr<Expr> to;

public:
  /*~RangeFromToExpr() {
      delete from;
      delete to;
  }*/

  ::std::string as_string () const;

  RangeFromToExpr (::std::unique_ptr<Expr> range_from,
		   ::std::unique_ptr<Expr> range_to, Location locus)
    : RangeExpr (locus), from (::std::move (range_from)),
      to (::std::move (range_to))
  {}

  // Copy constructor with cloning
  RangeFromToExpr (RangeFromToExpr const &other)
    : RangeExpr (other), from (other.from->clone_expr ()),
      to (other.to->clone_expr ())
  {}

  // Destructor - define here if required

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

  virtual void accept_vis (ASTVisitor &vis) OVERRIDE;

protected:
  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual RangeFromToExpr *clone_expr_impl () const OVERRIDE
  {
    return new RangeFromToExpr (*this);
  }

  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual RangeFromToExpr *clone_expr_without_block_impl () const OVERRIDE
  {
    return new RangeFromToExpr (*this);
  }
};

// Range from (inclusive) expression AST node object
// constructs a std::ops::RangeFrom object
class RangeFromExpr : public RangeExpr
{
  // Expr* from;
  ::std::unique_ptr<Expr> from;

public:
  /*~RangeFromExpr() {
      delete from;
  }*/

  ::std::string as_string () const;

  RangeFromExpr (::std::unique_ptr<Expr> range_from, Location locus)
    : RangeExpr (locus), from (::std::move (range_from))
  {}

  // Copy constructor with clone
  RangeFromExpr (RangeFromExpr const &other)
    : RangeExpr (other), from (other.from->clone_expr ())
  {}

  // Destructor - define here if required

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

  virtual void accept_vis (ASTVisitor &vis) OVERRIDE;

protected:
  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual RangeFromExpr *clone_expr_impl () const OVERRIDE
  {
    return new RangeFromExpr (*this);
  }

  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual RangeFromExpr *clone_expr_without_block_impl () const OVERRIDE
  {
    return new RangeFromExpr (*this);
  }
};

// Range to (exclusive) expression AST node object
// constructs a std::ops::RangeTo object
class RangeToExpr : public RangeExpr
{
  // Expr* to;
  ::std::unique_ptr<Expr> to;

public:
  /*~RangeToExpr() {
      delete to;
  }*/

  ::std::string as_string () const;

  // outer attributes not allowed
  RangeToExpr (::std::unique_ptr<Expr> range_to, Location locus)
    : RangeExpr (locus), to (::std::move (range_to))
  {}

  // Copy constructor with clone
  RangeToExpr (RangeToExpr const &other)
    : RangeExpr (other), to (other.to->clone_expr ())
  {}

  // Destructor - define here if required

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

  virtual void accept_vis (ASTVisitor &vis) OVERRIDE;

protected:
  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual RangeToExpr *clone_expr_impl () const OVERRIDE
  {
    return new RangeToExpr (*this);
  }

  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual RangeToExpr *clone_expr_without_block_impl () const OVERRIDE
  {
    return new RangeToExpr (*this);
  }
};

// Full range expression AST node object
// constructs a std::ops::RangeFull object
class RangeFullExpr : public RangeExpr
{
public:
  ::std::string as_string () const;

  RangeFullExpr (Location locus) : RangeExpr (locus) {}
  // outer attributes not allowed

  virtual void accept_vis (ASTVisitor &vis) OVERRIDE;

protected:
  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual RangeFullExpr *clone_expr_impl () const OVERRIDE
  {
    return new RangeFullExpr (*this);
  }

  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual RangeFullExpr *clone_expr_without_block_impl () const OVERRIDE
  {
    return new RangeFullExpr (*this);
  }
};

// Range from (inclusive) and to (inclusive) expression AST node object
// aka RangeInclusiveExpr; constructs a std::ops::RangeInclusive object
class RangeFromToInclExpr : public RangeExpr
{
  /*Expr* from;
  Expr* to;*/
  ::std::unique_ptr<Expr> from;
  ::std::unique_ptr<Expr> to;

public:
  /*~RangeFromToInclExpr() {
      delete from;
      delete to;
  }*/

  ::std::string as_string () const;

  RangeFromToInclExpr (::std::unique_ptr<Expr> range_from,
		       ::std::unique_ptr<Expr> range_to, Location locus)
    : RangeExpr (locus), from (::std::move (range_from)),
      to (::std::move (range_to))
  {}
  // outer attributes not allowed

  // Copy constructor with clone
  RangeFromToInclExpr (RangeFromToInclExpr const &other)
    : RangeExpr (other), from (other.from->clone_expr ()),
      to (other.to->clone_expr ())
  {}

  // Destructor - define here if required

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

  virtual void accept_vis (ASTVisitor &vis) OVERRIDE;

protected:
  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual RangeFromToInclExpr *clone_expr_impl () const OVERRIDE
  {
    return new RangeFromToInclExpr (*this);
  }

  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual RangeFromToInclExpr *clone_expr_without_block_impl () const OVERRIDE
  {
    return new RangeFromToInclExpr (*this);
  }
};

// Range to (inclusive) expression AST node object
// aka RangeToInclusiveExpr; constructs a std::ops::RangeToInclusive object
class RangeToInclExpr : public RangeExpr
{
  // Expr* to;
  ::std::unique_ptr<Expr> to;

public:
  /*~RangeToInclExpr() {
      delete to;
  }*/

  ::std::string as_string () const;

  RangeToInclExpr (::std::unique_ptr<Expr> range_to, Location locus)
    : RangeExpr (locus), to (::std::move (range_to))
  {}
  // outer attributes not allowed

  // Copy constructor with clone
  RangeToInclExpr (RangeToInclExpr const &other)
    : RangeExpr (other), to (other.to->clone_expr ())
  {}

  // Define destructor here if required

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

  virtual void accept_vis (ASTVisitor &vis) OVERRIDE;

protected:
  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual RangeToInclExpr *clone_expr_impl () const OVERRIDE
  {
    return new RangeToInclExpr (*this);
  }

  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual RangeToInclExpr *clone_expr_without_block_impl () const OVERRIDE
  {
    return new RangeToInclExpr (*this);
  }
};

// Return expression AST node representation
class ReturnExpr : public ExprWithoutBlock
{
  // bool has_return_expr;
  // Expr* return_expr;
  ::std::unique_ptr<Expr> return_expr;

  Location locus;

public:
  /*~ReturnExpr() {
      if (has_return_expr) {
	  delete return_expr;
      }
  }*/

  ::std::string as_string () const;

  // Returns whether the object has an expression returned (i.e. not void return
  // type).
  inline bool has_return_expr () const { return return_expr != NULL; }

  // Constructor for ReturnExpr.
  ReturnExpr (Location locus, ::std::unique_ptr<Expr> returned_expr = NULL,
	      ::std::vector<Attribute> outer_attribs
	      = ::std::vector<Attribute> ())
    : ExprWithoutBlock (::std::move (outer_attribs)),
      return_expr (::std::move (returned_expr)), locus (locus)
  {}

  // Copy constructor with clone
  ReturnExpr (ReturnExpr const &other)
    : ExprWithoutBlock (other), locus (other.locus)
  {
    // guard to protect from null pointer dereference
    if (other.return_expr != NULL)
      {
	return_expr = other.return_expr->clone_expr ();
      }
  }

  // Destructor - define here if required

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

  Location get_locus () const { return locus; }

  Location get_locus_slow () const OVERRIDE { return get_locus (); }

  virtual void accept_vis (ASTVisitor &vis) OVERRIDE;

protected:
  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual ReturnExpr *clone_expr_impl () const OVERRIDE
  {
    return new ReturnExpr (*this);
  }

  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual ReturnExpr *clone_expr_without_block_impl () const OVERRIDE
  {
    return new ReturnExpr (*this);
  }
};

// Forward decl - defined in rust-macro.h
class MacroInvocation;
/*class MacroInvocation : public ExprWithoutBlock {
  public:
    ::std::string as_string() const;
};*/

// An unsafe block AST node
class UnsafeBlockExpr : public ExprWithBlock
{
  // Or just have it extend BlockExpr
  // BlockExpr* expr;
  ::std::unique_ptr<BlockExpr> expr;

  Location locus;

public:
  /*~UnsafeBlockExpr() {
      delete expr;
  }*/

  ::std::string as_string () const;

  UnsafeBlockExpr (::std::unique_ptr<BlockExpr> block_expr,
		   ::std::vector<Attribute> outer_attribs, Location locus)
    : ExprWithBlock (::std::move (outer_attribs)),
      expr (::std::move (block_expr)), locus (locus)
  {}

  // Copy constructor with clone
  UnsafeBlockExpr (UnsafeBlockExpr const &other)
    : ExprWithBlock (other), expr (other.expr->clone_block_expr ()),
      locus (other.locus)
  {}

  // Destructor - define here if required

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

  Location get_locus () const { return locus; }

  Location get_locus_slow () const OVERRIDE { return get_locus (); }

  virtual void accept_vis (ASTVisitor &vis) OVERRIDE;

protected:
  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual UnsafeBlockExpr *clone_expr_impl () const OVERRIDE
  {
    return new UnsafeBlockExpr (*this);
  }

  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual UnsafeBlockExpr *clone_expr_with_block_impl () const OVERRIDE
  {
    return new UnsafeBlockExpr (*this);
  }
};

// Loop label expression AST node used with break and continue expressions
// TODO: inline?
class LoopLabel /*: public Node*/
{
  Lifetime label; // or type LIFETIME_OR_LABEL

  Location locus;

public:
  ::std::string as_string () const;

  LoopLabel (Lifetime loop_label, Location locus = Location ())
    : label (::std::move (loop_label)), locus (locus)
  {}

  // Returns whether the LoopLabel is in an error state.
  inline bool is_error () const { return label.is_error (); }

  // Creates an error state LoopLabel.
  static LoopLabel error () { return LoopLabel (Lifetime::error ()); }

  Location get_locus () const { return locus; }
};

// Base loop expression AST node - aka LoopExpr
class BaseLoopExpr : public ExprWithBlock
{
protected:
  // protected to allow subclasses better use of them
  // bool has_loop_label;
  LoopLabel loop_label;

  // BlockExpr* loop_block;
  ::std::unique_ptr<BlockExpr> loop_block;

private:
  Location locus;

protected:
  // Constructor for BaseLoopExpr
  BaseLoopExpr (::std::unique_ptr<BlockExpr> loop_block, Location locus,
		LoopLabel loop_label = LoopLabel::error (),
		::std::vector<Attribute> outer_attribs
		= ::std::vector<Attribute> ())
    : ExprWithBlock (::std::move (outer_attribs)),
      loop_label (::std::move (loop_label)),
      loop_block (::std::move (loop_block)), locus (locus)
  {}

  // Copy constructor for BaseLoopExpr with clone
  BaseLoopExpr (BaseLoopExpr const &other)
    : ExprWithBlock (other), loop_label (other.loop_label),
      loop_block (other.loop_block->clone_block_expr ()), locus (other.locus)
  {}

  // Destructor - define here if required

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

public:
  /*~BaseLoopExpr() {
      delete loop_block;
  }*/

  inline bool has_loop_label () const { return !loop_label.is_error (); }

  Location get_locus () const { return locus; }

  Location get_locus_slow () const OVERRIDE { return get_locus (); }
};

// 'Loop' expression (i.e. the infinite loop) AST node
class LoopExpr : public BaseLoopExpr
{
public:
  ::std::string as_string () const;

  // Constructor for LoopExpr
  LoopExpr (::std::unique_ptr<BlockExpr> loop_block, Location locus,
	    LoopLabel loop_label = LoopLabel::error (),
	    ::std::vector<Attribute> outer_attribs
	    = ::std::vector<Attribute> ())
    : BaseLoopExpr (::std::move (loop_block), locus, ::std::move (loop_label),
		    ::std::move (outer_attribs))
  {}

  // copy constructor, destructor, and assignment operator should not need
  // modification

  virtual void accept_vis (ASTVisitor &vis) OVERRIDE;

protected:
  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual LoopExpr *clone_expr_impl () const OVERRIDE
  {
    return new LoopExpr (*this);
  }

  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual LoopExpr *clone_expr_with_block_impl () const OVERRIDE
  {
    return new LoopExpr (*this);
  }
};

// While loop expression AST node (predicate loop)
class WhileLoopExpr : public BaseLoopExpr
{
  // Expr* condition;
  ::std::unique_ptr<Expr> condition;

public:
  /*~WhileLoopExpr() {
      delete condition;
  }*/

  ::std::string as_string () const;

  // Constructor for while loop with loop label
  WhileLoopExpr (::std::unique_ptr<Expr> loop_condition,
		 ::std::unique_ptr<BlockExpr> loop_block, Location locus,
		 LoopLabel loop_label = LoopLabel::error (),
		 ::std::vector<Attribute> outer_attribs
		 = ::std::vector<Attribute> ())
    : BaseLoopExpr (::std::move (loop_block), locus, ::std::move (loop_label),
		    ::std::move (outer_attribs)),
      condition (::std::move (loop_condition))
  {}

  // Copy constructor with clone
  WhileLoopExpr (WhileLoopExpr const &other)
    : BaseLoopExpr (other), condition (other.condition->clone_expr ())
  {}

  // Destructor - define here if required

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

  virtual void accept_vis (ASTVisitor &vis) OVERRIDE;

protected:
  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual WhileLoopExpr *clone_expr_impl () const OVERRIDE
  {
    return new WhileLoopExpr (*this);
  }

  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual WhileLoopExpr *clone_expr_with_block_impl () const OVERRIDE
  {
    return new WhileLoopExpr (*this);
  }
};

// Forward decl MatchArmPatterns
// struct MatchArmPatterns;

// While let loop expression AST node (predicate pattern loop)
class WhileLetLoopExpr : public BaseLoopExpr
{
  // MatchArmPatterns patterns;
  ::std::vector< ::std::unique_ptr<Pattern> > match_arm_patterns; // inlined
  // Expr* condition;
  ::std::unique_ptr<Expr> condition;

public:
  /*~WhileLetLoopExpr() {
      delete condition;
  }*/

  ::std::string as_string () const;

  // Constructor with a loop label
  WhileLetLoopExpr (
    ::std::vector< ::std::unique_ptr<Pattern> > match_arm_patterns,
    ::std::unique_ptr<Expr> condition, ::std::unique_ptr<BlockExpr> loop_block,
    Location locus, LoopLabel loop_label = LoopLabel::error (),
    ::std::vector<Attribute> outer_attribs = ::std::vector<Attribute> ())
    : BaseLoopExpr (::std::move (loop_block), locus, ::std::move (loop_label),
		    ::std::move (outer_attribs)),
      match_arm_patterns (::std::move (match_arm_patterns)),
      condition (::std::move (condition))
  {}

  // Copy constructor with clone
  WhileLetLoopExpr (WhileLetLoopExpr const &other)
    : BaseLoopExpr (other),
      /*match_arm_patterns(other.match_arm_patterns),*/ condition (
	other.condition->clone_expr ())
  {
    // crappy vector unique pointer clone - TODO is there a better way of doing
    // this?
    match_arm_patterns.reserve (other.match_arm_patterns.size ());

    for (const auto &e : other.match_arm_patterns)
      {
	match_arm_patterns.push_back (e->clone_pattern ());
      }
  }

  // Destructor - define here if required

  // Overloaded assignment operator to clone pointers
  WhileLetLoopExpr &operator= (WhileLetLoopExpr const &other)
  {
    BaseLoopExpr::operator= (other);
    // match_arm_patterns = other.match_arm_patterns;
    condition = other.condition->clone_expr ();
    // loop_block = other.loop_block->clone_block_expr();
    // loop_label = other.loop_label;
    // outer_attrs = other.outer_attrs;

    // crappy vector unique pointer clone - TODO is there a better way of doing
    // this?
    match_arm_patterns.reserve (other.match_arm_patterns.size ());

    for (const auto &e : other.match_arm_patterns)
      {
	match_arm_patterns.push_back (e->clone_pattern ());
      }

    return *this;
  }

  // move constructors
  WhileLetLoopExpr (WhileLetLoopExpr &&other) = default;
  WhileLetLoopExpr &operator= (WhileLetLoopExpr &&other) = default;

  virtual void accept_vis (ASTVisitor &vis) OVERRIDE;

protected:
  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual WhileLetLoopExpr *clone_expr_impl () const OVERRIDE
  {
    return new WhileLetLoopExpr (*this);
  }

  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual WhileLetLoopExpr *clone_expr_with_block_impl () const OVERRIDE
  {
    return new WhileLetLoopExpr (*this);
  }
};

// For loop expression AST node (iterator loop)
class ForLoopExpr : public BaseLoopExpr
{
  // Pattern pattern;
  ::std::unique_ptr<Pattern> pattern;
  // Expr* iterator_expr;
  ::std::unique_ptr<Expr> iterator_expr;

public:
  /*~ForLoopExpr() {
      delete iterator_expr;
  }*/

  ::std::string as_string () const;

  // Constructor with loop label
  ForLoopExpr (::std::unique_ptr<Pattern> loop_pattern,
	       ::std::unique_ptr<Expr> iterator_expr,
	       ::std::unique_ptr<BlockExpr> loop_body, Location locus,
	       LoopLabel loop_label = LoopLabel::error (),
	       ::std::vector<Attribute> outer_attribs
	       = ::std::vector<Attribute> ())
    : BaseLoopExpr (::std::move (loop_body), locus, ::std::move (loop_label),
		    ::std::move (outer_attribs)),
      pattern (::std::move (loop_pattern)),
      iterator_expr (::std::move (iterator_expr))
  {}

  // Copy constructor with clone
  ForLoopExpr (ForLoopExpr const &other)
    : BaseLoopExpr (other), pattern (other.pattern->clone_pattern ()),
      iterator_expr (other.iterator_expr->clone_expr ())
  {}

  // Destructor - define here if required

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

  virtual void accept_vis (ASTVisitor &vis) OVERRIDE;

protected:
  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual ForLoopExpr *clone_expr_impl () const OVERRIDE
  {
    return new ForLoopExpr (*this);
  }

  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual ForLoopExpr *clone_expr_with_block_impl () const OVERRIDE
  {
    return new ForLoopExpr (*this);
  }
};

// forward decl for IfExpr
class IfLetExpr;

// Base if expression with no "else" or "if let" AST node
class IfExpr : public ExprWithBlock
{
  /*Expr* condition;
  BlockExpr* if_block;*/
  ::std::unique_ptr<Expr> condition;
  ::std::unique_ptr<BlockExpr> if_block;
  /*union {
      BlockExpr else_block;
      IfExpr* if_expr;
      IfLetExpr if_let_expr;
  } consequent_block;*/

  Location locus;

public:
  /*virtual ~IfExpr() {
      delete condition;
      delete if_block;
  }*/

  ::std::string as_string () const;

  IfExpr (::std::unique_ptr<Expr> condition,
	  ::std::unique_ptr<BlockExpr> if_block, Location locus)
    : ExprWithBlock (::std::vector<Attribute> ()),
      condition (::std::move (condition)), if_block (::std::move (if_block)),
      locus (locus)
  {}
  // outer attributes are never allowed on IfExprs

  // Copy constructor with clone
  IfExpr (IfExpr const &other)
    : ExprWithBlock (other), condition (other.condition->clone_expr ()),
      if_block (other.if_block->clone_block_expr ()), locus (other.locus)
  {}

  // Destructor - define here if required

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
  ::std::unique_ptr<IfExpr> clone_if_expr () const
  {
    return ::std::unique_ptr<IfExpr> (clone_if_expr_impl ());
  }

  /* Note that multiple "else if"s are handled via nested ASTs rather than a
   * vector of else ifs - i.e. not like a switch statement. TODO - is this a
   * better approach? or does it not parse correctly and have downsides? */

  Location get_locus () const { return locus; }

  Location get_locus_slow () const OVERRIDE { return get_locus (); }

  virtual void accept_vis (ASTVisitor &vis) OVERRIDE;

protected:
  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual IfExpr *clone_expr_impl () const OVERRIDE
  {
    return new IfExpr (*this);
  }

  // Base clone function but still concrete as concrete base class
  virtual IfExpr *clone_if_expr_impl () const { return new IfExpr (*this); }

  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual IfExpr *clone_expr_with_block_impl () const OVERRIDE
  {
    return new IfExpr (*this);
  }
};

// If expression with an ending "else" expression AST node (trailing)
class IfExprConseqElse : public IfExpr
{
  // BlockExpr* else_block;
  ::std::unique_ptr<BlockExpr> else_block;

public:
  /*~IfExprConseqElse() {
      delete else_block;
  }*/

  ::std::string as_string () const;

  IfExprConseqElse (::std::unique_ptr<Expr> condition,
		    ::std::unique_ptr<BlockExpr> if_block,
		    ::std::unique_ptr<BlockExpr> else_block, Location locus)
    : IfExpr (::std::move (condition), ::std::move (if_block), locus),
      else_block (::std::move (else_block))
  {}
  // again, outer attributes not allowed

  // Copy constructor with clone
  IfExprConseqElse (IfExprConseqElse const &other)
    : IfExpr (other), else_block (other.else_block->clone_block_expr ())
  {}

  // Destructor - define here if required

  // Overloaded assignment operator with cloning
  IfExprConseqElse &operator= (IfExprConseqElse const &other)
  {
    IfExpr::operator= (other);
    // condition = other.condition->clone_expr();
    // if_block = other.if_block->clone_block_expr();
    else_block = other.else_block->clone_block_expr ();

    return *this;
  }

  // move constructors
  IfExprConseqElse (IfExprConseqElse &&other) = default;
  IfExprConseqElse &operator= (IfExprConseqElse &&other) = default;

  virtual void accept_vis (ASTVisitor &vis) OVERRIDE;

protected:
  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual IfExprConseqElse *clone_expr_impl () const OVERRIDE
  {
    return new IfExprConseqElse (*this);
  }

  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual IfExprConseqElse *clone_expr_with_block_impl () const OVERRIDE
  {
    return new IfExprConseqElse (*this);
  }

  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual IfExprConseqElse *clone_if_expr_impl () const OVERRIDE
  {
    return new IfExprConseqElse (*this);
  }
};

// If expression with an ending "else if" expression AST node
class IfExprConseqIf : public IfExpr
{
  // IfExpr* if_expr;
  ::std::unique_ptr<IfExpr> if_expr;

public:
  /*~IfExprConseqIf() {
      delete if_expr;
  }*/

  ::std::string as_string () const;

  IfExprConseqIf (::std::unique_ptr<Expr> condition,
		  ::std::unique_ptr<BlockExpr> if_block,
		  ::std::unique_ptr<IfExpr> conseq_if_expr, Location locus)
    : IfExpr (::std::move (condition), ::std::move (if_block), locus),
      if_expr (::std::move (conseq_if_expr))
  {}
  // outer attributes not allowed

  // Copy constructor with clone
  IfExprConseqIf (IfExprConseqIf const &other)
    : IfExpr (other), if_expr (other.if_expr->clone_if_expr ())
  {}

  // Destructor - define here if required

  // Overloaded assignment operator to use clone
  IfExprConseqIf &operator= (IfExprConseqIf const &other)
  {
    IfExpr::operator= (other);
    // condition = other.condition->clone_expr();
    // if_block = other.if_block->clone_block_expr();
    if_expr = other.if_expr->clone_if_expr ();

    return *this;
  }

  // move constructors
  IfExprConseqIf (IfExprConseqIf &&other) = default;
  IfExprConseqIf &operator= (IfExprConseqIf &&other) = default;

  virtual void accept_vis (ASTVisitor &vis) OVERRIDE;

protected:
  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual IfExprConseqIf *clone_expr_impl () const OVERRIDE
  {
    return new IfExprConseqIf (*this);
  }

  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual IfExprConseqIf *clone_expr_with_block_impl () const OVERRIDE
  {
    return new IfExprConseqIf (*this);
  }

  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual IfExprConseqIf *clone_if_expr_impl () const OVERRIDE
  {
    return new IfExprConseqIf (*this);
  }
};

// Basic "if let" expression AST node with no else
class IfLetExpr : public ExprWithBlock
{
  // MatchArmPatterns patterns;
  ::std::vector< ::std::unique_ptr<Pattern> > match_arm_patterns; // inlined
  /*Expr* value;
  BlockExpr* if_block;*/
  ::std::unique_ptr<Expr> value;
  ::std::unique_ptr<BlockExpr> if_block;
  /*union {
      BlockExpr else_block;
      IfExpr if_expr;
      IfLetExpr* if_let_expr;
  } consequent_block;*/

  Location locus;

public:
  ::std::string as_string () const;

  IfLetExpr (::std::vector< ::std::unique_ptr<Pattern> > match_arm_patterns,
	     ::std::unique_ptr<Expr> value,
	     ::std::unique_ptr<BlockExpr> if_block, Location locus)
    : ExprWithBlock (::std::vector<Attribute> ()),
      match_arm_patterns (::std::move (match_arm_patterns)),
      value (::std::move (value)), if_block (::std::move (if_block)),
      locus (locus)
  {}
  // outer attributes not allowed on if let exprs either

  // copy constructor with clone
  IfLetExpr (IfLetExpr const &other)
    : ExprWithBlock (other),
      /*match_arm_patterns(other.match_arm_patterns),*/ value (
	other.value->clone_expr ()),
      if_block (other.if_block->clone_block_expr ()), locus (other.locus)
  {
    // crappy vector unique pointer clone - TODO is there a better way of doing
    // this?
    match_arm_patterns.reserve (other.match_arm_patterns.size ());

    for (const auto &e : other.match_arm_patterns)
      {
	match_arm_patterns.push_back (e->clone_pattern ());
      }
  }

  // destructor - define here if required

  // overload assignment operator to clone
  IfLetExpr &operator= (IfLetExpr const &other)
  {
    ExprWithBlock::operator= (other);
    // match_arm_patterns = other.match_arm_patterns;
    value = other.value->clone_expr ();
    if_block = other.if_block->clone_block_expr ();
    locus = other.locus;

    // crappy vector unique pointer clone - TODO is there a better way of doing
    // this?
    match_arm_patterns.reserve (other.match_arm_patterns.size ());

    for (const auto &e : other.match_arm_patterns)
      {
	match_arm_patterns.push_back (e->clone_pattern ());
      }

    return *this;
  }

  // move constructors
  IfLetExpr (IfLetExpr &&other) = default;
  IfLetExpr &operator= (IfLetExpr &&other) = default;

  // Unique pointer custom clone function
  ::std::unique_ptr<IfLetExpr> clone_if_let_expr () const
  {
    return ::std::unique_ptr<IfLetExpr> (clone_if_let_expr_impl ());
  }

  Location get_locus () const { return locus; }

  Location get_locus_slow () const OVERRIDE { return get_locus (); }

  virtual void accept_vis (ASTVisitor &vis) OVERRIDE;

protected:
  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual IfLetExpr *clone_expr_impl () const OVERRIDE
  {
    return new IfLetExpr (*this);
  }

  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual IfLetExpr *clone_expr_with_block_impl () const OVERRIDE
  {
    return new IfLetExpr (*this);
  }

  // Base clone function but still concrete as concrete base class
  virtual IfLetExpr *clone_if_let_expr_impl () const
  {
    return new IfLetExpr (*this);
  }
};

// If expression with an ending "else if let" expression AST node
class IfExprConseqIfLet : public IfExpr
{
  // IfLetExpr* if_let_expr;
  ::std::unique_ptr<IfLetExpr> if_let_expr;

public:
  /*~IfExprIfConseqIfLet() {
      delete if_let_expr;
  }*/

  ::std::string as_string () const;

  IfExprConseqIfLet (::std::unique_ptr<Expr> condition,
		     ::std::unique_ptr<BlockExpr> if_block,
		     ::std::unique_ptr<IfLetExpr> conseq_if_let_expr,
		     Location locus)
    : IfExpr (::std::move (condition), ::std::move (if_block), locus),
      if_let_expr (::std::move (conseq_if_let_expr))
  {}
  // outer attributes not allowed

  // Copy constructor with clone
  IfExprConseqIfLet (IfExprConseqIfLet const &other)
    : IfExpr (other), if_let_expr (other.if_let_expr->clone_if_let_expr ())
  {}

  // Destructor - define here if required

  // Overloaded assignment operator to use clone
  IfExprConseqIfLet &operator= (IfExprConseqIfLet const &other)
  {
    IfExpr::operator= (other);
    // condition = other.condition->clone_expr();
    // if_block = other.if_block->clone_block_expr();
    if_let_expr = other.if_let_expr->clone_if_let_expr ();

    return *this;
  }

  // move constructors
  IfExprConseqIfLet (IfExprConseqIfLet &&other) = default;
  IfExprConseqIfLet &operator= (IfExprConseqIfLet &&other) = default;

  virtual void accept_vis (ASTVisitor &vis) OVERRIDE;

protected:
  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual IfExprConseqIfLet *clone_expr_impl () const OVERRIDE
  {
    return new IfExprConseqIfLet (*this);
  }

  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual IfExprConseqIfLet *clone_expr_with_block_impl () const OVERRIDE
  {
    return new IfExprConseqIfLet (*this);
  }

  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual IfExprConseqIfLet *clone_if_expr_impl () const OVERRIDE
  {
    return new IfExprConseqIfLet (*this);
  }
};

// AST node representing "if let" expression with an "else" expression at the
// end
class IfLetExprConseqElse : public IfLetExpr
{
  // BlockExpr* else_block;
  ::std::unique_ptr<BlockExpr> else_block;

public:
  /*~IfLetExprConseqElse() {
      delete else_block;
  }*/

  ::std::string as_string () const;

  IfLetExprConseqElse (
    ::std::vector< ::std::unique_ptr<Pattern> > match_arm_patterns,
    ::std::unique_ptr<Expr> value, ::std::unique_ptr<BlockExpr> if_block,
    ::std::unique_ptr<BlockExpr> else_block, Location locus)
    : IfLetExpr (::std::move (match_arm_patterns), ::std::move (value),
		 ::std::move (if_block), locus),
      else_block (::std::move (else_block))
  {}
  // outer attributes not allowed

  // copy constructor with clone
  IfLetExprConseqElse (IfLetExprConseqElse const &other)
    : IfLetExpr (other), else_block (other.else_block->clone_block_expr ())
  {}

  // destructor - define here if required

  // overload assignment operator to clone
  IfLetExprConseqElse &operator= (IfLetExprConseqElse const &other)
  {
    IfLetExpr::operator= (other);
    // match_arm_patterns = other.match_arm_patterns;
    // value = other.value->clone_expr();
    // if_block = other.if_block->clone_block_expr();
    else_block = other.else_block->clone_block_expr ();
    // outer_attrs = other.outer_attrs;

    return *this;
  }

  // move constructors
  IfLetExprConseqElse (IfLetExprConseqElse &&other) = default;
  IfLetExprConseqElse &operator= (IfLetExprConseqElse &&other) = default;

  virtual void accept_vis (ASTVisitor &vis) OVERRIDE;

protected:
  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual IfLetExprConseqElse *clone_expr_impl () const OVERRIDE
  {
    return new IfLetExprConseqElse (*this);
  }

  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual IfLetExprConseqElse *clone_expr_with_block_impl () const OVERRIDE
  {
    return new IfLetExprConseqElse (*this);
  }

  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual IfLetExprConseqElse *clone_if_let_expr_impl () const OVERRIDE
  {
    return new IfLetExprConseqElse (*this);
  }
};

// AST node representing "if let" expression with an "else if" expression at the
// end
class IfLetExprConseqIf : public IfLetExpr
{
  // IfExpr* if_expr;
  ::std::unique_ptr<IfExpr> if_expr;

public:
  /*~IfLetExprConseqIf() {
      delete if_expr;
  }*/

  ::std::string as_string () const;

  IfLetExprConseqIf (
    ::std::vector< ::std::unique_ptr<Pattern> > match_arm_patterns,
    ::std::unique_ptr<Expr> value, ::std::unique_ptr<BlockExpr> if_block,
    ::std::unique_ptr<IfExpr> if_expr, Location locus)
    : IfLetExpr (::std::move (match_arm_patterns), ::std::move (value),
		 ::std::move (if_block), locus),
      if_expr (::std::move (if_expr))
  {}
  // again, outer attributes not allowed

  // copy constructor with clone
  IfLetExprConseqIf (IfLetExprConseqIf const &other)
    : IfLetExpr (other), if_expr (other.if_expr->clone_if_expr ())
  {}

  // destructor - define here if required

  // overload assignment operator to clone
  IfLetExprConseqIf &operator= (IfLetExprConseqIf const &other)
  {
    IfLetExpr::operator= (other);
    // match_arm_patterns = other.match_arm_patterns;
    // value = other.value->clone_expr();
    // if_block = other.if_block->clone_block_expr();
    if_expr = other.if_expr->clone_if_expr ();

    return *this;
  }

  // move constructors
  IfLetExprConseqIf (IfLetExprConseqIf &&other) = default;
  IfLetExprConseqIf &operator= (IfLetExprConseqIf &&other) = default;

  virtual void accept_vis (ASTVisitor &vis) OVERRIDE;

protected:
  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual IfLetExprConseqIf *clone_expr_impl () const OVERRIDE
  {
    return new IfLetExprConseqIf (*this);
  }

  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual IfLetExprConseqIf *clone_expr_with_block_impl () const OVERRIDE
  {
    return new IfLetExprConseqIf (*this);
  }

  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual IfLetExprConseqIf *clone_if_let_expr_impl () const OVERRIDE
  {
    return new IfLetExprConseqIf (*this);
  }
};

// AST node representing "if let" expression with an "else if let" expression at
// the end
class IfLetExprConseqIfLet : public IfLetExpr
{
  // IfLetExpr* if_let_expr;
  ::std::unique_ptr<IfLetExpr> if_let_expr;

public:
  /*~IfLetExprConseqIfLet() {
      delete if_let_expr;
  }*/

  ::std::string as_string () const;

  IfLetExprConseqIfLet (
    ::std::vector< ::std::unique_ptr<Pattern> > match_arm_patterns,
    ::std::unique_ptr<Expr> value, ::std::unique_ptr<BlockExpr> if_block,
    ::std::unique_ptr<IfLetExpr> if_let_expr, Location locus)
    : IfLetExpr (::std::move (match_arm_patterns), ::std::move (value),
		 ::std::move (if_block), locus),
      if_let_expr (::std::move (if_let_expr))
  {}
  // outer attributes not allowed

  // copy constructor with clone
  IfLetExprConseqIfLet (IfLetExprConseqIfLet const &other)
    : IfLetExpr (other), if_let_expr (other.if_let_expr->clone_if_let_expr ())
  {}

  // destructor - define here if required

  // overload assignment operator to clone
  IfLetExprConseqIfLet &operator= (IfLetExprConseqIfLet const &other)
  {
    IfLetExpr::operator= (other);
    // match_arm_patterns = other.match_arm_patterns;
    // value = other.value->clone_expr();
    // if_block = other.if_block->clone_block_expr();
    if_let_expr = other.if_let_expr->clone_if_let_expr ();

    return *this;
  }

  // move constructors
  IfLetExprConseqIfLet (IfLetExprConseqIfLet &&other) = default;
  IfLetExprConseqIfLet &operator= (IfLetExprConseqIfLet &&other) = default;

  virtual void accept_vis (ASTVisitor &vis) OVERRIDE;

protected:
  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual IfLetExprConseqIfLet *clone_expr_impl () const OVERRIDE
  {
    return new IfLetExprConseqIfLet (*this);
  }

  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual IfLetExprConseqIfLet *clone_expr_with_block_impl () const OVERRIDE
  {
    return new IfLetExprConseqIfLet (*this);
  }

  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual IfLetExprConseqIfLet *clone_if_let_expr_impl () const OVERRIDE
  {
    return new IfLetExprConseqIfLet (*this);
  }
};

// Match arm expression
struct MatchArm
{
private:
  ::std::vector<Attribute> outer_attrs;
  // MatchArmPatterns patterns;
  ::std::vector< ::std::unique_ptr<Pattern> > match_arm_patterns; // inlined

  // bool has_match_arm_guard;
  // Expr* match_arm_guard; // inlined from MatchArmGuard
  ::std::unique_ptr<Expr> guard_expr;

  // TODO: should this store location data?

public:
  /*~MatchArm() {
      if (has_match_arm_guard) {
	  delete match_arm_guard;
      }
  }*/

  // Returns whether the MatchArm has a match arm guard expression
  inline bool has_match_arm_guard () const { return guard_expr != NULL; }

  // Constructor for match arm with a guard expression
  MatchArm (::std::vector< ::std::unique_ptr<Pattern> > match_arm_patterns,
	    ::std::unique_ptr<Expr> guard_expr = NULL,
	    ::std::vector<Attribute> outer_attrs = ::std::vector<Attribute> ())
    : outer_attrs (::std::move (outer_attrs)),
      match_arm_patterns (::std::move (match_arm_patterns)),
      guard_expr (::std::move (guard_expr))
  {}

  // Copy constructor with clone
  MatchArm (MatchArm const &other)
    : /*match_arm_patterns(other.match_arm_patterns),*/ outer_attrs (
      other.outer_attrs)
  {
    // guard to protect from null pointer dereference
    if (other.guard_expr != NULL)
      {
	guard_expr = other.guard_expr->clone_expr ();
      }

    // DEBUG
    fprintf (
      stderr,
      "started copy-constructing match arm (outer attrs, guard expr done)\n");

    // crappy vector unique pointer clone - TODO is there a better way of doing
    // this?
    match_arm_patterns.reserve (other.match_arm_patterns.size ());

    for (const auto &e : other.match_arm_patterns)
      {
	match_arm_patterns.push_back (e->clone_pattern ());

	// DEBUG
	fprintf (stderr, "successfully pushed back a match arm pattern\n");
      }

    // DEBUG
    fprintf (stderr, "successfully copy-constructed match arm\n");
  }

  ~MatchArm () = default;

  // Overload assignment operator to clone
  MatchArm &operator= (MatchArm const &other)
  {
    // match_arm_patterns = other.match_arm_patterns;
    outer_attrs = other.outer_attrs;
    guard_expr = other.guard_expr->clone_expr ();

    // crappy vector unique pointer clone - TODO is there a better way of doing
    // this?
    match_arm_patterns.reserve (other.match_arm_patterns.size ());

    for (const auto &e : other.match_arm_patterns)
      {
	match_arm_patterns.push_back (e->clone_pattern ());
      }

    return *this;
  }

  // move constructors
  MatchArm (MatchArm &&other) = default;
  MatchArm &operator= (MatchArm &&other) = default;

  // Returns whether match arm is in an error state.
  inline bool is_error () const { return match_arm_patterns.empty (); }

  // Creates a match arm in an error state.
  static MatchArm create_error ()
  {
    return MatchArm (::std::vector< ::std::unique_ptr<Pattern> > ());
  }

  ::std::string as_string () const;
};

// Base "match case" for a match expression - abstract
class MatchCase
{
  MatchArm arm;

protected:
  MatchCase (MatchArm arm) : arm (::std::move (arm)) {}

  // Should not require copy constructor or assignment operator overloading

  // Clone function implementation as pure virtual method
  virtual MatchCase *clone_match_case_impl () const = 0;

public:
  virtual ~MatchCase () {}

  // Unique pointer custom clone function
  ::std::unique_ptr<MatchCase> clone_match_case () const
  {
    // DEBUG
    fprintf (stderr, "about to call clone match case impl\n");

    return ::std::unique_ptr<MatchCase> (clone_match_case_impl ());
  }

  virtual ::std::string as_string () const;

  virtual void accept_vis (ASTVisitor &vis) = 0;
};

// Block expression match case
class MatchCaseBlockExpr : public MatchCase
{
  // BlockExpr* block_expr;
  ::std::unique_ptr<BlockExpr> block_expr;

  // TODO: should this store location data?

public:
  /*~MatchCaseBlockExpr() {
      delete block_expr;
  }*/

  MatchCaseBlockExpr (MatchArm arm, ::std::unique_ptr<BlockExpr> block_expr)
    : MatchCase (::std::move (arm)), block_expr (::std::move (block_expr))
  {}

  // Copy constructor requires clone
  MatchCaseBlockExpr (MatchCaseBlockExpr const &other)
    : MatchCase (other), block_expr (other.block_expr->clone_block_expr ())
  {
    // DEBUG
    fprintf (stderr, "successfully copy constructed match case expr\n");
  }

  // Destructor - define here if required

  // Overload assignment operator to have clone
  MatchCaseBlockExpr &operator= (MatchCaseBlockExpr const &other)
  {
    MatchCase::operator= (other);
    block_expr = other.block_expr->clone_block_expr ();
    // arm = other.arm;

    return *this;
  }

  // move constructors
  MatchCaseBlockExpr (MatchCaseBlockExpr &&other) = default;
  MatchCaseBlockExpr &operator= (MatchCaseBlockExpr &&other) = default;

  ::std::string as_string () const;

  virtual void accept_vis (ASTVisitor &vis) OVERRIDE;

protected:
  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual MatchCaseBlockExpr *clone_match_case_impl () const OVERRIDE
  {
    // DEBUG
    fprintf (stderr, "about to copy construct match case block expr\n");

    return new MatchCaseBlockExpr (*this);
  }
};

// Expression (except block expression) match case
class MatchCaseExpr : public MatchCase
{
  // Expr* expr;
  ::std::unique_ptr<Expr> expr;

  // TODO: should this store location data?

public:
  /*~MatchCaseExpr() {
      delete expr;
  }*/

  MatchCaseExpr (MatchArm arm, ::std::unique_ptr<Expr> expr)
    : MatchCase (::std::move (arm)), expr (::std::move (expr))
  {}

  // Copy constructor requires clone
  MatchCaseExpr (MatchCaseExpr const &other)
    : MatchCase (other), expr (other.expr->clone_expr ())
  {
    // DEBUG
    fprintf (stderr, "successfully copy constructed match case expr\n");
  }

  // Destructor - define here if required

  // Overload assignment operator to have clone
  MatchCaseExpr &operator= (MatchCaseExpr const &other)
  {
    MatchCase::operator= (other);
    expr = other.expr->clone_expr ();
    // arm = other.arm;

    return *this;
  }

  // move constructors
  MatchCaseExpr (MatchCaseExpr &&other) = default;
  MatchCaseExpr &operator= (MatchCaseExpr &&other) = default;

  ::std::string as_string () const;

  virtual void accept_vis (ASTVisitor &vis) OVERRIDE;

protected:
  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual MatchCaseExpr *clone_match_case_impl () const OVERRIDE
  {
    // DEBUG
    fprintf (stderr, "about to copy construct match case expr\n");
    if (expr == NULL)
      {
	fprintf (
	  stderr,
	  "warning: match case expr to be copy constructed has null expr!\n");
      }

    return new MatchCaseExpr (*this);
  }
};

// Match expression AST node
class MatchExpr : public ExprWithBlock
{
  // Expr* branch_value;
  ::std::unique_ptr<Expr> branch_value;
  ::std::vector<Attribute> inner_attrs;

  // bool has_match_arms;
  // MatchArms match_arms;
  ::std::vector< ::std::unique_ptr<MatchCase> >
    match_arms; // inlined from MatchArms

  Location locus;

public:
  /*~MatchExpr() {
      delete branch_value;
  }*/

  ::std::string as_string () const;

  // Returns whether the match expression has any match arms.
  inline bool has_match_arms () const { return !match_arms.empty (); }

  MatchExpr (::std::unique_ptr<Expr> branch_value,
	     ::std::vector< ::std::unique_ptr<MatchCase> > match_arms,
	     ::std::vector<Attribute> inner_attrs,
	     ::std::vector<Attribute> outer_attrs, Location locus)
    : ExprWithBlock (::std::move (outer_attrs)),
      branch_value (::std::move (branch_value)),
      inner_attrs (::std::move (inner_attrs)),
      match_arms (::std::move (match_arms)), locus (locus)
  {}

  // Copy constructor requires clone due to unique_ptr
  MatchExpr (MatchExpr const &other)
    : ExprWithBlock (other),
      branch_value (
	other.branch_value->clone_expr ()), /*match_arms(other.match_arms),*/
      inner_attrs (other.inner_attrs), locus (other.locus)
  {
    fprintf (stderr,
	     "copy constructor for matchexpr called - only match arm vector "
	     "copying after this\n");

    // crappy vector unique pointer clone - TODO is there a better way of doing
    // this?
    match_arms.reserve (other.match_arms.size ());

    fprintf (stderr, "match expr: successfully reserved size\n");

    for (const auto &e : other.match_arms)
      {
	match_arms.push_back (e->clone_match_case ());
	fprintf (stderr, "match expr: successfully pushed back a match case\n");
      }

    fprintf (stderr, "match expr: successfully pushed back all match cases\n");
  }

  // Destructor - define here if required

  // Overloaded assignment operator to clone due to unique_ptr
  MatchExpr &operator= (MatchExpr const &other)
  {
    ExprWithBlock::operator= (other);
    branch_value = other.branch_value->clone_expr ();
    // match_arms = other.match_arms;
    inner_attrs = other.inner_attrs;
    // outer_attrs = other.outer_attrs;
    locus = other.locus;

    // crappy vector unique pointer clone - TODO is there a better way of doing
    // this?
    match_arms.reserve (other.match_arms.size ());

    for (const auto &e : other.match_arms)
      {
	match_arms.push_back (e->clone_match_case ());
      }

    return *this;
  }

  // move constructors
  MatchExpr (MatchExpr &&other) = default;
  MatchExpr &operator= (MatchExpr &&other) = default;

  Location get_locus () const { return locus; }

  Location get_locus_slow () const OVERRIDE { return get_locus (); }

  virtual void accept_vis (ASTVisitor &vis) OVERRIDE;

protected:
  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual MatchExpr *clone_expr_impl () const OVERRIDE
  {
    return new MatchExpr (*this);
  }

  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual MatchExpr *clone_expr_with_block_impl () const OVERRIDE
  {
    return new MatchExpr (*this);
  }
};

// Await expression AST node (pseudo-member variable access)
class AwaitExpr : public ExprWithoutBlock
{
  ::std::unique_ptr<Expr> awaited_expr;

  Location locus;

public:
  // TODO: ensure outer attributes are actually allowed
  AwaitExpr (::std::unique_ptr<Expr> awaited_expr,
	     ::std::vector<Attribute> outer_attrs, Location locus)
    : ExprWithoutBlock (::std::move (outer_attrs)),
      awaited_expr (::std::move (awaited_expr)), locus (locus)
  {}

  // copy constructor with clone
  AwaitExpr (AwaitExpr const &other)
    : ExprWithoutBlock (other),
      awaited_expr (other.awaited_expr->clone_expr ()), locus (other.locus)
  {}

  // destructor - define here if required

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

  ::std::string as_string () const;

  Location get_locus () const { return locus; }

  Location get_locus_slow () const OVERRIDE { return get_locus (); }

  virtual void accept_vis (ASTVisitor &vis) OVERRIDE;

protected:
  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual AwaitExpr *clone_expr_without_block_impl () const OVERRIDE
  {
    return new AwaitExpr (*this);
  }
};

// Async block expression AST node (block expr that evaluates to a future)
class AsyncBlockExpr : public ExprWithBlock
{
  // TODO: should this extend BlockExpr rather than be a composite of it?
  bool has_move;
  ::std::unique_ptr<BlockExpr> block_expr;

  Location locus;

public:
  AsyncBlockExpr (::std::unique_ptr<BlockExpr> block_expr, bool has_move,
		  ::std::vector<Attribute> outer_attrs, Location locus)
    : ExprWithBlock (::std::move (outer_attrs)), has_move (has_move),
      block_expr (::std::move (block_expr)), locus (locus)
  {}

  // copy constructor with clone
  AsyncBlockExpr (AsyncBlockExpr const &other)
    : ExprWithBlock (other), has_move (other.has_move),
      block_expr (other.block_expr->clone_block_expr ()), locus (other.locus)
  {}

  // destructor - define if required

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

  ::std::string as_string () const;

  Location get_locus () const { return locus; }

  Location get_locus_slow () const OVERRIDE { return get_locus (); }

  virtual void accept_vis (ASTVisitor &vis) OVERRIDE;

protected:
  // Use covariance to implement clone function as returning this object rather
  // than base
  virtual AsyncBlockExpr *clone_expr_with_block_impl () const OVERRIDE
  {
    return new AsyncBlockExpr (*this);
  }
};
} // namespace AST
} // namespace Rust

#endif
