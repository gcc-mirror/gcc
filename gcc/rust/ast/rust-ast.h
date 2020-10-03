#ifndef RUST_AST_BASE_H
#define RUST_AST_BASE_H
// Base for AST used in gccrs, basically required by all specific ast things

#include "rust-system.h"

// STL imports
#include <memory>
#include <string>
#include <vector>

// gccrs imports
// required for AST::Token
#include "rust-token.h"

#include "rust-location.h"

namespace Rust {
// TODO: remove typedefs and make actual types for these
typedef std::string Identifier;
typedef int TupleIndex;

struct Session;

namespace AST {
// foward decl: ast visitor
class ASTVisitor;

// Delimiter types - used in macros and whatever.
enum DelimType
{
  PARENS,
  SQUARE,
  CURLY
};

// Base AST node object - TODO is this really required or useful? Where to draw
// line?
/*class Node {
  public:
    // Gets node's Location.
    Location get_locus() const {
	return loc;
    }

    // Sets node's Location.
    void set_locus(Location loc_) {
	loc = loc_;
    }

    // Get node output as a string. Pure virtual.
    virtual std::string as_string() const = 0;

    virtual ~Node() {}

    // TODO: constructor including Location? Make all derived classes have
Location?

  private:
    // The node's location.
    Location loc;
};*/
// decided to not have node as a "node" would never need to be stored

// Attribute body - abstract base class
class AttrInput
{
public:
  virtual ~AttrInput () {}

  // Unique pointer custom clone function
  std::unique_ptr<AttrInput> clone_attr_input () const
  {
    return std::unique_ptr<AttrInput> (clone_attr_input_impl ());
  }

  virtual std::string as_string () const = 0;

  virtual void accept_vis (ASTVisitor &vis) = 0;

  virtual bool check_cfg_predicate (const Session &session) const = 0;

  // Parse attribute input to meta item, if possible
  virtual AttrInput *parse_to_meta_item () const { return nullptr; }

protected:
  // pure virtual clone implementation
  virtual AttrInput *clone_attr_input_impl () const = 0;
};

// forward decl for use in token tree method
class Token;

// A tree of tokens (or a single token) - abstract base class
class TokenTree
{
public:
  virtual ~TokenTree () {}

  // Unique pointer custom clone function
  std::unique_ptr<TokenTree> clone_token_tree () const
  {
    return std::unique_ptr<TokenTree> (clone_token_tree_impl ());
  }

  virtual std::string as_string () const = 0;

  virtual void accept_vis (ASTVisitor &vis) = 0;

  /* Converts token tree to a flat token stream. Tokens must be pointer to avoid
   * mutual dependency with Token. */
  virtual std::vector<std::unique_ptr<Token>> to_token_stream () const = 0;

protected:
  // pure virtual clone implementation
  virtual TokenTree *clone_token_tree_impl () const = 0;
};

// Abstract base class for a macro match
class MacroMatch
{
public:
  virtual ~MacroMatch () {}

  virtual std::string as_string () const = 0;

  // Unique pointer custom clone function
  std::unique_ptr<MacroMatch> clone_macro_match () const
  {
    return std::unique_ptr<MacroMatch> (clone_macro_match_impl ());
  }

  virtual void accept_vis (ASTVisitor &vis) = 0;

protected:
  // pure virtual clone implementation
  virtual MacroMatch *clone_macro_match_impl () const = 0;
};

// A token is a kind of token tree (except delimiter tokens)
class Token : public TokenTree, public MacroMatch
{
  // A token is a kind of token tree (except delimiter tokens)
  // A token is a kind of MacroMatch (except $ and delimiter tokens)
  // TODO: improve member variables - current ones are the same as lexer token
  // Token kind.
  TokenId token_id;
  // Token location.
  Location locus;
  // Associated text (if any) of token.
  std::string str;
  // Token type hint (if any).
  PrimitiveCoreType type_hint;

public:
  // Unique pointer custom clone function
  std::unique_ptr<Token> clone_token () const
  {
    return std::unique_ptr<Token> (clone_token_impl ());
  }

  /* constructor from general text - avoid using if lexer const_TokenPtr is
   * available */
  Token (TokenId token_id, Location locus, std::string str,
	 PrimitiveCoreType type_hint)
    : token_id (token_id), locus (locus), str (std::move (str)),
      type_hint (type_hint)
  {}

  // Constructor from lexer const_TokenPtr
  /* TODO: find workaround for std::string being nullptr - probably have to
   * introduce new method in lexer Token, or maybe make conversion method
   * there */
  Token (const_TokenPtr lexer_token_ptr)
    : token_id (lexer_token_ptr->get_id ()),
      locus (lexer_token_ptr->get_locus ()), str (""),
      type_hint (lexer_token_ptr->get_type_hint ())
  {
    // FIXME: change to "should have str" later?
    if (lexer_token_ptr->has_str ())
      {
	str = lexer_token_ptr->get_str ();

	// DEBUG
	fprintf (stderr, "ast token created with str '%s'\n", str.c_str ());
      }
    else
      {
	// FIXME: is this returning correct thing?
	str = lexer_token_ptr->get_token_description ();

	// DEBUG
	fprintf (stderr, "ast token created with string '%s'\n", str.c_str ());
      }

    // DEBUG
    if (lexer_token_ptr->should_have_str () && !lexer_token_ptr->has_str ())
      {
	fprintf (stderr,
		 "BAD: for token '%s', should have string but does not!\n",
		 lexer_token_ptr->get_token_description ());
      }
  }

  bool is_string_lit () const
  {
    switch (token_id)
      {
      case STRING_LITERAL:
      case BYTE_STRING_LITERAL:
	return true;
      default:
	return false;
      }
  }

  std::string as_string () const override;

  void accept_vis (ASTVisitor &vis) override;

  // Return copy of itself but in token stream form.
  std::vector<std::unique_ptr<Token>> to_token_stream () const override;

  TokenId get_id () const { return token_id; }

  Location get_locus () const { return locus; }

protected:
  // No virtual for now as not polymorphic but can be in future
  /*virtual*/ Token *clone_token_impl () const { return new Token (*this); }

  /* Use covariance to implement clone function as returning this object rather
   * than base */
  Token *clone_token_tree_impl () const override { return clone_token_impl (); }

  /* Use covariance to implement clone function as returning this object rather
   * than base */
  Token *clone_macro_match_impl () const override
  {
    return clone_token_impl ();
  }
};

// A literal - value with a type. Used in LiteralExpr and LiteralPattern.
struct Literal
{
public:
  enum LitType
  {
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
  /* TODO: maybe make subclasses of each type of literal with their typed values
   * (or generics) */
  std::string value_as_string;
  LitType type;

public:
  std::string as_string () const { return value_as_string; }

  LitType get_lit_type () const { return type; }

  Literal (std::string value_as_string, LitType type)
    : value_as_string (std::move (value_as_string)), type (type)
  {}

  static Literal create_error () { return Literal ("", CHAR); }

  // Returns whether literal is in an invalid state.
  bool is_error () const { return value_as_string == ""; }
};

// A token tree with delimiters
class DelimTokenTree : public TokenTree, public AttrInput
{
  DelimType delim_type;
  std::vector<std::unique_ptr<TokenTree>> token_trees;
  Location locus;

protected:
  DelimTokenTree *clone_delim_tok_tree_impl () const
  {
    return new DelimTokenTree (*this);
  }

  /* Use covariance to implement clone function as returning a DelimTokenTree
   * object */
  DelimTokenTree *clone_attr_input_impl () const override
  {
    return clone_delim_tok_tree_impl ();
  }

  /* Use covariance to implement clone function as returning a DelimTokenTree
   * object */
  DelimTokenTree *clone_token_tree_impl () const override
  {
    return clone_delim_tok_tree_impl ();
  }

public:
  DelimTokenTree (DelimType delim_type,
		  std::vector<std::unique_ptr<TokenTree>> token_trees
		  = std::vector<std::unique_ptr<TokenTree>> (),
		  Location locus = Location ())
    : delim_type (delim_type), token_trees (std::move (token_trees)),
      locus (locus)
  {}

  // Copy constructor with vector clone
  DelimTokenTree (DelimTokenTree const &other)
    : delim_type (other.delim_type), locus (other.locus)
  {
    token_trees.reserve (other.token_trees.size ());
    for (const auto &e : other.token_trees)
      token_trees.push_back (e->clone_token_tree ());
  }

  // overloaded assignment operator with vector clone
  DelimTokenTree &operator= (DelimTokenTree const &other)
  {
    delim_type = other.delim_type;
    locus = other.locus;

    token_trees.reserve (other.token_trees.size ());
    for (const auto &e : other.token_trees)
      token_trees.push_back (e->clone_token_tree ());

    return *this;
  }

  // move constructors
  DelimTokenTree (DelimTokenTree &&other) = default;
  DelimTokenTree &operator= (DelimTokenTree &&other) = default;

  static DelimTokenTree create_empty () { return DelimTokenTree (PARENS); }

  std::string as_string () const override;

  void accept_vis (ASTVisitor &vis) override;

  bool
  check_cfg_predicate (const Session &session ATTRIBUTE_UNUSED) const override
  {
    // this should never be called - should be converted first
    return false;
  }

  AttrInput *parse_to_meta_item () const override;

  std::vector<std::unique_ptr<Token>> to_token_stream () const override;

  std::unique_ptr<DelimTokenTree> clone_delim_token_tree () const
  {
    return std::unique_ptr<DelimTokenTree> (clone_delim_tok_tree_impl ());
  }
};

/* Forward decl - definition moved to rust-expr.h as it requires LiteralExpr to
 * be defined */
class AttrInputLiteral;

/* TODO: move applicable stuff into here or just don't include it because
 * nothing uses it A segment of a path (maybe) */
class PathSegment
{
public:
  virtual ~PathSegment () {}

  virtual std::string as_string () const = 0;

  // TODO: add visitor here?
};

// A segment of a simple path without generic or type arguments
class SimplePathSegment : public PathSegment
{
  std::string segment_name;
  Location locus;

  // only allow identifiers, "super", "self", "crate", or "$crate"
public:
  // TODO: put checks in constructor to enforce this rule?
  SimplePathSegment (std::string segment_name, Location locus = Location ())
    : segment_name (std::move (segment_name)), locus (locus)
  {}

  /* Returns whether simple path segment is in an invalid state (currently, if
   * empty). */
  bool is_error () const { return segment_name.empty (); }

  // Creates an error SimplePathSegment
  static SimplePathSegment create_error ()
  {
    return SimplePathSegment (std::string (""));
  }

  std::string as_string () const override;

  Location get_locus () const { return locus; }

  // TODO: visitor pattern?
};

// A simple path without generic or type arguments
class SimplePath
{
  bool has_opening_scope_resolution;
  std::vector<SimplePathSegment> segments;
  Location locus;

public:
  // Constructor
  SimplePath (std::vector<SimplePathSegment> path_segments,
	      bool has_opening_scope_resolution = false,
	      Location locus = Location ())
    : has_opening_scope_resolution (has_opening_scope_resolution),
      segments (std::move (path_segments)), locus (locus)
  {}

  // Creates an empty SimplePath.
  static SimplePath create_empty ()
  {
    return SimplePath (std::vector<SimplePathSegment> ());
  }

  // Returns whether the SimplePath is empty, i.e. has path segments.
  bool is_empty () const { return segments.empty (); }

  std::string as_string () const;

  Location get_locus () const { return locus; }

  // does this need visitor if not polymorphic? probably not

  // path-to-string comparison operator
  bool operator== (const std::string &rhs)
  {
    return !has_opening_scope_resolution && segments.size () == 1
	   && segments[0].as_string () == rhs;
  }

  /* Creates a single-segment SimplePath from a string. This will not check to
   * ensure that this is a valid identifier in path, so be careful. Also, this
   * will have no location data.
   * TODO have checks? */
  static SimplePath from_str (std::string str)
  {
    std::vector<AST::SimplePathSegment> single_segments
      = {AST::SimplePathSegment (std::move (str))};
    return SimplePath (std::move (single_segments));
  }
};

// aka Attr
// Attribute AST representation
struct Attribute
{
private:
  SimplePath path;

  // bool has_attr_input;
  std::unique_ptr<AttrInput> attr_input;

  Location locus;

  // TODO: maybe a variable storing whether attr input is parsed or not

public:
  // Returns whether Attribute has AttrInput
  bool has_attr_input () const { return attr_input != nullptr; }

  // Constructor has pointer AttrInput for polymorphism reasons
  Attribute (SimplePath path, std::unique_ptr<AttrInput> input,
	     Location locus = Location ())
    : path (std::move (path)), attr_input (std::move (input)), locus (locus)
  {}

  // default destructor
  ~Attribute () = default;

  // Copy constructor must deep copy attr_input as unique pointer
  Attribute (Attribute const &other) : path (other.path), locus (other.locus)
  {
    // guard to protect from null pointer dereference
    if (other.attr_input != nullptr)
      attr_input = other.attr_input->clone_attr_input ();
  }

  // overload assignment operator to use custom clone method
  Attribute &operator= (Attribute const &other)
  {
    path = other.path;
    locus = other.locus;
    // guard to protect from null pointer dereference
    if (other.attr_input != nullptr)
      attr_input = other.attr_input->clone_attr_input ();

    return *this;
  }

  // default move semantics
  Attribute (Attribute &&other) = default;
  Attribute &operator= (Attribute &&other) = default;

  // Unique pointer custom clone function
  std::unique_ptr<Attribute> clone_attribute () const
  {
    return std::unique_ptr<Attribute> (clone_attribute_impl ());
  }

  // Creates an empty attribute (which is invalid)
  static Attribute create_empty ()
  {
    return Attribute (SimplePath::create_empty (), nullptr);
  }

  // Returns whether the attribute is considered an "empty" attribute.
  bool is_empty () const { return attr_input == nullptr && path.is_empty (); }

  /* e.g.:
      #![crate_type = "lib"]
      #[test]
      #[cfg(target_os = "linux")]
      #[allow(non_camel_case_types)]
      #![allow(unused_variables)]
  */

  // Full built-in attribute list:
  /*   cfg
   *   cfg_attr
   *   test
   *   ignore
   *   should_panic
   *   derive
   *   macro_export
   *   macro_use
   *   proc_macro
   *   proc_macro_derive
   *   proc_macro_attribute
   *   allow
   *   warn
   *   deny
   *   forbid
   *   deprecated
   *   must_use
   *   link
   *   link_name
   *   no_link
   *   repr
   *   crate_type
   *   no_main
   *   export_name
   *   link_section
   *   no_mangle
   *   used
   *   crate_name
   *   inline
   *   cold
   *   no_builtins
   *   target_feature
   *   doc
   *   no_std
   *   no_implicit_prelude
   *   path
   *   recursion_limit
   *   type_length_limit
   *   panic_handler
   *   global_allocator
   *   windows_subsystem
   *   feature     */

  std::string as_string () const;

  // TODO: does this require visitor pattern as not polymorphic?

  // Maybe change to const-reference in future
  SimplePath get_path () const { return path; }

  // Call to parse attribute body to meta item syntax.
  void parse_attr_to_meta_item ();

  /* Determines whether cfg predicate is true and item with attribute should not
   * be stripped. */
  bool check_cfg_predicate (const Session &session)
  {
    /* assume that cfg predicate actually can exist, i.e. attribute has cfg or
     * cfg_attr path */

    if (!has_attr_input ())
      return false;

    // TODO: maybe replace with storing a "has been parsed" variable?
    parse_attr_to_meta_item ();
    // can't be const because of this anyway

    return attr_input->check_cfg_predicate (session);
  }

protected:
  // not virtual as currently no subclasses of Attribute, but could be in future
  /*virtual*/ Attribute *clone_attribute_impl () const
  {
    return new Attribute (*this);
  }
};

// Forward decl - defined in rust-macro.h
class MetaNameValueStr;

// abstract base meta item inner class
class MetaItemInner
{
protected:
  // pure virtual as MetaItemInner
  virtual MetaItemInner *clone_meta_item_inner_impl () const = 0;

public:
  // Unique pointer custom clone function
  std::unique_ptr<MetaItemInner> clone_meta_item_inner () const
  {
    return std::unique_ptr<MetaItemInner> (clone_meta_item_inner_impl ());
  }

  virtual ~MetaItemInner () {}

  virtual std::string as_string () const = 0;

  virtual void accept_vis (ASTVisitor &vis) = 0;

  /* HACK: used to simplify parsing - creates a copy of that type, or returns
   * null */
  virtual std::unique_ptr<MetaNameValueStr> to_meta_name_value_str () const { return nullptr; }

  // HACK: used to simplify parsing - same thing
  virtual SimplePath to_path_item () const
  {
    return SimplePath::create_empty ();
  }

  virtual Attribute to_attribute() const { return Attribute::create_empty (); }

  virtual bool check_cfg_predicate (const Session &session) const = 0;
};

// Container used to store MetaItems as AttrInput (bridge-ish kinda thing)
class AttrInputMetaItemContainer : public AttrInput
{
  std::vector<std::unique_ptr<MetaItemInner>> items;

public:
  AttrInputMetaItemContainer (
    std::vector<std::unique_ptr<MetaItemInner>> items)
    : items (std::move (items))
  {}

  // no destructor definition required

  // default move constructors
  AttrInputMetaItemContainer (AttrInputMetaItemContainer &&other) = default;
  AttrInputMetaItemContainer &operator= (AttrInputMetaItemContainer &&other)
    = default;

  std::string as_string () const override;

  void accept_vis (ASTVisitor &vis) override;

  bool check_cfg_predicate (const Session &session) const override;

  // Clones this object.
  std::unique_ptr<AttrInputMetaItemContainer>
  clone_attr_input_meta_item_container () const
  {
    return std::unique_ptr<AttrInputMetaItemContainer> (
      clone_attr_input_meta_item_container_impl ());
  }

protected:
  // Use covariance to implement clone function as returning this type
  AttrInputMetaItemContainer *clone_attr_input_impl () const override
  {
    return clone_attr_input_meta_item_container_impl ();
  }

  AttrInputMetaItemContainer *clone_attr_input_meta_item_container_impl () const
  {
    return new AttrInputMetaItemContainer (*this);
  }

  // copy constructor with vector clone
  AttrInputMetaItemContainer (const AttrInputMetaItemContainer &other)
  {
    items.reserve (other.items.size ());
    for (const auto &e : other.items)
      items.push_back (e->clone_meta_item_inner ());
  }

  // copy assignment operator with vector clone
  AttrInputMetaItemContainer &
  operator= (const AttrInputMetaItemContainer &other)
  {
    AttrInput::operator= (other);

    items.reserve (other.items.size ());
    for (const auto &e : other.items)
      items.push_back (e->clone_meta_item_inner ());

    return *this;
  }
};

// abstract base meta item class
class MetaItem : public MetaItemInner
{
};

// Forward decl - defined in rust-expr.h
class MetaItemLitExpr;

// Forward decl - defined in rust-expr.h
class MetaItemPathLit;

// Forward decl - defined in rust-macro.h
class MetaItemPath;

// Forward decl - defined in rust-macro.h
class MetaItemSeq;

// Forward decl - defined in rust-macro.h
class MetaWord;

// Forward decl - defined in rust-macro.h
class MetaListPaths;

// Forward decl - defined in rust-macro.h
struct MetaListNameValueStr;

/* Base statement abstract class. Note that most "statements" are not allowed in
 * top-level module scope - only a subclass of statements called "items" are. */
class Stmt
{
public:
  // Unique pointer custom clone function
  std::unique_ptr<Stmt> clone_stmt () const
  {
    return std::unique_ptr<Stmt> (clone_stmt_impl ());
  }

  virtual ~Stmt () {}

  virtual std::string as_string () const = 0;

  virtual void accept_vis (ASTVisitor &vis) = 0;

  /* HACK: slow way of getting location from base expression through virtual
   * methods. */
  virtual Location get_locus_slow () const { return Location (); }

protected:
  // Clone function implementation as pure virtual method
  virtual Stmt *clone_stmt_impl () const = 0;
};

// Rust "item" AST node (declaration of top-level/module-level allowed stuff)
class Item : public Stmt
{
  std::vector<Attribute> outer_attrs;

  // TODO: should outer attrs be defined here or in each derived class?

public:
  // Unique pointer custom clone function
  std::unique_ptr<Item> clone_item () const
  {
    return std::unique_ptr<Item> (clone_item_impl ());
  }

  std::string as_string () const;

  /* Adds crate names to the vector passed by reference, if it can
   * (polymorphism). */
  virtual void
  add_crate_name (std::vector<std::string> &names ATTRIBUTE_UNUSED) const
  {}

  virtual void accept_vis (ASTVisitor &vis ATTRIBUTE_UNUSED) {}

protected:
  // Constructor
  Item (std::vector<Attribute> outer_attribs = std::vector<Attribute> ())
    : outer_attrs (std::move (outer_attribs))
  {}

  // Clone function implementation as pure virtual method
  virtual Item *clone_item_impl () const = 0;

  /* Save having to specify two clone methods in derived classes by making
   * statement clone return item clone. Hopefully won't affect performance too
   * much. */
  Item *clone_stmt_impl () const override { return clone_item_impl (); }
};

// forward decl of ExprWithoutBlock
class ExprWithoutBlock;

// Base expression AST node - abstract
class Expr
{
  // TODO: move outer attribute data to derived classes?
  std::vector<Attribute> outer_attrs;

public:
  const std::vector<Attribute> &get_outer_attrs () const { return outer_attrs; }

  // Unique pointer custom clone function
  std::unique_ptr<Expr> clone_expr () const
  {
    return std::unique_ptr<Expr> (clone_expr_impl ());
  }

  /* TODO: public methods that could be useful:
   *  - get_type() - returns type of expression. set_type() may also be useful
   * for some?
   *  - evaluate() - evaluates expression if constant? can_evaluate()? */

  /* HACK: downcasting without dynamic_cast (if possible) via polymorphism -
   * overrided in subclasses of ExprWithoutBlock */
  virtual ExprWithoutBlock *as_expr_without_block () const { return nullptr; }

  // TODO: make pure virtual if move out outer attributes to derived classes
  virtual std::string as_string () const;

  virtual ~Expr () {}

  /* HACK: slow way of getting location from base expression through virtual
   * methods. */
  virtual Location get_locus_slow () const { return Location (); }

  // HACK: strictly not needed, but faster than full downcast clone
  virtual bool is_expr_without_block () const = 0;

  virtual void accept_vis (ASTVisitor &vis) = 0;

protected:
  // Constructor
  Expr (std::vector<Attribute> outer_attribs = std::vector<Attribute> ())
    : outer_attrs (std::move (outer_attribs))
  {}

  // Clone function implementation as pure virtual method
  virtual Expr *clone_expr_impl () const = 0;

  // TODO: think of less hacky way to implement this kind of thing
  // Sets outer attributes.
  void set_outer_attrs (std::vector<Attribute> outer_attrs_to_set)
  {
    outer_attrs = std::move (outer_attrs_to_set);
  }
};

// AST node for an expression without an accompanying block - abstract
class ExprWithoutBlock : public Expr
{
protected:
  // Constructor
  ExprWithoutBlock (std::vector<Attribute> outer_attribs
		    = std::vector<Attribute> ())
    : Expr (std::move (outer_attribs))
  {}

  // pure virtual clone implementation
  virtual ExprWithoutBlock *clone_expr_without_block_impl () const = 0;

  /* Save having to specify two clone methods in derived classes by making expr
   * clone return exprwithoutblock clone. Hopefully won't affect performance too
   * much. */
  ExprWithoutBlock *clone_expr_impl () const override
  {
    return clone_expr_without_block_impl ();
  }

  bool is_expr_without_block () const final override { return true; };

public:
  // Unique pointer custom clone function
  std::unique_ptr<ExprWithoutBlock> clone_expr_without_block () const
  {
    return std::unique_ptr<ExprWithoutBlock> (clone_expr_without_block_impl ());
  }

  /* downcasting hack from expr to use pratt parsing with
   * parse_expr_without_block */
  ExprWithoutBlock *as_expr_without_block () const override
  {
    return clone_expr_without_block_impl ();
  }
};

/* HACK: IdentifierExpr, delete when figure out identifier vs expr problem in
 * Pratt parser */
/* Alternatively, identifiers could just be represented as single-segment paths
 */
class IdentifierExpr : public ExprWithoutBlock
{
public:
  Identifier ident;

  Location locus;

  IdentifierExpr (Identifier ident, Location locus = Location (),
		  std::vector<Attribute> outer_attrs
		  = std::vector<Attribute> ())
    : ExprWithoutBlock (std::move (outer_attrs)), ident (std::move (ident)),
      locus (locus)
  {}

  std::string as_string () const override { return ident; }

  Location get_locus () const { return locus; }
  Location get_locus_slow () const override { return get_locus (); }

  void accept_vis (ASTVisitor &vis) override;

  // Clones this object.
  std::unique_ptr<IdentifierExpr> clone_identifier_expr () const
  {
    return std::unique_ptr<IdentifierExpr> (clone_identifier_expr_impl ());
  }

protected:
  // Clone method implementation
  IdentifierExpr *clone_expr_without_block_impl () const override
  {
    return clone_identifier_expr_impl ();
  }

  IdentifierExpr *clone_identifier_expr_impl () const
  {
    return new IdentifierExpr (*this);
  }

  IdentifierExpr (IdentifierExpr const &other) = default;
  IdentifierExpr &operator= (IdentifierExpr const &other) = default;
};

// Pattern base AST node
class Pattern
{
public:
  // Unique pointer custom clone function
  std::unique_ptr<Pattern> clone_pattern () const
  {
    return std::unique_ptr<Pattern> (clone_pattern_impl ());
  }

  // possible virtual methods: is_refutable()

  virtual ~Pattern () {}

  virtual std::string as_string () const = 0;

  virtual void accept_vis (ASTVisitor &vis) = 0;

protected:
  // Clone pattern implementation as pure virtual method
  virtual Pattern *clone_pattern_impl () const = 0;
};

// forward decl for Type
class TraitBound;

// Base class for types as represented in AST - abstract
class Type
{
public:
  // Unique pointer custom clone function
  std::unique_ptr<Type> clone_type () const
  {
    return std::unique_ptr<Type> (clone_type_impl ());
  }

  // virtual destructor
  virtual ~Type () {}

  virtual std::string as_string () const = 0;

  /* HACK: convert to trait bound. Virtual method overriden by classes that
   * enable this. */
  virtual TraitBound *to_trait_bound (bool in_parens ATTRIBUTE_UNUSED) const
  {
    return nullptr;
  }
  /* as pointer, shouldn't require definition beforehand, only forward
   * declaration. */

  virtual void accept_vis (ASTVisitor &vis) = 0;

protected:
  // Clone function implementation as pure virtual method
  virtual Type *clone_type_impl () const = 0;
};

// A type without parentheses? - abstract
class TypeNoBounds : public Type
{
public:
  // Unique pointer custom clone function
  std::unique_ptr<TypeNoBounds> clone_type_no_bounds () const
  {
    return std::unique_ptr<TypeNoBounds> (clone_type_no_bounds_impl ());
  }

protected:
  // Clone function implementation as pure virtual method
  virtual TypeNoBounds *clone_type_no_bounds_impl () const = 0;

  /* Save having to specify two clone methods in derived classes by making type
   * clone return typenobounds clone. Hopefully won't affect performance too
   * much. */
  TypeNoBounds *clone_type_impl () const override
  {
    return clone_type_no_bounds_impl ();
  }
};

/* Abstract base class representing a type param bound - Lifetime and TraitBound
 * extends it */
class TypeParamBound
{
public:
  virtual ~TypeParamBound () {}

  // Unique pointer custom clone function
  std::unique_ptr<TypeParamBound> clone_type_param_bound () const
  {
    return std::unique_ptr<TypeParamBound> (clone_type_param_bound_impl ());
  }

  virtual std::string as_string () const = 0;

  virtual void accept_vis (ASTVisitor &vis) = 0;

protected:
  // Clone function implementation as pure virtual method
  virtual TypeParamBound *clone_type_param_bound_impl () const = 0;
};

// Represents a lifetime (and is also a kind of type param bound)
class Lifetime : public TypeParamBound
{
public:
  enum LifetimeType
  {
    NAMED,   // corresponds to LIFETIME_OR_LABEL
    STATIC,  // corresponds to 'static
    WILDCARD // corresponds to '_
  };

private:
  LifetimeType lifetime_type;

  // TODO: LIFETIME_OR_LABEL (aka lifetime token) is only field
  // find way of enclosing token or something
  std::string lifetime_name;
  // only applies for NAMED lifetime_type

  Location locus;

public:
  // Constructor
  Lifetime (LifetimeType type, std::string name = std::string (),
	    Location locus = Location ())
    : lifetime_type (type), lifetime_name (std::move (name)), locus (locus)
  {}

  // Creates an "error" lifetime.
  static Lifetime error () { return Lifetime (NAMED, std::string ("")); }

  // Returns true if the lifetime is in an error state.
  bool is_error () const
  {
    return lifetime_type == NAMED && lifetime_name.empty ();
  }

  std::string as_string () const override;

  void accept_vis (ASTVisitor &vis) override;

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  Lifetime *clone_type_param_bound_impl () const override
  {
    return new Lifetime (*this);
  }
};

/* Base generic parameter in AST. Abstract - can be represented by a Lifetime or
 * Type param */
class GenericParam
{
public:
  virtual ~GenericParam () {}

  // Unique pointer custom clone function
  std::unique_ptr<GenericParam> clone_generic_param () const
  {
    return std::unique_ptr<GenericParam> (clone_generic_param_impl ());
  }

  virtual std::string as_string () const = 0;

  virtual void accept_vis (ASTVisitor &vis) = 0;

protected:
  // Clone function implementation as pure virtual method
  virtual GenericParam *clone_generic_param_impl () const = 0;
};

// A lifetime generic parameter (as opposed to a type generic parameter)
class LifetimeParam : public GenericParam
{
  Lifetime lifetime;

  // bool has_lifetime_bounds;
  // LifetimeBounds lifetime_bounds;
  std::vector<Lifetime> lifetime_bounds; // inlined LifetimeBounds

  // bool has_outer_attribute;
  // std::unique_ptr<Attribute> outer_attr;
  Attribute outer_attr;

  Location locus;

public:
  // Returns whether the lifetime param has any lifetime bounds.
  bool has_lifetime_bounds () const { return !lifetime_bounds.empty (); }

  // Returns whether the lifetime param has an outer attribute.
  bool has_outer_attribute () const { return !outer_attr.is_empty (); }

  // Creates an error state lifetime param.
  static LifetimeParam create_error ()
  {
    return LifetimeParam (Lifetime::error ());
  }

  // Returns whether the lifetime param is in an error state.
  bool is_error () const { return lifetime.is_error (); }

  // Constructor
  LifetimeParam (Lifetime lifetime, Location locus = Location (),
		 std::vector<Lifetime> lifetime_bounds
		 = std::vector<Lifetime> (),
		 Attribute outer_attr = Attribute::create_empty ())
    : lifetime (std::move (lifetime)),
      lifetime_bounds (std::move (lifetime_bounds)),
      outer_attr (std::move (outer_attr)), locus (locus)
  {}

  // TODO: remove copy and assignment operator definitions - not required

  // Copy constructor with clone
  LifetimeParam (LifetimeParam const &other)
    : lifetime (other.lifetime), lifetime_bounds (other.lifetime_bounds),
      outer_attr (other.outer_attr), locus (other.locus)
  {}

  // Overloaded assignment operator to clone attribute
  LifetimeParam &operator= (LifetimeParam const &other)
  {
    lifetime = other.lifetime;
    lifetime_bounds = other.lifetime_bounds;
    outer_attr = other.outer_attr;
    locus = other.locus;

    return *this;
  }

  // move constructors
  LifetimeParam (LifetimeParam &&other) = default;
  LifetimeParam &operator= (LifetimeParam &&other) = default;

  std::string as_string () const override;

  void accept_vis (ASTVisitor &vis) override;

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  LifetimeParam *clone_generic_param_impl () const override
  {
    return new LifetimeParam (*this);
  }
};

// A macro item AST node - potentially abstract base class
class MacroItem : public Item
{
  /*public:
  std::string as_string() const;*/
protected:
  MacroItem (std::vector<Attribute> outer_attribs)
    : Item (std::move (outer_attribs))
  {}
};

// Item used in trait declarations - abstract base class
class TraitItem
{
  // bool has_outer_attrs;
  // TODO: remove and rely on virtual functions and VisItem-derived attributes?
  // std::vector<Attribute> outer_attrs;

  // NOTE: all children should have outer attributes

protected:
  // Constructor
  /*TraitItem(std::vector<Attribute> outer_attrs = std::vector<Attribute>())
    : outer_attrs(std::move(outer_attrs)) {}*/

  // Clone function implementation as pure virtual method
  virtual TraitItem *clone_trait_item_impl () const = 0;

public:
  virtual ~TraitItem () {}

  // Returns whether TraitItem has outer attributes.
  /*bool has_outer_attrs() const {
      return !outer_attrs.empty();
  }*/

  // Unique pointer custom clone function
  std::unique_ptr<TraitItem> clone_trait_item () const
  {
    return std::unique_ptr<TraitItem> (clone_trait_item_impl ());
  }

  virtual std::string as_string () const = 0;

  virtual void accept_vis (ASTVisitor &vis) = 0;
};

/* Abstract base class for items used within an inherent impl block (the impl
 * name {} one) */
class InherentImplItem
{
protected:
  // Clone function implementation as pure virtual method
  virtual InherentImplItem *clone_inherent_impl_item_impl () const = 0;

public:
  virtual ~InherentImplItem () {}

  // Unique pointer custom clone function
  std::unique_ptr<InherentImplItem> clone_inherent_impl_item () const
  {
    return std::unique_ptr<InherentImplItem> (clone_inherent_impl_item_impl ());
  }

  virtual std::string as_string () const = 0;

  virtual void accept_vis (ASTVisitor &vis) = 0;
};

// Abstract base class for items used in a trait impl
class TraitImplItem
{
protected:
  virtual TraitImplItem *clone_trait_impl_item_impl () const = 0;

public:
  virtual ~TraitImplItem (){};

  // Unique pointer custom clone function
  std::unique_ptr<TraitImplItem> clone_trait_impl_item () const
  {
    return std::unique_ptr<TraitImplItem> (clone_trait_impl_item_impl ());
  }

  virtual std::string as_string () const = 0;

  virtual void accept_vis (ASTVisitor &vis) = 0;
};

/* A macro invocation item (or statement) AST node (i.e. semi-coloned macro
 * invocation) */
class MacroInvocationSemi : public MacroItem,
			    public TraitItem,
			    public InherentImplItem,
			    public TraitImplItem
{
  SimplePath path;
  // all delim types except curly must have invocation end with a semicolon
  DelimType delim_type;
  std::vector<std::unique_ptr<TokenTree>> token_trees;
  Location locus;

public:
  std::string as_string () const override;

  MacroInvocationSemi (SimplePath macro_path, DelimType delim_type,
		       std::vector<std::unique_ptr<TokenTree>> token_trees,
		       std::vector<Attribute> outer_attribs, Location locus)
    : MacroItem (std::move (outer_attribs)), path (std::move (macro_path)),
      delim_type (delim_type), token_trees (std::move (token_trees)),
      locus (locus)
  {}
  /* TODO: possible issue with Item and TraitItem hierarchies both having outer
   * attributes
   * - storage inefficiency at least.
   * Best current idea is to make Item preferred and have TraitItem get virtual
   * functions for attributes or something. Or just redo the "composition"
   * approach, but then this prevents polymorphism and would entail redoing
   * quite a bit of the parser. */

  // Move constructors
  MacroInvocationSemi (MacroInvocationSemi &&other) = default;
  MacroInvocationSemi &operator= (MacroInvocationSemi &&other) = default;

  void accept_vis (ASTVisitor &vis) override;

  // Clones this macro invocation semi.
  std::unique_ptr<MacroInvocationSemi> clone_macro_invocation_semi () const
  {
    return std::unique_ptr<MacroInvocationSemi> (
      clone_macro_invocation_semi_impl ());
  }

protected:
  // Copy constructor with vector clone
  MacroInvocationSemi (MacroInvocationSemi const &other)
    : MacroItem (other), TraitItem (other), InherentImplItem (other),
      TraitImplItem (other), path (other.path), delim_type (other.delim_type),
      locus (other.locus)
  {
    token_trees.reserve (other.token_trees.size ());
    for (const auto &e : other.token_trees)
      token_trees.push_back (e->clone_token_tree ());
  }

  // Overloaded assignment operator to vector clone
  MacroInvocationSemi &operator= (MacroInvocationSemi const &other)
  {
    MacroItem::operator= (other);
    TraitItem::operator= (other);
    InherentImplItem::operator= (other);
    TraitImplItem::operator= (other);
    path = other.path;
    delim_type = other.delim_type;
    locus = other.locus;

    token_trees.reserve (other.token_trees.size ());
    for (const auto &e : other.token_trees)
      token_trees.push_back (e->clone_token_tree ());

    return *this;
  }

  MacroInvocationSemi *clone_macro_invocation_semi_impl () const
  {
    return new MacroInvocationSemi (*this);
  }

  /* Use covariance to implement clone function as returning this object rather
   * than base */
  MacroInvocationSemi *clone_item_impl () const override
  {
    return clone_macro_invocation_semi_impl ();
  }

  /* Use covariance to implement clone function as returning this object rather
   * than base */
  MacroInvocationSemi *clone_inherent_impl_item_impl () const override
  {
    return clone_macro_invocation_semi_impl ();
  }

  /* Use covariance to implement clone function as returning this object rather
   * than base */
  MacroInvocationSemi *clone_trait_impl_item_impl () const override
  {
    return clone_macro_invocation_semi_impl ();
  }

  // FIXME: remove if item impl virtual override works properly
  // Use covariance to implement clone function as returning this object rather
  // than base
  /*MacroInvocationSemi* clone_statement_impl() const override {
      return clone_macro_invocation_semi_impl ();
  }*/

  /* Use covariance to implement clone function as returning this object rather
   * than base */
  MacroInvocationSemi *clone_trait_item_impl () const override
  {
    return clone_macro_invocation_semi_impl ();
  }
};

// A crate AST object - holds all the data for a single compilation unit
struct Crate
{
  bool has_utf8bom;
  bool has_shebang;

  std::vector<Attribute> inner_attrs;
  // dodgy spacing required here
  /* TODO: is it better to have a vector of items here or a module (implicit
   * top-level one)? */
  std::vector<std::unique_ptr<Item>> items;

public:
  // Constructor
  Crate (std::vector<std::unique_ptr<Item>> items,
	 std::vector<Attribute> inner_attrs, bool has_utf8bom = false,
	 bool has_shebang = false)
    : has_utf8bom (has_utf8bom), has_shebang (has_shebang),
      inner_attrs (std::move (inner_attrs)), items (std::move (items))
  {}

  // Copy constructor with vector clone
  Crate (Crate const &other)
    : has_utf8bom (other.has_utf8bom), has_shebang (other.has_shebang),
      inner_attrs (other.inner_attrs)
  {
    items.reserve (other.items.size ());
    for (const auto &e : other.items)
      items.push_back (e->clone_item ());
  }

  ~Crate () = default;

  // Overloaded assignment operator with vector clone
  Crate &operator= (Crate const &other)
  {
    inner_attrs = other.inner_attrs;
    has_shebang = other.has_shebang;
    has_utf8bom = other.has_utf8bom;

    items.reserve (other.items.size ());
    for (const auto &e : other.items)
      items.push_back (e->clone_item ());

    return *this;
  }

  // Move constructors
  Crate (Crate &&other) = default;
  Crate &operator= (Crate &&other) = default;

  // Get crate representation as string (e.g. for debugging).
  std::string as_string () const;
};

// Base path expression AST node - abstract
class PathExpr : public ExprWithoutBlock
{
protected:
  PathExpr (std::vector<Attribute> outer_attribs)
    : ExprWithoutBlock (std::move (outer_attribs))
  {}

public:
  // TODO: think of a better and less hacky way to allow this

  /* Replaces the outer attributes of this path expression with the given outer
   * attributes. */
  void replace_outer_attrs (std::vector<Attribute> outer_attrs)
  {
    set_outer_attrs (std::move (outer_attrs));
  }
};
} // namespace AST
} // namespace Rust

#endif
