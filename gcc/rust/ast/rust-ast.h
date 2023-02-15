// Copyright (C) 2020-2023 Free Software Foundation, Inc.

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

#ifndef RUST_AST_BASE_H
#define RUST_AST_BASE_H
// Base for AST used in gccrs, basically required by all specific ast things

#include "rust-system.h"
#include "rust-hir-map.h"
#include "rust-token.h"
#include "rust-location.h"
#include "rust-diagnostics.h"

namespace Rust {
// TODO: remove typedefs and make actual types for these
typedef std::string Identifier;
typedef int TupleIndex;
struct Session;
struct MacroExpander;

namespace AST {
// foward decl: ast visitor
class ASTVisitor;
using AttrVec = std::vector<Attribute>;

// The available kinds of AST Nodes
enum Kind
{
  UNKNOWN,
  MACRO_RULES_DEFINITION,
  MACRO_INVOCATION,
};

// Abstract base class for all AST elements
class Node
{
public:
  /**
   * Get the kind of Node this is. This is used to differentiate various AST
   * elements with very little overhead when extracting the derived type through
   * static casting is not necessary.
   */
  // FIXME: Mark this as `= 0` in the future to make sure every node implements
  // it
  virtual Kind get_ast_kind () const { return Kind::UNKNOWN; }
};

// Delimiter types - used in macros and whatever.
enum DelimType
{
  PARENS,
  SQUARE,
  CURLY
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
  virtual std::vector<std::unique_ptr<Token> > to_token_stream () const = 0;

protected:
  // pure virtual clone implementation
  virtual TokenTree *clone_token_tree_impl () const = 0;
};

// Abstract base class for a macro match
class MacroMatch
{
public:
  enum MacroMatchType
  {
    Fragment,
    Repetition,
    Matcher,
    Tok
  };

  virtual ~MacroMatch () {}

  virtual std::string as_string () const = 0;
  virtual Location get_match_locus () const = 0;

  // Unique pointer custom clone function
  std::unique_ptr<MacroMatch> clone_macro_match () const
  {
    return std::unique_ptr<MacroMatch> (clone_macro_match_impl ());
  }

  virtual void accept_vis (ASTVisitor &vis) = 0;

  virtual MacroMatchType get_macro_match_type () const = 0;

protected:
  // pure virtual clone implementation
  virtual MacroMatch *clone_macro_match_impl () const = 0;
};

// A token is a kind of token tree (except delimiter tokens)
class Token : public TokenTree, public MacroMatch
{
  // A token is a kind of token tree (except delimiter tokens)
  // A token is a kind of MacroMatch (except $ and delimiter tokens)
#if 0
  // TODO: improve member variables - current ones are the same as lexer token
  // Token kind.
  TokenId token_id;
  // Token location.
  Location locus;
  // Associated text (if any) of token.
  std::string str;
  // Token type hint (if any).
  PrimitiveCoreType type_hint;
#endif

  const_TokenPtr tok_ref;

  /* new idea: wrapper around const_TokenPtr used for heterogeneuous storage in
   * token trees. rather than convert back and forth when parsing macros, just
   * wrap it. */

public:
  // Unique pointer custom clone function
  std::unique_ptr<Token> clone_token () const
  {
    return std::unique_ptr<Token> (clone_token_impl ());
  }

#if 0
  /* constructor from general text - avoid using if lexer const_TokenPtr is
   * available */
  Token (TokenId token_id, Location locus, std::string str,
	 PrimitiveCoreType type_hint)
    : token_id (token_id), locus (locus), str (std::move (str)),
      type_hint (type_hint)
  {}
#endif
  // not doable with new implementation - will have to make a const_TokenPtr

  // Constructor from lexer const_TokenPtr
#if 0
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
	rust_debug ("ast token created with str '%s'", str.c_str ());
      }
    else
      {
	// FIXME: is this returning correct thing?
	str = lexer_token_ptr->get_token_description ();

	// DEBUG
	rust_debug ("ast token created with string '%s'", str.c_str ());
      }

    // DEBUG
    if (lexer_token_ptr->should_have_str () && !lexer_token_ptr->has_str ())
      {
	rust_debug (
		 "BAD: for token '%s', should have string but does not!",
		 lexer_token_ptr->get_token_description ());
      }
  }
#endif
  Token (const_TokenPtr lexer_tok_ptr) : tok_ref (std::move (lexer_tok_ptr)) {}

  bool is_string_lit () const
  {
    switch (get_id ())
      {
      case STRING_LITERAL:
      case BYTE_STRING_LITERAL:
	return true;
      default:
	return false;
      }
  }

  std::string as_string () const override;
  Location get_match_locus () const override { return tok_ref->get_locus (); };

  void accept_vis (ASTVisitor &vis) override;

  // Return copy of itself but in token stream form.
  std::vector<std::unique_ptr<Token> > to_token_stream () const override;

  TokenId get_id () const { return tok_ref->get_id (); }
  const std::string &get_str () const { return tok_ref->get_str (); }

  Location get_locus () const { return tok_ref->get_locus (); }

  PrimitiveCoreType get_type_hint () const { return tok_ref->get_type_hint (); }

  // Get a new token pointer copy.
  const_TokenPtr get_tok_ptr () const { return tok_ref; }

  MacroMatchType get_macro_match_type () const override
  {
    return MacroMatchType::Tok;
  }

protected:
  // No virtual for now as not polymorphic but can be in future
  /*virtual*/ Token *clone_token_impl () const { return new Token (*this); }

  /* Use covariance to implement clone function as returning this object rather
   * than base */
  Token *clone_token_tree_impl () const final override
  {
    return clone_token_impl ();
  }

  /* Use covariance to implement clone function as returning this object rather
   * than base */
  Token *clone_macro_match_impl () const final override
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
    BYTE,
    BYTE_STRING,
    INT,
    FLOAT,
    BOOL,
    ERROR
  };

private:
  /* TODO: maybe make subclasses of each type of literal with their typed values
   * (or generics) */
  std::string value_as_string;
  LitType type;
  PrimitiveCoreType type_hint;

public:
  std::string as_string () const { return value_as_string; }

  LitType get_lit_type () const { return type; }

  PrimitiveCoreType get_type_hint () const { return type_hint; }

  Literal (std::string value_as_string, LitType type,
	   PrimitiveCoreType type_hint)
    : value_as_string (std::move (value_as_string)), type (type),
      type_hint (type_hint)
  {}

  static Literal create_error ()
  {
    return Literal ("", ERROR, PrimitiveCoreType::CORETYPE_UNKNOWN);
  }

  // Returns whether literal is in an invalid state.
  bool is_error () const { return type == ERROR; }
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
  NodeId node_id;

  // only allow identifiers, "super", "self", "crate", or "$crate"
public:
  // TODO: put checks in constructor to enforce this rule?
  SimplePathSegment (std::string segment_name, Location locus)
    : segment_name (std::move (segment_name)), locus (locus),
      node_id (Analysis::Mappings::get ()->get_next_node_id ())
  {}

  /* Returns whether simple path segment is in an invalid state (currently, if
   * empty). */
  bool is_error () const { return segment_name.empty (); }

  // Creates an error SimplePathSegment
  static SimplePathSegment create_error ()
  {
    return SimplePathSegment (std::string (""), Location ());
  }

  std::string as_string () const override;

  Location get_locus () const { return locus; }
  NodeId get_node_id () const { return node_id; }
  const std::string &get_segment_name () const { return segment_name; }
  bool is_super_path_seg () const
  {
    return as_string ().compare ("super") == 0;
  }
  bool is_crate_path_seg () const
  {
    return as_string ().compare ("crate") == 0;
  }
  bool is_lower_self () const { return as_string ().compare ("self") == 0; }
  bool is_big_self () const { return as_string ().compare ("Self") == 0; }
};

// A simple path without generic or type arguments
class SimplePath
{
  bool has_opening_scope_resolution;
  std::vector<SimplePathSegment> segments;
  Location locus;
  NodeId node_id;

public:
  // Constructor
  SimplePath (std::vector<SimplePathSegment> path_segments,
	      bool has_opening_scope_resolution = false,
	      Location locus = Location ())
    : has_opening_scope_resolution (has_opening_scope_resolution),
      segments (std::move (path_segments)), locus (locus),
      node_id (Analysis::Mappings::get ()->get_next_node_id ())
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
  NodeId get_node_id () const { return node_id; }

  // does this need visitor if not polymorphic? probably not

  // path-to-string comparison operator
  bool operator== (const std::string &rhs) const
  {
    return !has_opening_scope_resolution && segments.size () == 1
	   && segments[0].as_string () == rhs;
  }

  /* Creates a single-segment SimplePath from a string. This will not check to
   * ensure that this is a valid identifier in path, so be careful. Also, this
   * will have no location data.
   * TODO have checks? */
  static SimplePath from_str (std::string str, Location locus)
  {
    std::vector<AST::SimplePathSegment> single_segments
      = {AST::SimplePathSegment (std::move (str), locus)};
    return SimplePath (std::move (single_segments));
  }

  const std::vector<SimplePathSegment> &get_segments () const
  {
    return segments;
  }

  std::vector<SimplePathSegment> &get_segments () { return segments; }
};

// path-to-string inverse comparison operator
inline bool
operator!= (const SimplePath &lhs, const std::string &rhs)
{
  return !(lhs == rhs);
}

// forward decl for Attribute
class AttrInput;

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

  // no point in being defined inline as requires virtual call anyway
  Attribute (const Attribute &other);

  // no point in being defined inline as requires virtual call anyway
  Attribute &operator= (const Attribute &other);

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

  Location get_locus () const { return locus; }

  AttrInput &get_attr_input () const { return *attr_input; }

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

  // no visitor pattern as not currently polymorphic

  const SimplePath &get_path () const { return path; }
  SimplePath &get_path () { return path; }

  // Call to parse attribute body to meta item syntax.
  void parse_attr_to_meta_item ();

  /* Determines whether cfg predicate is true and item with attribute should not
   * be stripped. Attribute body must already be parsed to meta item. */
  bool check_cfg_predicate (const Session &session) const;

  // Returns whether body has been parsed to meta item form or not.
  bool is_parsed_to_meta_item () const;

  /* Returns any attributes generated from cfg_attr attributes. Attribute body
   * must already be parsed to meta item. */
  std::vector<Attribute> separate_cfg_attrs () const;

protected:
  // not virtual as currently no subclasses of Attribute, but could be in future
  /*virtual*/ Attribute *clone_attribute_impl () const
  {
    return new Attribute (*this);
  }
};

// Attribute body - abstract base class
class AttrInput
{
public:
  enum AttrInputType
  {
    LITERAL,
    META_ITEM,
    TOKEN_TREE,
  };

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

  virtual std::vector<Attribute> separate_cfg_attrs () const { return {}; }

  // Returns whether attr input has been parsed to meta item syntax.
  virtual bool is_meta_item () const = 0;

  virtual AttrInputType get_attr_input_type () const = 0;

protected:
  // pure virtual clone implementation
  virtual AttrInput *clone_attr_input_impl () const = 0;
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

  virtual ~MetaItemInner ();

  virtual Location get_locus () const = 0;

  virtual std::string as_string () const = 0;

  virtual void accept_vis (ASTVisitor &vis) = 0;

  /* HACK: used to simplify parsing - creates a copy of that type, or returns
   * null */
  virtual std::unique_ptr<MetaNameValueStr> to_meta_name_value_str () const;

  // HACK: used to simplify parsing - same thing
  virtual SimplePath to_path_item () const
  {
    return SimplePath::create_empty ();
  }

  virtual Attribute to_attribute () const { return Attribute::create_empty (); }

  virtual bool check_cfg_predicate (const Session &session) const = 0;

  virtual bool is_key_value_pair () const { return false; }
};

// Container used to store MetaItems as AttrInput (bridge-ish kinda thing)
class AttrInputMetaItemContainer : public AttrInput
{
  std::vector<std::unique_ptr<MetaItemInner> > items;

public:
  AttrInputMetaItemContainer (
    std::vector<std::unique_ptr<MetaItemInner> > items)
    : items (std::move (items))
  {}

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

  // default move constructors
  AttrInputMetaItemContainer (AttrInputMetaItemContainer &&other) = default;
  AttrInputMetaItemContainer &operator= (AttrInputMetaItemContainer &&other)
    = default;

  std::string as_string () const override;

  void accept_vis (ASTVisitor &vis) override;

  bool check_cfg_predicate (const Session &session) const override;

  AttrInputType get_attr_input_type () const final override
  {
    return AttrInput::AttrInputType::META_ITEM;
  }

  // Clones this object.
  std::unique_ptr<AttrInputMetaItemContainer>
  clone_attr_input_meta_item_container () const
  {
    return std::unique_ptr<AttrInputMetaItemContainer> (
      clone_attr_input_meta_item_container_impl ());
  }

  std::vector<Attribute> separate_cfg_attrs () const override;

  bool is_meta_item () const override { return true; }

  // TODO: this mutable getter seems dodgy
  std::vector<std::unique_ptr<MetaItemInner> > &get_items () { return items; }
  const std::vector<std::unique_ptr<MetaItemInner> > &get_items () const
  {
    return items;
  }

protected:
  // Use covariance to implement clone function as returning this type
  AttrInputMetaItemContainer *clone_attr_input_impl () const final override
  {
    return clone_attr_input_meta_item_container_impl ();
  }

  AttrInputMetaItemContainer *clone_attr_input_meta_item_container_impl () const
  {
    return new AttrInputMetaItemContainer (*this);
  }
};

// A token tree with delimiters
class DelimTokenTree : public TokenTree, public AttrInput
{
  DelimType delim_type;
  std::vector<std::unique_ptr<TokenTree> > token_trees;
  Location locus;

protected:
  DelimTokenTree *clone_delim_tok_tree_impl () const
  {
    return new DelimTokenTree (*this);
  }

  /* Use covariance to implement clone function as returning a DelimTokenTree
   * object */
  DelimTokenTree *clone_attr_input_impl () const final override
  {
    return clone_delim_tok_tree_impl ();
  }

  /* Use covariance to implement clone function as returning a DelimTokenTree
   * object */
  DelimTokenTree *clone_token_tree_impl () const final override
  {
    return clone_delim_tok_tree_impl ();
  }

public:
  DelimTokenTree (DelimType delim_type,
		  std::vector<std::unique_ptr<TokenTree> > token_trees
		  = std::vector<std::unique_ptr<TokenTree> > (),
		  Location locus = Location ())
    : delim_type (delim_type), token_trees (std::move (token_trees)),
      locus (locus)
  {}

  // Copy constructor with vector clone
  DelimTokenTree (DelimTokenTree const &other)
    : delim_type (other.delim_type), locus (other.locus)
  {
    token_trees.clear ();
    token_trees.reserve (other.token_trees.size ());
    for (const auto &e : other.token_trees)
      token_trees.push_back (e->clone_token_tree ());
  }

  // overloaded assignment operator with vector clone
  DelimTokenTree &operator= (DelimTokenTree const &other)
  {
    delim_type = other.delim_type;
    locus = other.locus;

    token_trees.clear ();
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

  bool check_cfg_predicate (const Session &) const override
  {
    // this should never be called - should be converted first
    rust_assert (false);
    return false;
  }

  AttrInputMetaItemContainer *parse_to_meta_item () const override;

  std::vector<std::unique_ptr<Token> > to_token_stream () const override;

  std::unique_ptr<DelimTokenTree> clone_delim_token_tree () const
  {
    return std::unique_ptr<DelimTokenTree> (clone_delim_tok_tree_impl ());
  }

  bool is_meta_item () const override { return false; }

  AttrInputType get_attr_input_type () const final override
  {
    return AttrInput::AttrInputType::TOKEN_TREE;
  }

  std::vector<std::unique_ptr<TokenTree> > &get_token_trees ()
  {
    return token_trees;
  }

  DelimType get_delim_type () const { return delim_type; }
};

/* Forward decl - definition moved to rust-expr.h as it requires LiteralExpr to
 * be defined */
class AttrInputLiteral;

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
class MetaListNameValueStr;

/* Base statement abstract class. Note that most "statements" are not allowed in
 * top-level module scope - only a subclass of statements called "items" are. */
class Stmt : public Node
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

  virtual Location get_locus () const = 0;

  virtual void mark_for_strip () = 0;
  virtual bool is_marked_for_strip () const = 0;
  NodeId get_node_id () const { return node_id; }

  virtual bool is_item () const = 0;

protected:
  Stmt () : node_id (Analysis::Mappings::get ()->get_next_node_id ()) {}

  // Clone function implementation as pure virtual method
  virtual Stmt *clone_stmt_impl () const = 0;

  NodeId node_id;
};

// Rust "item" AST node (declaration of top-level/module-level allowed stuff)
class Item : public Stmt
{
public:
  // Unique pointer custom clone function
  std::unique_ptr<Item> clone_item () const
  {
    return std::unique_ptr<Item> (clone_item_impl ());
  }

  /* Adds crate names to the vector passed by reference, if it can
   * (polymorphism). TODO: remove, unused. */
  virtual void
  add_crate_name (std::vector<std::string> &names ATTRIBUTE_UNUSED) const
  {}

  // FIXME: ARTHUR: Is it okay to have removed that final? Is it *required*
  // behavior that we have items that can also be expressions?
  bool is_item () const override { return true; }

protected:
  // Clone function implementation as pure virtual method
  virtual Item *clone_item_impl () const = 0;

  /* Save having to specify two clone methods in derived classes by making
   * statement clone return item clone. Hopefully won't affect performance too
   * much. */
  Item *clone_stmt_impl () const final override { return clone_item_impl (); }
};

// forward decl of ExprWithoutBlock
class ExprWithoutBlock;

// Base expression AST node - abstract
class Expr : public Node
{
public:
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

  virtual std::string as_string () const = 0;

  virtual ~Expr () {}

  virtual Location get_locus () const = 0;

  virtual bool is_literal () const { return false; }

  // HACK: strictly not needed, but faster than full downcast clone
  virtual bool is_expr_without_block () const = 0;

  virtual void accept_vis (ASTVisitor &vis) = 0;

  virtual void mark_for_strip () = 0;
  virtual bool is_marked_for_strip () const = 0;

  virtual NodeId get_node_id () const { return node_id; }

  virtual void set_node_id (NodeId id) { node_id = id; }

protected:
  // Constructor
  Expr () : node_id (Analysis::Mappings::get ()->get_next_node_id ()) {}

  // Clone function implementation as pure virtual method
  virtual Expr *clone_expr_impl () const = 0;

  // TODO: think of less hacky way to implement this kind of thing
  // Sets outer attributes.
  virtual void set_outer_attrs (std::vector<Attribute>) = 0;

  NodeId node_id;
};

// AST node for an expression without an accompanying block - abstract
class ExprWithoutBlock : public Expr
{
protected:
  // pure virtual clone implementation
  virtual ExprWithoutBlock *clone_expr_without_block_impl () const = 0;

  /* Save having to specify two clone methods in derived classes by making expr
   * clone return exprwithoutblock clone. Hopefully won't affect performance too
   * much. */
  ExprWithoutBlock *clone_expr_impl () const final override
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
  ExprWithoutBlock *as_expr_without_block () const final override
  {
    return clone_expr_without_block_impl ();
  }

  virtual ExprWithoutBlock *to_stmt () const { return clone_expr_impl (); }
};

/* HACK: IdentifierExpr, delete when figure out identifier vs expr problem in
 * Pratt parser */
/* Alternatively, identifiers could just be represented as single-segment paths
 */
class IdentifierExpr : public ExprWithoutBlock
{
  std::vector<Attribute> outer_attrs;
  Identifier ident;
  Location locus;

public:
  IdentifierExpr (Identifier ident, std::vector<Attribute> outer_attrs,
		  Location locus)
    : outer_attrs (std::move (outer_attrs)), ident (std::move (ident)),
      locus (locus)
  {}

  std::string as_string () const override { return ident; }

  Location get_locus () const override final { return locus; }

  Identifier get_ident () const { return ident; }

  void accept_vis (ASTVisitor &vis) override;

  // Clones this object.
  std::unique_ptr<IdentifierExpr> clone_identifier_expr () const
  {
    return std::unique_ptr<IdentifierExpr> (clone_identifier_expr_impl ());
  }

  // "Error state" if ident is empty, so base stripping on this.
  void mark_for_strip () override { ident = {}; }
  bool is_marked_for_strip () const override { return ident.empty (); }

  const std::vector<Attribute> &get_outer_attrs () const { return outer_attrs; }
  std::vector<Attribute> &get_outer_attrs () { return outer_attrs; }

  void set_outer_attrs (std::vector<Attribute> new_attrs) override
  {
    outer_attrs = std::move (new_attrs);
  }

protected:
  // Clone method implementation
  IdentifierExpr *clone_expr_without_block_impl () const final override
  {
    return clone_identifier_expr_impl ();
  }

  IdentifierExpr *clone_identifier_expr_impl () const
  {
    return new IdentifierExpr (*this);
  }
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

  // as only one kind of pattern can be stripped, have default of nothing
  virtual void mark_for_strip () {}
  virtual bool is_marked_for_strip () const { return false; }

  virtual Location get_locus () const = 0;
  virtual NodeId get_pattern_node_id () const = 0;

protected:
  // Clone pattern implementation as pure virtual method
  virtual Pattern *clone_pattern_impl () const = 0;
};

// forward decl for Type
class TraitBound;

// Base class for types as represented in AST - abstract
class Type : public Node
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
  virtual TraitBound *to_trait_bound (bool) const { return nullptr; }
  /* as pointer, shouldn't require definition beforehand, only forward
   * declaration. */

  virtual void accept_vis (ASTVisitor &vis) = 0;

  // as only two kinds of types can be stripped, have default of nothing
  virtual void mark_for_strip () {}
  virtual bool is_marked_for_strip () const { return false; }

  virtual Location get_locus () const = 0;

  NodeId get_node_id () const { return node_id; }

protected:
  Type () : node_id (Analysis::Mappings::get ()->get_next_node_id ()) {}

  // Clone function implementation as pure virtual method
  virtual Type *clone_type_impl () const = 0;

  NodeId node_id;
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
  TypeNoBounds *clone_type_impl () const final override
  {
    return clone_type_no_bounds_impl ();
  }

  TypeNoBounds () : Type () {}
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

  NodeId get_node_id () const { return node_id; }

  virtual Location get_locus () const = 0;

protected:
  // Clone function implementation as pure virtual method
  virtual TypeParamBound *clone_type_param_bound_impl () const = 0;

  TypeParamBound (NodeId node_id) : node_id (node_id) {}

  NodeId node_id;
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
  std::string lifetime_name;
  Location locus;
  NodeId node_id;

public:
  // Constructor
  Lifetime (LifetimeType type, std::string name = std::string (),
	    Location locus = Location ())
    : TypeParamBound (Analysis::Mappings::get ()->get_next_node_id ()),
      lifetime_type (type), lifetime_name (std::move (name)), locus (locus)
  {}

  Lifetime (NodeId id, LifetimeType type, std::string name = std::string (),
	    Location locus = Location ())
    : TypeParamBound (id), lifetime_type (type),
      lifetime_name (std::move (name)), locus (locus)
  {}

  // Creates an "error" lifetime.
  static Lifetime error () { return Lifetime (NAMED, ""); }

  // Returns true if the lifetime is in an error state.
  bool is_error () const
  {
    return lifetime_type == NAMED && lifetime_name.empty ();
  }

  std::string as_string () const override;

  void accept_vis (ASTVisitor &vis) override;

  LifetimeType get_lifetime_type () { return lifetime_type; }

  Location get_locus () const override final { return locus; }

  std::string get_lifetime_name () const { return lifetime_name; }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  Lifetime *clone_type_param_bound_impl () const override
  {
    return new Lifetime (node_id, lifetime_type, lifetime_name, locus);
  }
};

/* Base generic parameter in AST. Abstract - can be represented by a Lifetime or
 * Type param */
class GenericParam
{
public:
  enum class Kind
  {
    Lifetime,
    Type,
    Const,
  };

  virtual ~GenericParam () {}

  // Unique pointer custom clone function
  std::unique_ptr<GenericParam> clone_generic_param () const
  {
    return std::unique_ptr<GenericParam> (clone_generic_param_impl ());
  }

  virtual std::string as_string () const = 0;

  virtual void accept_vis (ASTVisitor &vis) = 0;

  virtual Location get_locus () const = 0;

  virtual Kind get_kind () const = 0;

  NodeId get_node_id () { return node_id; }

protected:
  GenericParam () : node_id (Analysis::Mappings::get ()->get_next_node_id ()) {}
  GenericParam (NodeId node_id) : node_id (node_id) {}

  // Clone function implementation as pure virtual method
  virtual GenericParam *clone_generic_param_impl () const = 0;

  NodeId node_id;
};

// A lifetime generic parameter (as opposed to a type generic parameter)
class LifetimeParam : public GenericParam
{
  Lifetime lifetime;
  std::vector<Lifetime> lifetime_bounds;
  Attribute outer_attr;
  Location locus;

public:
  Lifetime get_lifetime () const { return lifetime; }

  // Returns whether the lifetime param has any lifetime bounds.
  bool has_lifetime_bounds () const { return !lifetime_bounds.empty (); }

  std::vector<Lifetime> &get_lifetime_bounds () { return lifetime_bounds; }

  // Returns whether the lifetime param has an outer attribute.
  bool has_outer_attribute () const { return !outer_attr.is_empty (); }

  // Creates an error state lifetime param.
  static LifetimeParam create_error ()
  {
    return LifetimeParam (Lifetime::error (), {}, Attribute::create_empty (),
			  Location ());
  }

  // Returns whether the lifetime param is in an error state.
  bool is_error () const { return lifetime.is_error (); }

  // Constructor
  LifetimeParam (Lifetime lifetime, std::vector<Lifetime> lifetime_bounds,
		 Attribute outer_attr, Location locus)
    : lifetime (std::move (lifetime)),
      lifetime_bounds (std::move (lifetime_bounds)),
      outer_attr (std::move (outer_attr)), locus (locus)
  {}

  std::string as_string () const override;

  void accept_vis (ASTVisitor &vis) override;

  Location get_locus () const override final { return locus; }

  Kind get_kind () const override final { return Kind::Lifetime; }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  LifetimeParam *clone_generic_param_impl () const override
  {
    return new LifetimeParam (*this);
  }
};

// Item used in trait declarations - abstract base class
class TraitItem
{
protected:
  TraitItem (Location locus)
    : node_id (Analysis::Mappings::get ()->get_next_node_id ()), locus (locus)
  {}

  // Clone function implementation as pure virtual method
  virtual TraitItem *clone_trait_item_impl () const = 0;

  NodeId node_id;
  Location locus;

public:
  virtual ~TraitItem () {}

  // Unique pointer custom clone function
  std::unique_ptr<TraitItem> clone_trait_item () const
  {
    return std::unique_ptr<TraitItem> (clone_trait_item_impl ());
  }

  virtual std::string as_string () const = 0;

  virtual void accept_vis (ASTVisitor &vis) = 0;

  virtual void mark_for_strip () = 0;
  virtual bool is_marked_for_strip () const = 0;

  NodeId get_node_id () const { return node_id; }
  Location get_locus () const { return locus; }
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

  virtual void mark_for_strip () = 0;
  virtual bool is_marked_for_strip () const = 0;

  virtual Location get_locus () const = 0;
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

  virtual void mark_for_strip () = 0;
  virtual bool is_marked_for_strip () const = 0;
};

// Abstract base class for an item used inside an extern block
class ExternalItem
{
public:
  ExternalItem () : node_id (Analysis::Mappings::get ()->get_next_node_id ()) {}

  virtual ~ExternalItem () {}

  // Unique pointer custom clone function
  std::unique_ptr<ExternalItem> clone_external_item () const
  {
    return std::unique_ptr<ExternalItem> (clone_external_item_impl ());
  }

  virtual std::string as_string () const = 0;

  virtual void accept_vis (ASTVisitor &vis) = 0;

  virtual void mark_for_strip () = 0;
  virtual bool is_marked_for_strip () const = 0;

  NodeId get_node_id () const { return node_id; }

protected:
  // Clone function implementation as pure virtual method
  virtual ExternalItem *clone_external_item_impl () const = 0;

  NodeId node_id;
};

/* Data structure to store the data used in macro invocations and macro
 * invocations with semicolons. */
struct MacroInvocData
{
private:
  SimplePath path;
  DelimTokenTree token_tree;

  // One way of parsing the macro. Probably not applicable for all macros.
  std::vector<std::unique_ptr<MetaItemInner> > parsed_items;
  bool parsed_to_meta_item = false;
  MacroExpander *expander = nullptr;

public:
  std::string as_string () const;

  MacroInvocData (SimplePath path, DelimTokenTree token_tree)
    : path (std::move (path)), token_tree (std::move (token_tree))
  {}

  // Copy constructor with vector clone
  MacroInvocData (const MacroInvocData &other)
    : path (other.path), token_tree (other.token_tree),
      parsed_to_meta_item (other.parsed_to_meta_item)
  {
    parsed_items.reserve (other.parsed_items.size ());
    for (const auto &e : other.parsed_items)
      parsed_items.push_back (e->clone_meta_item_inner ());
  }

  // Copy assignment operator with vector clone
  MacroInvocData &operator= (const MacroInvocData &other)
  {
    path = other.path;
    token_tree = other.token_tree;
    parsed_to_meta_item = other.parsed_to_meta_item;
    expander = other.expander;

    parsed_items.reserve (other.parsed_items.size ());
    for (const auto &e : other.parsed_items)
      parsed_items.push_back (e->clone_meta_item_inner ());

    return *this;
  }

  // Move constructors
  MacroInvocData (MacroInvocData &&other) = default;
  MacroInvocData &operator= (MacroInvocData &&other) = default;

  // Invalid if path is empty, so base stripping on that.
  void mark_for_strip () { path = SimplePath::create_empty (); }
  bool is_marked_for_strip () const { return path.is_empty (); }

  // Returns whether the macro has been parsed already.
  bool is_parsed () const { return parsed_to_meta_item; }
  // TODO: update on other ways of parsing it

  // TODO: this mutable getter seems kinda dodgy
  DelimTokenTree &get_delim_tok_tree () { return token_tree; }
  const DelimTokenTree &get_delim_tok_tree () const { return token_tree; }

  // Set the delim token tree of a macro invocation
  void set_delim_tok_tree (DelimTokenTree tree) { token_tree = tree; }

  // TODO: this mutable getter seems kinda dodgy
  SimplePath &get_path () { return path; }
  const SimplePath &get_path () const { return path; }

  void set_expander (MacroExpander *new_expander) { expander = new_expander; }
  MacroExpander *get_expander ()
  {
    rust_assert (expander);
    return expander;
  }

  void
  set_meta_item_output (std::vector<std::unique_ptr<MetaItemInner> > new_items)
  {
    parsed_items = std::move (new_items);
  }
  // TODO: mutable getter seems kinda dodgy
  std::vector<std::unique_ptr<MetaItemInner> > &get_meta_items ()
  {
    return parsed_items;
  }
  const std::vector<std::unique_ptr<MetaItemInner> > &get_meta_items () const
  {
    return parsed_items;
  }
};

class SingleASTNode
{
public:
  enum NodeType
  {
    EXPRESSION,
    ITEM,
    STMT,
    EXTERN,
    TRAIT,
    IMPL,
    TRAIT_IMPL,
    TYPE,
  };

private:
  NodeType kind;

  // FIXME make this a union
  std::unique_ptr<Expr> expr;
  std::unique_ptr<Item> item;
  std::unique_ptr<Stmt> stmt;
  std::unique_ptr<ExternalItem> external_item;
  std::unique_ptr<TraitItem> trait_item;
  std::unique_ptr<InherentImplItem> impl_item;
  std::unique_ptr<TraitImplItem> trait_impl_item;
  std::unique_ptr<Type> type;

public:
  SingleASTNode (std::unique_ptr<Expr> expr)
    : kind (EXPRESSION), expr (std::move (expr))
  {}

  SingleASTNode (std::unique_ptr<Item> item)
    : kind (ITEM), item (std::move (item))
  {}

  SingleASTNode (std::unique_ptr<Stmt> stmt)
    : kind (STMT), stmt (std::move (stmt))
  {}

  SingleASTNode (std::unique_ptr<ExternalItem> item)
    : kind (EXTERN), external_item (std::move (item))
  {}

  SingleASTNode (std::unique_ptr<TraitItem> item)
    : kind (TRAIT), trait_item (std::move (item))
  {}

  SingleASTNode (std::unique_ptr<InherentImplItem> item)
    : kind (IMPL), impl_item (std::move (item))
  {}

  SingleASTNode (std::unique_ptr<TraitImplItem> trait_impl_item)
    : kind (TRAIT_IMPL), trait_impl_item (std::move (trait_impl_item))
  {}

  SingleASTNode (std::unique_ptr<Type> type)
    : kind (TYPE), type (std::move (type))
  {}

  SingleASTNode (SingleASTNode const &other)
  {
    kind = other.kind;
    switch (kind)
      {
      case EXPRESSION:
	expr = other.expr->clone_expr ();
	break;

      case ITEM:
	item = other.item->clone_item ();
	break;

      case STMT:
	stmt = other.stmt->clone_stmt ();
	break;

      case EXTERN:
	external_item = other.external_item->clone_external_item ();
	break;

      case TRAIT:
	trait_item = other.trait_item->clone_trait_item ();
	break;

      case IMPL:
	impl_item = other.impl_item->clone_inherent_impl_item ();
	break;

      case TRAIT_IMPL:
	trait_impl_item = other.trait_impl_item->clone_trait_impl_item ();
	break;

      case TYPE:
	type = other.type->clone_type ();
	break;
      }
  }

  SingleASTNode operator= (SingleASTNode const &other)
  {
    kind = other.kind;
    switch (kind)
      {
      case EXPRESSION:
	expr = other.expr->clone_expr ();
	break;

      case ITEM:
	item = other.item->clone_item ();
	break;

      case STMT:
	stmt = other.stmt->clone_stmt ();
	break;

      case EXTERN:
	external_item = other.external_item->clone_external_item ();
	break;

      case TRAIT:
	trait_item = other.trait_item->clone_trait_item ();
	break;

      case IMPL:
	impl_item = other.impl_item->clone_inherent_impl_item ();
	break;

      case TRAIT_IMPL:
	trait_impl_item = other.trait_impl_item->clone_trait_impl_item ();
	break;

      case TYPE:
	type = other.type->clone_type ();
	break;
      }
    return *this;
  }

  SingleASTNode (SingleASTNode &&other) = default;
  SingleASTNode &operator= (SingleASTNode &&other) = default;

  NodeType get_kind () const { return kind; }

  std::unique_ptr<Expr> &get_expr ()
  {
    rust_assert (kind == EXPRESSION);
    return expr;
  }

  std::unique_ptr<Item> &get_item ()
  {
    rust_assert (kind == ITEM);
    return item;
  }

  std::unique_ptr<Stmt> &get_stmt ()
  {
    rust_assert (kind == STMT);
    return stmt;
  }

  /**
   * Access the inner nodes and take ownership of them.
   * You can only call these functions once per node
   */

  std::unique_ptr<Stmt> take_stmt ()
  {
    rust_assert (!is_error ());
    return std::move (stmt);
  }

  std::unique_ptr<Expr> take_expr ()
  {
    rust_assert (!is_error ());
    return std::move (expr);
  }

  std::unique_ptr<Item> take_item ()
  {
    rust_assert (!is_error ());
    return std::move (item);
  }

  std::unique_ptr<TraitItem> take_trait_item ()
  {
    rust_assert (!is_error ());
    return std::move (trait_item);
  }

  std::unique_ptr<ExternalItem> take_external_item ()
  {
    rust_assert (!is_error ());
    return std::move (external_item);
  }

  std::unique_ptr<InherentImplItem> take_impl_item ()
  {
    rust_assert (!is_error ());
    return std::move (impl_item);
  }

  std::unique_ptr<TraitImplItem> take_trait_impl_item ()
  {
    rust_assert (!is_error ());
    return std::move (trait_impl_item);
  }

  std::unique_ptr<Type> take_type ()
  {
    rust_assert (!is_error ());
    return std::move (type);
  }

  void accept_vis (ASTVisitor &vis)
  {
    switch (kind)
      {
      case EXPRESSION:
	expr->accept_vis (vis);
	break;

      case ITEM:
	item->accept_vis (vis);
	break;

      case STMT:
	stmt->accept_vis (vis);
	break;

      case EXTERN:
	external_item->accept_vis (vis);
	break;

      case TRAIT:
	trait_item->accept_vis (vis);
	break;

      case IMPL:
	impl_item->accept_vis (vis);
	break;

      case TRAIT_IMPL:
	trait_impl_item->accept_vis (vis);
	break;

      case TYPE:
	type->accept_vis (vis);
	break;
      }
  }

  bool is_error ()
  {
    switch (kind)
      {
      case EXPRESSION:
	return expr == nullptr;
      case ITEM:
	return item == nullptr;
      case STMT:
	return stmt == nullptr;
      case EXTERN:
	return external_item == nullptr;
      case TRAIT:
	return trait_item == nullptr;
      case IMPL:
	return impl_item == nullptr;
      case TRAIT_IMPL:
	return trait_impl_item == nullptr;
      case TYPE:
	return type == nullptr;
      }

    gcc_unreachable ();
    return true;
  }

  std::string as_string () const
  {
    switch (kind)
      {
      case EXPRESSION:
	return "Expr: " + expr->as_string ();
      case ITEM:
	return "Item: " + item->as_string ();
      case STMT:
	return "Stmt: " + stmt->as_string ();
      case EXTERN:
	return "External Item: " + external_item->as_string ();
      case TRAIT:
	return "Trait Item: " + trait_item->as_string ();
      case IMPL:
	return "Impl Item: " + impl_item->as_string ();
      case TRAIT_IMPL:
	return "Trait Impl Item: " + trait_impl_item->as_string ();
      case TYPE:
	return "Type: " + type->as_string ();
      }

    gcc_unreachable ();
    return "";
  }
};

// A crate AST object - holds all the data for a single compilation unit
struct Crate
{
  std::vector<Attribute> inner_attrs;
  // dodgy spacing required here
  /* TODO: is it better to have a vector of items here or a module (implicit
   * top-level one)? */
  std::vector<std::unique_ptr<Item> > items;

  NodeId node_id;

public:
  // Constructor
  Crate (std::vector<std::unique_ptr<Item> > items,
	 std::vector<Attribute> inner_attrs)
    : inner_attrs (std::move (inner_attrs)), items (std::move (items)),
      node_id (Analysis::Mappings::get ()->get_next_node_id ())
  {}

  // Copy constructor with vector clone
  Crate (Crate const &other)
    : inner_attrs (other.inner_attrs), node_id (other.node_id)
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
    node_id = other.node_id;

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

  // Delete all crate information, e.g. if fails cfg.
  void strip_crate ()
  {
    inner_attrs.clear ();
    inner_attrs.shrink_to_fit ();

    items.clear ();
    items.shrink_to_fit ();
    // TODO: is this the best way to do this?
  }

  NodeId get_node_id () const { return node_id; }
  const std::vector<Attribute> &get_inner_attrs () const { return inner_attrs; }
};

// Base path expression AST node - abstract
class PathExpr : public ExprWithoutBlock
{
};
} // namespace AST
} // namespace Rust

#endif
