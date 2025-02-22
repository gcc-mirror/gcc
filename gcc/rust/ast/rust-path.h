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

#ifndef RUST_AST_PATH_H
#define RUST_AST_PATH_H
/* "Path" (identifier within namespaces, essentially) handling. Required include
 * for virtually all AST-related functionality. */

#include "optional.h"
#include "rust-ast.h"
#include "rust-hir-map.h"
#include "rust-mapping-common.h"
#include "rust-system.h"
#include "system.h"

namespace Rust {
namespace AST {

// The "identifier" (not generic args) aspect of each path expression segment
class PathIdentSegment
{
  std::string segment_name;
  location_t locus;

  // only allow identifiers, "super", "self", "Self", "crate", or "$crate"
public:
  PathIdentSegment (std::string segment_name, location_t locus)
    : segment_name (std::move (segment_name)), locus (locus)
  {}

  // Creates an error PathIdentSegment.
  static PathIdentSegment create_error ()
  {
    return PathIdentSegment ("", UNDEF_LOCATION);
  }

  // Returns whether PathIdentSegment is in an error state.
  bool is_error () const { return segment_name.empty (); }

  std::string as_string () const { return segment_name; }

  location_t get_locus () const { return locus; }

  bool is_super_path_seg () const
  {
    return as_string ().compare ("super") == 0;
  }
  bool is_crate_path_seg () const
  {
    return as_string ().compare ("crate") == 0;
  }
  bool is_lower_self_seg () const { return as_string ().compare ("self") == 0; }
  bool is_big_self_seg () const { return as_string ().compare ("Self") == 0; }
};

// A binding of an identifier to a type used in generic arguments in paths
struct GenericArgsBinding
{
private:
  Identifier identifier;
  std::unique_ptr<Type> type;
  location_t locus;

public:
  // Returns whether binding is in an error state.
  bool is_error () const
  {
    return type == nullptr;
    // and also identifier is empty, but cheaper computation
  }

  // Creates an error state generic args binding.
  static GenericArgsBinding create_error ()
  {
    return GenericArgsBinding ({""}, nullptr);
  }

  // Pointer type for type in constructor to enable polymorphism
  GenericArgsBinding (Identifier ident, std::unique_ptr<Type> type_ptr,
		      location_t locus = UNDEF_LOCATION)
    : identifier (std::move (ident)), type (std::move (type_ptr)), locus (locus)
  {}

  // Copy constructor has to deep copy the type as it is a unique pointer
  GenericArgsBinding (GenericArgsBinding const &other)
    : identifier (other.identifier), locus (other.locus)
  {
    // guard to protect from null pointer dereference
    if (other.type != nullptr)
      type = other.type->clone_type ();
  }

  // default destructor
  ~GenericArgsBinding () = default;

  // Overload assignment operator to deep copy the pointed-to type
  GenericArgsBinding &operator= (GenericArgsBinding const &other)
  {
    identifier = other.identifier;
    locus = other.locus;

    // guard to protect from null pointer dereference
    if (other.type != nullptr)
      type = other.type->clone_type ();
    else
      type = nullptr;

    return *this;
  }

  // move constructors
  GenericArgsBinding (GenericArgsBinding &&other) = default;
  GenericArgsBinding &operator= (GenericArgsBinding &&other) = default;

  std::string as_string () const;

  // TODO: is this better? Or is a "vis_pattern" better?
  Type &get_type ()
  {
    rust_assert (type != nullptr);
    return *type;
  }

  std::unique_ptr<Type> &get_type_ptr ()
  {
    rust_assert (type != nullptr);
    return type;
  }

  location_t get_locus () const { return locus; }

  Identifier get_identifier () const { return identifier; }
};

/* Class representing a const generic application */
class GenericArg
{
public:
  /**
   * const generic arguments cannot always be differentiated with generic type
   * arguments during parsing, e.g:
   * ```rust
   * let a: Foo<N>;
   * ```
   *
   * Is N a type? A constant defined elsewhere? The parser cannot know, and must
   * not draw any conclusions. We must wait until later passes of the compiler
   * to decide whether this refers to a constant item or a type.
   *
   * On the other hand, simple expressions like literals or block expressions
   * will always be constant expressions: There is no ambiguity at all.
   */
  enum class Kind
  {
    Error,
    Const,  // A const value
    Type,   // A type argument (not discernable during parsing)
    Either, // Either a type or a const value, cleared up during resolving
  };

  static GenericArg create_error ()
  {
    return GenericArg (nullptr, nullptr, {""}, Kind::Error, UNDEF_LOCATION);
  }

  static GenericArg create_const (std::unique_ptr<Expr> expression)
  {
    auto locus = expression->get_locus ();
    return GenericArg (std::move (expression), nullptr, {""}, Kind::Const,
		       locus);
  }

  static GenericArg create_type (std::unique_ptr<Type> type)
  {
    auto locus = type->get_locus ();
    return GenericArg (nullptr, std::move (type), {""}, Kind::Type, locus);
  }

  static GenericArg create_ambiguous (Identifier path, location_t locus)
  {
    return GenericArg (nullptr, nullptr, std::move (path), Kind::Either, locus);
  }

  GenericArg (const GenericArg &other)
    : path (other.path), kind (other.kind), locus (other.locus)
  {
    if (other.expression)
      expression = other.expression->clone_expr ();
    if (other.type)
      type = other.type->clone_type ();
  }

  GenericArg operator= (const GenericArg &other)
  {
    kind = other.kind;
    path = other.path;
    locus = other.locus;

    if (other.expression)
      expression = other.expression->clone_expr ();
    if (other.type)
      type = other.type->clone_type ();

    return *this;
  }

  GenericArg (GenericArg &&other) = default;
  GenericArg &operator= (GenericArg &&other) = default;

  bool is_error () const { return kind == Kind::Error; }

  Kind get_kind () const { return kind; }
  location_t get_locus () const { return locus; }

  void accept_vis (AST::ASTVisitor &visitor)
  {
    switch (get_kind ())
      {
      case Kind::Const:
	get_expression ().accept_vis (visitor);
	break;
      case Kind::Type:
	get_type ().accept_vis (visitor);
	break;
      case Kind::Either:
	break;
      case Kind::Error:
	rust_unreachable ();
      }
  }

  Expr &get_expression ()
  {
    rust_assert (kind == Kind::Const);

    return *expression;
  }

  std::unique_ptr<Expr> &get_expression_ptr ()
  {
    rust_assert (kind == Kind::Const);

    return expression;
  }

  Type &get_type ()
  {
    rust_assert (kind == Kind::Type);

    return *type;
  }

  std::unique_ptr<Type> &get_type_ptr ()
  {
    rust_assert (kind == Kind::Type);

    return type;
  }

  const std::string get_path () const
  {
    rust_assert (kind == Kind::Either);

    return path.as_string ();
  }

  std::string as_string () const
  {
    switch (get_kind ())
      {
      case Kind::Error:
	rust_unreachable ();
      case Kind::Either:
	return "Ambiguous: " + path.as_string ();
      case Kind::Const:
	return "Const: { " + expression->as_string () + " }";
      case Kind::Type:
	return "Type: " + type->as_string ();
      }

    return "";
  }

  /**
   * Disambiguate an ambiguous generic argument to a const generic argument,
   * unequivocally
   */
  GenericArg disambiguate_to_const () const;

  /**
   * Disambiguate an ambiguous generic argument to a type argument,
   * unequivocally
   */
  GenericArg disambiguate_to_type () const;

private:
  GenericArg (std::unique_ptr<Expr> expression, std::unique_ptr<Type> type,
	      Identifier path, Kind kind, location_t locus)
    : expression (std::move (expression)), type (std::move (type)),
      path (std::move (path)), kind (kind), locus (locus)
  {}

  /**
   * Expression associated with a `Clear` const generic application
   * A null pointer here is allowed in the case that the const argument is
   * ambiguous.
   */
  std::unique_ptr<Expr> expression;

  /**
   * If the argument ends up being a type argument instead. A null pointer will
   * be present here until the resolving phase.
   */
  std::unique_ptr<Type> type;

  /**
   * Optional path which cannot be differentiated between a constant item and
   * a type. Only used for ambiguous const generic arguments, otherwise
   * empty.
   */
  Identifier path;

  /* Which kind of const generic application are we dealing with */
  Kind kind;

  location_t locus;
};

/**
 * Representation of const generic parameters
 */
class ConstGenericParam : public GenericParam
{
  /* Name of the parameter */
  Identifier name;

  /* Mandatory type of the const parameter - a null pointer is an error */
  std::unique_ptr<AST::Type> type;

  /**
   * Default value for the const generic parameter
   */
  GenericArg default_value;

  AST::AttrVec outer_attrs;
  location_t locus;

public:
  ConstGenericParam (Identifier name, std::unique_ptr<AST::Type> type,
		     GenericArg default_value, AST::AttrVec outer_attrs,
		     location_t locus)
    : name (name), type (std::move (type)),
      default_value (std::move (default_value)), outer_attrs (outer_attrs),
      locus (locus)
  {}

  ConstGenericParam (const ConstGenericParam &other)
    : GenericParam (), name (other.name), type (other.type->clone_type ()),
      default_value (other.default_value), outer_attrs (other.outer_attrs),
      locus (other.locus)
  {}

  bool has_type () const { return type != nullptr; }
  bool has_default_value () const { return !default_value.is_error (); }

  const Identifier &get_name () const { return name; }

  AST::AttrVec &get_outer_attrs () { return outer_attrs; }

  AST::Type &get_type ()
  {
    rust_assert (has_type ());

    return *type;
  }

  GenericArg &get_default_value ()
  {
    rust_assert (has_default_value ());

    return default_value;
  }

  const GenericArg &get_default_value () const
  {
    rust_assert (has_default_value ());

    return default_value;
  }

  std::string as_string () const override;

  void accept_vis (ASTVisitor &vis) override;

  location_t get_locus () const override final { return locus; }

  Kind get_kind () const override final { return Kind::Const; }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  ConstGenericParam *clone_generic_param_impl () const override
  {
    return new ConstGenericParam (*this);
  }
};

// Generic arguments allowed in each path expression segment - inline?
struct GenericArgs
{
  std::vector<Lifetime> lifetime_args;
  std::vector<GenericArg> generic_args;
  std::vector<GenericArgsBinding> binding_args;
  location_t locus;

public:
  // Returns true if there are any generic arguments
  bool has_generic_args () const
  {
    return !(lifetime_args.empty () && generic_args.empty ()
	     && binding_args.empty ());
  }

  GenericArgs (std::vector<Lifetime> lifetime_args,
	       std::vector<GenericArg> generic_args,
	       std::vector<GenericArgsBinding> binding_args,
	       location_t locus = UNDEF_LOCATION)
    : lifetime_args (std::move (lifetime_args)),
      generic_args (std::move (generic_args)),
      binding_args (std::move (binding_args)), locus (locus)
  {}

  // copy constructor with vector clone
  GenericArgs (GenericArgs const &other)
    : lifetime_args (other.lifetime_args), binding_args (other.binding_args),
      locus (other.locus)
  {
    generic_args.clear ();
    generic_args.reserve (other.generic_args.size ());
    for (const auto &arg : other.generic_args)
      {
	generic_args.push_back (GenericArg (arg));
      }
  }

  ~GenericArgs () = default;

  // overloaded assignment operator to vector clone
  GenericArgs &operator= (GenericArgs const &other)
  {
    lifetime_args = other.lifetime_args;
    binding_args = other.binding_args;
    locus = other.locus;

    generic_args.clear ();
    generic_args.reserve (other.generic_args.size ());
    for (const auto &arg : other.generic_args)
      {
	generic_args.push_back (GenericArg (arg));
      }

    return *this;
  }

  // move constructors
  GenericArgs (GenericArgs &&other) = default;
  GenericArgs &operator= (GenericArgs &&other) = default;

  // Creates an empty GenericArgs (no arguments)
  static GenericArgs create_empty () { return GenericArgs ({}, {}, {}); }

  std::string as_string () const;

  std::vector<GenericArg> &get_generic_args () { return generic_args; }

  std::vector<GenericArgsBinding> &get_binding_args () { return binding_args; }

  const std::vector<GenericArgsBinding> &get_binding_args () const
  {
    return binding_args;
  }

  std::vector<Lifetime> &get_lifetime_args () { return lifetime_args; };

  const std::vector<Lifetime> &get_lifetime_args () const
  {
    return lifetime_args;
  };

  location_t get_locus () const { return locus; }
};

/* A segment of a path in expression, including an identifier aspect and maybe
 * generic args */
class PathExprSegment
{ // or should this extend PathIdentSegment?
private:
  PathIdentSegment segment_name;
  GenericArgs generic_args;
  location_t locus;
  NodeId node_id;

public:
  // Returns true if there are any generic arguments
  bool has_generic_args () const { return generic_args.has_generic_args (); }

  // Constructor for segment (from IdentSegment and GenericArgs)
  PathExprSegment (PathIdentSegment segment_name, location_t locus,
		   GenericArgs generic_args = GenericArgs::create_empty ())
    : segment_name (std::move (segment_name)),
      generic_args (std::move (generic_args)), locus (locus),
      node_id (Analysis::Mappings::get ().get_next_node_id ())
  {}

  /* Constructor for segment with generic arguments (from segment name and all
   * args) */
  PathExprSegment (std::string segment_name, location_t locus,
		   std::vector<Lifetime> lifetime_args = {},
		   std::vector<GenericArg> generic_args = {},
		   std::vector<GenericArgsBinding> binding_args = {})
    : segment_name (PathIdentSegment (std::move (segment_name), locus)),
      generic_args (GenericArgs (std::move (lifetime_args),
				 std::move (generic_args),
				 std::move (binding_args))),
      locus (locus), node_id (Analysis::Mappings::get ().get_next_node_id ())
  {}

  // Returns whether path expression segment is in an error state.
  bool is_error () const { return segment_name.is_error (); }

  // Creates an error-state path expression segment.
  static PathExprSegment create_error ()
  {
    return PathExprSegment (PathIdentSegment::create_error (), UNDEF_LOCATION);
  }

  std::string as_string () const;

  location_t get_locus () const { return locus; }

  // TODO: is this better? Or is a "vis_pattern" better?
  GenericArgs &get_generic_args ()
  {
    rust_assert (has_generic_args ());
    return generic_args;
  }

  PathIdentSegment &get_ident_segment () { return segment_name; }
  const PathIdentSegment &get_ident_segment () const { return segment_name; }

  NodeId get_node_id () const { return node_id; }

  bool is_super_path_seg () const
  {
    return !has_generic_args () && get_ident_segment ().is_super_path_seg ();
  }

  bool is_crate_path_seg () const
  {
    return !has_generic_args () && get_ident_segment ().is_crate_path_seg ();
  }

  bool is_lower_self_seg () const
  {
    return !has_generic_args () && get_ident_segment ().is_lower_self_seg ();
  }
};

// AST node representing a pattern that involves a "path" - abstract base
// class
class Path : public Pattern
{
public:
  enum class Kind
  {
    LangItem,
    Regular,
  };

  Path (std::vector<PathExprSegment> segments)
    : segments (std::move (segments)), lang_item (tl::nullopt),
      kind (Kind::Regular)
  {}

  Path (LangItem::Kind lang_item)
    : segments ({}), lang_item (lang_item), kind (Kind::LangItem)
  {}

  // Returns whether path has segments.
  bool has_segments () const
  {
    rust_assert (kind == Kind::Regular);
    return !segments.empty ();
  }

  /* Converts path segments to their equivalent SimplePath segments if
   * possible, and creates a SimplePath from them. */
  SimplePath convert_to_simple_path (bool with_opening_scope_resolution) const;

  /* Returns whether the path is a single segment (excluding qualified path
   * initial as segment). */
  bool is_single_segment () const
  {
    rust_assert (kind == Kind::Regular);
    return segments.size () == 1;
  }

  std::string as_string () const override;

  bool is_lang_item () const { return kind == Kind::LangItem; }

  // TODO: this seems kinda dodgy
  std::vector<PathExprSegment> &get_segments ()
  {
    rust_assert (kind == Kind::Regular);
    return segments;
  }
  const std::vector<PathExprSegment> &get_segments () const
  {
    rust_assert (kind == Kind::Regular);
    return segments;
  }

  LangItem::Kind get_lang_item () const
  {
    rust_assert (kind == Kind::LangItem);
    return *lang_item;
  }

  Pattern::Kind get_pattern_kind () override { return Pattern::Kind::Path; }
  Path::Kind get_path_kind () { return kind; }

protected:
  std::vector<PathExprSegment> segments;
  tl::optional<LangItem::Kind> lang_item;

  Path::Kind kind;
};

/* AST node representing a path-in-expression pattern (path that allows
 * generic arguments) */
class PathInExpression : public Path, public ExprWithoutBlock
{
  std::vector<Attribute> outer_attrs;
  bool has_opening_scope_resolution;
  location_t locus;
  NodeId _node_id;

  bool marked_for_strip;

public:
  std::string as_string () const override;

  // Constructor
  PathInExpression (std::vector<PathExprSegment> path_segments,
		    std::vector<Attribute> outer_attrs, location_t locus,
		    bool has_opening_scope_resolution = false)
    : Path (std::move (path_segments)), outer_attrs (std::move (outer_attrs)),
      has_opening_scope_resolution (has_opening_scope_resolution),
      locus (locus), _node_id (Analysis::Mappings::get ().get_next_node_id ()),
      marked_for_strip (false)
  {}

  PathInExpression (LangItem::Kind lang_item,
		    std::vector<Attribute> outer_attrs, location_t locus)
    : Path (lang_item), outer_attrs (std::move (outer_attrs)),
      has_opening_scope_resolution (false), locus (locus),
      _node_id (Analysis::Mappings::get ().get_next_node_id ()),
      marked_for_strip (false)
  {}

  // Creates an error state path in expression.
  static PathInExpression create_error ()
  {
    return PathInExpression (std::vector<PathExprSegment> (), {},
			     UNDEF_LOCATION);
  }

  // Returns whether path in expression is in an error state.
  bool is_error () const { return !has_segments (); }

  /* Converts PathInExpression to SimplePath if possible (i.e. no generic
   * arguments). Otherwise returns an empty SimplePath. */
  SimplePath as_simple_path () const
  {
    /* delegate to parent class as can't access segments. however,
     * QualifiedPathInExpression conversion to simple path wouldn't make
     * sense, so the method in the parent class should be protected, not
     * public. Have to pass in opening scope resolution as parent class has no
     * access to it.
     */
    return convert_to_simple_path (has_opening_scope_resolution);
  }

  location_t get_locus () const override final { return locus; }

  void accept_vis (ASTVisitor &vis) override;

  void mark_for_strip () override { marked_for_strip = true; }
  bool is_marked_for_strip () const override { return marked_for_strip; }

  bool opening_scope_resolution () const
  {
    return has_opening_scope_resolution;
  }

  NodeId get_node_id () const override { return _node_id; }

  const std::vector<Attribute> &get_outer_attrs () const { return outer_attrs; }
  std::vector<Attribute> &get_outer_attrs () override { return outer_attrs; }

  void set_outer_attrs (std::vector<Attribute> new_attrs) override
  {
    outer_attrs = std::move (new_attrs);
  }

  PathExprSegment &get_final_segment () { return get_segments ().back (); }
  const PathExprSegment &get_final_segment () const
  {
    return get_segments ().back ();
  }

  Expr::Kind get_expr_kind () const override
  {
    return Expr::Kind::PathInExpression;
  }

protected:
  /* Use covariance to implement clone function as returning this object
   * rather than base */
  PathInExpression *clone_pattern_impl () const final override
  {
    return clone_path_in_expression_impl ();
  }

  /* Use covariance to implement clone function as returning this object
   * rather than base */
  PathInExpression *clone_expr_without_block_impl () const final override
  {
    return clone_path_in_expression_impl ();
  }

  /*virtual*/ PathInExpression *clone_path_in_expression_impl () const
  {
    return new PathInExpression (*this);
  }
};

/* Base class for segments used in type paths - not abstract (represents an
 * ident-only segment) */
class TypePathSegment
{
public:
  enum SegmentType
  {
    REG,
    GENERIC,
    FUNCTION
  };

private:
  tl::optional<LangItem::Kind> lang_item;
  tl::optional<PathIdentSegment> ident_segment;
  location_t locus;

protected:
  /* This is protected because it is only really used by derived classes, not
   * the base. */
  bool has_separating_scope_resolution;
  NodeId node_id;

public:
  // Clone function implementation - not pure virtual as overrided by
  // subclasses
  virtual TypePathSegment *clone_type_path_segment_impl () const
  {
    return new TypePathSegment (*this);
  }

public:
  virtual ~TypePathSegment () {}

  virtual SegmentType get_type () const { return SegmentType::REG; }

  // Unique pointer custom clone function
  std::unique_ptr<TypePathSegment> clone_type_path_segment () const
  {
    return std::unique_ptr<TypePathSegment> (clone_type_path_segment_impl ());
  }

  TypePathSegment (PathIdentSegment ident_segment,
		   bool has_separating_scope_resolution, location_t locus)
    : lang_item (tl::nullopt), ident_segment (std::move (ident_segment)),
      locus (locus),
      has_separating_scope_resolution (has_separating_scope_resolution),
      node_id (Analysis::Mappings::get ().get_next_node_id ())
  {}

  TypePathSegment (LangItem::Kind lang_item, location_t locus)
    : lang_item (lang_item), ident_segment (tl::nullopt), locus (locus),
      has_separating_scope_resolution (false),
      node_id (Analysis::Mappings::get ().get_next_node_id ())
  {}

  TypePathSegment (std::string segment_name,
		   bool has_separating_scope_resolution, location_t locus)
    : lang_item (tl::nullopt),
      ident_segment (PathIdentSegment (std::move (segment_name), locus)),
      locus (locus),
      has_separating_scope_resolution (has_separating_scope_resolution),
      node_id (Analysis::Mappings::get ().get_next_node_id ())
  {}

  TypePathSegment (TypePathSegment const &other)
    : lang_item (other.lang_item), ident_segment (other.ident_segment),
      locus (other.locus),
      has_separating_scope_resolution (other.has_separating_scope_resolution),
      node_id (other.node_id)
  {}

  TypePathSegment &operator= (TypePathSegment const &other)
  {
    ident_segment = other.ident_segment;
    lang_item = other.lang_item;
    locus = other.locus;
    has_separating_scope_resolution = other.has_separating_scope_resolution;
    node_id = other.node_id;

    return *this;
  }

  TypePathSegment (TypePathSegment &&other) = default;
  TypePathSegment &operator= (TypePathSegment &&other) = default;

  virtual std::string as_string () const
  {
    if (lang_item.has_value ())
      return LangItem::PrettyString (*lang_item);

    return ident_segment->as_string ();
  }

  /* Returns whether the type path segment is in an error state. May be
   * virtual in future. */
  bool is_error () const
  {
    rust_assert (ident_segment);
    return ident_segment->is_error ();
  }

  /* Returns whether segment is identifier only (as opposed to generic args or
   * function). Overridden in derived classes with other segments. */
  virtual bool is_ident_only () const { return true; }

  bool is_lang_item () const { return lang_item.has_value (); }

  location_t get_locus () const { return locus; }

  // not pure virtual as class not abstract
  virtual void accept_vis (ASTVisitor &vis);

  bool get_separating_scope_resolution () const
  {
    return has_separating_scope_resolution;
  }

  PathIdentSegment &get_ident_segment ()
  {
    rust_assert (!is_lang_item ());
    return *ident_segment;
  };

  const PathIdentSegment &get_ident_segment () const
  {
    rust_assert (!is_lang_item ());
    return *ident_segment;
  };

  LangItem::Kind get_lang_item () const
  {
    rust_assert (is_lang_item ());
    return *lang_item;
  }

  NodeId get_node_id () const { return node_id; }

  bool is_crate_path_seg () const
  {
    return get_ident_segment ().is_crate_path_seg ();
  }
  bool is_super_path_seg () const
  {
    return get_ident_segment ().is_super_path_seg ();
  }
  bool is_big_self_seg () const
  {
    return get_ident_segment ().is_big_self_seg ();
  }
  bool is_lower_self_seg () const
  {
    return get_ident_segment ().is_lower_self_seg ();
  }
};

// Segment used in type path with generic args
class TypePathSegmentGeneric : public TypePathSegment
{
  GenericArgs generic_args;

public:
  SegmentType get_type () const override { return SegmentType::GENERIC; }

  bool has_generic_args () const { return generic_args.has_generic_args (); }

  bool is_ident_only () const override { return false; }

  // Constructor with PathIdentSegment and GenericArgs
  TypePathSegmentGeneric (PathIdentSegment ident_segment,
			  bool has_separating_scope_resolution,
			  GenericArgs generic_args, location_t locus)
    : TypePathSegment (std::move (ident_segment),
		       has_separating_scope_resolution, locus),
      generic_args (std::move (generic_args))
  {}

  TypePathSegmentGeneric (LangItem::Kind lang_item, GenericArgs generic_args,
			  location_t locus)
    : TypePathSegment (lang_item, locus),
      generic_args (std::move (generic_args))
  {}

  // Constructor from segment name and all args
  TypePathSegmentGeneric (std::string segment_name,
			  bool has_separating_scope_resolution,
			  std::vector<Lifetime> lifetime_args,
			  std::vector<GenericArg> generic_args,
			  std::vector<GenericArgsBinding> binding_args,
			  location_t locus)
    : TypePathSegment (std::move (segment_name),
		       has_separating_scope_resolution, locus),
      generic_args (GenericArgs (std::move (lifetime_args),
				 std::move (generic_args),
				 std::move (binding_args)))
  {}

  // Copy constructor with vector clone
  TypePathSegmentGeneric (TypePathSegmentGeneric const &other)
    : TypePathSegment (other), generic_args (other.generic_args)
  {}

  // Overloaded assignment operator with vector clone
  TypePathSegmentGeneric &operator= (TypePathSegmentGeneric const &other)
  {
    generic_args = other.generic_args;

    return *this;
  }

  // move constructors
  TypePathSegmentGeneric (TypePathSegmentGeneric &&other) = default;
  TypePathSegmentGeneric &operator= (TypePathSegmentGeneric &&other) = default;

  std::string as_string () const override;

  void accept_vis (ASTVisitor &vis) override;

  // TODO: is this better? Or is a "vis_pattern" better?
  GenericArgs &get_generic_args ()
  {
    rust_assert (has_generic_args ());
    return generic_args;
  }

  // Use covariance to override base class method
  TypePathSegmentGeneric *clone_type_path_segment_impl () const override
  {
    return new TypePathSegmentGeneric (*this);
  }
};

// A function as represented in a type path
struct TypePathFunction
{
private:
  // TODO: remove
  /*bool has_inputs;
  TypePathFnInputs inputs;*/
  // inlined from TypePathFnInputs
  std::vector<std::unique_ptr<Type>> inputs;

  // bool has_type;
  std::unique_ptr<Type> return_type;

  // FIXME: think of better way to mark as invalid than taking up storage
  bool is_invalid;

  location_t locus;

protected:
  // Constructor only used to create invalid type path functions.
  TypePathFunction (bool is_invalid, location_t locus)
    : is_invalid (is_invalid), locus (locus)
  {}

public:
  // Returns whether the return type of the function has been specified.
  bool has_return_type () const { return return_type != nullptr; }

  // Returns whether the function has inputs.
  bool has_inputs () const { return !inputs.empty (); }

  // Returns whether function is in an error state.
  bool is_error () const { return is_invalid; }

  // Creates an error state function.
  static TypePathFunction create_error ()
  {
    return TypePathFunction (true, UNDEF_LOCATION);
  }

  // Constructor
  TypePathFunction (std::vector<std::unique_ptr<Type>> inputs, location_t locus,
		    std::unique_ptr<Type> type = nullptr)
    : inputs (std::move (inputs)), return_type (std::move (type)),
      is_invalid (false), locus (locus)
  {}

  // Copy constructor with clone
  TypePathFunction (TypePathFunction const &other)
    : is_invalid (other.is_invalid)
  {
    // guard to protect from null pointer dereference
    if (other.return_type != nullptr)
      return_type = other.return_type->clone_type ();

    inputs.reserve (other.inputs.size ());
    for (const auto &e : other.inputs)
      inputs.push_back (e->clone_type ());
  }

  ~TypePathFunction () = default;

  // Overloaded assignment operator to clone type
  TypePathFunction &operator= (TypePathFunction const &other)
  {
    is_invalid = other.is_invalid;

    // guard to protect from null pointer dereference
    if (other.return_type != nullptr)
      return_type = other.return_type->clone_type ();
    else
      return_type = nullptr;

    inputs.reserve (other.inputs.size ());
    for (const auto &e : other.inputs)
      inputs.push_back (e->clone_type ());

    return *this;
  }

  // move constructors
  TypePathFunction (TypePathFunction &&other) = default;
  TypePathFunction &operator= (TypePathFunction &&other) = default;

  std::string as_string () const;

  // TODO: this mutable getter seems really dodgy. Think up better way.
  const std::vector<std::unique_ptr<Type>> &get_params () const
  {
    return inputs;
  }
  std::vector<std::unique_ptr<Type>> &get_params () { return inputs; }

  // TODO: is this better? Or is a "vis_pattern" better?
  Type &get_return_type ()
  {
    rust_assert (has_return_type ());
    return *return_type;
  }

  std::unique_ptr<Type> &get_return_type_ptr ()
  {
    rust_assert (has_return_type ());
    return return_type;
  }

  location_t get_locus () const { return locus; }
};

// Segment used in type path with a function argument
class TypePathSegmentFunction : public TypePathSegment
{
  TypePathFunction function_path;

public:
  SegmentType get_type () const override { return SegmentType::FUNCTION; }

  // Constructor with PathIdentSegment and TypePathFn
  TypePathSegmentFunction (PathIdentSegment ident_segment,
			   bool has_separating_scope_resolution,
			   TypePathFunction function_path, location_t locus)
    : TypePathSegment (std::move (ident_segment),
		       has_separating_scope_resolution, locus),
      function_path (std::move (function_path))
  {}

  // Constructor with segment name and TypePathFn
  TypePathSegmentFunction (std::string segment_name,
			   bool has_separating_scope_resolution,
			   TypePathFunction function_path, location_t locus)
    : TypePathSegment (std::move (segment_name),
		       has_separating_scope_resolution, locus),
      function_path (std::move (function_path))
  {}

  std::string as_string () const override;

  bool is_ident_only () const override { return false; }

  void accept_vis (ASTVisitor &vis) override;

  // TODO: is this better? Or is a "vis_pattern" better?
  TypePathFunction &get_type_path_function ()
  {
    rust_assert (!function_path.is_error ());
    return function_path;
  }

  // Use covariance to override base class method
  TypePathSegmentFunction *clone_type_path_segment_impl () const override
  {
    return new TypePathSegmentFunction (*this);
  }
};

class TypePath : public TypeNoBounds
{
  bool has_opening_scope_resolution;
  std::vector<std::unique_ptr<TypePathSegment>> segments;
  location_t locus;

protected:
  /* Use covariance to implement clone function as returning this object
   * rather than base */
  TypePath *clone_type_no_bounds_impl () const override
  {
    return new TypePath (*this);
  }

public:
  /* Returns whether the TypePath has an opening scope resolution operator
   * (i.e. is global path or crate-relative path, not module-relative) */
  bool has_opening_scope_resolution_op () const
  {
    return has_opening_scope_resolution;
  }

  // Returns whether the TypePath is in an invalid state.
  bool is_error () const { return segments.empty (); }

  // Creates an error state TypePath.
  static TypePath create_error ()
  {
    return TypePath (std::vector<std::unique_ptr<TypePathSegment>> (),
		     UNDEF_LOCATION);
  }

  // Constructor
  TypePath (std::vector<std::unique_ptr<TypePathSegment>> segments,
	    location_t locus, bool has_opening_scope_resolution = false)
    : TypeNoBounds (),
      has_opening_scope_resolution (has_opening_scope_resolution),
      segments (std::move (segments)), locus (locus)
  {}

  TypePath (LangItem::Kind lang_item,
	    std::vector<std::unique_ptr<TypePathSegment>> segments,
	    location_t locus, bool has_opening_scope_resolution = false)
    : TypeNoBounds (),
      has_opening_scope_resolution (has_opening_scope_resolution),
      segments (std::move (segments)), locus (locus)
  {}

  // Copy constructor with vector clone
  TypePath (TypePath const &other)
    : has_opening_scope_resolution (other.has_opening_scope_resolution),
      locus (other.locus)
  {
    node_id = other.node_id;
    segments.reserve (other.segments.size ());
    for (const auto &e : other.segments)
      segments.push_back (e->clone_type_path_segment ());
  }

  // Overloaded assignment operator with clone
  TypePath &operator= (TypePath const &other)
  {
    node_id = other.node_id;
    has_opening_scope_resolution = other.has_opening_scope_resolution;
    locus = other.locus;

    segments.reserve (other.segments.size ());
    for (const auto &e : other.segments)
      segments.push_back (e->clone_type_path_segment ());

    return *this;
  }

  // move constructors
  TypePath (TypePath &&other) = default;
  TypePath &operator= (TypePath &&other) = default;

  std::string as_string () const override;

  /* Converts TypePath to SimplePath if possible (i.e. no generic or function
   * arguments). Otherwise returns an empty SimplePath. */
  SimplePath as_simple_path () const;

  // Creates a trait bound with a clone of this type path as its only element.
  TraitBound *to_trait_bound (bool in_parens) const override;

  location_t get_locus () const override final { return locus; }
  NodeId get_node_id () const { return node_id; }

  void mark_for_strip () override {}
  bool is_marked_for_strip () const override { return false; }

  void accept_vis (ASTVisitor &vis) override;

  // TODO: this seems kinda dodgy
  std::vector<std::unique_ptr<TypePathSegment>> &get_segments ()
  {
    return segments;
  }
  const std::vector<std::unique_ptr<TypePathSegment>> &get_segments () const
  {
    return segments;
  }

  size_t get_num_segments () const { return segments.size (); }
};

struct QualifiedPathType
{
private:
  std::unique_ptr<Type> type_to_invoke_on;
  TypePath trait_path;
  location_t locus;
  NodeId node_id;

public:
  // Constructor
  QualifiedPathType (std::unique_ptr<Type> invoke_on_type,
		     location_t locus = UNDEF_LOCATION,
		     TypePath trait_path = TypePath::create_error ())
    : type_to_invoke_on (std::move (invoke_on_type)), trait_path (trait_path),
      locus (locus), node_id (Analysis::Mappings::get ().get_next_node_id ())
  {}

  // Copy constructor uses custom deep copy for Type to preserve polymorphism
  QualifiedPathType (QualifiedPathType const &other)
    : trait_path (other.trait_path), locus (other.locus)
  {
    node_id = other.node_id;
    // guard to prevent null dereference
    if (other.type_to_invoke_on != nullptr)
      type_to_invoke_on = other.type_to_invoke_on->clone_type ();
  }

  // default destructor
  ~QualifiedPathType () = default;

  // overload assignment operator to use custom clone method
  QualifiedPathType &operator= (QualifiedPathType const &other)
  {
    node_id = other.node_id;
    trait_path = other.trait_path;
    locus = other.locus;

    // guard to prevent null dereference
    if (other.type_to_invoke_on != nullptr)
      type_to_invoke_on = other.type_to_invoke_on->clone_type ();
    else
      type_to_invoke_on = nullptr;

    return *this;
  }

  // move constructor
  QualifiedPathType (QualifiedPathType &&other) = default;
  QualifiedPathType &operator= (QualifiedPathType &&other) = default;

  // Returns whether the qualified path type has a rebind as clause.
  bool has_as_clause () const { return !trait_path.is_error (); }

  // Returns whether the qualified path type is in an error state.
  bool is_error () const { return type_to_invoke_on == nullptr; }

  // Creates an error state qualified path type.
  static QualifiedPathType create_error ()
  {
    return QualifiedPathType (nullptr);
  }

  std::string as_string () const;

  location_t get_locus () const { return locus; }

  // TODO: is this better? Or is a "vis_pattern" better?
  Type &get_type ()
  {
    rust_assert (type_to_invoke_on != nullptr);
    return *type_to_invoke_on;
  }

  std::unique_ptr<Type> &get_type_ptr ()
  {
    rust_assert (type_to_invoke_on != nullptr);
    return type_to_invoke_on;
  }

  // TODO: is this better? Or is a "vis_pattern" better?
  TypePath &get_as_type_path ()
  {
    rust_assert (has_as_clause ());
    return trait_path;
  }

  NodeId get_node_id () const { return node_id; }
};

/* AST node representing a qualified path-in-expression pattern (path that
 * allows specifying trait functions) */
class QualifiedPathInExpression : public Path, public ExprWithoutBlock
{
  std::vector<Attribute> outer_attrs;
  QualifiedPathType path_type;
  location_t locus;
  NodeId _node_id;

public:
  std::string as_string () const override;

  QualifiedPathInExpression (QualifiedPathType qual_path_type,
			     std::vector<PathExprSegment> path_segments,
			     std::vector<Attribute> outer_attrs,
			     location_t locus)
    : Path (std::move (path_segments)), outer_attrs (std::move (outer_attrs)),
      path_type (std::move (qual_path_type)), locus (locus),
      _node_id (Analysis::Mappings::get ().get_next_node_id ())
  {}

  /* TODO: maybe make a shortcut constructor that has QualifiedPathType
   * elements as params */

  // Returns whether qualified path in expression is in an error state.
  bool is_error () const { return path_type.is_error (); }

  // Creates an error qualified path in expression.
  static QualifiedPathInExpression create_error ()
  {
    return QualifiedPathInExpression (QualifiedPathType::create_error (), {},
				      {}, UNDEF_LOCATION);
  }

  location_t get_locus () const override final { return locus; }

  void accept_vis (ASTVisitor &vis) override;

  // Invalid if path_type is error, so base stripping on that.
  void mark_for_strip () override
  {
    path_type = QualifiedPathType::create_error ();
  }
  bool is_marked_for_strip () const override { return is_error (); }

  // TODO: is this better? Or is a "vis_pattern" better?
  QualifiedPathType &get_qualified_path_type ()
  {
    rust_assert (!path_type.is_error ());
    return path_type;
  }

  const std::vector<Attribute> &get_outer_attrs () const { return outer_attrs; }
  std::vector<Attribute> &get_outer_attrs () override { return outer_attrs; }

  void set_outer_attrs (std::vector<Attribute> new_attrs) override
  {
    outer_attrs = std::move (new_attrs);
  }

  NodeId get_node_id () const override { return _node_id; }

  Expr::Kind get_expr_kind () const override
  {
    return Expr::Kind::QualifiedPathInExpression;
  }

protected:
  /* Use covariance to implement clone function as returning this object
   * rather than base */
  QualifiedPathInExpression *clone_pattern_impl () const final override
  {
    return clone_qual_path_in_expression_impl ();
  }

  /* Use covariance to implement clone function as returning this object
   * rather than base */
  QualifiedPathInExpression *
  clone_expr_without_block_impl () const final override
  {
    return clone_qual_path_in_expression_impl ();
  }

  /*virtual*/ QualifiedPathInExpression *
  clone_qual_path_in_expression_impl () const
  {
    return new QualifiedPathInExpression (*this);
  }
};

/* Represents a qualified path in a type; used for disambiguating trait
 * function calls */
class QualifiedPathInType : public TypeNoBounds
{
  QualifiedPathType path_type;
  std::unique_ptr<TypePathSegment> associated_segment;
  std::vector<std::unique_ptr<TypePathSegment>> segments;
  location_t locus;

protected:
  /* Use covariance to implement clone function as returning this object
   * rather than base */
  QualifiedPathInType *clone_type_no_bounds_impl () const override
  {
    return new QualifiedPathInType (*this);
  }

public:
  QualifiedPathInType (
    QualifiedPathType qual_path_type,
    std::unique_ptr<TypePathSegment> associated_segment,
    std::vector<std::unique_ptr<TypePathSegment>> path_segments,
    location_t locus)
    : path_type (std::move (qual_path_type)),
      associated_segment (std::move (associated_segment)),
      segments (std::move (path_segments)), locus (locus)
  {}

  // Copy constructor with vector clone
  QualifiedPathInType (QualifiedPathInType const &other)
    : path_type (other.path_type), locus (other.locus)
  {
    auto seg = other.associated_segment->clone_type_path_segment_impl ();
    associated_segment = std::unique_ptr<TypePathSegment> (seg);

    segments.reserve (other.segments.size ());
    for (const auto &e : other.segments)
      segments.push_back (e->clone_type_path_segment ());
  }

  // Overloaded assignment operator with vector clone
  QualifiedPathInType &operator= (QualifiedPathInType const &other)
  {
    auto seg = other.associated_segment->clone_type_path_segment_impl ();
    associated_segment = std::unique_ptr<TypePathSegment> (seg);

    path_type = other.path_type;
    locus = other.locus;

    segments.reserve (other.segments.size ());
    for (const auto &e : other.segments)
      segments.push_back (e->clone_type_path_segment ());

    return *this;
  }

  // move constructors
  QualifiedPathInType (QualifiedPathInType &&other) = default;
  QualifiedPathInType &operator= (QualifiedPathInType &&other) = default;

  // Returns whether qualified path in type is in an error state.
  bool is_error () const { return path_type.is_error (); }

  // Creates an error state qualified path in type.
  static QualifiedPathInType create_error ()
  {
    return QualifiedPathInType (
      QualifiedPathType::create_error (), nullptr,
      std::vector<std::unique_ptr<TypePathSegment>> (), UNDEF_LOCATION);
  }

  std::string as_string () const override;

  void accept_vis (ASTVisitor &vis) override;

  // TODO: is this better? Or is a "vis_pattern" better?
  QualifiedPathType &get_qualified_path_type ()
  {
    rust_assert (!path_type.is_error ());
    return path_type;
  }

  std::unique_ptr<TypePathSegment> &get_associated_segment ()
  {
    return associated_segment;
  }

  // TODO: this seems kinda dodgy
  std::vector<std::unique_ptr<TypePathSegment>> &get_segments ()
  {
    return segments;
  }
  const std::vector<std::unique_ptr<TypePathSegment>> &get_segments () const
  {
    return segments;
  }

  location_t get_locus () const override final { return locus; }
};
} // namespace AST
} // namespace Rust

#endif
