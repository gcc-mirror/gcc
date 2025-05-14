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

#ifndef RUST_HIR_PATH_H
#define RUST_HIR_PATH_H

#include "rust-hir-map.h"
#include "rust-hir-simple-path.h"
#include "rust-hir-type-no-bounds.h"
#include "rust-hir-pattern-abstract.h"
#include "rust-hir-expr-abstract.h"

namespace Rust {
namespace HIR {

// The "identifier" (not generic args) aspect of each path expression segment
class PathIdentSegment
{
  std::string segment_name;

  // TODO: should this have location info stored?

  // only allow identifiers, "super", "self", "Self", "crate", or "$crate"
public:
  PathIdentSegment (std::string segment_name)
    : segment_name (std::move (segment_name))
  {}

  /* TODO: insert check in constructor for this? Or is this a semantic error
   * best handled then? */

  /* TODO: does this require visitor? pretty sure this isn't polymorphic, but
   * not entirely sure */

  // Creates an error PathIdentSegment.
  static PathIdentSegment create_error () { return PathIdentSegment (""); }

  // Returns whether PathIdentSegment is in an error state.
  bool is_error () const { return segment_name.empty (); }

  std::string as_string () const { return segment_name; }
};

// A binding of an identifier to a type used in generic arguments in paths
class GenericArgsBinding
{
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
		      location_t locus = UNDEF_LOCATION);

  // Copy constructor has to deep copy the type as it is a unique pointer
  GenericArgsBinding (GenericArgsBinding const &other);

  // default destructor
  ~GenericArgsBinding () = default;

  // Overload assignment operator to deep copy the pointed-to type
  GenericArgsBinding &operator= (GenericArgsBinding const &other);

  // move constructors
  GenericArgsBinding (GenericArgsBinding &&other) = default;
  GenericArgsBinding &operator= (GenericArgsBinding &&other) = default;

  std::string as_string () const;

  Identifier &get_identifier () { return identifier; }
  const Identifier &get_identifier () const { return identifier; }

  Type &get_type ()
  {
    rust_assert (type);
    return *type;
  }
  const Type &get_type () const
  {
    rust_assert (type);
    return *type;
  }

  location_t get_locus () const { return locus; }
};

class ConstGenericArg
{
  // FIXME: Do we need to disambiguate or no? We should be able to disambiguate
  // at name-resolution, hence no need for ambiguities here

public:
  ConstGenericArg (std::unique_ptr<Expr> expression, location_t locus);

  ConstGenericArg (const ConstGenericArg &other);

  ConstGenericArg operator= (const ConstGenericArg &other);

  std::unique_ptr<Expr> &get_expression () { return expression; }

private:
  std::unique_ptr<Expr> expression;
  location_t locus;
};

class GenericArgs
{
  std::vector<Lifetime> lifetime_args;
  std::vector<std::unique_ptr<Type> > type_args;
  std::vector<GenericArgsBinding> binding_args;
  std::vector<ConstGenericArg> const_args;
  location_t locus;

public:
  // Returns true if there are any generic arguments
  bool has_generic_args () const
  {
    return !(lifetime_args.empty () && type_args.empty ()
	     && binding_args.empty ());
  }

  GenericArgs (std::vector<Lifetime> lifetime_args,
	       std::vector<std::unique_ptr<Type> > type_args,
	       std::vector<GenericArgsBinding> binding_args,
	       std::vector<ConstGenericArg> const_args, location_t locus);

  // copy constructor with vector clone
  GenericArgs (GenericArgs const &other);

  ~GenericArgs () = default;

  // overloaded assignment operator to vector clone
  GenericArgs &operator= (GenericArgs const &other);

  // move constructors
  GenericArgs (GenericArgs &&other) = default;
  GenericArgs &operator= (GenericArgs &&other) = default;

  // Creates an empty GenericArgs (no arguments)
  static GenericArgs create_empty (location_t locus = UNDEF_LOCATION)
  {
    return GenericArgs ({}, {}, {}, {}, locus);
  }

  bool is_empty () const;

  std::string as_string () const;

  std::vector<Lifetime> &get_lifetime_args () { return lifetime_args; }
  const std::vector<Lifetime> &get_lifetime_args () const
  {
    return lifetime_args;
  }

  std::vector<std::unique_ptr<Type> > &get_type_args () { return type_args; }

  std::vector<GenericArgsBinding> &get_binding_args () { return binding_args; }

  std::vector<ConstGenericArg> &get_const_args () { return const_args; }

  location_t get_locus () const { return locus; }
};

/* A segment of a path in expression, including an identifier aspect and maybe
 * generic args */
class PathExprSegment
{
private:
  Analysis::NodeMapping mappings;
  PathIdentSegment segment_name;
  GenericArgs generic_args;
  location_t locus;

public:
  PathExprSegment (Analysis::NodeMapping mappings,
		   PathIdentSegment segment_name, location_t locus,
		   GenericArgs generic_args);

  PathExprSegment (PathExprSegment const &other);

  PathExprSegment &operator= (PathExprSegment const &other);

  // move constructors
  PathExprSegment (PathExprSegment &&other) = default;
  PathExprSegment &operator= (PathExprSegment &&other) = default;

  std::string as_string () const;

  location_t get_locus () const { return locus; }

  PathIdentSegment &get_segment () { return segment_name; }
  const PathIdentSegment &get_segment () const { return segment_name; }

  GenericArgs &get_generic_args () { return generic_args; }

  const Analysis::NodeMapping &get_mappings () const { return mappings; }

  bool has_generic_args () const { return generic_args.has_generic_args (); }
};

// HIR node representing a pattern that involves a "path" - abstract base class
class PathPattern : public Pattern
{
public:
  enum class Kind
  {
    Segmented,
    LangItem
  };

private:
  std::vector<PathExprSegment> segments;
  tl::optional<LangItem::Kind> lang_item;
  Kind kind;

protected:
  PathPattern (std::vector<PathExprSegment> segments)
    : segments (std::move (segments)), lang_item (tl::nullopt),
      kind (Kind::Segmented)
  {}

  PathPattern (LangItem::Kind lang_item)
    : segments ({}), lang_item (lang_item), kind (Kind::LangItem)
  {}

  // Returns whether path has segments.
  bool has_segments () const
  {
    rust_assert (kind == Kind::Segmented);
    return !segments.empty ();
  }

  /* Converts path segments to their equivalent SimplePath segments if possible,
   * and creates a SimplePath from them. */
  AST::SimplePath
  convert_to_simple_path (bool with_opening_scope_resolution) const;

public:
  /* Returns whether the path is a single segment (excluding qualified path
   * initial as segment). */
  bool is_single_segment () const
  {
    rust_assert (kind == Kind::Segmented);
    return segments.size () == 1;
  }

  std::string as_string () const override;

  void iterate_path_segments (std::function<bool (PathExprSegment &)> cb);

  size_t get_num_segments () const
  {
    rust_assert (kind == Kind::Segmented);
    return segments.size ();
  }

  std::vector<PathExprSegment> &get_segments ()
  {
    rust_assert (kind == Kind::Segmented);
    return segments;
  }

  const std::vector<PathExprSegment> &get_segments () const
  {
    rust_assert (kind == Kind::Segmented);
    return segments;
  }

  PathExprSegment &get_root_seg ()
  {
    rust_assert (kind == Kind::Segmented);
    return segments.at (0);
  }

  const PathExprSegment &get_final_segment () const
  {
    rust_assert (kind == Kind::Segmented);
    return segments.back ();
  }

  LangItem::Kind get_lang_item () const
  {
    rust_assert (kind == Kind::LangItem);

    return *lang_item;
  }

  PatternType get_pattern_type () const override final
  {
    return PatternType::PATH;
  }

  bool is_lang_item () const { return kind == Kind::LangItem; }

  Kind get_path_kind () const { return kind; }
};

/* HIR node representing a path-in-expression pattern (path that allows generic
 * arguments) */
class PathInExpression : public PathPattern, public PathExpr
{
  bool has_opening_scope_resolution;
  location_t locus;

public:
  std::string as_string () const override;

  // Constructor
  PathInExpression (Analysis::NodeMapping mappings,
		    std::vector<PathExprSegment> path_segments,
		    location_t locus = UNDEF_LOCATION,
		    bool has_opening_scope_resolution = false,
		    std::vector<AST::Attribute> outer_attrs
		    = std::vector<AST::Attribute> ());

  // lang-item Constructor
  PathInExpression (Analysis::NodeMapping mappings, LangItem::Kind kind,
		    location_t locus = UNDEF_LOCATION,
		    bool has_opening_scope_resolution = false,
		    std::vector<AST::Attribute> outer_attrs
		    = std::vector<AST::Attribute> ());

  // Creates an error state path in expression.
  static PathInExpression create_error ()
  {
    return PathInExpression (Analysis::NodeMapping::get_error (),
			     std::vector<PathExprSegment> ());
  }

  // Returns whether path in expression is in an error state.
  bool is_error () const { return !has_segments (); }

  /* Converts PathInExpression to SimplePath if possible (i.e. no generic
   * arguments). Otherwise returns an empty SimplePath. */
  AST::SimplePath as_simple_path () const
  {
    /* delegate to parent class as can't access segments. however,
     * QualifiedPathInExpression conversion to simple path wouldn't make sense,
     * so the method in the parent class should be protected, not public. Have
     * to pass in opening scope resolution as parent class has no access to it.
     */
    return convert_to_simple_path (has_opening_scope_resolution);
  }

  location_t get_locus () const override final { return locus; }

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;
  void accept_vis (HIRPatternVisitor &vis) override;

  bool opening_scope_resolution () { return has_opening_scope_resolution; }

  bool is_self () const;

  const Analysis::NodeMapping &get_mappings () const override final
  {
    return mappings;
  }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  PathInExpression *clone_pattern_impl () const override
  {
    return new PathInExpression (*this);
  }

  /* Use covariance to implement clone function as returning this object rather
   * than base */
  PathInExpression *clone_expr_without_block_impl () const override
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
  Analysis::NodeMapping mappings;
  tl::optional<PathIdentSegment> ident_segment;
  tl::optional<LangItem::Kind> lang_item;
  location_t locus;

protected:
  bool has_separating_scope_resolution;
  SegmentType type;

public:
  // Clone function implementation - not pure virtual as overrided by subclasses
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

  TypePathSegment (Analysis::NodeMapping mappings,
		   PathIdentSegment ident_segment,
		   bool has_separating_scope_resolution, location_t locus);

  TypePathSegment (Analysis::NodeMapping mappings, LangItem::Kind lang_item,
		   location_t locus);

  TypePathSegment (Analysis::NodeMapping mappings, std::string segment_name,
		   bool has_separating_scope_resolution, location_t locus);

  virtual std::string as_string () const
  {
    if (ident_segment)
      return ident_segment->as_string ();

    return LangItem::PrettyString (*lang_item);
  }

  /* Returns whether the type path segment is in an error state. May be virtual
   * in future. */
  bool is_error () const
  {
    rust_assert (ident_segment);
    return ident_segment->is_error ();
  }

  /* Returns whether segment is identifier only (as opposed to generic args or
   * function). Overriden in derived classes with other segments. */
  virtual bool is_ident_only () const { return true; }

  location_t get_locus () const { return locus; }

  // not pure virtual as class not abstract
  virtual void accept_vis (HIRFullVisitor &vis);

  const Analysis::NodeMapping &get_mappings () const { return mappings; }

  const PathIdentSegment &get_ident_segment () const
  {
    rust_assert (ident_segment);
    return *ident_segment;
  }

  const LangItem::Kind &get_lang_item () const
  {
    rust_assert (lang_item);
    return *lang_item;
  }

  bool is_generic_segment () const
  {
    return get_type () == SegmentType::GENERIC;
  }

  bool is_lang_item () const { return lang_item.has_value (); }
};

// Segment used in type path with generic args
class TypePathSegmentGeneric : public TypePathSegment
{
  GenericArgs generic_args;

public:
  bool has_generic_args () const { return generic_args.has_generic_args (); }

  bool is_ident_only () const override { return false; }

  // Constructor with PathIdentSegment and GenericArgs
  TypePathSegmentGeneric (Analysis::NodeMapping mappings,
			  PathIdentSegment ident_segment,
			  bool has_separating_scope_resolution,
			  GenericArgs generic_args, location_t locus);

  TypePathSegmentGeneric (Analysis::NodeMapping mappings,
			  LangItem::Kind lang_item, GenericArgs generic_args,
			  location_t locus);

  // Constructor from segment name and all args
  TypePathSegmentGeneric (Analysis::NodeMapping mappings,
			  std::string segment_name,
			  bool has_separating_scope_resolution,
			  std::vector<Lifetime> lifetime_args,
			  std::vector<std::unique_ptr<Type> > type_args,
			  std::vector<GenericArgsBinding> binding_args,
			  std::vector<ConstGenericArg> const_args,
			  location_t locus);

  std::string as_string () const override;

  void accept_vis (HIRFullVisitor &vis) override;

  GenericArgs &get_generic_args () { return generic_args; }

  virtual SegmentType get_type () const override final
  {
    return SegmentType::GENERIC;
  }

  // Use covariance to override base class method
  TypePathSegmentGeneric *clone_type_path_segment_impl () const override
  {
    return new TypePathSegmentGeneric (*this);
  }
};

// A function as represented in a type path
class TypePathFunction
{
  std::vector<std::unique_ptr<Type> > inputs;
  std::unique_ptr<Type> return_type;

public:
  // Returns whether the return type of the function has been specified.
  bool has_return_type () const { return return_type != nullptr; }

  // Returns whether the function has inputs.
  bool has_inputs () const { return !inputs.empty (); }

  // Constructor
  TypePathFunction (std::vector<std::unique_ptr<Type> > inputs,
		    std::unique_ptr<Type> type);

  // Copy constructor with clone
  TypePathFunction (TypePathFunction const &other);

  ~TypePathFunction () = default;

  // Overloaded assignment operator to clone type
  TypePathFunction &operator= (TypePathFunction const &other);

  // move constructors
  TypePathFunction (TypePathFunction &&other) = default;
  TypePathFunction &operator= (TypePathFunction &&other) = default;

  std::string as_string () const;

  const std::vector<std::unique_ptr<Type> > &get_params () const
  {
    return inputs;
  };
  std::vector<std::unique_ptr<Type> > &get_params () { return inputs; };

  const Type &get_return_type () const { return *return_type; };
  Type &get_return_type () { return *return_type; };
};

// Segment used in type path with a function argument
class TypePathSegmentFunction : public TypePathSegment
{
  TypePathFunction function_path;

public:
  // Constructor with PathIdentSegment and TypePathFn
  TypePathSegmentFunction (Analysis::NodeMapping mappings,
			   PathIdentSegment ident_segment,
			   bool has_separating_scope_resolution,
			   TypePathFunction function_path, location_t locus);

  // Constructor with segment name and TypePathFn
  TypePathSegmentFunction (Analysis::NodeMapping mappings,
			   std::string segment_name,
			   bool has_separating_scope_resolution,
			   TypePathFunction function_path, location_t locus);

  std::string as_string () const override;

  bool is_ident_only () const override { return false; }

  void accept_vis (HIRFullVisitor &vis) override;

  SegmentType get_type () const override final { return SegmentType::FUNCTION; }

  TypePathFunction &get_function_path () { return function_path; }

  // Use covariance to override base class method
  TypePathSegmentFunction *clone_type_path_segment_impl () const override
  {
    return new TypePathSegmentFunction (*this);
  }
};

// Path used inside types
class TypePath : public TypeNoBounds
{
public:
  bool has_opening_scope_resolution;
  std::vector<std::unique_ptr<TypePathSegment> > segments;

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  TypePath *clone_type_impl () const override { return new TypePath (*this); }

  /* Use covariance to implement clone function as returning this object rather
   * than base */
  TypePath *clone_type_no_bounds_impl () const override
  {
    return new TypePath (*this);
  }

public:
  /* Returns whether the TypePath has an opening scope resolution operator (i.e.
   * is global path or crate-relative path, not module-relative) */
  bool has_opening_scope_resolution_op () const
  {
    return has_opening_scope_resolution;
  }

  // Returns whether the TypePath is in an invalid state.
  bool is_error () const { return segments.empty (); }

  // Creates an error state TypePath.
  static TypePath create_error ()
  {
    return TypePath (Analysis::NodeMapping::get_error (),
		     std::vector<std::unique_ptr<TypePathSegment> > (),
		     UNDEF_LOCATION);
  }

  // Constructor
  TypePath (Analysis::NodeMapping mappings,
	    std::vector<std::unique_ptr<TypePathSegment> > segments,
	    location_t locus, bool has_opening_scope_resolution = false);

  // Copy constructor with vector clone
  TypePath (TypePath const &other);

  // Overloaded assignment operator with clone
  TypePath &operator= (TypePath const &other);

  // move constructors
  TypePath (TypePath &&other) = default;
  TypePath &operator= (TypePath &&other) = default;

  std::string as_string () const override;

  /* Converts TypePath to SimplePath if possible (i.e. no generic or function
   * arguments). Otherwise returns an empty SimplePath. */
  AST::SimplePath as_simple_path () const;

  // Creates a trait bound with a clone of this type path as its only element.
  std::unique_ptr<TraitBound> to_trait_bound (bool in_parens) const override;

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRTypeVisitor &vis) override;

  size_t get_num_segments () const { return segments.size (); }

  std::vector<std::unique_ptr<TypePathSegment> > &get_segments ()
  {
    return segments;
  }

  TypePathSegment &get_final_segment () { return *segments.back (); }
};

class QualifiedPathType
{
  std::unique_ptr<Type> type;
  std::unique_ptr<TypePath> trait;
  location_t locus;
  Analysis::NodeMapping mappings;

public:
  // Constructor
  QualifiedPathType (Analysis::NodeMapping mappings, std::unique_ptr<Type> type,
		     std::unique_ptr<TypePath> trait, location_t locus);

  // Copy constructor uses custom deep copy for Type to preserve polymorphism
  QualifiedPathType (QualifiedPathType const &other);

  // default destructor
  ~QualifiedPathType () = default;

  // overload assignment operator to use custom clone method
  QualifiedPathType &operator= (QualifiedPathType const &other);

  // move constructor
  QualifiedPathType (QualifiedPathType &&other) = default;
  QualifiedPathType &operator= (QualifiedPathType &&other) = default;

  // Returns whether the qualified path type has a rebind as clause.
  bool has_as_clause () const { return trait != nullptr; }

  std::string as_string () const;

  location_t get_locus () const { return locus; }

  Analysis::NodeMapping get_mappings () const { return mappings; }

  bool has_type () { return type != nullptr; }
  bool has_trait () { return trait != nullptr; }

  Type &get_type ()
  {
    rust_assert (type);
    return *type;
  }

  TypePath &get_trait ()
  {
    rust_assert (trait);
    return *trait;
  }

  bool trait_has_generic_args () const;

  GenericArgs &get_trait_generic_args ();
};

/* HIR node representing a qualified path-in-expression pattern (path that
 * allows specifying trait functions) */
class QualifiedPathInExpression : public PathPattern, public PathExpr
{
  QualifiedPathType path_type;
  location_t locus;

public:
  std::string as_string () const override;

  QualifiedPathInExpression (Analysis::NodeMapping mappings,
			     QualifiedPathType qual_path_type,
			     std::vector<PathExprSegment> path_segments,
			     location_t locus = UNDEF_LOCATION,
			     std::vector<AST::Attribute> outer_attrs
			     = std::vector<AST::Attribute> ());

  // lang-item constructor
  QualifiedPathInExpression (Analysis::NodeMapping mappings,
			     QualifiedPathType qual_path_type,
			     LangItem::Kind lang_item,
			     location_t locus = UNDEF_LOCATION,
			     std::vector<AST::Attribute> outer_attrs
			     = std::vector<AST::Attribute> ());

  location_t get_locus () const override final { return locus; }

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExpressionVisitor &vis) override;
  void accept_vis (HIRPatternVisitor &vis) override;

  QualifiedPathType &get_path_type () { return path_type; }

  location_t get_locus () { return locus; }

  const Analysis::NodeMapping &get_mappings () const override final
  {
    return mappings;
  }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  QualifiedPathInExpression *clone_pattern_impl () const override
  {
    return new QualifiedPathInExpression (*this);
  }

  /* Use covariance to implement clone function as returning this object rather
   * than base */
  QualifiedPathInExpression *clone_expr_without_block_impl () const override
  {
    return new QualifiedPathInExpression (*this);
  }
};

/* Represents a qualified path in a type; used for disambiguating trait function
 * calls */
class QualifiedPathInType : public TypeNoBounds
{
  QualifiedPathType path_type;
  std::unique_ptr<TypePathSegment> associated_segment;
  std::vector<std::unique_ptr<TypePathSegment> > segments;

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  QualifiedPathInType *clone_type_impl () const override
  {
    return new QualifiedPathInType (*this);
  }

  /* Use covariance to implement clone function as returning this object rather
   * than base */
  QualifiedPathInType *clone_type_no_bounds_impl () const override
  {
    return new QualifiedPathInType (*this);
  }

public:
  QualifiedPathInType (
    Analysis::NodeMapping mappings, QualifiedPathType qual_path_type,
    std::unique_ptr<TypePathSegment> associated_segment,
    std::vector<std::unique_ptr<TypePathSegment> > path_segments,
    location_t locus = UNDEF_LOCATION);

  // Copy constructor with vector clone
  QualifiedPathInType (QualifiedPathInType const &other);

  // Overloaded assignment operator with vector clone
  QualifiedPathInType &operator= (QualifiedPathInType const &other);

  // move constructors
  QualifiedPathInType (QualifiedPathInType &&other) = default;
  QualifiedPathInType &operator= (QualifiedPathInType &&other) = default;

  std::string as_string () const override;

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRTypeVisitor &vis) override;

  QualifiedPathType &get_path_type () { return path_type; }

  TypePathSegment &get_associated_segment () { return *associated_segment; }

  std::vector<std::unique_ptr<TypePathSegment> > &get_segments ()
  {
    return segments;
  }
};

} // namespace HIR
} // namespace Rust

#endif
