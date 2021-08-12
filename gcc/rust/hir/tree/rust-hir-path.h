// Copyright (C) 2020 Free Software Foundation, Inc.

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

#include "rust-hir.h"

#include <string>
#include <vector>

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
struct GenericArgsBinding
{
private:
  Identifier identifier;
  std::unique_ptr<Type> type;

  Location locus;

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
    return GenericArgsBinding ("", nullptr);
  }

  // Pointer type for type in constructor to enable polymorphism
  GenericArgsBinding (Identifier ident, std::unique_ptr<Type> type_ptr,
		      Location locus = Location ())
    : identifier (std::move (ident)), type (std::move (type_ptr)), locus (locus)
  {}

  // Copy constructor has to deep copy the type as it is a unique pointer
  GenericArgsBinding (GenericArgsBinding const &other)
    : identifier (other.identifier), type (other.type->clone_type ()),
      locus (other.locus)
  {}

  // default destructor
  ~GenericArgsBinding () = default;

  // Overload assignment operator to deep copy the pointed-to type
  GenericArgsBinding &operator= (GenericArgsBinding const &other)
  {
    identifier = other.identifier;
    type = other.type->clone_type ();
    locus = other.locus;
    return *this;
  }

  // move constructors
  GenericArgsBinding (GenericArgsBinding &&other) = default;
  GenericArgsBinding &operator= (GenericArgsBinding &&other) = default;

  std::string as_string () const;

  Identifier get_identifier () const { return identifier; }

  std::unique_ptr<Type> &get_type () { return type; }

  Location get_locus () const { return locus; }
};

// Generic arguments allowed in each path expression segment - inline?
struct GenericArgs
{
  std::vector<Lifetime> lifetime_args;
  std::vector<std::unique_ptr<Type> > type_args;
  std::vector<GenericArgsBinding> binding_args;
  Location locus;

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
	       Location locus = Location ())
    : lifetime_args (std::move (lifetime_args)),
      type_args (std::move (type_args)),
      binding_args (std::move (binding_args)), locus (locus)
  {}

  // copy constructor with vector clone
  GenericArgs (GenericArgs const &other)
    : lifetime_args (other.lifetime_args), binding_args (other.binding_args),
      locus (other.locus)
  {
    type_args.reserve (other.type_args.size ());
    for (const auto &e : other.type_args)
      type_args.push_back (e->clone_type ());
  }

  ~GenericArgs () = default;

  // overloaded assignment operator to vector clone
  GenericArgs &operator= (GenericArgs const &other)
  {
    lifetime_args = other.lifetime_args;
    binding_args = other.binding_args;
    locus = other.locus;

    type_args.reserve (other.type_args.size ());
    for (const auto &e : other.type_args)
      type_args.push_back (e->clone_type ());

    return *this;
  }

  // move constructors
  GenericArgs (GenericArgs &&other) = default;
  GenericArgs &operator= (GenericArgs &&other) = default;

  // Creates an empty GenericArgs (no arguments)
  static GenericArgs create_empty (Location locus = Location ())
  {
    return GenericArgs (std::vector<Lifetime> (),
			std::vector<std::unique_ptr<Type> > (),
			std::vector<GenericArgsBinding> (), locus);
  }

  bool is_empty () const
  {
    return lifetime_args.size () == 0 && type_args.size () == 0
	   && binding_args.size () == 0;
  }

  std::string as_string () const;

  std::vector<Lifetime> &get_lifetime_args () { return lifetime_args; }

  std::vector<std::unique_ptr<Type> > &get_type_args () { return type_args; }

  std::vector<GenericArgsBinding> &get_binding_args () { return binding_args; }

  Location get_locus () const { return locus; }
};

/* A segment of a path in expression, including an identifier aspect and maybe
 * generic args */
class PathExprSegment
{
private:
  Analysis::NodeMapping mappings;
  PathIdentSegment segment_name;
  GenericArgs generic_args;
  Location locus;

public:
  // Returns true if there are any generic arguments
  bool has_generic_args () const { return generic_args.has_generic_args (); }

  // Constructor for segment (from IdentSegment and GenericArgs)
  PathExprSegment (Analysis::NodeMapping mappings,
		   PathIdentSegment segment_name, Location locus = Location (),
		   GenericArgs generic_args = GenericArgs::create_empty ())
    : mappings (std::move (mappings)), segment_name (std::move (segment_name)),
      generic_args (std::move (generic_args)), locus (locus)
  {}

  std::string as_string () const;

  Location get_locus () const { return locus; }

  PathIdentSegment get_segment () const { return segment_name; }

  GenericArgs &get_generic_args () { return generic_args; }

  const Analysis::NodeMapping &get_mappings () const { return mappings; }
};

// HIR node representing a pattern that involves a "path" - abstract base class
class PathPattern : public Pattern
{
  std::vector<PathExprSegment> segments;

protected:
  PathPattern (std::vector<PathExprSegment> segments)
    : segments (std::move (segments))
  {}

  // Returns whether path has segments.
  bool has_segments () const { return !segments.empty (); }

  /* Converts path segments to their equivalent SimplePath segments if possible,
   * and creates a SimplePath from them. */
  AST::SimplePath
  convert_to_simple_path (bool with_opening_scope_resolution) const;

public:
  /* Returns whether the path is a single segment (excluding qualified path
   * initial as segment). */
  bool is_single_segment () const { return segments.size () == 1; }

  std::string as_string () const override;

  void iterate_path_segments (std::function<bool (PathExprSegment &)> cb)
  {
    for (auto it = segments.begin (); it != segments.end (); it++)
      {
	if (!cb (*it))
	  return;
      }
  }

  size_t get_num_segments () const { return segments.size (); }

  std::vector<PathExprSegment> &get_segments () { return segments; }

  PathExprSegment &get_root_seg () { return segments.at (0); }

  PathExprSegment get_final_segment () const { return segments.back (); }
};

/* HIR node representing a path-in-expression pattern (path that allows generic
 * arguments) */
class PathInExpression : public PathPattern, public PathExpr
{
  bool has_opening_scope_resolution;
  Location locus;

public:
  std::string as_string () const override;

  // Constructor
  PathInExpression (Analysis::NodeMapping mappings,
		    std::vector<PathExprSegment> path_segments,
		    Location locus = Location (),
		    bool has_opening_scope_resolution = false,
		    std::vector<AST::Attribute> outer_attrs
		    = std::vector<AST::Attribute> ())
    : PathPattern (std::move (path_segments)),
      PathExpr (std::move (mappings), std::move (outer_attrs)),
      has_opening_scope_resolution (has_opening_scope_resolution), locus (locus)
  {}

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

  Location get_locus () const { return locus; }
  Location get_locus_slow () const override { return get_locus (); }

  void accept_vis (HIRVisitor &vis) override;

  bool opening_scope_resolution () { return has_opening_scope_resolution; }

  bool is_self () const
  {
    if (!is_single_segment ())
      return false;

    return get_final_segment ().get_segment ().as_string ().compare ("self")
	   == 0;
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
  PathIdentSegment ident_segment;
  Location locus;

protected:
  bool has_separating_scope_resolution;
  SegmentType type;

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
		   bool has_separating_scope_resolution, Location locus)
    : mappings (std::move (mappings)),
      ident_segment (std::move (ident_segment)), locus (locus),
      has_separating_scope_resolution (has_separating_scope_resolution),
      type (SegmentType::REG)
  {}

  TypePathSegment (Analysis::NodeMapping mappings, std::string segment_name,
		   bool has_separating_scope_resolution, Location locus)
    : mappings (std::move (mappings)),
      ident_segment (PathIdentSegment (std::move (segment_name))),
      locus (locus),
      has_separating_scope_resolution (has_separating_scope_resolution),
      type (SegmentType::REG)
  {}

  virtual std::string as_string () const { return ident_segment.as_string (); }

  /* Returns whether the type path segment is in an error state. May be virtual
   * in future. */
  bool is_error () const { return ident_segment.is_error (); }

  /* Returns whether segment is identifier only (as opposed to generic args or
   * function). Overriden in derived classes with other segments. */
  virtual bool is_ident_only () const { return true; }

  Location get_locus () const { return locus; }

  // not pure virtual as class not abstract
  virtual void accept_vis (HIRVisitor &vis);

  const Analysis::NodeMapping &get_mappings () const { return mappings; }

  const PathIdentSegment &get_ident_segment () const { return ident_segment; }
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
			  GenericArgs generic_args, Location locus)
    : TypePathSegment (std::move (mappings), std::move (ident_segment),
		       has_separating_scope_resolution, locus),
      generic_args (std::move (generic_args))
  {}

  // Constructor from segment name and all args
  TypePathSegmentGeneric (Analysis::NodeMapping mappings,
			  std::string segment_name,
			  bool has_separating_scope_resolution,
			  std::vector<Lifetime> lifetime_args,
			  std::vector<std::unique_ptr<Type> > type_args,
			  std::vector<GenericArgsBinding> binding_args,
			  Location locus)
    : TypePathSegment (std::move (mappings), std::move (segment_name),
		       has_separating_scope_resolution, locus),
      generic_args (GenericArgs (std::move (lifetime_args),
				 std::move (type_args),
				 std::move (binding_args)))
  {}

  std::string as_string () const override;

  void accept_vis (HIRVisitor &vis) override;

  GenericArgs &get_generic_args () { return generic_args; }

  virtual SegmentType get_type () const override final
  {
    return SegmentType::GENERIC;
  }

protected:
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
  std::vector<std::unique_ptr<Type> > inputs;

  // bool has_type;
  std::unique_ptr<Type> return_type;

  // FIXME: think of better way to mark as invalid than taking up storage
  bool is_invalid;

  // TODO: should this have location info?

protected:
  // Constructor only used to create invalid type path functions.
  TypePathFunction (bool is_invalid) : is_invalid (is_invalid) {}

public:
  // Returns whether the return type of the function has been specified.
  bool has_return_type () const { return return_type != nullptr; }

  // Returns whether the function has inputs.
  bool has_inputs () const { return !inputs.empty (); }

  // Returns whether function is in an error state.
  bool is_error () const { return is_invalid; }

  // Creates an error state function.
  static TypePathFunction create_error () { return TypePathFunction (true); }

  // Constructor
  TypePathFunction (std::vector<std::unique_ptr<Type> > inputs,
		    Type *type = nullptr)
    : inputs (std::move (inputs)), return_type (type), is_invalid (false)
  {}
  // FIXME: deprecated

  // Constructor
  TypePathFunction (std::vector<std::unique_ptr<Type> > inputs,
		    std::unique_ptr<Type> type = nullptr)
    : inputs (std::move (inputs)), return_type (std::move (type)),
      is_invalid (false)
  {}

  // Copy constructor with clone
  TypePathFunction (TypePathFunction const &other)
    : return_type (other.return_type->clone_type ()),
      is_invalid (other.is_invalid)
  {
    inputs.reserve (other.inputs.size ());
    for (const auto &e : other.inputs)
      inputs.push_back (e->clone_type ());
  }

  ~TypePathFunction () = default;

  // Overloaded assignment operator to clone type
  TypePathFunction &operator= (TypePathFunction const &other)
  {
    return_type = other.return_type->clone_type ();
    is_invalid = other.is_invalid;

    inputs.reserve (other.inputs.size ());
    for (const auto &e : other.inputs)
      inputs.push_back (e->clone_type ());

    return *this;
  }

  // move constructors
  TypePathFunction (TypePathFunction &&other) = default;
  TypePathFunction &operator= (TypePathFunction &&other) = default;

  std::string as_string () const;
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
			   TypePathFunction function_path, Location locus)
    : TypePathSegment (std::move (mappings), std::move (ident_segment),
		       has_separating_scope_resolution, locus),
      function_path (std::move (function_path))
  {}

  // Constructor with segment name and TypePathFn
  TypePathSegmentFunction (Analysis::NodeMapping mappings,
			   std::string segment_name,
			   bool has_separating_scope_resolution,
			   TypePathFunction function_path, Location locus)
    : TypePathSegment (std::move (mappings), std::move (segment_name),
		       has_separating_scope_resolution, locus),
      function_path (std::move (function_path))
  {}

  std::string as_string () const override;

  bool is_ident_only () const override { return false; }

  void accept_vis (HIRVisitor &vis) override;

  virtual SegmentType get_type () const override final
  {
    return SegmentType::FUNCTION;
  }

protected:
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
  Location locus;

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
		     Location ());
  }

  // Constructor
  TypePath (Analysis::NodeMapping mappings,
	    std::vector<std::unique_ptr<TypePathSegment> > segments,
	    Location locus, bool has_opening_scope_resolution = false)
    : TypeNoBounds (mappings),
      has_opening_scope_resolution (has_opening_scope_resolution),
      segments (std::move (segments)), locus (locus)
  {}

  // Copy constructor with vector clone
  TypePath (TypePath const &other)
    : TypeNoBounds (other.mappings),
      has_opening_scope_resolution (other.has_opening_scope_resolution),
      locus (other.locus)
  {
    segments.reserve (other.segments.size ());
    for (const auto &e : other.segments)
      segments.push_back (e->clone_type_path_segment ());
  }

  // Overloaded assignment operator with clone
  TypePath &operator= (TypePath const &other)
  {
    has_opening_scope_resolution = other.has_opening_scope_resolution;
    locus = other.locus;
    mappings = other.mappings;

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
  AST::SimplePath as_simple_path () const;

  // Creates a trait bound with a clone of this type path as its only element.
  TraitBound *to_trait_bound (bool in_parens) const override;

  Location get_locus () const { return locus; }

  void accept_vis (HIRVisitor &vis) override;

  size_t get_num_segments () const { return segments.size (); }

  std::vector<std::unique_ptr<TypePathSegment> > &get_segments ()
  {
    return segments;
  }

  std::unique_ptr<TypePathSegment> &get_final_segment ()
  {
    return segments.back ();
  }
};

struct QualifiedPathType
{
private:
  std::unique_ptr<Type> type;
  std::unique_ptr<TypePath> trait;
  Location locus;
  Analysis::NodeMapping mappings;

public:
  // Constructor
  QualifiedPathType (Analysis::NodeMapping mappings, std::unique_ptr<Type> type,
		     std::unique_ptr<TypePath> trait, Location locus)
    : type (std::move (type)), trait (std::move (trait)), locus (locus),
      mappings (mappings)
  {}

  // Copy constructor uses custom deep copy for Type to preserve polymorphism
  QualifiedPathType (QualifiedPathType const &other)
    : type (other.type->clone_type ()),
      trait (other.has_as_clause () ? std::unique_ptr<HIR::TypePath> (
	       new HIR::TypePath (*other.trait))
				    : nullptr),
      locus (other.locus), mappings (other.mappings)
  {}

  // default destructor
  ~QualifiedPathType () = default;

  // overload assignment operator to use custom clone method
  QualifiedPathType &operator= (QualifiedPathType const &other)
  {
    type = other.type->clone_type ();
    locus = other.locus;
    mappings = other.mappings;
    trait
      = other.has_as_clause ()
	  ? std::unique_ptr<HIR::TypePath> (new HIR::TypePath (*other.trait))
	  : nullptr;

    return *this;
  }

  // move constructor
  QualifiedPathType (QualifiedPathType &&other) = default;
  QualifiedPathType &operator= (QualifiedPathType &&other) = default;

  // Returns whether the qualified path type has a rebind as clause.
  bool has_as_clause () const { return trait != nullptr; }

  std::string as_string () const;

  Location get_locus () const { return locus; }

  Analysis::NodeMapping get_mappings () const { return mappings; }

  std::unique_ptr<Type> &get_type () { return type; }

  std::unique_ptr<TypePath> &get_trait ()
  {
    rust_assert (has_as_clause ());
    return trait;
  }
};

/* HIR node representing a qualified path-in-expression pattern (path that
 * allows specifying trait functions) */
class QualifiedPathInExpression : public PathPattern, public PathExpr
{
  QualifiedPathType path_type;
  Location locus;

public:
  std::string as_string () const override;

  QualifiedPathInExpression (Analysis::NodeMapping mappings,
			     QualifiedPathType qual_path_type,
			     std::vector<PathExprSegment> path_segments,
			     Location locus = Location (),
			     std::vector<AST::Attribute> outer_attrs
			     = std::vector<AST::Attribute> ())
    : PathPattern (std::move (path_segments)),
      PathExpr (std::move (mappings), std::move (outer_attrs)),
      path_type (std::move (qual_path_type)), locus (locus)
  {}

  Location get_locus () const { return locus; }
  Location get_locus_slow () const override { return get_locus (); }

  void accept_vis (HIRVisitor &vis) override;

  QualifiedPathType &get_path_type () { return path_type; }

  Location get_locus () { return locus; }

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
  std::vector<std::unique_ptr<TypePathSegment> > segments;
  Location locus;

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
    std::vector<std::unique_ptr<TypePathSegment> > path_segments,
    Location locus = Location ())
    : TypeNoBounds (mappings), path_type (std::move (qual_path_type)),
      segments (std::move (path_segments)), locus (locus)
  {}

  /* TODO: maybe make a shortcut constructor that has QualifiedPathType elements
   * as params */

  // Copy constructor with vector clone
  QualifiedPathInType (QualifiedPathInType const &other)
    : TypeNoBounds (other.mappings), path_type (other.path_type),
      locus (other.locus)
  {
    segments.reserve (other.segments.size ());
    for (const auto &e : other.segments)
      segments.push_back (e->clone_type_path_segment ());

    // Untested.
    gcc_unreachable ();
  }

  // Overloaded assignment operator with vector clone
  QualifiedPathInType &operator= (QualifiedPathInType const &other)
  {
    path_type = other.path_type;
    locus = other.locus;
    mappings = other.mappings;

    segments.reserve (other.segments.size ());
    for (const auto &e : other.segments)
      segments.push_back (e->clone_type_path_segment ());

    return *this;
  }

  // move constructors
  QualifiedPathInType (QualifiedPathInType &&other) = default;
  QualifiedPathInType &operator= (QualifiedPathInType &&other) = default;

  std::string as_string () const override;

  void accept_vis (HIRVisitor &vis) override;
};
} // namespace HIR
} // namespace Rust

#endif
