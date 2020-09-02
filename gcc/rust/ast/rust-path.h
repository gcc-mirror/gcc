#ifndef RUST_AST_PATH_H
#define RUST_AST_PATH_H
/* "Path" (identifier within namespaces, essentially) handling. Required include
 * for virtually all AST-related functionality. */

#include "rust-ast.h"

#include <string>
#include <vector>

namespace Rust {
namespace AST {

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
};

// Generic arguments allowed in each path expression segment - inline?
struct GenericArgs
{
  std::vector<Lifetime> lifetime_args;
  std::vector<std::unique_ptr<Type>> type_args;
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
	       std::vector<std::unique_ptr<Type>> type_args,
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
  static GenericArgs create_empty ()
  {
    return GenericArgs (std::vector<Lifetime> (),
			std::vector<std::unique_ptr<Type>> (),
			std::vector<GenericArgsBinding> ());
  }

  std::string as_string () const;
};

/* A segment of a path in expression, including an identifier aspect and maybe
 * generic args */
class PathExprSegment
{ // or should this extend PathIdentSegment?
private:
  PathIdentSegment segment_name;

  // bool has_generic_args;
  GenericArgs generic_args;

  Location locus;

  // TODO: does this require visitor? pretty sure not polymorphic

public:
  // Returns true if there are any generic arguments
  bool has_generic_args () const { return generic_args.has_generic_args (); }

  // Constructor for segment (from IdentSegment and GenericArgs)
  PathExprSegment (PathIdentSegment segment_name, Location locus = Location (),
		   GenericArgs generic_args = GenericArgs::create_empty ())
    : segment_name (std::move (segment_name)),
      generic_args (std::move (generic_args)), locus (locus)
  {}

  /* Constructor for segment with generic arguments (from segment name and all
   * args) */
  PathExprSegment (std::string segment_name, Location locus,
		   std::vector<Lifetime> lifetime_args
		   = std::vector<Lifetime> (),
		   std::vector<std::unique_ptr<Type>> type_args
		   = std::vector<std::unique_ptr<Type>> (),
		   std::vector<GenericArgsBinding> binding_args
		   = std::vector<GenericArgsBinding> ())
    : segment_name (PathIdentSegment (std::move (segment_name))),
      generic_args (GenericArgs (std::move (lifetime_args),
				 std::move (type_args),
				 std::move (binding_args))),
      locus (locus)
  {}

  // Returns whether path expression segment is in an error state.
  bool is_error () const { return segment_name.is_error (); }

  // Creates an error-state path expression segment.
  static PathExprSegment create_error ()
  {
    return PathExprSegment (PathIdentSegment::create_error ());
  }

  std::string as_string () const;

  Location get_locus () const { return locus; }
};

// AST node representing a pattern that involves a "path" - abstract base class
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
  SimplePath convert_to_simple_path (bool with_opening_scope_resolution) const;

public:
  /* Returns whether the path is a single segment (excluding qualified path
   * initial as segment). */
  bool is_single_segment () const { return segments.size () == 1; }

  std::string as_string () const override;
};

/* AST node representing a path-in-expression pattern (path that allows generic
 * arguments) */
class PathInExpression : public PathPattern, public PathExpr
{
  bool has_opening_scope_resolution;
  Location locus;

public:
  std::string as_string () const override;

  // Constructor
  PathInExpression (std::vector<PathExprSegment> path_segments,
		    Location locus = Location (),
		    bool has_opening_scope_resolution = false,
		    std::vector<Attribute> outer_attrs
		    = std::vector<Attribute> ())
    : PathPattern (std::move (path_segments)),
      PathExpr (std::move (outer_attrs)),
      has_opening_scope_resolution (has_opening_scope_resolution), locus (locus)
  {}

  // Creates an error state path in expression.
  static PathInExpression create_error ()
  {
    return PathInExpression (std::vector<PathExprSegment> ());
  }

  // Returns whether path in expression is in an error state.
  bool is_error () const { return !has_segments (); }

  /* Converts PathInExpression to SimplePath if possible (i.e. no generic
   * arguments). Otherwise returns an empty SimplePath. */
  SimplePath as_simple_path () const
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

  void accept_vis (ASTVisitor &vis) override;

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
  /* TODO: may have to unify TypePathSegment and PathExprSegment (which are
   * mostly the same anyway) in order to resolve goddamn syntax ambiguities. One
   * difference is that function on TypePathSegment is not allowed if
   * GenericArgs are, so could disallow that in constructor, which won't give
   * that much size overhead. */
  PathIdentSegment ident_segment;

  Location locus;

protected:
  /* This is protected because it is only really used by derived classes, not
   * the base. */
  bool has_separating_scope_resolution;

  // Clone function implementation - not pure virtual as overrided by subclasses
  virtual TypePathSegment *clone_type_path_segment_impl () const
  {
    return new TypePathSegment (*this);
  }

public:
  virtual ~TypePathSegment () {}

  // Unique pointer custom clone function
  std::unique_ptr<TypePathSegment> clone_type_path_segment () const
  {
    return std::unique_ptr<TypePathSegment> (clone_type_path_segment_impl ());
  }

  TypePathSegment (PathIdentSegment ident_segment,
		   bool has_separating_scope_resolution, Location locus)
    : ident_segment (std::move (ident_segment)), locus (locus),
      has_separating_scope_resolution (has_separating_scope_resolution)
  {}

  TypePathSegment (std::string segment_name,
		   bool has_separating_scope_resolution, Location locus)
    : ident_segment (PathIdentSegment (std::move (segment_name))),
      locus (locus),
      has_separating_scope_resolution (has_separating_scope_resolution)
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
  virtual void accept_vis (ASTVisitor &vis);
};

// Segment used in type path with generic args
class TypePathSegmentGeneric : public TypePathSegment
{
  GenericArgs generic_args;

public:
  bool has_generic_args () const { return generic_args.has_generic_args (); }

  bool is_ident_only () const override { return false; }

  // Constructor with PathIdentSegment and GenericArgs
  TypePathSegmentGeneric (PathIdentSegment ident_segment,
			  bool has_separating_scope_resolution,
			  GenericArgs generic_args, Location locus)
    : TypePathSegment (std::move (ident_segment),
		       has_separating_scope_resolution, locus),
      generic_args (std::move (generic_args))
  {}

  // Constructor from segment name and all args
  TypePathSegmentGeneric (std::string segment_name,
			  bool has_separating_scope_resolution,
			  std::vector<Lifetime> lifetime_args,
			  std::vector<std::unique_ptr<Type>> type_args,
			  std::vector<GenericArgsBinding> binding_args,
			  Location locus)
    : TypePathSegment (std::move (segment_name),
		       has_separating_scope_resolution, locus),
      generic_args (GenericArgs (std::move (lifetime_args),
				 std::move (type_args),
				 std::move (binding_args)))
  {}

  std::string as_string () const override;

  void accept_vis (ASTVisitor &vis) override;

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
  std::vector<std::unique_ptr<Type>> inputs;

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
  TypePathFunction (std::vector<std::unique_ptr<Type>> inputs,
		    Type *type = nullptr)
    : inputs (std::move (inputs)), return_type (type), is_invalid (false)
  {}
  // FIXME: deprecated

  // Constructor
  TypePathFunction (std::vector<std::unique_ptr<Type>> inputs,
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
  TypePathSegmentFunction (PathIdentSegment ident_segment,
			   bool has_separating_scope_resolution,
			   TypePathFunction function_path, Location locus)
    : TypePathSegment (std::move (ident_segment),
		       has_separating_scope_resolution, locus),
      function_path (std::move (function_path))
  {}

  // Constructor with segment name and TypePathFn
  TypePathSegmentFunction (std::string segment_name,
			   bool has_separating_scope_resolution,
			   TypePathFunction function_path, Location locus)
    : TypePathSegment (std::move (segment_name),
		       has_separating_scope_resolution, locus),
      function_path (std::move (function_path))
  {}

  std::string as_string () const override;

  bool is_ident_only () const override { return false; }

  void accept_vis (ASTVisitor &vis) override;

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
  std::vector<std::unique_ptr<TypePathSegment>> segments;
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
    return TypePath (std::vector<std::unique_ptr<TypePathSegment>> (),
		     Location ());
  }

  // Constructor
  TypePath (std::vector<std::unique_ptr<TypePathSegment>> segments,
	    Location locus, bool has_opening_scope_resolution = false)
    : has_opening_scope_resolution (has_opening_scope_resolution),
      segments (std::move (segments)), locus (locus)
  {}

  // Copy constructor with vector clone
  TypePath (TypePath const &other)
    : has_opening_scope_resolution (other.has_opening_scope_resolution),
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

  Location get_locus () const { return locus; }

  void accept_vis (ASTVisitor &vis) override;
};

struct QualifiedPathType
{
private:
  std::unique_ptr<Type> type_to_invoke_on;

  // bool has_as_clause;
  TypePath trait_path;

  Location locus;

public:
  // Constructor
  QualifiedPathType (std::unique_ptr<Type> invoke_on_type,
		     Location locus = Location (),
		     TypePath trait_path = TypePath::create_error ())
    : type_to_invoke_on (std::move (invoke_on_type)),
      trait_path (std::move (trait_path)), locus (locus)
  {}

  // Copy constructor uses custom deep copy for Type to preserve polymorphism
  QualifiedPathType (QualifiedPathType const &other)
    : type_to_invoke_on (other.type_to_invoke_on->clone_type ()),
      trait_path (other.trait_path), locus (other.locus)
  {}

  // default destructor
  ~QualifiedPathType () = default;

  // overload assignment operator to use custom clone method
  QualifiedPathType &operator= (QualifiedPathType const &other)
  {
    type_to_invoke_on = other.type_to_invoke_on->clone_type ();
    trait_path = other.trait_path;
    locus = other.locus;
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

  Location get_locus () const { return locus; }
};

/* AST node representing a qualified path-in-expression pattern (path that
 * allows specifying trait functions) */
class QualifiedPathInExpression : public PathPattern, public PathExpr
{
  QualifiedPathType path_type;
  Location locus;

public:
  std::string as_string () const override;

  QualifiedPathInExpression (QualifiedPathType qual_path_type,
			     std::vector<PathExprSegment> path_segments,
			     Location locus = Location (),
			     std::vector<Attribute> outer_attrs
			     = std::vector<Attribute> ())
    : PathPattern (std::move (path_segments)),
      PathExpr (std::move (outer_attrs)),
      path_type (std::move (qual_path_type)), locus (locus)
  {}

  /* TODO: maybe make a shortcut constructor that has QualifiedPathType elements
   * as params */

  // Returns whether qualified path in expression is in an error state.
  bool is_error () const { return path_type.is_error (); }

  // Creates an error qualified path in expression.
  static QualifiedPathInExpression create_error ()
  {
    return QualifiedPathInExpression (QualifiedPathType::create_error (),
				      std::vector<PathExprSegment> ());
  }

  Location get_locus () const { return locus; }
  Location get_locus_slow () const override { return get_locus (); }

  void accept_vis (ASTVisitor &vis) override;

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
  std::vector<std::unique_ptr<TypePathSegment>> segments;
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
    QualifiedPathType qual_path_type,
    std::vector<std::unique_ptr<TypePathSegment>> path_segments,
    Location locus = Location ())
    : path_type (std::move (qual_path_type)),
      segments (std::move (path_segments)), locus (locus)
  {}

  /* TODO: maybe make a shortcut constructor that has QualifiedPathType elements
   * as params */

  // Copy constructor with vector clone
  QualifiedPathInType (QualifiedPathInType const &other)
    : path_type (other.path_type), locus (other.locus)
  {
    segments.reserve (other.segments.size ());
    for (const auto &e : other.segments)
      segments.push_back (e->clone_type_path_segment ());
  }

  // Overloaded assignment operator with vector clone
  QualifiedPathInType &operator= (QualifiedPathInType const &other)
  {
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
      QualifiedPathType::create_error (),
      std::vector<std::unique_ptr<TypePathSegment>> ());
  }

  std::string as_string () const override;

  void accept_vis (ASTVisitor &vis) override;
};
} // namespace AST
} // namespace Rust

#endif
