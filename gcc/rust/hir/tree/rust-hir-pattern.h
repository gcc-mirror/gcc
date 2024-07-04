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

#ifndef RUST_HIR_PATTERN_H
#define RUST_HIR_PATTERN_H

#include "rust-common.h"
#include "rust-hir.h"

namespace Rust {
namespace HIR {

// Literal pattern HIR node (comparing to a literal)
class LiteralPattern : public Pattern
{
  Literal lit;
  location_t locus;
  Analysis::NodeMapping mappings;

public:
  std::string as_string () const override;

  // Constructor for a literal pattern
  LiteralPattern (Analysis::NodeMapping mappings, Literal lit, location_t locus)
    : lit (std::move (lit)), locus (locus), mappings (mappings)
  {}

  LiteralPattern (Analysis::NodeMapping mappings, std::string val,
		  Literal::LitType type, location_t locus)
    : lit (Literal (std::move (val), type, PrimitiveCoreType::CORETYPE_STR)),
      locus (locus), mappings (mappings)
  {}

  location_t get_locus () const override { return locus; }

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRPatternVisitor &vis) override;

  const Analysis::NodeMapping &get_mappings () const override final
  {
    return mappings;
  }

  PatternType get_pattern_type () const override final
  {
    return PatternType::LITERAL;
  }

  Literal &get_literal () { return lit; }
  const Literal &get_literal () const { return lit; }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  virtual LiteralPattern *clone_pattern_impl () const override
  {
    return new LiteralPattern (*this);
  }
};

// Identifier pattern HIR node (bind value matched to a variable)
class IdentifierPattern : public Pattern
{
  Identifier variable_ident;
  bool is_ref;
  Mutability mut;
  std::unique_ptr<Pattern> to_bind;
  location_t locus;
  Analysis::NodeMapping mappings;

public:
  std::string as_string () const override;

  // Returns whether the IdentifierPattern has a pattern to bind.
  bool has_pattern_to_bind () const { return to_bind != nullptr; }

  // Constructor
  IdentifierPattern (Analysis::NodeMapping mappings, Identifier ident,
		     location_t locus, bool is_ref = false,
		     Mutability mut = Mutability::Imm,
		     std::unique_ptr<Pattern> to_bind = nullptr)
    : variable_ident (std::move (ident)), is_ref (is_ref), mut (mut),
      to_bind (std::move (to_bind)), locus (locus), mappings (mappings)
  {}

  // Copy constructor with clone
  IdentifierPattern (IdentifierPattern const &other)
    : variable_ident (other.variable_ident), is_ref (other.is_ref),
      mut (other.mut), locus (other.locus), mappings (other.mappings)
  {
    // fix to get prevent null pointer dereference
    if (other.to_bind != nullptr)
      to_bind = other.to_bind->clone_pattern ();
  }

  // Overload assignment operator to use clone
  IdentifierPattern &operator= (IdentifierPattern const &other)
  {
    variable_ident = other.variable_ident;
    is_ref = other.is_ref;
    mut = other.mut;
    locus = other.locus;
    mappings = other.mappings;

    // fix to get prevent null pointer dereference
    if (other.to_bind != nullptr)
      to_bind = other.to_bind->clone_pattern ();

    return *this;
  }

  // default move semantics
  IdentifierPattern (IdentifierPattern &&other) = default;
  IdentifierPattern &operator= (IdentifierPattern &&other) = default;

  location_t get_locus () const override { return locus; }

  bool is_mut () const { return mut == Mutability::Mut; }
  bool get_is_ref () const { return is_ref; }
  std::unique_ptr<Pattern> &get_to_bind () { return to_bind; }

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRPatternVisitor &vis) override;

  const Analysis::NodeMapping &get_mappings () const override final
  {
    return mappings;
  }

  Identifier get_identifier () const { return variable_ident; }

  PatternType get_pattern_type () const override final
  {
    return PatternType::IDENTIFIER;
  }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  IdentifierPattern *clone_pattern_impl () const override
  {
    return new IdentifierPattern (*this);
  }
};

// HIR node for using the '_' wildcard "match any value" pattern
class WildcardPattern : public Pattern
{
  location_t locus;
  Analysis::NodeMapping mappings;

public:
  std::string as_string () const override { return std::string (1, '_'); }

  WildcardPattern (Analysis::NodeMapping mappings, location_t locus)
    : locus (locus), mappings (mappings)
  {}

  location_t get_locus () const override { return locus; }

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRPatternVisitor &vis) override;

  const Analysis::NodeMapping &get_mappings () const override final
  {
    return mappings;
  }

  PatternType get_pattern_type () const override final
  {
    return PatternType::WILDCARD;
  }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  WildcardPattern *clone_pattern_impl () const override
  {
    return new WildcardPattern (*this);
  }
};

// Base range pattern bound (lower or upper limit) - abstract
class RangePatternBound
{
public:
  enum RangePatternBoundType
  {
    LITERAL,
    PATH,
    QUALPATH
  };

  virtual ~RangePatternBound () {}

  // Unique pointer custom clone function
  std::unique_ptr<RangePatternBound> clone_range_pattern_bound () const
  {
    return std::unique_ptr<RangePatternBound> (
      clone_range_pattern_bound_impl ());
  }

  virtual std::string as_string () const = 0;

  virtual void accept_vis (HIRFullVisitor &vis) = 0;

  virtual RangePatternBoundType get_bound_type () const = 0;

protected:
  // pure virtual as RangePatternBound is abstract
  virtual RangePatternBound *clone_range_pattern_bound_impl () const = 0;
};

// Literal-based pattern bound
class RangePatternBoundLiteral : public RangePatternBound
{
  Literal literal;
  /* Can only be a char, byte, int, or float literal - same impl here as
   * previously */

  // Minus prefixed to literal (if integer or floating-point)
  bool has_minus;

  location_t locus;

public:
  // Constructor
  RangePatternBoundLiteral (Literal literal, location_t locus,
			    bool has_minus = false)
    : literal (literal), has_minus (has_minus), locus (locus)
  {}

  std::string as_string () const override;

  location_t get_locus () const { return locus; }

  Literal get_literal () const { return literal; }
  bool get_has_minus () const { return has_minus; }

  void accept_vis (HIRFullVisitor &vis) override;

  RangePatternBoundType get_bound_type () const override
  {
    return RangePatternBoundType::LITERAL;
  }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  RangePatternBoundLiteral *clone_range_pattern_bound_impl () const override
  {
    return new RangePatternBoundLiteral (*this);
  }
};

// Path-based pattern bound
class RangePatternBoundPath : public RangePatternBound
{
  PathInExpression path;

  /* TODO: should this be refactored so that PathInExpression is a subclass of
   * RangePatternBound? */

public:
  RangePatternBoundPath (PathInExpression path) : path (std::move (path)) {}

  std::string as_string () const override { return path.as_string (); }

  location_t get_locus () const { return path.get_locus (); }

  PathInExpression &get_path () { return path; }
  const PathInExpression &get_path () const { return path; }

  void accept_vis (HIRFullVisitor &vis) override;

  RangePatternBoundType get_bound_type () const override
  {
    return RangePatternBoundType::PATH;
  }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  RangePatternBoundPath *clone_range_pattern_bound_impl () const override
  {
    return new RangePatternBoundPath (*this);
  }
};

// Qualified path-based pattern bound
class RangePatternBoundQualPath : public RangePatternBound
{
  QualifiedPathInExpression path;

  /* TODO: should this be refactored so that QualifiedPathInExpression is a
   * subclass of RangePatternBound? */

public:
  RangePatternBoundQualPath (QualifiedPathInExpression path)
    : path (std::move (path))
  {}

  std::string as_string () const override { return path.as_string (); }

  location_t get_locus () const { return path.get_locus (); }

  void accept_vis (HIRFullVisitor &vis) override;

  QualifiedPathInExpression &get_qualified_path () { return path; }
  const QualifiedPathInExpression &get_qualified_path () const { return path; }

  RangePatternBoundType get_bound_type () const override
  {
    return RangePatternBoundType::QUALPATH;
  }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  RangePatternBoundQualPath *clone_range_pattern_bound_impl () const override
  {
    return new RangePatternBoundQualPath (*this);
  }
};

// HIR node for matching within a certain range (range pattern)
class RangePattern : public Pattern
{
  std::unique_ptr<RangePatternBound> lower;
  std::unique_ptr<RangePatternBound> upper;

  bool has_ellipsis_syntax;

  /* location only stored to avoid a dereference - lower pattern should give
   * correct location so maybe change in future */
  location_t locus;
  Analysis::NodeMapping mappings;

public:
  std::string as_string () const override;

  // Constructor
  RangePattern (Analysis::NodeMapping mappings,
		std::unique_ptr<RangePatternBound> lower,
		std::unique_ptr<RangePatternBound> upper, location_t locus,
		bool has_ellipsis_syntax = false)
    : lower (std::move (lower)), upper (std::move (upper)),
      has_ellipsis_syntax (has_ellipsis_syntax), locus (locus),
      mappings (mappings)
  {}

  // Copy constructor with clone
  RangePattern (RangePattern const &other)
    : lower (other.lower->clone_range_pattern_bound ()),
      upper (other.upper->clone_range_pattern_bound ()),
      has_ellipsis_syntax (other.has_ellipsis_syntax), locus (other.locus),
      mappings (other.mappings)
  {}

  // Overloaded assignment operator to clone
  RangePattern &operator= (RangePattern const &other)
  {
    lower = other.lower->clone_range_pattern_bound ();
    upper = other.upper->clone_range_pattern_bound ();
    has_ellipsis_syntax = other.has_ellipsis_syntax;
    locus = other.locus;
    mappings = other.mappings;

    return *this;
  }

  // default move semantics
  RangePattern (RangePattern &&other) = default;
  RangePattern &operator= (RangePattern &&other) = default;

  location_t get_locus () const override { return locus; }

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRPatternVisitor &vis) override;

  bool get_has_ellipsis_syntax () { return has_ellipsis_syntax; };

  const Analysis::NodeMapping &get_mappings () const override final
  {
    return mappings;
  }

  PatternType get_pattern_type () const override final
  {
    return PatternType::RANGE;
  }

  std::unique_ptr<RangePatternBound> &get_lower_bound () { return lower; }

  std::unique_ptr<RangePatternBound> &get_upper_bound () { return upper; }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  RangePattern *clone_pattern_impl () const override
  {
    return new RangePattern (*this);
  }
};

// HIR node for pattern based on dereferencing the pointers given
class ReferencePattern : public Pattern
{
  Mutability mut;
  std::unique_ptr<Pattern> pattern;
  location_t locus;
  Analysis::NodeMapping mappings;

public:
  std::string as_string () const override;

  ReferencePattern (Analysis::NodeMapping mappings,
		    std::unique_ptr<Pattern> pattern, Mutability reference_mut,
		    location_t locus)
    : mut (reference_mut), pattern (std::move (pattern)), locus (locus),
      mappings (mappings)
  {}

  // Copy constructor requires clone
  ReferencePattern (ReferencePattern const &other)
    : mut (other.mut), pattern (other.pattern->clone_pattern ()),
      locus (other.locus), mappings (other.mappings)
  {}

  // Overload assignment operator to clone
  ReferencePattern &operator= (ReferencePattern const &other)
  {
    pattern = other.pattern->clone_pattern ();
    mut = other.mut;
    locus = other.locus;
    mappings = other.mappings;

    return *this;
  }

  // default move semantics
  ReferencePattern (ReferencePattern &&other) = default;
  ReferencePattern &operator= (ReferencePattern &&other) = default;

  bool is_mut () const { return mut == Mutability::Mut; }

  Mutability get_mutability () const { return mut; }

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRPatternVisitor &vis) override;

  const Analysis::NodeMapping &get_mappings () const override final
  {
    return mappings;
  }

  location_t get_locus () const override final { return locus; }

  PatternType get_pattern_type () const override final
  {
    return PatternType::REFERENCE;
  }

  std::unique_ptr<Pattern> &get_referenced_pattern () { return pattern; }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  ReferencePattern *clone_pattern_impl () const override
  {
    return new ReferencePattern (*this);
  }
};

// Base class for a single field in a struct pattern - abstract
class StructPatternField
{
  AST::AttrVec outer_attrs;
  location_t locus;
  Analysis::NodeMapping mappings;

public:
  enum ItemType
  {
    TUPLE_PAT,
    IDENT_PAT,
    IDENT
  };

  virtual ~StructPatternField () {}

  // Unique pointer custom clone function
  std::unique_ptr<StructPatternField> clone_struct_pattern_field () const
  {
    return std::unique_ptr<StructPatternField> (
      clone_struct_pattern_field_impl ());
  }

  virtual std::string as_string () const;
  virtual void accept_vis (HIRFullVisitor &vis) = 0;
  virtual ItemType get_item_type () const = 0;

  location_t get_locus () const { return locus; }
  Analysis::NodeMapping get_mappings () const { return mappings; };
  AST::AttrVec get_outer_attrs () { return outer_attrs; }

protected:
  StructPatternField (Analysis::NodeMapping mappings,
		      AST::AttrVec outer_attribs, location_t locus)
    : outer_attrs (std::move (outer_attribs)), locus (locus),
      mappings (mappings)
  {}

  // Clone function implementation as pure virtual method
  virtual StructPatternField *clone_struct_pattern_field_impl () const = 0;
};

// Tuple pattern single field in a struct pattern
class StructPatternFieldTuplePat : public StructPatternField
{
  TupleIndex index;
  std::unique_ptr<Pattern> tuple_pattern;

public:
  StructPatternFieldTuplePat (Analysis::NodeMapping mappings, TupleIndex index,
			      std::unique_ptr<Pattern> tuple_pattern,
			      AST::AttrVec outer_attribs, location_t locus)
    : StructPatternField (mappings, std::move (outer_attribs), locus),
      index (index), tuple_pattern (std::move (tuple_pattern))
  {}

  // Copy constructor requires clone
  StructPatternFieldTuplePat (StructPatternFieldTuplePat const &other)
    : StructPatternField (other), index (other.index),
      tuple_pattern (other.tuple_pattern->clone_pattern ())
  {}

  // Overload assignment operator to perform clone
  StructPatternFieldTuplePat &
  operator= (StructPatternFieldTuplePat const &other)
  {
    StructPatternField::operator= (other);
    tuple_pattern = other.tuple_pattern->clone_pattern ();
    index = other.index;
    // outer_attrs = other.outer_attrs;

    return *this;
  }

  // default move semantics
  StructPatternFieldTuplePat (StructPatternFieldTuplePat &&other) = default;
  StructPatternFieldTuplePat &operator= (StructPatternFieldTuplePat &&other)
    = default;

  std::string as_string () const override;

  void accept_vis (HIRFullVisitor &vis) override;

  TupleIndex get_index () { return index; }
  std::unique_ptr<Pattern> &get_tuple_pattern () { return tuple_pattern; }

  ItemType get_item_type () const override final { return ItemType::TUPLE_PAT; }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  StructPatternFieldTuplePat *clone_struct_pattern_field_impl () const override
  {
    return new StructPatternFieldTuplePat (*this);
  }
};

// Identifier pattern single field in a struct pattern
class StructPatternFieldIdentPat : public StructPatternField
{
  Identifier ident;
  std::unique_ptr<Pattern> ident_pattern;

public:
  StructPatternFieldIdentPat (Analysis::NodeMapping mappings, Identifier ident,
			      std::unique_ptr<Pattern> ident_pattern,
			      AST::AttrVec outer_attrs, location_t locus)
    : StructPatternField (mappings, std::move (outer_attrs), locus),
      ident (std::move (ident)), ident_pattern (std::move (ident_pattern))
  {}

  // Copy constructor requires clone
  StructPatternFieldIdentPat (StructPatternFieldIdentPat const &other)
    : StructPatternField (other), ident (other.ident),
      ident_pattern (other.ident_pattern->clone_pattern ())
  {}

  // Overload assignment operator to clone
  StructPatternFieldIdentPat &
  operator= (StructPatternFieldIdentPat const &other)
  {
    StructPatternField::operator= (other);
    ident = other.ident;
    ident_pattern = other.ident_pattern->clone_pattern ();
    // outer_attrs = other.outer_attrs;

    return *this;
  }

  // default move semantics
  StructPatternFieldIdentPat (StructPatternFieldIdentPat &&other) = default;
  StructPatternFieldIdentPat &operator= (StructPatternFieldIdentPat &&other)
    = default;

  std::string as_string () const override;

  void accept_vis (HIRFullVisitor &vis) override;

  ItemType get_item_type () const override final { return ItemType::IDENT_PAT; }

  Identifier get_identifier () const { return ident; }

  std::unique_ptr<Pattern> &get_pattern () { return ident_pattern; }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  StructPatternFieldIdentPat *clone_struct_pattern_field_impl () const override
  {
    return new StructPatternFieldIdentPat (*this);
  }
};

// Identifier only (with no pattern) single field in a struct pattern
class StructPatternFieldIdent : public StructPatternField
{
  bool has_ref;
  Mutability mut;
  Identifier ident;

public:
  StructPatternFieldIdent (Analysis::NodeMapping mappings, Identifier ident,
			   bool is_ref, Mutability mut,
			   AST::AttrVec outer_attrs, location_t locus)
    : StructPatternField (mappings, std::move (outer_attrs), locus),
      has_ref (is_ref), mut (mut), ident (std::move (ident))
  {}

  std::string as_string () const override;

  bool is_mut () const { return mut == Mutability::Mut; }

  void accept_vis (HIRFullVisitor &vis) override;

  ItemType get_item_type () const override final { return ItemType::IDENT; }
  bool get_has_ref () const { return has_ref; }
  Identifier get_identifier () const { return ident; };

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  StructPatternFieldIdent *clone_struct_pattern_field_impl () const override
  {
    return new StructPatternFieldIdent (*this);
  }
};

// Elements of a struct pattern
class StructPatternElements
{
  std::vector<std::unique_ptr<StructPatternField>> fields;

public:
  // Returns whether there are any struct pattern fields
  bool has_struct_pattern_fields () const { return !fields.empty (); }

  /* Returns whether the struct pattern elements is entirely empty (no fields,
   * no etc). */
  bool is_empty () const { return !has_struct_pattern_fields (); }

  // Constructor for StructPatternElements with both (potentially)
  StructPatternElements (
    std::vector<std::unique_ptr<StructPatternField>> fields)
    : fields (std::move (fields))
  {}

  // Copy constructor with vector clone
  StructPatternElements (StructPatternElements const &other)
  {
    fields.reserve (other.fields.size ());
    for (const auto &e : other.fields)
      fields.push_back (e->clone_struct_pattern_field ());
  }

  // Overloaded assignment operator with vector clone
  StructPatternElements &operator= (StructPatternElements const &other)
  {
    fields.clear ();
    fields.reserve (other.fields.size ());
    for (const auto &e : other.fields)
      fields.push_back (e->clone_struct_pattern_field ());

    return *this;
  }

  // move constructors
  StructPatternElements (StructPatternElements &&other) = default;
  StructPatternElements &operator= (StructPatternElements &&other) = default;

  // Creates an empty StructPatternElements
  static StructPatternElements create_empty ()
  {
    return StructPatternElements (
      std::vector<std::unique_ptr<StructPatternField>> ());
  }

  std::string as_string () const;

  std::vector<std::unique_ptr<StructPatternField>> &get_struct_pattern_fields ()
  {
    return fields;
  }
};

// Struct pattern HIR node representation
class StructPattern : public Pattern
{
  PathInExpression path;
  StructPatternElements elems;
  Analysis::NodeMapping mappings;

public:
  std::string as_string () const override;

  StructPattern (Analysis::NodeMapping mappings, PathInExpression struct_path,
		 StructPatternElements elems)
    : path (std::move (struct_path)), elems (std::move (elems)),
      mappings (mappings)
  {}

  bool has_struct_pattern_elems () const { return !elems.is_empty (); }

  location_t get_locus () const override { return path.get_locus (); }

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRPatternVisitor &vis) override;

  PathInExpression &get_path () { return path; }
  StructPatternElements &get_struct_pattern_elems () { return elems; }

  const Analysis::NodeMapping &get_mappings () const override final
  {
    return mappings;
  }

  PatternType get_pattern_type () const override final
  {
    return PatternType::STRUCT;
  }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  StructPattern *clone_pattern_impl () const override
  {
    return new StructPattern (*this);
  }
};

// Base abstract class for TupleStructItems and TuplePatternItems
class TupleItems : public FullVisitable
{
public:
  enum ItemType
  {
    MULTIPLE,
    RANGED,
  };

  virtual ~TupleItems () {}

  // TODO: should this store location data?

  // Unique pointer custom clone function
  std::unique_ptr<TupleItems> clone_tuple_items () const
  {
    return std::unique_ptr<TupleItems> (clone_tuple_items_impl ());
  }

  virtual ItemType get_item_type () const = 0;

  virtual std::string as_string () const = 0;

protected:
  // pure virtual clone implementation
  virtual TupleItems *clone_tuple_items_impl () const = 0;
};

// Base abstract class for patterns used in TupleStructPattern
class TupleStructItems : public TupleItems
{
public:
  // Unique pointer custom clone function
  std::unique_ptr<TupleStructItems> clone_tuple_struct_items () const
  {
    return std::unique_ptr<TupleStructItems> (clone_tuple_items_impl ());
  }

protected:
  // pure virtual clone implementation
  virtual TupleStructItems *clone_tuple_items_impl () const override = 0;
};

// Class for non-ranged tuple struct pattern patterns
class TupleStructItemsNoRange : public TupleStructItems
{
  std::vector<std::unique_ptr<Pattern>> patterns;

public:
  TupleStructItemsNoRange (std::vector<std::unique_ptr<Pattern>> patterns)
    : patterns (std::move (patterns))
  {}

  // Copy constructor with vector clone
  TupleStructItemsNoRange (TupleStructItemsNoRange const &other)
  {
    patterns.reserve (other.patterns.size ());
    for (const auto &e : other.patterns)
      patterns.push_back (e->clone_pattern ());
  }

  // Overloaded assignment operator with vector clone
  TupleStructItemsNoRange &operator= (TupleStructItemsNoRange const &other)
  {
    patterns.clear ();
    patterns.reserve (other.patterns.size ());
    for (const auto &e : other.patterns)
      patterns.push_back (e->clone_pattern ());

    return *this;
  }

  // move constructors
  TupleStructItemsNoRange (TupleStructItemsNoRange &&other) = default;
  TupleStructItemsNoRange &operator= (TupleStructItemsNoRange &&other)
    = default;

  std::string as_string () const override;

  void accept_vis (HIRFullVisitor &vis) override;

  std::vector<std::unique_ptr<Pattern>> &get_patterns () { return patterns; }
  const std::vector<std::unique_ptr<Pattern>> &get_patterns () const
  {
    return patterns;
  }

  ItemType get_item_type () const override final { return ItemType::MULTIPLE; }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  TupleStructItemsNoRange *clone_tuple_items_impl () const override
  {
    return new TupleStructItemsNoRange (*this);
  }
};

// Class for ranged tuple struct pattern patterns
class TupleStructItemsRange : public TupleStructItems
{
  std::vector<std::unique_ptr<Pattern>> lower_patterns;
  std::vector<std::unique_ptr<Pattern>> upper_patterns;

public:
  TupleStructItemsRange (std::vector<std::unique_ptr<Pattern>> lower_patterns,
			 std::vector<std::unique_ptr<Pattern>> upper_patterns)
    : lower_patterns (std::move (lower_patterns)),
      upper_patterns (std::move (upper_patterns))
  {}

  // Copy constructor with vector clone
  TupleStructItemsRange (TupleStructItemsRange const &other)
  {
    lower_patterns.reserve (other.lower_patterns.size ());
    for (const auto &e : other.lower_patterns)
      lower_patterns.push_back (e->clone_pattern ());

    upper_patterns.reserve (other.upper_patterns.size ());
    for (const auto &e : other.upper_patterns)
      upper_patterns.push_back (e->clone_pattern ());
  }

  // Overloaded assignment operator to clone
  TupleStructItemsRange &operator= (TupleStructItemsRange const &other)
  {
    lower_patterns.clear ();
    lower_patterns.reserve (other.lower_patterns.size ());
    for (const auto &e : other.lower_patterns)
      lower_patterns.push_back (e->clone_pattern ());

    upper_patterns.clear ();
    upper_patterns.reserve (other.upper_patterns.size ());
    for (const auto &e : other.upper_patterns)
      upper_patterns.push_back (e->clone_pattern ());

    return *this;
  }

  // move constructors
  TupleStructItemsRange (TupleStructItemsRange &&other) = default;
  TupleStructItemsRange &operator= (TupleStructItemsRange &&other) = default;

  std::string as_string () const override;

  void accept_vis (HIRFullVisitor &vis) override;

  std::vector<std::unique_ptr<Pattern>> &get_lower_patterns ()
  {
    return lower_patterns;
  }
  const std::vector<std::unique_ptr<Pattern>> &get_lower_patterns () const
  {
    return lower_patterns;
  }

  // TODO: seems kinda dodgy. Think of better way.
  std::vector<std::unique_ptr<Pattern>> &get_upper_patterns ()
  {
    return upper_patterns;
  }
  const std::vector<std::unique_ptr<Pattern>> &get_upper_patterns () const
  {
    return upper_patterns;
  }

  ItemType get_item_type () const override final { return ItemType::RANGED; }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  TupleStructItemsRange *clone_tuple_items_impl () const override
  {
    return new TupleStructItemsRange (*this);
  }
};

// HIR node representing a tuple struct pattern
class TupleStructPattern : public Pattern
{
  PathInExpression path;
  std::unique_ptr<TupleStructItems> items;
  Analysis::NodeMapping mappings;

  /* TOOD: should this store location data? current accessor uses path location
   * data */

public:
  std::string as_string () const override;

  TupleStructPattern (Analysis::NodeMapping mappings,
		      PathInExpression tuple_struct_path,
		      std::unique_ptr<TupleStructItems> items)
    : path (std::move (tuple_struct_path)), items (std::move (items)),
      mappings (mappings)
  {}

  // Copy constructor required to clone
  TupleStructPattern (TupleStructPattern const &other)
    : path (other.path), items (other.items->clone_tuple_struct_items ()),
      mappings (other.mappings)
  {}

  // Operator overload assignment operator to clone
  TupleStructPattern &operator= (TupleStructPattern const &other)
  {
    path = other.path;
    items = other.items->clone_tuple_struct_items ();
    mappings = other.mappings;

    return *this;
  }

  // move constructors
  TupleStructPattern (TupleStructPattern &&other) = default;
  TupleStructPattern &operator= (TupleStructPattern &&other) = default;

  location_t get_locus () const override { return path.get_locus (); }

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRPatternVisitor &vis) override;

  PathInExpression &get_path () { return path; }

  std::unique_ptr<TupleStructItems> &get_items () { return items; }

  const Analysis::NodeMapping &get_mappings () const override final
  {
    return mappings;
  }

  PatternType get_pattern_type () const override final
  {
    return PatternType::TUPLE_STRUCT;
  }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  TupleStructPattern *clone_pattern_impl () const override
  {
    return new TupleStructPattern (*this);
  }
};

// Base abstract class representing TuplePattern patterns
class TuplePatternItems : public TupleItems
{
public:
  // Unique pointer custom clone function
  std::unique_ptr<TuplePatternItems> clone_tuple_pattern_items () const
  {
    return std::unique_ptr<TuplePatternItems> (clone_tuple_items_impl ());
  }

protected:
  // pure virtual clone implementation
  virtual TuplePatternItems *clone_tuple_items_impl () const override = 0;
};

// Class representing TuplePattern patterns where there are multiple patterns
class TuplePatternItemsMultiple : public TuplePatternItems
{
  std::vector<std::unique_ptr<Pattern>> patterns;

public:
  TuplePatternItemsMultiple (std::vector<std::unique_ptr<Pattern>> patterns)
    : patterns (std::move (patterns))
  {}

  // Copy constructor with vector clone
  TuplePatternItemsMultiple (TuplePatternItemsMultiple const &other)
  {
    patterns.reserve (other.patterns.size ());
    for (const auto &e : other.patterns)
      patterns.push_back (e->clone_pattern ());
  }

  // Overloaded assignment operator to vector clone
  TuplePatternItemsMultiple &operator= (TuplePatternItemsMultiple const &other)
  {
    patterns.clear ();
    patterns.reserve (other.patterns.size ());
    for (const auto &e : other.patterns)
      patterns.push_back (e->clone_pattern ());

    return *this;
  }

  // move constructors
  TuplePatternItemsMultiple (TuplePatternItemsMultiple &&other) = default;
  TuplePatternItemsMultiple &operator= (TuplePatternItemsMultiple &&other)
    = default;

  std::string as_string () const override;

  void accept_vis (HIRFullVisitor &vis) override;

  ItemType get_item_type () const override { return ItemType::MULTIPLE; }

  std::vector<std::unique_ptr<Pattern>> &get_patterns () { return patterns; }
  const std::vector<std::unique_ptr<Pattern>> &get_patterns () const
  {
    return patterns;
  }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  TuplePatternItemsMultiple *clone_tuple_items_impl () const override
  {
    return new TuplePatternItemsMultiple (*this);
  }
};

// Class representing TuplePattern patterns where there are a range of patterns
class TuplePatternItemsRanged : public TuplePatternItems
{
  std::vector<std::unique_ptr<Pattern>> lower_patterns;
  std::vector<std::unique_ptr<Pattern>> upper_patterns;

public:
  TuplePatternItemsRanged (std::vector<std::unique_ptr<Pattern>> lower_patterns,
			   std::vector<std::unique_ptr<Pattern>> upper_patterns)
    : lower_patterns (std::move (lower_patterns)),
      upper_patterns (std::move (upper_patterns))
  {}

  // Copy constructor with vector clone
  TuplePatternItemsRanged (TuplePatternItemsRanged const &other)
  {
    lower_patterns.reserve (other.lower_patterns.size ());
    for (const auto &e : other.lower_patterns)
      lower_patterns.push_back (e->clone_pattern ());

    upper_patterns.reserve (other.upper_patterns.size ());
    for (const auto &e : other.upper_patterns)
      upper_patterns.push_back (e->clone_pattern ());
  }

  // Overloaded assignment operator to clone
  TuplePatternItemsRanged &operator= (TuplePatternItemsRanged const &other)
  {
    lower_patterns.clear ();
    lower_patterns.reserve (other.lower_patterns.size ());
    for (const auto &e : other.lower_patterns)
      lower_patterns.push_back (e->clone_pattern ());

    lower_patterns.clear ();
    upper_patterns.reserve (other.upper_patterns.size ());
    for (const auto &e : other.upper_patterns)
      upper_patterns.push_back (e->clone_pattern ());

    return *this;
  }

  // move constructors
  TuplePatternItemsRanged (TuplePatternItemsRanged &&other) = default;
  TuplePatternItemsRanged &operator= (TuplePatternItemsRanged &&other)
    = default;

  std::string as_string () const override;

  void accept_vis (HIRFullVisitor &vis) override;

  ItemType get_item_type () const override { return ItemType::RANGED; }

  std::vector<std::unique_ptr<Pattern>> &get_lower_patterns ()
  {
    return lower_patterns;
  }
  const std::vector<std::unique_ptr<Pattern>> &get_lower_patterns () const
  {
    return lower_patterns;
  }

  std::vector<std::unique_ptr<Pattern>> &get_upper_patterns ()
  {
    return upper_patterns;
  }
  const std::vector<std::unique_ptr<Pattern>> &get_upper_patterns () const
  {
    return upper_patterns;
  }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  TuplePatternItemsRanged *clone_tuple_items_impl () const override
  {
    return new TuplePatternItemsRanged (*this);
  }
};

// HIR node representing a tuple pattern
class TuplePattern : public Pattern
{
  std::unique_ptr<TuplePatternItems> items;
  location_t locus;
  Analysis::NodeMapping mappings;

public:
  std::string as_string () const override;

  // Returns true if the tuple pattern has items
  bool has_tuple_pattern_items () const { return items != nullptr; }

  TuplePattern (Analysis::NodeMapping mappings,
		std::unique_ptr<TuplePatternItems> items, location_t locus)
    : items (std::move (items)), locus (locus), mappings (mappings)
  {}

  // Copy constructor requires clone
  TuplePattern (TuplePattern const &other)
    : items (other.items->clone_tuple_pattern_items ()), locus (other.locus),
      mappings (other.mappings)
  {}

  // Overload assignment operator to clone
  TuplePattern &operator= (TuplePattern const &other)
  {
    items = other.items->clone_tuple_pattern_items ();
    locus = other.locus;
    mappings = other.mappings;

    return *this;
  }

  location_t get_locus () const override { return locus; }

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRPatternVisitor &vis) override;

  const Analysis::NodeMapping &get_mappings () const override final
  {
    return mappings;
  }

  PatternType get_pattern_type () const override final
  {
    return PatternType::TUPLE;
  }

  std::unique_ptr<TuplePatternItems> &get_items () { return items; }
  const std::unique_ptr<TuplePatternItems> &get_items () const { return items; }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  TuplePattern *clone_pattern_impl () const override
  {
    return new TuplePattern (*this);
  }
};

// HIR node representing patterns that can match slices and arrays
class SlicePattern : public Pattern
{
  std::vector<std::unique_ptr<Pattern>> items;
  location_t locus;
  Analysis::NodeMapping mappings;

public:
  std::string as_string () const override;

  SlicePattern (Analysis::NodeMapping mappings,
		std::vector<std::unique_ptr<Pattern>> items, location_t locus)
    : items (std::move (items)), locus (locus), mappings (mappings)
  {}

  // Copy constructor with vector clone
  SlicePattern (SlicePattern const &other)
    : locus (other.locus), mappings (other.mappings)
  {
    items.reserve (other.items.size ());
    for (const auto &e : other.items)
      items.push_back (e->clone_pattern ());
  }

  // Overloaded assignment operator to vector clone
  SlicePattern &operator= (SlicePattern const &other)
  {
    locus = other.locus;
    mappings = other.mappings;

    items.clear ();
    items.reserve (other.items.size ());
    for (const auto &e : other.items)
      items.push_back (e->clone_pattern ());

    return *this;
  }

  // move constructors
  SlicePattern (SlicePattern &&other) = default;
  SlicePattern &operator= (SlicePattern &&other) = default;

  std::vector<std::unique_ptr<Pattern>> &get_items () { return items; }
  const std::vector<std::unique_ptr<Pattern>> &get_items () const
  {
    return items;
  }

  location_t get_locus () const override { return locus; }

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRPatternVisitor &vis) override;

  const Analysis::NodeMapping &get_mappings () const override final
  {
    return mappings;
  }

  PatternType get_pattern_type () const override final
  {
    return PatternType::SLICE;
  }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  SlicePattern *clone_pattern_impl () const override
  {
    return new SlicePattern (*this);
  }
};

// HIR node for alternative patterns
class AltPattern : public Pattern
{
  std::vector<std::unique_ptr<Pattern>> alts;
  location_t locus;
  Analysis::NodeMapping mappings;

public:
  std::string as_string () const override;

  AltPattern (Analysis::NodeMapping mappings,
	      std::vector<std::unique_ptr<Pattern>> alts, location_t locus)
    : alts (std::move (alts)), locus (locus), mappings (mappings)
  {}

  // Copy constructor with vector clone
  AltPattern (AltPattern const &other)
    : locus (other.locus), mappings (other.mappings)
  {
    alts.reserve (other.alts.size ());
    for (const auto &e : other.alts)
      alts.push_back (e->clone_pattern ());
  }

  // Overloaded assignment operator to vector clone
  AltPattern &operator= (AltPattern const &other)
  {
    locus = other.locus;
    mappings = other.mappings;

    alts.clear ();
    alts.reserve (other.alts.size ());
    for (const auto &e : other.alts)
      alts.push_back (e->clone_pattern ());

    return *this;
  }

  // move constructors
  AltPattern (AltPattern &&other) = default;
  AltPattern &operator= (AltPattern &&other) = default;

  std::vector<std::unique_ptr<Pattern>> &get_alts () { return alts; }
  const std::vector<std::unique_ptr<Pattern>> &get_alts () const
  {
    return alts;
  }

  location_t get_locus () const override { return locus; }

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRPatternVisitor &vis) override;

  const Analysis::NodeMapping &get_mappings () const override final
  {
    return mappings;
  }

  PatternType get_pattern_type () const override final
  {
    return PatternType::ALT;
  }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  AltPattern *clone_pattern_impl () const override
  {
    return new AltPattern (*this);
  }
};

// Moved definition to rust-path.h
class PathPattern;

// Forward decls for paths (defined in rust-path.h)
class PathInExpression;
class QualifiedPathInExpression;

} // namespace HIR
} // namespace Rust

#endif
