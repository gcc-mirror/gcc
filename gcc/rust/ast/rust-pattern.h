// Copyright (C) 2020-2026 Free Software Foundation, Inc.

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

#ifndef RUST_AST_PATTERN_H
#define RUST_AST_PATTERN_H

#include "rust-ast.h"
#include "rust-path.h"
#include "rust-cloneable.h"

namespace Rust {
namespace AST {
// Literal pattern AST node (comparing to a literal)
class LiteralPattern : public Pattern
{
  Literal lit;
  location_t locus;
  NodeId node_id;
  bool has_minus;

public:
  std::string as_string () const override;

  // Constructor for a literal pattern
  LiteralPattern (Literal lit, location_t locus)
    : lit (std::move (lit)), locus (locus),
      node_id (Analysis::Mappings::get ().get_next_node_id ()),
      has_minus (false)
  {}

  LiteralPattern (Literal lit, location_t locus, bool has_minus)
    : lit (std::move (lit)), locus (locus),
      node_id (Analysis::Mappings::get ().get_next_node_id ()),
      has_minus (has_minus)
  {}

  LiteralPattern (std::string val, Literal::LitType type, location_t locus,
		  PrimitiveCoreType type_hint)
    : lit (Literal (std::move (val), type, type_hint)), locus (locus),
      node_id (Analysis::Mappings::get ().get_next_node_id ()),
      has_minus (false)
  {}

  LiteralPattern (std::string val, Literal::LitType type, location_t locus,
		  PrimitiveCoreType type_hint, bool has_minus)
    : lit (Literal (std::move (val), type, type_hint)), locus (locus),
      node_id (Analysis::Mappings::get ().get_next_node_id ()),
      has_minus (has_minus)
  {}

  location_t get_locus () const override final { return locus; }

  bool get_has_minus () const { return has_minus; }

  void accept_vis (ASTVisitor &vis) override;

  NodeId get_node_id () const override { return node_id; }

  Literal &get_literal () { return lit; }

  const Literal &get_literal () const { return lit; }

  Pattern::Kind get_pattern_kind () override { return Pattern::Kind::Literal; }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  virtual LiteralPattern *clone_pattern_impl () const override
  {
    return new LiteralPattern (*this);
  }
};

// Identifier pattern AST node (bind value matched to a variable)
class IdentifierPattern : public Pattern
{
  Identifier variable_ident;
  bool is_ref;
  bool is_mut;

  // bool has_pattern;
  Cloneable<std::unique_ptr<Pattern>> subpattern;
  location_t locus;
  NodeId node_id;

public:
  std::string as_string () const override;

  // Returns whether the IdentifierPattern has a pattern to bind.
  bool has_subpattern () const { return subpattern != nullptr; }

  // Constructor
  IdentifierPattern (Identifier ident, location_t locus, bool is_ref = false,
		     bool is_mut = false,
		     std::unique_ptr<Pattern> subpattern = nullptr)
    : Pattern (), variable_ident (std::move (ident)), is_ref (is_ref),
      is_mut (is_mut), subpattern (std::move (subpattern)), locus (locus),
      node_id (Analysis::Mappings::get ().get_next_node_id ())
  {}

  IdentifierPattern (NodeId node_id, Identifier ident, location_t locus,
		     bool is_ref = false, bool is_mut = false,
		     std::unique_ptr<Pattern> subpattern = nullptr)
    : Pattern (), variable_ident (std::move (ident)), is_ref (is_ref),
      is_mut (is_mut), subpattern (std::move (subpattern)), locus (locus),
      node_id (node_id)
  {}

  location_t get_locus () const override final { return locus; }

  void accept_vis (ASTVisitor &vis) override;

  Pattern &get_subpattern ()
  {
    rust_assert (has_subpattern ());
    return *subpattern.get ();
  }

  std::unique_ptr<Pattern> &get_subpattern_ptr ()
  {
    rust_assert (has_subpattern ());
    return subpattern.get ();
  }

  Identifier get_ident () const { return variable_ident; }

  bool get_is_mut () const { return is_mut; }
  bool get_is_ref () const { return is_ref; }

  NodeId get_node_id () const override { return node_id; }

  Pattern::Kind get_pattern_kind () override
  {
    return Pattern::Kind::Identifier;
  }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  IdentifierPattern *clone_pattern_impl () const override
  {
    return new IdentifierPattern (*this);
  }
};

// AST node for using the '_' wildcard "match any value" pattern
class WildcardPattern : public Pattern
{
  location_t locus;
  NodeId node_id;

public:
  std::string as_string () const override { return std::string (1, '_'); }

  WildcardPattern (location_t locus)
    : locus (locus), node_id (Analysis::Mappings::get ().get_next_node_id ())
  {}

  location_t get_locus () const override final { return locus; }

  void accept_vis (ASTVisitor &vis) override;

  NodeId get_node_id () const override { return node_id; }

  Pattern::Kind get_pattern_kind () override { return Pattern::Kind::Wildcard; }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  WildcardPattern *clone_pattern_impl () const override
  {
    return new WildcardPattern (*this);
  }
};

class RestPattern : public Pattern
{
  location_t locus;
  NodeId node_id;

public:
  std::string as_string () const override { return ".."; }

  RestPattern (location_t locus)
    : locus (locus), node_id (Analysis::Mappings::get ().get_next_node_id ())
  {}

  location_t get_locus () const override final { return locus; }

  void accept_vis (ASTVisitor &vis) override;

  NodeId get_node_id () const override final { return node_id; }

  Pattern::Kind get_pattern_kind () override { return Pattern::Kind::Rest; }

protected:
  RestPattern *clone_pattern_impl () const override
  {
    return new RestPattern (*this);
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

  virtual void accept_vis (ASTVisitor &vis) = 0;

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

  Literal get_literal () const { return literal; }

  bool get_has_minus () const { return has_minus; }

  location_t get_locus () const { return locus; }

  void accept_vis (ASTVisitor &vis) override;

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

  void accept_vis (ASTVisitor &vis) override;

  // TODO: this mutable getter seems kinda dodgy
  PathInExpression &get_path () { return path; }
  const PathInExpression &get_path () const { return path; }

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

  void accept_vis (ASTVisitor &vis) override;

  // TODO: this mutable getter seems kinda dodgy
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

enum class RangeKind
{
  INCLUDED,
  ELLIPSIS,
  EXCLUDED,
};

RangeKind tokenid_to_rangekind (TokenId id);
// AST node for matching within a certain range (range pattern)
class RangePattern : public Pattern
{
  Cloneable<std::unique_ptr<RangePatternBound>> lower;
  Cloneable<std::unique_ptr<RangePatternBound>> upper;

  RangeKind range_kind;

  /* location only stored to avoid a dereference - lower pattern should give
   * correct location so maybe change in future */
  location_t locus;
  NodeId node_id;

public:
  std::string as_string () const override;

  // Constructor
  RangePattern (std::unique_ptr<RangePatternBound> lower,
		std::unique_ptr<RangePatternBound> upper, RangeKind range_kind,
		location_t locus)
    : lower (std::move (lower)), upper (std::move (upper)),
      range_kind (range_kind), locus (locus),
      node_id (Analysis::Mappings::get ().get_next_node_id ())
  {}

  location_t get_locus () const override final { return locus; }

  bool get_has_ellipsis_syntax () const
  {
    return range_kind == RangeKind::ELLIPSIS;
  }

  RangeKind get_range_kind () const { return range_kind; }

  bool get_has_lower_bound () const { return lower != nullptr; }

  bool get_has_upper_bound () const { return upper != nullptr; }

  void accept_vis (ASTVisitor &vis) override;

  // TODO: is this better? or is a "vis_bound" better?
  RangePatternBound &get_lower_bound ()
  {
    rust_assert (lower != nullptr);
    return *lower.get ();
  }

  RangePatternBound &get_upper_bound ()
  {
    rust_assert (upper != nullptr);
    return *upper.get ();
  }

  NodeId get_node_id () const override { return node_id; }

  Pattern::Kind get_pattern_kind () override { return Pattern::Kind::Range; }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  RangePattern *clone_pattern_impl () const override
  {
    return new RangePattern (*this);
  }
};

// AST node for pattern based on dereferencing the pointers given
class ReferencePattern : public Pattern
{
  bool has_two_amps;
  bool is_mut;
  Cloneable<std::unique_ptr<Pattern>> pattern;
  location_t locus;
  NodeId node_id;

public:
  std::string as_string () const override;

  ReferencePattern (std::unique_ptr<Pattern> pattern, bool is_mut_reference,
		    bool ref_has_two_amps, location_t locus)
    : has_two_amps (ref_has_two_amps), is_mut (is_mut_reference),
      pattern (std::move (pattern)), locus (locus),
      node_id (Analysis::Mappings::get ().get_next_node_id ())
  {}

  location_t get_locus () const override final { return locus; }

  void accept_vis (ASTVisitor &vis) override;

  // TODO: is this better? Or is a "vis_pattern" better?
  Pattern &get_referenced_pattern ()
  {
    rust_assert (pattern != nullptr);
    return *pattern.get ();
  }

  std::unique_ptr<Pattern> &get_referenced_pattern_ptr ()
  {
    rust_assert (pattern != nullptr);
    return pattern.get ();
  }

  bool is_double_reference () const { return has_two_amps; }

  bool get_is_mut () const { return is_mut; }

  NodeId get_node_id () const override { return node_id; }

  Pattern::Kind get_pattern_kind () override
  {
    return Pattern::Kind::Reference;
  }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  ReferencePattern *clone_pattern_impl () const override
  {
    return new ReferencePattern (*this);
  }
};

#if 0
// aka StructPatternEtCetera; potential element in struct pattern
struct StructPatternEtc
{
private:
  std::vector<Attribute> outer_attrs;

  // should this store location data?

public:
  StructPatternEtc (std::vector<Attribute> outer_attribs)
    : outer_attrs (std::move (outer_attribs))
  {}

  // Creates an empty StructPatternEtc
  static StructPatternEtc create_empty ()
  {
    return StructPatternEtc (std::vector<Attribute> ());
  }
};
#endif

// Base class for a single field in a struct pattern - abstract
class StructPatternField
{
  std::vector<Attribute> outer_attrs;
  location_t locus;

protected:
  NodeId node_id;

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

  location_t get_locus () const { return locus; }

  virtual void accept_vis (ASTVisitor &vis) = 0;

  virtual void mark_for_strip () = 0;
  virtual bool is_marked_for_strip () const = 0;
  virtual ItemType get_item_type () const = 0;

  NodeId get_node_id () const { return node_id; }

  // TODO: seems kinda dodgy. Think of better way.
  std::vector<Attribute> &get_outer_attrs () { return outer_attrs; }
  const std::vector<Attribute> &get_outer_attrs () const { return outer_attrs; }

protected:
  StructPatternField (std::vector<Attribute> outer_attribs, location_t locus,
		      NodeId node_id)
    : outer_attrs (std::move (outer_attribs)), locus (locus), node_id (node_id)
  {}

  // Clone function implementation as pure virtual method
  virtual StructPatternField *clone_struct_pattern_field_impl () const = 0;
};

// Tuple pattern single field in a struct pattern
class StructPatternFieldTuplePat : public StructPatternField
{
  TupleIndex index;
  Cloneable<std::unique_ptr<Pattern>> tuple_pattern;

public:
  StructPatternFieldTuplePat (TupleIndex index,
			      std::unique_ptr<Pattern> tuple_pattern,
			      std::vector<Attribute> outer_attribs,
			      location_t locus)
    : StructPatternField (std::move (outer_attribs), locus,
			  Analysis::Mappings::get ().get_next_node_id ()),
      index (index), tuple_pattern (std::move (tuple_pattern))
  {}

  std::string as_string () const override;

  void accept_vis (ASTVisitor &vis) override;

  // based on idea of tuple pattern no longer existing
  void mark_for_strip () override { tuple_pattern = nullptr; }
  bool is_marked_for_strip () const override
  {
    return tuple_pattern == nullptr;
  }

  TupleIndex get_index () { return index; }

  // TODO: is this better? Or is a "vis_pattern" better?
  Pattern &get_index_pattern ()
  {
    rust_assert (tuple_pattern != nullptr);
    return *tuple_pattern.get ();
  }

  std::unique_ptr<Pattern> &get_index_pattern_ptr ()
  {
    rust_assert (tuple_pattern != nullptr);
    return tuple_pattern.get ();
  }

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
  Cloneable<std::unique_ptr<Pattern>> ident_pattern;

public:
  StructPatternFieldIdentPat (Identifier ident,
			      std::unique_ptr<Pattern> ident_pattern,
			      std::vector<Attribute> outer_attrs,
			      location_t locus)
    : StructPatternField (std::move (outer_attrs), locus,
			  Analysis::Mappings::get ().get_next_node_id ()),
      ident (std::move (ident)), ident_pattern (std::move (ident_pattern))
  {}

  std::string as_string () const override;

  void accept_vis (ASTVisitor &vis) override;

  // based on idea of identifier pattern no longer existing
  void mark_for_strip () override { ident_pattern = nullptr; }
  bool is_marked_for_strip () const override
  {
    return ident_pattern == nullptr;
  }

  const Identifier &get_identifier () const { return ident; }

  // TODO: is this better? Or is a "vis_pattern" better?
  Pattern &get_ident_pattern ()
  {
    rust_assert (ident_pattern != nullptr);
    return *ident_pattern.get ();
  }

  std::unique_ptr<Pattern> &get_ident_pattern_ptr ()
  {
    rust_assert (ident_pattern != nullptr);
    return ident_pattern.get ();
  }

  ItemType get_item_type () const override final { return ItemType::IDENT_PAT; }

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
  bool has_mut;
  Identifier ident;

public:
  StructPatternFieldIdent (Identifier ident, bool is_ref, bool is_mut,
			   std::vector<Attribute> outer_attrs, location_t locus)
    : StructPatternField (std::move (outer_attrs), locus,
			  Analysis::Mappings::get ().get_next_node_id ()),
      has_ref (is_ref), has_mut (is_mut), ident (std::move (ident))
  {}

  std::string as_string () const override;

  void accept_vis (ASTVisitor &vis) override;

  // based on idea of identifier no longer existing
  void mark_for_strip () override { ident = {""}; }
  bool is_marked_for_strip () const override { return ident.empty (); }

  const Identifier &get_identifier () const { return ident; }

  ItemType get_item_type () const override final { return ItemType::IDENT; }

  bool is_ref () const { return has_ref; }

  bool is_mut () const { return has_mut; }

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
  // bool has_struct_pattern_fields;
  Cloneable<std::vector<std::unique_ptr<StructPatternField>>> fields;

  bool has_rest_pattern;
  std::vector<Attribute> struct_pattern_etc_attrs;
  // StructPatternEtc etc;

  // must have at least one of the two and maybe both

  // should this store location data?

public:
  // Returns whether there are any struct pattern fields
  bool has_struct_pattern_fields () const { return !fields.get ().empty (); }

  /* Returns whether the struct pattern elements is entirely empty (no fields,
   * no etc). */
  bool is_empty () const
  {
    return !has_struct_pattern_fields () && !has_rest_pattern;
  }

  bool has_rest () const { return has_rest_pattern; }

  // Constructor for StructPatternElements with both (potentially)
  StructPatternElements (
    std::vector<std::unique_ptr<StructPatternField>> fields,
    std::vector<Attribute> etc_attrs)
    : fields (std::move (fields)), has_rest_pattern (true),
      struct_pattern_etc_attrs (std::move (etc_attrs))
  {}

  // Constructor for StructPatternElements with no StructPatternEtc
  StructPatternElements (
    std::vector<std::unique_ptr<StructPatternField>> fields)
    : fields (std::move (fields)), has_rest_pattern (false),
      struct_pattern_etc_attrs ()
  {}

  // Creates an empty StructPatternElements
  static StructPatternElements create_empty ()
  {
    return StructPatternElements (
      std::vector<std::unique_ptr<StructPatternField>> ());
  }

  std::string as_string () const;

  // TODO: seems kinda dodgy. Think of better way.
  std::vector<std::unique_ptr<StructPatternField>> &get_struct_pattern_fields ()
  {
    return fields.get ();
  }
  const std::vector<std::unique_ptr<StructPatternField>> &
  get_struct_pattern_fields () const
  {
    return fields.get ();
  }

  std::vector<Attribute> &get_etc_outer_attrs ()
  {
    return struct_pattern_etc_attrs;
  }
  const std::vector<Attribute> &get_etc_outer_attrs () const
  {
    return struct_pattern_etc_attrs;
  }

  void strip_etc ()
  {
    has_rest_pattern = false;
    struct_pattern_etc_attrs.clear ();
    struct_pattern_etc_attrs.shrink_to_fit ();
  }
};

// Struct pattern AST node representation
class StructPattern : public Pattern
{
  PathInExpression path;

  // bool has_struct_pattern_elements;
  StructPatternElements elems;

  NodeId node_id;
  location_t locus;

public:
  std::string as_string () const override;

  // Constructs a struct pattern from specified StructPatternElements
  StructPattern (PathInExpression struct_path, location_t locus,
		 StructPatternElements elems
		 = StructPatternElements::create_empty ())
    : path (std::move (struct_path)), elems (std::move (elems)),
      node_id (Analysis::Mappings::get ().get_next_node_id ()), locus (locus)
  {}

  /* TODO: constructor to construct via elements included in
   * StructPatternElements */

  /* Returns whether struct pattern has any struct pattern elements (if not, it
   * is empty). */
  bool has_struct_pattern_elems () const { return !elems.is_empty (); }

  location_t get_locus () const override { return locus; }

  void accept_vis (ASTVisitor &vis) override;

  // TODO: seems kinda dodgy. Think of better way.
  StructPatternElements &get_struct_pattern_elems () { return elems; }
  const StructPatternElements &get_struct_pattern_elems () const
  {
    return elems;
  }

  PathInExpression &get_path () { return path; }
  const PathInExpression &get_path () const { return path; }

  NodeId get_node_id () const override { return node_id; }

  Pattern::Kind get_pattern_kind () override { return Pattern::Kind::Struct; }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  StructPattern *clone_pattern_impl () const override
  {
    return new StructPattern (*this);
  }
};

// Base abstract class for TupleStructItems, TuplePatternItems &
// SlicePatternItems
class PatternItems
{
public:
  enum ItemType
  {
    NO_REST,
    HAS_REST,
  };

  virtual ~PatternItems () {}

  // TODO: should this store location data?

  // Unique pointer custom clone function
  std::unique_ptr<PatternItems> clone_pattern_items () const
  {
    return std::unique_ptr<PatternItems> (clone_pattern_items_impl ());
  }

  virtual std::string as_string () const = 0;
  virtual ItemType get_item_type () const = 0;
  virtual void accept_vis (ASTVisitor &vis) = 0;

protected:
  virtual PatternItems *clone_pattern_items_impl () const = 0;
};

// Base abstract class for patterns used in TupleStructPattern
class TupleStructItems : public PatternItems
{
public:
  // Unique pointer custom clone function
  std::unique_ptr<TupleStructItems> clone_tuple_struct_items () const
  {
    return std::unique_ptr<TupleStructItems> (clone_pattern_items_impl ());
  }

protected:
  // pure virtual clone implementation
  virtual TupleStructItems *clone_pattern_items_impl () const = 0;
};

// Class for non-ranged tuple struct pattern patterns
class TupleStructItemsNoRest : public TupleStructItems
{
  Cloneable<std::vector<std::unique_ptr<Pattern>>> patterns;

public:
  TupleStructItemsNoRest (std::vector<std::unique_ptr<Pattern>> patterns)
    : patterns (std::move (patterns))
  {}

  std::string as_string () const override;

  void accept_vis (ASTVisitor &vis) override;

  // TODO: seems kinda dodgy. Think of better way.
  std::vector<std::unique_ptr<Pattern>> &get_patterns ()
  {
    return patterns.get ();
  }
  const std::vector<std::unique_ptr<Pattern>> &get_patterns () const
  {
    return patterns.get ();
  }

  ItemType get_item_type () const override final { return ItemType::NO_REST; }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  TupleStructItemsNoRest *clone_pattern_items_impl () const override
  {
    return new TupleStructItemsNoRest (*this);
  }
};

// Class for ranged tuple struct pattern patterns
class TupleStructItemsHasRest : public TupleStructItems
{
  Cloneable<std::vector<std::unique_ptr<Pattern>>> lower_patterns;
  Cloneable<std::vector<std::unique_ptr<Pattern>>> upper_patterns;

public:
  TupleStructItemsHasRest (std::vector<std::unique_ptr<Pattern>> lower_patterns,
			   std::vector<std::unique_ptr<Pattern>> upper_patterns)
    : lower_patterns (std::move (lower_patterns)),
      upper_patterns (std::move (upper_patterns))
  {}

  std::string as_string () const override;

  void accept_vis (ASTVisitor &vis) override;

  // TODO: seems kinda dodgy. Think of better way.
  std::vector<std::unique_ptr<Pattern>> &get_lower_patterns ()
  {
    return lower_patterns.get ();
  }
  const std::vector<std::unique_ptr<Pattern>> &get_lower_patterns () const
  {
    return lower_patterns.get ();
  }

  // TODO: seems kinda dodgy. Think of better way.
  std::vector<std::unique_ptr<Pattern>> &get_upper_patterns ()
  {
    return upper_patterns.get ();
  }
  const std::vector<std::unique_ptr<Pattern>> &get_upper_patterns () const
  {
    return upper_patterns.get ();
  }

  ItemType get_item_type () const override final { return ItemType::HAS_REST; }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  TupleStructItemsHasRest *clone_pattern_items_impl () const override
  {
    return new TupleStructItemsHasRest (*this);
  }
};

// AST node representing a tuple struct pattern
class TupleStructPattern : public Pattern
{
  PathInExpression path;
  Cloneable<std::unique_ptr<TupleStructItems>> items;
  NodeId node_id;

  /* TOOD: should this store location data? current accessor uses path location
   * data */

public:
  std::string as_string () const override;

  TupleStructPattern (PathInExpression tuple_struct_path,
		      std::unique_ptr<TupleStructItems> items)
    : path (std::move (tuple_struct_path)), items (std::move (items)),
      node_id (Analysis::Mappings::get ().get_next_node_id ())
  {
    rust_assert (this->items != nullptr);
  }

  location_t get_locus () const override { return path.get_locus (); }

  void accept_vis (ASTVisitor &vis) override;

  TupleStructItems &get_items ()
  {
    rust_assert (items != nullptr);
    return *items.get ();
  }

  PathInExpression &get_path () { return path; }
  const PathInExpression &get_path () const { return path; }

  NodeId get_node_id () const override { return node_id; }

  Pattern::Kind get_pattern_kind () override
  {
    return Pattern::Kind::TupleStruct;
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
class TuplePatternItems : public PatternItems
{
public:
  // Unique pointer custom clone function
  std::unique_ptr<TuplePatternItems> clone_tuple_pattern_items () const
  {
    return std::unique_ptr<TuplePatternItems> (clone_pattern_items_impl ());
  }

protected:
  // pure virtual clone implementation
  virtual TuplePatternItems *clone_pattern_items_impl () const = 0;
};

// Class representing TuplePattern patterns which contains no rest pattern
class TuplePatternItemsNoRest : public TuplePatternItems
{
  Cloneable<std::vector<std::unique_ptr<Pattern>>> patterns;

public:
  TuplePatternItemsNoRest (std::vector<std::unique_ptr<Pattern>> patterns)
    : patterns (std::move (patterns))
  {}

  std::string as_string () const override;

  void accept_vis (ASTVisitor &vis) override;

  // TODO: seems kinda dodgy. Think of better way.
  std::vector<std::unique_ptr<Pattern>> &get_patterns ()
  {
    return patterns.get ();
  }
  const std::vector<std::unique_ptr<Pattern>> &get_patterns () const
  {
    return patterns.get ();
  }

  ItemType get_item_type () const override { return ItemType::NO_REST; }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  TuplePatternItemsNoRest *clone_pattern_items_impl () const override
  {
    return new TuplePatternItemsNoRest (*this);
  }
};

// Class representing TuplePattern patterns which contains a rest pattern
class TuplePatternItemsHasRest : public TuplePatternItems
{
  Cloneable<std::vector<std::unique_ptr<Pattern>>> lower_patterns;
  Cloneable<std::vector<std::unique_ptr<Pattern>>> upper_patterns;

public:
  TuplePatternItemsHasRest (
    std::vector<std::unique_ptr<Pattern>> lower_patterns,
    std::vector<std::unique_ptr<Pattern>> upper_patterns)
    : lower_patterns (std::move (lower_patterns)),
      upper_patterns (std::move (upper_patterns))
  {}

  std::string as_string () const override;

  void accept_vis (ASTVisitor &vis) override;

  // TODO: seems kinda dodgy. Think of better way.
  std::vector<std::unique_ptr<Pattern>> &get_lower_patterns ()
  {
    return lower_patterns.get ();
  }
  const std::vector<std::unique_ptr<Pattern>> &get_lower_patterns () const
  {
    return lower_patterns.get ();
  }

  // TODO: seems kinda dodgy. Think of better way.
  std::vector<std::unique_ptr<Pattern>> &get_upper_patterns ()
  {
    return upper_patterns.get ();
  }
  const std::vector<std::unique_ptr<Pattern>> &get_upper_patterns () const
  {
    return upper_patterns.get ();
  }

  ItemType get_item_type () const override { return ItemType::HAS_REST; }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  TuplePatternItemsHasRest *clone_pattern_items_impl () const override
  {
    return new TuplePatternItemsHasRest (*this);
  }
};

// AST node representing a tuple pattern
class TuplePattern : public Pattern
{
  Cloneable<std::unique_ptr<TuplePatternItems>> items;
  location_t locus;
  NodeId node_id;

public:
  std::string as_string () const override;

  TuplePattern (std::unique_ptr<TuplePatternItems> items, location_t locus)
    : items (std::move (items)), locus (locus),
      node_id (Analysis::Mappings::get ().get_next_node_id ())
  {
    rust_assert (this->items != nullptr);
  }

  location_t get_locus () const override final { return locus; }

  void accept_vis (ASTVisitor &vis) override;

  // TODO: seems kinda dodgy. Think of better way.
  TuplePatternItems &get_items ()
  {
    rust_assert (items != nullptr);
    return *items.get ();
  }

  NodeId get_node_id () const override { return node_id; }

  Pattern::Kind get_pattern_kind () override { return Pattern::Kind::Tuple; }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  TuplePattern *clone_pattern_impl () const override
  {
    return new TuplePattern (*this);
  }
};

// AST node representing a pattern in parentheses, used to control precedence
class GroupedPattern : public Pattern
{
  Cloneable<std::unique_ptr<Pattern>> pattern_in_parens;
  location_t locus;
  NodeId node_id;

public:
  std::string as_string () const override
  {
    return "(" + pattern_in_parens.get ()->as_string () + ")";
  }

  GroupedPattern (std::unique_ptr<Pattern> pattern_in_parens, location_t locus)
    : pattern_in_parens (std::move (pattern_in_parens)), locus (locus),
      node_id (Analysis::Mappings::get ().get_next_node_id ())
  {}

  location_t get_locus () const override final { return locus; }

  void accept_vis (ASTVisitor &vis) override;

  // TODO: seems kinda dodgy. Think of better way.
  Pattern &get_pattern_in_parens ()
  {
    rust_assert (pattern_in_parens != nullptr);
    return *pattern_in_parens.get ();
  }

  std::unique_ptr<Pattern> &get_pattern_in_parens_ptr ()
  {
    rust_assert (pattern_in_parens != nullptr);
    return pattern_in_parens.get ();
  }

  NodeId get_node_id () const override { return node_id; }

  Pattern::Kind get_pattern_kind () override { return Pattern::Kind::Grouped; }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  GroupedPattern *clone_pattern_impl () const override
  {
    return new GroupedPattern (*this);
  }
};

// AST node representing patterns that can match slices and arrays
class SlicePattern : public Pattern
{
  Cloneable<std::vector<std::unique_ptr<Pattern>>> patterns;
  location_t locus;
  NodeId node_id;

public:
  std::string as_string () const override;

  SlicePattern (std::vector<std::unique_ptr<Pattern>> patterns,
		location_t locus)
    : patterns (std::move (patterns)), locus (locus),
      node_id (Analysis::Mappings::get ().get_next_node_id ())
  {}

  location_t get_locus () const override final { return locus; }

  void accept_vis (ASTVisitor &vis) override;

  // TODO: seems kinda dodgy. Think of better way.
  std::vector<std::unique_ptr<Pattern>> &get_patterns ()
  {
    return patterns.get ();
  }

  NodeId get_node_id () const override { return node_id; }

  Pattern::Kind get_pattern_kind () override { return Pattern::Kind::Slice; }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  SlicePattern *clone_pattern_impl () const override
  {
    return new SlicePattern (*this);
  }
};

// AST node for alternate patterns
// joins together what are technically 'PatternNoTopAlt's
class AltPattern : public Pattern
{
  Cloneable<std::vector<std::unique_ptr<Pattern>>> alts;
  location_t locus;
  NodeId node_id;

public:
  std::string as_string () const override;

  AltPattern (std::vector<std::unique_ptr<Pattern>> alts, location_t locus)
    : alts (std::move (alts)), locus (locus),
      node_id (Analysis::Mappings::get ().get_next_node_id ())
  {}

  location_t get_locus () const override final { return locus; }

  void accept_vis (ASTVisitor &vis) override;

  // TODO: seems kinda dodgy. Think of better way.
  std::vector<std::unique_ptr<Pattern>> &get_alts () { return alts.get (); }
  const std::vector<std::unique_ptr<Pattern>> &get_alts () const
  {
    return alts.get ();
  }

  NodeId get_node_id () const override { return node_id; }

  Pattern::Kind get_pattern_kind () override { return Pattern::Kind::Alt; }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  AltPattern *clone_pattern_impl () const override
  {
    return new AltPattern (*this);
  }
};

// Moved definition to rust-path.h
class Path;

// Forward decls for paths (defined in rust-path.h)
class PathInExpression;
class QualifiedPathInExpression;

// Replaced with forward decl - defined in rust-macro.h
class MacroInvocation;
} // namespace AST

template <> struct CloneableDelegate<std::unique_ptr<AST::RangePatternBound>>
{
  static std::unique_ptr<AST::RangePatternBound>
  clone (const std::unique_ptr<AST::RangePatternBound> &other)
  {
    if (other == nullptr)
      return nullptr;
    else
      return other->clone_range_pattern_bound ();
  }
};

template <> struct CloneableDelegate<std::unique_ptr<AST::TupleStructItems>>
{
  static std::unique_ptr<AST::TupleStructItems>
  clone (const std::unique_ptr<AST::TupleStructItems> &other)
  {
    if (other == nullptr)
      return nullptr;
    else
      return other->clone_tuple_struct_items ();
  }
};

template <> struct CloneableDelegate<std::unique_ptr<AST::TuplePatternItems>>
{
  static std::unique_ptr<AST::TuplePatternItems>
  clone (const std::unique_ptr<AST::TuplePatternItems> &other)
  {
    if (other == nullptr)
      return nullptr;
    else
      return other->clone_tuple_pattern_items ();
  }
};

template <> struct CloneableDelegate<std::unique_ptr<AST::StructPatternField>>
{
  static std::unique_ptr<AST::StructPatternField>
  clone (const std::unique_ptr<AST::StructPatternField> &other)
  {
    if (other == nullptr)
      return nullptr;
    else
      return other->clone_struct_pattern_field ();
  }
};

} // namespace Rust

#endif
