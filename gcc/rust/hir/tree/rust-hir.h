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

#ifndef RUST_HIR_BASE_H
#define RUST_HIR_BASE_H

#include "rust-ast.h"
#include "rust-system.h"
#include "rust-token.h"
#include "rust-location.h"
#include "rust-hir-map.h"
#include "rust-diagnostics.h"

namespace Rust {
typedef int TupleIndex;

namespace HIR {
// foward decl: ast visitor
class HIRFullVisitor;
class HIRStmtVisitor;
class HIRTraitItemVisitor;
class HIRExternalItemVisitor;
class HIRVisItemVisitor;
class HIRExpressionVisitor;
class HIRPatternVisitor;
class HIRImplVisitor;
class HIRTypeVisitor;

class WithOuterAttrs
{
protected:
  AST::AttrVec outer_attrs;

public:
  AST::AttrVec &get_outer_attrs () { return outer_attrs; }
  const AST::AttrVec &get_outer_attrs () const { return outer_attrs; }

  WithOuterAttrs (AST::AttrVec outer_attrs)
    : outer_attrs (std::move (outer_attrs)){};
};

class WithInnerAttrs
{
protected:
  AST::AttrVec inner_attrs;

public:
  AST::AttrVec get_inner_attrs () const { return inner_attrs; }
  WithInnerAttrs (AST::AttrVec inner_attrs)
    : inner_attrs (std::move (inner_attrs)){};
};

class FullVisitable
{
public:
  virtual void accept_vis (HIRFullVisitor &vis) = 0;
};

// forward decl for use in token tree method
class Token;

class Node
{
public:
  // Kind for downcasting various HIR nodes to other base classes when visiting
  // them
  enum BaseKind
  {
    /* class ExternalItem */
    EXTERNAL,
    /* class TraitItem */
    TRAIT_ITEM,
    /* class VisItem */
    VIS_ITEM,
    /* class Item */
    ITEM,
    /* class ImplItem */
    IMPL,
    /* class Type */
    TYPE,
    /* class Stmt */
    STMT,
    /* class Expr */
    EXPR,
    /* class Pattern */
    PATTERN,
  };

  /**
   * Get the kind of HIR node we are dealing with. This is useful for
   * downcasting to more precise types when necessary, i.e going from an `Item*`
   * to a `VisItem*`
   */
  virtual BaseKind get_hir_kind () = 0;
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
    BOOL
  };

private:
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
    return Literal ("", CHAR, PrimitiveCoreType::CORETYPE_UNKNOWN);
  }

  void set_lit_type (LitType lt) { type = lt; }

  // Returns whether literal is in an invalid state.
  bool is_error () const { return value_as_string == ""; }

  bool is_equal (Literal &other)
  {
    return value_as_string == other.value_as_string && type == other.type
	   && type_hint == other.type_hint;
  }
};

/* Base statement abstract class. Note that most "statements" are not allowed in
 * top-level module scope - only a subclass of statements called "items" are. */
class Stmt : public Node, public FullVisitable
{
public:
  using FullVisitable::accept_vis;

  // Unique pointer custom clone function
  std::unique_ptr<Stmt> clone_stmt () const
  {
    return std::unique_ptr<Stmt> (clone_stmt_impl ());
  }

  BaseKind get_hir_kind () override { return STMT; }

  virtual ~Stmt () {}

  virtual std::string as_string () const = 0;

  virtual void accept_vis (HIRStmtVisitor &vis) = 0;

  virtual location_t get_locus () const = 0;

  virtual bool is_unit_check_needed () const { return false; }

  const Analysis::NodeMapping &get_mappings () const { return mappings; }

  virtual bool is_item () const = 0;

protected:
  Stmt (Analysis::NodeMapping mappings) : mappings (std::move (mappings)) {}

  // Clone function implementation as pure virtual method
  virtual Stmt *clone_stmt_impl () const = 0;

  Analysis::NodeMapping mappings;
};

// Rust "item" HIR node (declaration of top-level/module-level allowed stuff)
class Item : public Stmt, public WithOuterAttrs
{
  // TODO: should outer attrs be defined here or in each derived class?
public:
  enum class ItemKind
  {
    Static,
    Constant,
    TypeAlias,
    Function,
    UseDeclaration,
    ExternBlock,
    ExternCrate,
    Struct,
    Union,
    Enum,
    EnumItem, // FIXME: ARTHUR: Do we need that?
    Trait,
    Impl,
    Module,
  };

  virtual ItemKind get_item_kind () const = 0;

  // Unique pointer custom clone function
  std::unique_ptr<Item> clone_item () const
  {
    return std::unique_ptr<Item> (clone_item_impl ());
  }

  BaseKind get_hir_kind () override { return ITEM; }

  std::string as_string () const override;

  /* Adds crate names to the vector passed by reference, if it can
   * (polymorphism). */
  virtual void
  add_crate_name (std::vector<std::string> &names ATTRIBUTE_UNUSED) const
  {}

  bool is_item () const override final { return true; }

protected:
  // Constructor
  Item (Analysis::NodeMapping mappings,
	AST::AttrVec outer_attribs = AST::AttrVec ())
    : Stmt (std::move (mappings)), WithOuterAttrs (std::move (outer_attribs))
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

// Base expression HIR node - abstract
class Expr : public Node, virtual public FullVisitable
{
public:
  using FullVisitable::accept_vis;

protected:
  AST::AttrVec outer_attrs;
  Analysis::NodeMapping mappings;

public:
  enum BlockType
  {
    WITH_BLOCK,
    WITHOUT_BLOCK,
  };

  enum ExprType
  {
    Lit,
    Operator,
    Grouped,
    Array,
    ArrayIndex,
    Tuple,
    TupleIdx,
    Struct,
    Call,
    MethodCall,
    FieldAccess,
    Closure,
    Block,
    Continue,
    Break,
    Range,
    Return,
    UnsafeBlock,
    BaseLoop,
    If,
    IfLet,
    Match,
    Await,
    AsyncBlock,
    Path,
  };

  BaseKind get_hir_kind () override final { return EXPR; }

  const AST::AttrVec &get_outer_attrs () const { return outer_attrs; }

  // Unique pointer custom clone function
  std::unique_ptr<Expr> clone_expr () const
  {
    return std::unique_ptr<Expr> (clone_expr_impl ());
  }

  // TODO: make pure virtual if move out outer attributes to derived classes
  virtual std::string as_string () const;

  virtual ~Expr () {}

  virtual location_t get_locus () const = 0;

  const Analysis::NodeMapping &get_mappings () const { return mappings; }

  // Clone function implementation as pure virtual method
  virtual Expr *clone_expr_impl () const = 0;

  virtual BlockType get_block_expr_type () const = 0;

  virtual ExprType get_expression_type () const = 0;

  virtual void accept_vis (HIRExpressionVisitor &vis) = 0;

protected:
  // Constructor
  Expr (Analysis::NodeMapping mappings,
	AST::AttrVec outer_attribs = AST::AttrVec ())
    : outer_attrs (std::move (outer_attribs)), mappings (std::move (mappings))
  {}

  // TODO: think of less hacky way to implement this kind of thing
  // Sets outer attributes.
  void set_outer_attrs (AST::AttrVec outer_attrs_to_set)
  {
    outer_attrs = std::move (outer_attrs_to_set);
  }
};

// HIR node for an expression without an accompanying block - abstract
class ExprWithoutBlock : public Expr
{
protected:
  // Constructor
  ExprWithoutBlock (Analysis::NodeMapping mappings,
		    AST::AttrVec outer_attribs = AST::AttrVec ())
    : Expr (std::move (mappings), std::move (outer_attribs))
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

public:
  // Unique pointer custom clone function
  std::unique_ptr<ExprWithoutBlock> clone_expr_without_block () const
  {
    return std::unique_ptr<ExprWithoutBlock> (clone_expr_without_block_impl ());
  }

  BlockType get_block_expr_type () const final override
  {
    return BlockType::WITHOUT_BLOCK;
  };
};

// Pattern base HIR node
class Pattern : public Node, virtual public FullVisitable
{
public:
  using FullVisitable::accept_vis;

  enum PatternType
  {
    PATH,
    LITERAL,
    IDENTIFIER,
    WILDCARD,
    RANGE,
    REFERENCE,
    STRUCT,
    TUPLE_STRUCT,
    TUPLE,
    GROUPED,
    SLICE,
    ALT
  };

  BaseKind get_hir_kind () override final { return PATTERN; }

  // Unique pointer custom clone function
  std::unique_ptr<Pattern> clone_pattern () const
  {
    return std::unique_ptr<Pattern> (clone_pattern_impl ());
  }

  // possible virtual methods: is_refutable()

  virtual ~Pattern () {}

  virtual std::string as_string () const = 0;

  virtual void accept_vis (HIRPatternVisitor &vis) = 0;

  virtual const Analysis::NodeMapping &get_mappings () const = 0;

  virtual location_t get_locus () const = 0;

  virtual PatternType get_pattern_type () const = 0;

protected:
  // Clone pattern implementation as pure virtual method
  virtual Pattern *clone_pattern_impl () const = 0;
};

// forward decl for Type
class TraitBound;

// Base class for types as represented in HIR - abstract
class Type : public Node, public FullVisitable
{
public:
  using FullVisitable::accept_vis;
  // Unique pointer custom clone function
  std::unique_ptr<Type> clone_type () const
  {
    return std::unique_ptr<Type> (clone_type_impl ());
  }

  // virtual destructor
  virtual ~Type () {}

  BaseKind get_hir_kind () override final { return TYPE; }

  virtual std::string as_string () const = 0;

  /* HACK: convert to trait bound. Virtual method overriden by classes that
   * enable this. */
  virtual TraitBound *to_trait_bound (bool in_parens ATTRIBUTE_UNUSED) const
  {
    return nullptr;
  }
  /* as pointer, shouldn't require definition beforehand, only forward
   * declaration. */

  virtual void accept_vis (HIRTypeVisitor &vis) = 0;

  virtual Analysis::NodeMapping get_mappings () const { return mappings; }
  virtual location_t get_locus () const { return locus; }

protected:
  Type (Analysis::NodeMapping mappings, location_t locus)
    : mappings (mappings), locus (locus)
  {}

  // Clone function implementation as pure virtual method
  virtual Type *clone_type_impl () const = 0;

  Analysis::NodeMapping mappings;
  location_t locus;
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
  TypeNoBounds (Analysis::NodeMapping mappings, location_t locus)
    : Type (mappings, locus)
  {}

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
class TypeParamBound : public FullVisitable
{
public:
  using FullVisitable::accept_vis;
  enum BoundType
  {
    LIFETIME,
    TRAITBOUND
  };

  virtual ~TypeParamBound () {}

  // Unique pointer custom clone function
  std::unique_ptr<TypeParamBound> clone_type_param_bound () const
  {
    return std::unique_ptr<TypeParamBound> (clone_type_param_bound_impl ());
  }

  virtual std::string as_string () const = 0;

  virtual Analysis::NodeMapping get_mappings () const = 0;

  virtual location_t get_locus () const = 0;

  virtual BoundType get_bound_type () const = 0;

protected:
  // Clone function implementation as pure virtual method
  virtual TypeParamBound *clone_type_param_bound_impl () const = 0;
};

// Represents a lifetime (and is also a kind of type param bound)
class Lifetime : public TypeParamBound
{
private:
  AST::Lifetime::LifetimeType lifetime_type;
  std::string lifetime_name;
  location_t locus;
  Analysis::NodeMapping mappings;

public:
  // Constructor
  Lifetime (Analysis::NodeMapping mapping, AST::Lifetime::LifetimeType type,
	    std::string name, location_t locus)
    : lifetime_type (type), lifetime_name (std::move (name)), locus (locus),
      mappings (mapping)
  {}

  // Returns true if the lifetime is in an error state.
  bool is_error () const
  {
    return lifetime_type == AST::Lifetime::LifetimeType::NAMED
	   && lifetime_name.empty ();
  }

  static Lifetime error ()
  {
    return Lifetime (Analysis::NodeMapping::get_error (),
		     AST::Lifetime::LifetimeType::NAMED, "", UNDEF_LOCATION);
  }

  std::string as_string () const override;

  void accept_vis (HIRFullVisitor &vis) override;

  WARN_UNUSED_RESULT const std::string &get_name () const
  {
    return lifetime_name;
  }

  AST::Lifetime::LifetimeType get_lifetime_type () const
  {
    return lifetime_type;
  }

  location_t get_locus () const override final { return locus; }

  Analysis::NodeMapping get_mappings () const override final
  {
    return mappings;
  }

  BoundType get_bound_type () const final override { return LIFETIME; }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  Lifetime *clone_type_param_bound_impl () const override
  {
    return new Lifetime (*this);
  }
};

/* Base generic parameter in HIR. Abstract - can be represented by a Lifetime or
 * Type param */
class GenericParam : public FullVisitable
{
public:
  using FullVisitable::accept_vis;

  virtual ~GenericParam () {}

  enum class GenericKind
  {
    TYPE,
    LIFETIME,
    CONST,
  };

  // Unique pointer custom clone function
  std::unique_ptr<GenericParam> clone_generic_param () const
  {
    return std::unique_ptr<GenericParam> (clone_generic_param_impl ());
  }

  virtual std::string as_string () const = 0;

  virtual location_t get_locus () const = 0;

  Analysis::NodeMapping get_mappings () const { return mappings; }

  enum GenericKind get_kind () const { return kind; }

protected:
  // Clone function implementation as pure virtual method
  virtual GenericParam *clone_generic_param_impl () const = 0;

  Analysis::NodeMapping mappings;

  enum GenericKind kind;

  GenericParam (Analysis::NodeMapping mapping,
		enum GenericKind kind = GenericKind::TYPE)
    : mappings (mapping), kind (kind)
  {}
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
  AST::Attribute outer_attr;

  location_t locus;

public:
  Lifetime get_lifetime () { return lifetime; }

  // Returns whether the lifetime param has any lifetime bounds.
  bool has_lifetime_bounds () const { return !lifetime_bounds.empty (); }

  std::vector<Lifetime> &get_lifetime_bounds () { return lifetime_bounds; }

  // Returns whether the lifetime param has an outer attribute.
  bool has_outer_attribute () const { return !outer_attr.is_empty (); }

  // Returns whether the lifetime param is in an error state.
  bool is_error () const { return lifetime.is_error (); }

  // Constructor
  LifetimeParam (Analysis::NodeMapping mappings, Lifetime lifetime,
		 location_t locus = UNDEF_LOCATION,
		 std::vector<Lifetime> lifetime_bounds
		 = std::vector<Lifetime> (),
		 AST::Attribute outer_attr = AST::Attribute::create_empty ())
    : GenericParam (mappings, GenericKind::LIFETIME),
      lifetime (std::move (lifetime)),
      lifetime_bounds (std::move (lifetime_bounds)),
      outer_attr (std::move (outer_attr)), locus (locus)
  {}

  // TODO: remove copy and assignment operator definitions - not required

  // Copy constructor with clone
  LifetimeParam (LifetimeParam const &other)
    : GenericParam (other.mappings, GenericKind::LIFETIME),
      lifetime (other.lifetime), lifetime_bounds (other.lifetime_bounds),
      outer_attr (other.outer_attr), locus (other.locus)
  {}

  // Overloaded assignment operator to clone attribute
  LifetimeParam &operator= (LifetimeParam const &other)
  {
    lifetime = other.lifetime;
    lifetime_bounds = other.lifetime_bounds;
    outer_attr = other.outer_attr;
    locus = other.locus;
    mappings = other.mappings;

    return *this;
  }

  // move constructors
  LifetimeParam (LifetimeParam &&other) = default;
  LifetimeParam &operator= (LifetimeParam &&other) = default;

  std::string as_string () const override;

  void accept_vis (HIRFullVisitor &vis) override;

  location_t get_locus () const override final { return locus; }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  LifetimeParam *clone_generic_param_impl () const override
  {
    return new LifetimeParam (*this);
  }
};

class ConstGenericParam : public GenericParam
{
public:
  ConstGenericParam (std::string name, std::unique_ptr<Type> type,
		     std::unique_ptr<Expr> default_expression,
		     Analysis::NodeMapping mapping, location_t locus)
    : GenericParam (mapping, GenericKind::CONST), name (std::move (name)),
      type (std::move (type)),
      default_expression (std::move (default_expression)), locus (locus)
  {}

  ConstGenericParam (const ConstGenericParam &other) : GenericParam (other)
  {
    name = other.name;
    locus = other.locus;

    if (other.type)
      type = other.type->clone_type ();
    if (other.default_expression)
      default_expression = other.default_expression->clone_expr ();
  }

  std::string as_string () const override final;

  void accept_vis (HIRFullVisitor &vis) override final;

  location_t get_locus () const override final { return locus; };

  bool has_default_expression () { return default_expression != nullptr; }

  std::string get_name () { return name; }
  std::unique_ptr<Type> &get_type () { return type; }
  std::unique_ptr<Expr> &get_default_expression ()
  {
    return default_expression;
  }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  ConstGenericParam *clone_generic_param_impl () const override
  {
    return new ConstGenericParam (*this);
  }

private:
  std::string name;
  std::unique_ptr<Type> type;

  /* Optional - can be a null pointer if there is no default expression */
  std::unique_ptr<Expr> default_expression;

  location_t locus;
};

// Item used in trait declarations - abstract base class
class TraitItem : public Node, public FullVisitable
{
public:
  using FullVisitable::accept_vis;
  enum TraitItemKind
  {
    FUNC,
    CONST,
    TYPE
  };

  BaseKind get_hir_kind () override final { return TRAIT_ITEM; }

protected:
  // Constructor
  TraitItem (Analysis::NodeMapping mappings) : mappings (mappings) {}

  // Clone function implementation as pure virtual method
  virtual TraitItem *clone_trait_item_impl () const = 0;

  Analysis::NodeMapping mappings;

public:
  virtual ~TraitItem () {}

  std::unique_ptr<TraitItem> clone_trait_item () const
  {
    return std::unique_ptr<TraitItem> (clone_trait_item_impl ());
  }

  virtual std::string as_string () const = 0;

  virtual void accept_vis (HIRTraitItemVisitor &vis) = 0;

  virtual const std::string trait_identifier () const = 0;

  const Analysis::NodeMapping &get_mappings () const { return mappings; }

  virtual location_t get_trait_locus () const = 0;

  virtual TraitItemKind get_item_kind () const = 0;

  virtual AST::AttrVec &get_outer_attrs () = 0;
  virtual const AST::AttrVec &get_outer_attrs () const = 0;
};

class ImplItem : public Node, public FullVisitable
{
public:
  using FullVisitable::accept_vis;
  enum ImplItemType
  {
    FUNCTION,
    TYPE_ALIAS,
    CONSTANT
  };

  virtual ~ImplItem () {}

  BaseKind get_hir_kind () override final { return IMPL; }

  // Unique pointer custom clone function
  std::unique_ptr<ImplItem> clone_inherent_impl_item () const
  {
    return std::unique_ptr<ImplItem> (clone_inherent_impl_item_impl ());
  }

  virtual std::string as_string () const = 0;

  virtual void accept_vis (HIRImplVisitor &vis) = 0;
  virtual void accept_vis (HIRStmtVisitor &vis) = 0;

  virtual Analysis::NodeMapping get_impl_mappings () const = 0;

  virtual location_t get_locus () const = 0;

  virtual ImplItemType get_impl_item_type () const = 0;

  virtual std::string get_impl_item_name () const = 0;

protected:
  // Clone function implementation as pure virtual method
  virtual ImplItem *clone_inherent_impl_item_impl () const = 0;
};

// A crate HIR object - holds all the data for a single compilation unit
class Crate : public WithInnerAttrs
{
  // dodgy spacing required here
  /* TODO: is it better to have a vector of items here or a module (implicit
   * top-level one)? */
  std::vector<std::unique_ptr<Item>> items;

  Analysis::NodeMapping mappings;

public:
  // Constructor
  Crate (std::vector<std::unique_ptr<Item>> items, AST::AttrVec inner_attrs,
	 Analysis::NodeMapping mappings)
    : WithInnerAttrs (std::move (inner_attrs)), items (std::move (items)),
      mappings (mappings)
  {}

  // Copy constructor with vector clone
  Crate (Crate const &other)
    : WithInnerAttrs (other.inner_attrs), mappings (other.mappings)
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
    mappings = other.mappings;

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

  const Analysis::NodeMapping &get_mappings () const { return mappings; }
  std::vector<std::unique_ptr<Item>> &get_items () { return items; }
};

// Base path expression HIR node - abstract
class PathExpr : public ExprWithoutBlock
{
protected:
  PathExpr (Analysis::NodeMapping mappings, AST::AttrVec outer_attribs)
    : ExprWithoutBlock (std::move (mappings), std::move (outer_attribs))
  {}

public:
  /* Replaces the outer attributes of this path expression with the given outer
   * attributes. */
  void replace_outer_attrs (AST::AttrVec outer_attrs)
  {
    set_outer_attrs (std::move (outer_attrs));
  }

  ExprType get_expression_type () const final override
  {
    return ExprType::Path;
  }
};
} // namespace HIR
} // namespace Rust

#endif
