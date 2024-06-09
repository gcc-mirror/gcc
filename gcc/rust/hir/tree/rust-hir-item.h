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

#ifndef RUST_HIR_ITEM_H
#define RUST_HIR_ITEM_H

#include "rust-abi.h"
#include "rust-ast-full-decls.h"
#include "rust-common.h"
#include "rust-hir-expr.h"
#include "rust-hir.h"
#include "rust-hir-path.h"

namespace Rust {
namespace HIR {
// forward decls
class BlockExpr;
class TypePath;

// A type generic parameter (as opposed to a lifetime generic parameter)
class TypeParam : public GenericParam
{
  // bool has_outer_attribute;
  // std::unique_ptr<Attribute> outer_attr;
  AST::Attribute outer_attr;

  Identifier type_representation;

  // bool has_type_param_bounds;
  // TypeParamBounds type_param_bounds;
  std::vector<std::unique_ptr<TypeParamBound>>
    type_param_bounds; // inlined form

  // bool has_type;
  std::unique_ptr<Type> type;

  location_t locus;

public:
  // Returns whether the type of the type param has been specified.
  bool has_type () const { return type != nullptr; }

  // Returns whether the type param has type param bounds.
  bool has_type_param_bounds () const { return !type_param_bounds.empty (); }

  // Returns whether the type param has an outer attribute.
  bool has_outer_attribute () const { return !outer_attr.is_empty (); }
  AST::Attribute &get_outer_attribute () { return outer_attr; }

  TypeParam (Analysis::NodeMapping mappings, Identifier type_representation,
	     location_t locus = UNDEF_LOCATION,
	     std::vector<std::unique_ptr<TypeParamBound>> type_param_bounds
	     = std::vector<std::unique_ptr<TypeParamBound>> (),
	     std::unique_ptr<Type> type = nullptr,
	     AST::Attribute outer_attr = AST::Attribute::create_empty ())
    : GenericParam (mappings), outer_attr (std::move (outer_attr)),
      type_representation (std::move (type_representation)),
      type_param_bounds (std::move (type_param_bounds)),
      type (std::move (type)), locus (locus)
  {}

  // Copy constructor uses clone
  TypeParam (TypeParam const &other)
    : GenericParam (other.mappings), outer_attr (other.outer_attr),
      type_representation (other.type_representation), locus (other.locus)
  {
    // guard to prevent null pointer dereference
    if (other.type != nullptr)
      type = other.type->clone_type ();

    type_param_bounds.reserve (other.type_param_bounds.size ());
    for (const auto &e : other.type_param_bounds)
      type_param_bounds.push_back (e->clone_type_param_bound ());
  }

  // Overloaded assignment operator to clone
  TypeParam &operator= (TypeParam const &other)
  {
    type_representation = other.type_representation;
    outer_attr = other.outer_attr;
    locus = other.locus;
    mappings = other.mappings;

    // guard to prevent null pointer dereference
    if (other.type != nullptr)
      type = other.type->clone_type ();
    else
      type = nullptr;

    type_param_bounds.reserve (other.type_param_bounds.size ());
    for (const auto &e : other.type_param_bounds)
      type_param_bounds.push_back (e->clone_type_param_bound ());

    return *this;
  }
  // move constructors
  TypeParam (TypeParam &&other) = default;
  TypeParam &operator= (TypeParam &&other) = default;

  std::string as_string () const override;

  location_t get_locus () const override final { return locus; }

  void accept_vis (HIRFullVisitor &vis) override;

  Identifier get_type_representation () const { return type_representation; }

  std::unique_ptr<Type> &get_type () { return type; }

  Analysis::NodeMapping get_type_mappings () const
  {
    rust_assert (type != nullptr);
    return type->get_mappings ();
  }

  std::vector<std::unique_ptr<TypeParamBound>> &get_type_param_bounds ()
  {
    return type_param_bounds;
  }

protected:
  // Clone function implementation as (not pure) virtual method
  TypeParam *clone_generic_param_impl () const override
  {
    return new TypeParam (*this);
  }
};

/* "where" clause item base. Abstract - use LifetimeWhereClauseItem,
 * TypeBoundWhereClauseItem */
class WhereClauseItem : public FullVisitable
{
public:
  enum ItemType
  {
    LIFETIME,
    TYPE_BOUND,
  };

  virtual ~WhereClauseItem () {}

  // Unique pointer custom clone function
  std::unique_ptr<WhereClauseItem> clone_where_clause_item () const
  {
    return std::unique_ptr<WhereClauseItem> (clone_where_clause_item_impl ());
  }

  virtual std::string as_string () const = 0;

  virtual void accept_vis (HIRFullVisitor &vis) = 0;

  virtual Analysis::NodeMapping get_mappings () const = 0;

  virtual ItemType get_item_type () const = 0;

protected:
  // Clone function implementation as pure virtual method
  virtual WhereClauseItem *clone_where_clause_item_impl () const = 0;
};

// A lifetime where clause item
class LifetimeWhereClauseItem : public WhereClauseItem
{
  Lifetime lifetime;
  std::vector<Lifetime> lifetime_bounds;
  location_t locus;
  Analysis::NodeMapping mappings;

public:
  LifetimeWhereClauseItem (Analysis::NodeMapping mappings, Lifetime lifetime,
			   std::vector<Lifetime> lifetime_bounds,
			   location_t locus)
    : lifetime (std::move (lifetime)),
      lifetime_bounds (std::move (lifetime_bounds)), locus (locus),
      mappings (std::move (mappings))
  {}

  std::string as_string () const override;

  void accept_vis (HIRFullVisitor &vis) override;

  Lifetime &get_lifetime () { return lifetime; }

  std::vector<Lifetime> &get_lifetime_bounds () { return lifetime_bounds; }

  Analysis::NodeMapping get_mappings () const override final
  {
    return mappings;
  };

  ItemType get_item_type () const override final
  {
    return WhereClauseItem::ItemType::LIFETIME;
  }

protected:
  // Clone function implementation as (not pure) virtual method
  LifetimeWhereClauseItem *clone_where_clause_item_impl () const override
  {
    return new LifetimeWhereClauseItem (*this);
  }
};

// A type bound where clause item
class TypeBoundWhereClauseItem : public WhereClauseItem
{
  std::vector<LifetimeParam> for_lifetimes;
  std::unique_ptr<Type> bound_type;
  std::vector<std::unique_ptr<TypeParamBound>> type_param_bounds;
  Analysis::NodeMapping mappings;
  location_t locus;

public:
  // Returns whether the item has ForLifetimes
  bool has_for_lifetimes () const { return !for_lifetimes.empty (); }

  // Returns whether the item has type param bounds
  bool has_type_param_bounds () const { return !type_param_bounds.empty (); }

  TypeBoundWhereClauseItem (
    Analysis::NodeMapping mappings, std::vector<LifetimeParam> for_lifetimes,
    std::unique_ptr<Type> bound_type,
    std::vector<std::unique_ptr<TypeParamBound>> type_param_bounds,
    location_t locus)
    : for_lifetimes (std::move (for_lifetimes)),
      bound_type (std::move (bound_type)),
      type_param_bounds (std::move (type_param_bounds)),
      mappings (std::move (mappings)), locus (locus)
  {}

  // Copy constructor requires clone
  TypeBoundWhereClauseItem (TypeBoundWhereClauseItem const &other)
    : for_lifetimes (other.for_lifetimes),
      bound_type (other.bound_type->clone_type ()), mappings (other.mappings)
  {
    type_param_bounds.reserve (other.type_param_bounds.size ());
    for (const auto &e : other.type_param_bounds)
      type_param_bounds.push_back (e->clone_type_param_bound ());
  }

  // Overload assignment operator to clone
  TypeBoundWhereClauseItem &operator= (TypeBoundWhereClauseItem const &other)
  {
    mappings = other.mappings;
    for_lifetimes = other.for_lifetimes;
    bound_type = other.bound_type->clone_type ();
    type_param_bounds.reserve (other.type_param_bounds.size ());
    for (const auto &e : other.type_param_bounds)
      type_param_bounds.push_back (e->clone_type_param_bound ());

    return *this;
  }

  // move constructors
  TypeBoundWhereClauseItem (TypeBoundWhereClauseItem &&other) = default;
  TypeBoundWhereClauseItem &operator= (TypeBoundWhereClauseItem &&other)
    = default;

  location_t get_locus () const { return locus; }

  std::string as_string () const override;

  void accept_vis (HIRFullVisitor &vis) override;

  std::vector<LifetimeParam> &get_for_lifetimes () { return for_lifetimes; }

  std::unique_ptr<Type> &get_bound_type () { return bound_type; }

  std::vector<std::unique_ptr<TypeParamBound>> &get_type_param_bounds ()
  {
    return type_param_bounds;
  }

  Analysis::NodeMapping get_mappings () const override final
  {
    return mappings;
  };

  ItemType get_item_type () const override final
  {
    return WhereClauseItem::ItemType::TYPE_BOUND;
  }

protected:
  // Clone function implementation as (not pure) virtual method
  TypeBoundWhereClauseItem *clone_where_clause_item_impl () const override
  {
    return new TypeBoundWhereClauseItem (*this);
  }
};

// A where clause
struct WhereClause
{
private:
  std::vector<std::unique_ptr<WhereClauseItem>> where_clause_items;

  // should this store location info?

public:
  WhereClause (std::vector<std::unique_ptr<WhereClauseItem>> where_clause_items)
    : where_clause_items (std::move (where_clause_items))
  {}

  // copy constructor with vector clone
  WhereClause (WhereClause const &other)
  {
    where_clause_items.reserve (other.where_clause_items.size ());
    for (const auto &e : other.where_clause_items)
      where_clause_items.push_back (e->clone_where_clause_item ());
  }

  // overloaded assignment operator with vector clone
  WhereClause &operator= (WhereClause const &other)
  {
    where_clause_items.reserve (other.where_clause_items.size ());
    for (const auto &e : other.where_clause_items)
      where_clause_items.push_back (e->clone_where_clause_item ());

    return *this;
  }

  // move constructors
  WhereClause (WhereClause &&other) = default;
  WhereClause &operator= (WhereClause &&other) = default;

  // Creates a WhereClause with no items.
  static WhereClause create_empty ()
  {
    return WhereClause (std::vector<std::unique_ptr<WhereClauseItem>> ());
  }

  // Returns whether the WhereClause has no items.
  bool is_empty () const { return where_clause_items.empty (); }

  std::string as_string () const;

  std::vector<std::unique_ptr<WhereClauseItem>> &get_items ()
  {
    return where_clause_items;
  }
  const std::vector<std::unique_ptr<WhereClauseItem>> &get_items () const
  {
    return where_clause_items;
  }
};

// A self parameter in a method
struct SelfParam
{
public:
  enum ImplicitSelfKind
  {
    IMM,     // self
    MUT,     // mut self
    IMM_REF, // &self
    MUT_REF, // &mut self
    NONE
  };

private:
  ImplicitSelfKind self_kind;
  Lifetime lifetime;
  std::unique_ptr<Type> type;
  location_t locus;
  Analysis::NodeMapping mappings;

  SelfParam (Analysis::NodeMapping mappings, ImplicitSelfKind self_kind,
	     Lifetime lifetime, Type *type)
    : self_kind (self_kind), lifetime (std::move (lifetime)), type (type),
      mappings (mappings)
  {}

public:
  // Type-based self parameter (not ref, no lifetime)
  SelfParam (Analysis::NodeMapping mappings, std::unique_ptr<Type> type,
	     bool is_mut, location_t locus)
    : self_kind (is_mut ? ImplicitSelfKind::MUT : ImplicitSelfKind::IMM),
      lifetime (
	Lifetime (mappings, AST::Lifetime::LifetimeType::NAMED, "", locus)),
      type (std::move (type)), locus (locus), mappings (mappings)
  {}

  // Lifetime-based self parameter (is ref, no type)
  SelfParam (Analysis::NodeMapping mappings, Lifetime lifetime, bool is_mut,
	     location_t locus)
    : self_kind (is_mut ? ImplicitSelfKind::MUT_REF
			: ImplicitSelfKind::IMM_REF),
      lifetime (std::move (lifetime)), locus (locus), mappings (mappings)
  {}

  // Copy constructor requires clone
  SelfParam (SelfParam const &other)
    : self_kind (other.self_kind), lifetime (other.lifetime),
      locus (other.locus), mappings (other.mappings)
  {
    if (other.type != nullptr)
      type = other.type->clone_type ();
  }

  // Overload assignment operator to use clone
  SelfParam &operator= (SelfParam const &other)
  {
    if (other.type != nullptr)
      type = other.type->clone_type ();

    self_kind = other.self_kind;
    lifetime = other.lifetime;
    locus = other.locus;
    mappings = other.mappings;

    return *this;
  }

  // move constructors
  SelfParam (SelfParam &&other) = default;
  SelfParam &operator= (SelfParam &&other) = default;

  static SelfParam error ()
  {
    return SelfParam (Analysis::NodeMapping::get_error (),
		      ImplicitSelfKind::NONE, Lifetime::error (), nullptr);
  }

  // Returns whether the self-param has a type field.
  bool has_type () const { return type != nullptr; }

  // Returns whether the self-param has a valid lifetime.
  bool has_lifetime () const { return !lifetime.is_error (); }

  const Lifetime &get_lifetime () const { return lifetime; }

  // Returns whether the self-param is in an error state.
  bool is_error () const { return self_kind == ImplicitSelfKind::NONE; }

  std::string as_string () const;

  location_t get_locus () const { return locus; }

  ImplicitSelfKind get_self_kind () const { return self_kind; }

  std::unique_ptr<Type> &get_type () { return type; }

  Analysis::NodeMapping get_mappings () { return mappings; }

  Mutability get_mut () const
  {
    return (self_kind == ImplicitSelfKind::MUT
	    || self_kind == ImplicitSelfKind::MUT_REF)
	     ? Mutability::Mut
	     : Mutability::Imm;
  }

  bool is_mut () const
  {
    return self_kind == ImplicitSelfKind::MUT
	   || self_kind == ImplicitSelfKind::MUT_REF;
  }

  bool is_ref () const
  {
    return self_kind == ImplicitSelfKind::IMM_REF
	   || self_kind == ImplicitSelfKind::MUT_REF;
  }
};

// Qualifiers for function, i.e. const, unsafe, extern etc.
struct FunctionQualifiers
{
private:
  Async async_status;
  Const const_status;
  Unsafety unsafety;
  bool has_extern;
  ABI abi;

public:
  FunctionQualifiers (Async async_status, Const const_status, Unsafety unsafety,
		      bool has_extern, ABI abi)
    : async_status (async_status), const_status (const_status),
      unsafety (unsafety), has_extern (has_extern), abi (abi)
  {}

  std::string as_string () const;

  Const get_const_status () const { return const_status; }

  bool is_const () const { return const_status == Const::Yes; }
  bool is_unsafe () const { return unsafety == Unsafety::Unsafe; }
  bool is_async () const { return async_status == Async::Yes; }

  ABI get_abi () const { return abi; }
};

// A function parameter
struct FunctionParam
{
  std::unique_ptr<Pattern> param_name;
  std::unique_ptr<Type> type;
  location_t locus;
  Analysis::NodeMapping mappings;

public:
  FunctionParam (Analysis::NodeMapping mappings,
		 std::unique_ptr<Pattern> param_name,
		 std::unique_ptr<Type> param_type, location_t locus)
    : param_name (std::move (param_name)), type (std::move (param_type)),
      locus (locus), mappings (mappings)
  {}

  // Copy constructor uses clone
  FunctionParam (FunctionParam const &other)
    : param_name (other.param_name->clone_pattern ()),
      type (other.type->clone_type ()), locus (other.locus),
      mappings (other.mappings)
  {}

  // Overload assignment operator to use clone
  FunctionParam &operator= (FunctionParam const &other)
  {
    param_name = other.param_name->clone_pattern ();
    type = other.type->clone_type ();
    locus = other.locus;
    mappings = other.mappings;

    return *this;
  }

  // move constructors
  FunctionParam (FunctionParam &&other) = default;
  FunctionParam &operator= (FunctionParam &&other) = default;

  std::string as_string () const;

  location_t get_locus () const { return locus; }

  std::unique_ptr<Pattern> &get_param_name () { return param_name; }

  std::unique_ptr<Type> &get_type () { return type; }

  const Analysis::NodeMapping &get_mappings () const { return mappings; }
};

// Visibility of an item
struct Visibility
{
public:
  enum VisType
  {
    PRIVATE,
    PUBLIC,
    RESTRICTED,
    ERROR,
  };

private:
  VisType vis_type;
  HIR::SimplePath path;
  location_t locus;

  // should this store location info?

public:
  Visibility (VisType vis_type,
	      HIR::SimplePath path = HIR::SimplePath::create_empty (),
	      location_t locus = UNDEF_LOCATION)
    : vis_type (vis_type), path (std::move (path)), locus (locus)
  {}

  // Returns whether visibility is in an error state.
  bool is_error () const { return vis_type == ERROR; }

  // Does the current visibility refer to a simple `pub <item>` entirely public
  bool is_public () const { return vis_type == PUBLIC; }

  // Is the current visibility public restricted to a certain path
  bool is_restricted () const { return vis_type == RESTRICTED; }

  // Creates an error visibility.
  static Visibility create_error ()
  {
    return Visibility (ERROR, HIR::SimplePath::create_empty ());
  }

  VisType get_vis_type () const { return vis_type; }

  const HIR::SimplePath &get_path () const
  {
    rust_assert (!is_error ());
    return path;
  }

  std::string as_string () const;
};

// Item that supports visibility - abstract base class
class VisItem : public Item
{
  Visibility visibility;

protected:
  // Visibility constructor
  VisItem (Analysis::NodeMapping mappings, Visibility visibility,
	   AST::AttrVec outer_attrs = AST::AttrVec ())
    : Item (std::move (mappings), std::move (outer_attrs)),
      visibility (std::move (visibility))
  {}

  // Visibility copy constructor
  VisItem (VisItem const &other) : Item (other), visibility (other.visibility)
  {}

  // Overload assignment operator to clone
  VisItem &operator= (VisItem const &other)
  {
    Item::operator= (other);
    visibility = other.visibility;
    // outer_attrs = other.outer_attrs;

    return *this;
  }

  // move constructors
  VisItem (VisItem &&other) = default;
  VisItem &operator= (VisItem &&other) = default;

public:
  using HIR::Stmt::accept_vis;

  BaseKind get_hir_kind () override final { return VIS_ITEM; }

  /* Does the item have some kind of public visibility (non-default
   * visibility)? */
  bool has_visibility () const { return !visibility.is_error (); }

  virtual void accept_vis (HIRVisItemVisitor &vis) = 0;

  Visibility &get_visibility () { return visibility; }
  const Visibility &get_visibility () const { return visibility; }

  std::string as_string () const override;
};

// Rust module item - abstract base class
class Module : public VisItem, public WithInnerAttrs
{
  Identifier module_name;
  location_t locus;
  // bool has_items;
  std::vector<std::unique_ptr<Item>> items;

public:
  std::string as_string () const override;

  // Returns whether the module has items in its body.
  bool has_items () const { return !items.empty (); }

  // Full constructor
  Module (Analysis::NodeMapping mappings, Identifier module_name,
	  location_t locus, std::vector<std::unique_ptr<Item>> items,
	  Visibility visibility = Visibility::create_error (),
	  AST::AttrVec inner_attrs = AST::AttrVec (),
	  AST::AttrVec outer_attrs = AST::AttrVec ())
    : VisItem (std::move (mappings), std::move (visibility),
	       std::move (outer_attrs)),
      WithInnerAttrs (std::move (inner_attrs)), module_name (module_name),
      locus (locus), items (std::move (items))
  {}

  // Copy constructor with vector clone
  Module (Module const &other)
    : VisItem (other), WithInnerAttrs (other.inner_attrs), module_name ("")
  {
    items.reserve (other.items.size ());
    for (const auto &e : other.items)
      items.push_back (e->clone_item ());
  }

  // Overloaded assignment operator with vector clone
  Module &operator= (Module const &other)
  {
    VisItem::operator= (other);
    inner_attrs = other.inner_attrs;

    items.reserve (other.items.size ());
    for (const auto &e : other.items)
      items.push_back (e->clone_item ());

    return *this;
  }

  // move constructors
  Module (Module &&other) = default;
  Module &operator= (Module &&other) = default;

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRStmtVisitor &vis) override;
  void accept_vis (HIRVisItemVisitor &vis) override;

  Identifier get_module_name () const { return module_name; }
  std::vector<std::unique_ptr<Item>> &get_items () { return items; };

  /* Override that runs the function recursively on all items contained within
   * the module. */
  void add_crate_name (std::vector<std::string> &names) const override;

  location_t get_locus () const override final { return locus; }

  ItemKind get_item_kind () const override { return ItemKind::Module; }

protected:
  /* Use covariance to implement clone function as returning this object
   * rather than base */
  Module *clone_item_impl () const override { return new Module (*this); }

  /* Use covariance to implement clone function as returning this object
   * rather than base */
  /*virtual Module* clone_statement_impl() const override {
      return new Module(*this);
  }*/
};

// Rust extern crate declaration HIR node
class ExternCrate : public VisItem
{
  // this is either an identifier or "self", with self parsed to string
  std::string referenced_crate;
  // bool has_as_clause;
  // AsClause as_clause;
  // this is either an identifier or "_", with _ parsed to string
  std::string as_clause_name;

  location_t locus;

  /* e.g.
      "extern crate foo as _"
      "extern crate foo"
      "extern crate std as cool_std"  */
public:
  std::string as_string () const override;

  // Returns whether extern crate declaration has an as clause.
  bool has_as_clause () const { return !as_clause_name.empty (); }

  /* Returns whether extern crate declaration references the current crate
   * (i.e. self). */
  bool references_self () const { return referenced_crate == "self"; }

  // Constructor
  ExternCrate (Analysis::NodeMapping mappings, std::string referenced_crate,
	       Visibility visibility, AST::AttrVec outer_attrs,
	       location_t locus, std::string as_clause_name = std::string ())
    : VisItem (std::move (mappings), std::move (visibility),
	       std::move (outer_attrs)),
      referenced_crate (std::move (referenced_crate)),
      as_clause_name (std::move (as_clause_name)), locus (locus)
  {}

  location_t get_locus () const override final { return locus; }

  ItemKind get_item_kind () const override { return ItemKind::ExternCrate; }
  std::string get_referenced_crate () { return referenced_crate; }
  std::string get_as_clause_name () { return as_clause_name; }

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRStmtVisitor &vis) override;
  void accept_vis (HIRVisItemVisitor &vis) override;

  // Override that adds extern crate name in decl to passed list of names.
  void add_crate_name (std::vector<std::string> &names) const override
  {
    names.push_back (referenced_crate);
  }

protected:
  /* Use covariance to implement clone function as returning this object
   * rather than base */
  ExternCrate *clone_item_impl () const override
  {
    return new ExternCrate (*this);
  }

  /* Use covariance to implement clone function as returning this object
   * rather than base */
  /*virtual ExternCrate* clone_statement_impl() const override {
      return new ExternCrate(*this);
  }*/
};

// The path-ish thing referred to in a use declaration - abstract base class
class UseTree : public FullVisitable
{
  location_t locus;

public:
  virtual ~UseTree () {}

  // Unique pointer custom clone function
  std::unique_ptr<UseTree> clone_use_tree () const
  {
    return std::unique_ptr<UseTree> (clone_use_tree_impl ());
  }

  virtual std::string as_string () const = 0;

  location_t get_locus () const { return locus; }

protected:
  // Clone function implementation as pure virtual method
  virtual UseTree *clone_use_tree_impl () const = 0;

  UseTree (location_t locus) : locus (locus) {}
};

// Use tree with a glob (wildcard) operator
class UseTreeGlob : public UseTree
{
public:
  enum PathType
  {
    NO_PATH,
    GLOBAL,
    PATH_PREFIXED
  };

private:
  PathType glob_type;
  AST::SimplePath path;

public:
  UseTreeGlob (PathType glob_type, AST::SimplePath path, location_t locus)
    : UseTree (locus), glob_type (glob_type), path (std::move (path))
  {
    if (this->glob_type != PATH_PREFIXED)
      {
	// compiler implementation error if there is a path with a
	// non-path-prefixed use tree glob
	gcc_assert (!has_path ());
      }
    // TODO: do path-prefixed paths also have to have a path? If so, have an
    // assert for that too.
  }

  /* Returns whether has path. Should be made redundant by PathType
   * PATH_PREFIXED. */
  bool has_path () const { return !path.is_empty (); }

  PathType get_glob_type () { return glob_type; }
  AST::SimplePath get_path () { return path; };

  std::string as_string () const override;

  void accept_vis (HIRFullVisitor &vis) override;

  /* TODO: find way to ensure only PATH_PREFIXED glob_type has path - factory
   * methods? */
protected:
  /* Use covariance to implement clone function as returning this object
   * rather than base */
  UseTreeGlob *clone_use_tree_impl () const override
  {
    return new UseTreeGlob (*this);
  }
};

// Use tree with a list of paths with a common prefix
class UseTreeList : public UseTree
{
public:
  enum PathType
  {
    NO_PATH,
    GLOBAL,
    PATH_PREFIXED
  };

private:
  PathType path_type;
  AST::SimplePath path;

  std::vector<std::unique_ptr<UseTree>> trees;

public:
  UseTreeList (PathType path_type, AST::SimplePath path,
	       std::vector<std::unique_ptr<UseTree>> trees, location_t locus)
    : UseTree (locus), path_type (path_type), path (std::move (path)),
      trees (std::move (trees))
  {
    if (this->path_type != PATH_PREFIXED)
      {
	// compiler implementation error if there is a path with a
	// non-path-prefixed use tree glob
	gcc_assert (!has_path ());
      }
    // TODO: do path-prefixed paths also have to have a path? If so, have an
    // assert for that too.
  }

  // copy constructor with vector clone
  UseTreeList (UseTreeList const &other)
    : UseTree (other), path_type (other.path_type), path (other.path)
  {
    trees.reserve (other.trees.size ());
    for (const auto &e : other.trees)
      trees.push_back (e->clone_use_tree ());
  }

  // overloaded assignment operator with vector clone
  UseTreeList &operator= (UseTreeList const &other)
  {
    UseTree::operator= (other);
    path_type = other.path_type;
    path = other.path;

    trees.reserve (other.trees.size ());
    for (const auto &e : other.trees)
      trees.push_back (e->clone_use_tree ());

    return *this;
  }

  // move constructors
  UseTreeList (UseTreeList &&other) = default;
  UseTreeList &operator= (UseTreeList &&other) = default;

  // Returns whether has path. Should be made redundant by path_type.
  bool has_path () const { return !path.is_empty (); }

  // Returns whether has inner tree elements.
  bool has_trees () const { return !trees.empty (); }

  std::string as_string () const override;

  void accept_vis (HIRFullVisitor &vis) override;

  PathType get_path_type () { return path_type; }
  AST::SimplePath get_path () { return path; }
  std::vector<std::unique_ptr<UseTree>> &get_trees () { return trees; }

  // TODO: find way to ensure only PATH_PREFIXED path_type has path - factory
  // methods?
protected:
  /* Use covariance to implement clone function as returning this object
   * rather than base */
  UseTreeList *clone_use_tree_impl () const override
  {
    return new UseTreeList (*this);
  }
};

// Use tree where it rebinds the module name as something else
class UseTreeRebind : public UseTree
{
public:
  enum NewBindType
  {
    NONE,
    IDENTIFIER,
    WILDCARD
  };

private:
  AST::SimplePath path;

  NewBindType bind_type;
  Identifier identifier; // only if NewBindType is IDENTIFIER

public:
  UseTreeRebind (NewBindType bind_type, AST::SimplePath path, location_t locus,
		 Identifier identifier = std::string ())
    : UseTree (locus), path (std::move (path)), bind_type (bind_type),
      identifier (std::move (identifier))
  {}

  // Returns whether has path (this should always be true).
  bool has_path () const { return !path.is_empty (); }

  AST::SimplePath get_path () { return path; }

  Identifier get_identifier () const { return identifier; }

  NewBindType get_bind_type () const { return bind_type; }

  // Returns whether has identifier (or, rather, is allowed to).
  bool has_identifier () const { return bind_type == IDENTIFIER; }

  std::string as_string () const override;

  void accept_vis (HIRFullVisitor &vis) override;

  // TODO: find way to ensure only PATH_PREFIXED path_type has path - factory
  // methods?
protected:
  /* Use covariance to implement clone function as returning this object
   * rather than base */
  virtual UseTreeRebind *clone_use_tree_impl () const override
  {
    return new UseTreeRebind (*this);
  }
};

std::string enum_to_str (UseTreeRebind::NewBindType);

// Rust use declaration (i.e. for modules) HIR node
class UseDeclaration : public VisItem
{
  std::unique_ptr<UseTree> use_tree;
  location_t locus;

public:
  std::string as_string () const override;

  UseDeclaration (Analysis::NodeMapping mappings,
		  std::unique_ptr<UseTree> use_tree, Visibility visibility,
		  AST::AttrVec outer_attrs, location_t locus)
    : VisItem (std::move (mappings), std::move (visibility),
	       std::move (outer_attrs)),
      use_tree (std::move (use_tree)), locus (locus)
  {}

  // Copy constructor with clone
  UseDeclaration (UseDeclaration const &other)
    : VisItem (other), use_tree (other.use_tree->clone_use_tree ()),
      locus (other.locus)
  {}

  // Overloaded assignment operator to clone
  UseDeclaration &operator= (UseDeclaration const &other)
  {
    VisItem::operator= (other);
    use_tree = other.use_tree->clone_use_tree ();
    // visibility = other.visibility->clone_visibility();
    // outer_attrs = other.outer_attrs;
    locus = other.locus;

    return *this;
  }

  // move constructors
  UseDeclaration (UseDeclaration &&other) = default;
  UseDeclaration &operator= (UseDeclaration &&other) = default;

  location_t get_locus () const override final { return locus; }
  ItemKind get_item_kind () const override { return ItemKind::UseDeclaration; }

  std::unique_ptr<UseTree> &get_use_tree () { return use_tree; }
  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRStmtVisitor &vis) override;
  void accept_vis (HIRVisItemVisitor &vis) override;

protected:
  /* Use covariance to implement clone function as returning this object
   * rather than base */
  UseDeclaration *clone_item_impl () const override
  {
    return new UseDeclaration (*this);
  }

  /* Use covariance to implement clone function as returning this object
   * rather than base */
  /*virtual UseDeclaration* clone_statement_impl() const override {
      return new UseDeclaration(*this);
  }*/
};

class LetStmt;

// Rust function declaration HIR node
class Function : public VisItem, public ImplItem
{
  FunctionQualifiers qualifiers;
  Identifier function_name;
  std::vector<std::unique_ptr<GenericParam>> generic_params;
  std::vector<FunctionParam> function_params;
  std::unique_ptr<Type> return_type;
  WhereClause where_clause;
  std::unique_ptr<BlockExpr> function_body;
  SelfParam self;
  location_t locus;

public:
  std::string as_string () const override;

  // Returns whether function has generic parameters.
  bool has_generics () const { return !generic_params.empty (); }

  // Returns whether function has regular parameters.
  bool has_function_params () const { return !function_params.empty (); }

  // Returns whether function has return type - if not, it is void.
  bool has_function_return_type () const { return return_type != nullptr; }

  // Returns whether function has a where clause.
  bool has_where_clause () const { return !where_clause.is_empty (); }

  ImplItemType get_impl_item_type () const override final
  {
    return ImplItem::ImplItemType::FUNCTION;
  }

  ItemKind get_item_kind () const override { return ItemKind::Function; }

  // Mega-constructor with all possible fields
  Function (Analysis::NodeMapping mappings, Identifier function_name,
	    FunctionQualifiers qualifiers,
	    std::vector<std::unique_ptr<GenericParam>> generic_params,
	    std::vector<FunctionParam> function_params,
	    std::unique_ptr<Type> return_type, WhereClause where_clause,
	    std::unique_ptr<BlockExpr> function_body, Visibility vis,
	    AST::AttrVec outer_attrs, SelfParam self, location_t locus)
    : VisItem (std::move (mappings), std::move (vis), std::move (outer_attrs)),
      qualifiers (std::move (qualifiers)),
      function_name (std::move (function_name)),
      generic_params (std::move (generic_params)),
      function_params (std::move (function_params)),
      return_type (std::move (return_type)),
      where_clause (std::move (where_clause)),
      function_body (std::move (function_body)), self (std::move (self)),
      locus (locus)
  {}

  // Copy constructor with clone
  Function (Function const &other)
    : VisItem (other), qualifiers (other.qualifiers),
      function_name (other.function_name),
      function_params (other.function_params),
      where_clause (other.where_clause),
      function_body (other.function_body->clone_block_expr ()),
      self (other.self), locus (other.locus)
  {
    // guard to prevent null dereference (always required)
    if (other.return_type != nullptr)
      return_type = other.return_type->clone_type ();
    else
      return_type = nullptr;

    generic_params.reserve (other.generic_params.size ());
    for (const auto &e : other.generic_params)
      generic_params.push_back (e->clone_generic_param ());
  }

  // Overloaded assignment operator to clone
  Function &operator= (Function const &other)
  {
    VisItem::operator= (other);
    function_name = other.function_name;
    qualifiers = other.qualifiers;
    function_params = other.function_params;

    // guard to prevent null dereference (always required)
    if (other.return_type != nullptr)
      return_type = other.return_type->clone_type ();
    else
      return_type = nullptr;

    where_clause = other.where_clause;
    function_body = other.function_body->clone_block_expr ();
    locus = other.locus;
    self = other.self;

    generic_params.reserve (other.generic_params.size ());
    for (const auto &e : other.generic_params)
      generic_params.push_back (e->clone_generic_param ());

    return *this;
  }

  // move constructors
  Function (Function &&other) = default;
  Function &operator= (Function &&other) = default;

  location_t get_locus () const override final { return locus; }

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRImplVisitor &vis) override;
  void accept_vis (HIRStmtVisitor &vis) override;
  void accept_vis (HIRVisItemVisitor &vis) override;

  Analysis::NodeMapping get_impl_mappings () const override
  {
    return get_mappings ();
  };

  std::vector<FunctionParam> &get_function_params () { return function_params; }
  const std::vector<FunctionParam> &get_function_params () const
  {
    return function_params;
  }

  std::vector<std::unique_ptr<GenericParam>> &get_generic_params ()
  {
    return generic_params;
  }
  const std::vector<std::unique_ptr<GenericParam>> &get_generic_params () const
  {
    return generic_params;
  }

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<BlockExpr> &get_definition () { return function_body; }

  const FunctionQualifiers &get_qualifiers () const { return qualifiers; }

  Identifier get_function_name () const { return function_name; }

  // TODO: is this better? Or is a "vis_block" better?
  WhereClause &get_where_clause () { return where_clause; }

  bool has_return_type () const { return return_type != nullptr; }

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<Type> &get_return_type () { return return_type; }

  bool is_method () const { return !self.is_error (); }

  SelfParam &get_self_param () { return self; }

  std::string get_impl_item_name () const override final
  {
    return get_function_name ().as_string ();
  }

protected:
  /* Use covariance to implement clone function as returning this object
   * rather than base */
  Function *clone_item_impl () const override { return new Function (*this); }

  /* Use covariance to implement clone function as returning this object
   * rather than base */
  Function *clone_inherent_impl_item_impl () const override
  {
    return new Function (*this);
  }
};

// Rust type alias (i.e. typedef) HIR node
class TypeAlias : public VisItem, public ImplItem
{
  Identifier new_type_name;

  // bool has_generics;
  // Generics generic_params;
  std::vector<std::unique_ptr<GenericParam>> generic_params; // inlined

  // bool has_where_clause;
  WhereClause where_clause;

  std::unique_ptr<Type> existing_type;

  location_t locus;

public:
  std::string as_string () const override;

  // Returns whether type alias has generic parameters.
  bool has_generics () const { return !generic_params.empty (); }

  // Returns whether type alias has a where clause.
  bool has_where_clause () const { return !where_clause.is_empty (); }

  ImplItemType get_impl_item_type () const override final
  {
    return ImplItem::ImplItemType::TYPE_ALIAS;
  }

  // Mega-constructor with all possible fields
  TypeAlias (Analysis::NodeMapping mappings, Identifier new_type_name,
	     std::vector<std::unique_ptr<GenericParam>> generic_params,
	     WhereClause where_clause, std::unique_ptr<Type> existing_type,
	     Visibility vis, AST::AttrVec outer_attrs, location_t locus)
    : VisItem (std::move (mappings), std::move (vis), std::move (outer_attrs)),
      new_type_name (std::move (new_type_name)),
      generic_params (std::move (generic_params)),
      where_clause (std::move (where_clause)),
      existing_type (std::move (existing_type)), locus (locus)
  {}

  // Copy constructor
  TypeAlias (TypeAlias const &other)
    : VisItem (other), new_type_name (other.new_type_name),
      where_clause (other.where_clause),
      existing_type (other.existing_type->clone_type ()), locus (other.locus)
  {
    generic_params.reserve (other.generic_params.size ());
    for (const auto &e : other.generic_params)
      generic_params.push_back (e->clone_generic_param ());
  }

  // Overloaded assignment operator to clone
  TypeAlias &operator= (TypeAlias const &other)
  {
    VisItem::operator= (other);
    new_type_name = other.new_type_name;
    where_clause = other.where_clause;
    existing_type = other.existing_type->clone_type ();
    locus = other.locus;

    generic_params.reserve (other.generic_params.size ());
    for (const auto &e : other.generic_params)
      generic_params.push_back (e->clone_generic_param ());

    return *this;
  }

  // move constructors
  TypeAlias (TypeAlias &&other) = default;
  TypeAlias &operator= (TypeAlias &&other) = default;

  location_t get_locus () const override final { return locus; }

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRImplVisitor &vis) override;
  void accept_vis (HIRStmtVisitor &vis) override;
  void accept_vis (HIRVisItemVisitor &vis) override;

  std::vector<std::unique_ptr<GenericParam>> &get_generic_params ()
  {
    return generic_params;
  }
  const std::vector<std::unique_ptr<GenericParam>> &get_generic_params () const
  {
    return generic_params;
  }

  WhereClause &get_where_clause () { return where_clause; }

  std::unique_ptr<Type> &get_type_aliased () { return existing_type; }

  Identifier get_new_type_name () const { return new_type_name; }

  ItemKind get_item_kind () const override { return ItemKind::TypeAlias; }

  Analysis::NodeMapping get_impl_mappings () const override
  {
    return get_mappings ();
  };

  std::string get_impl_item_name () const override final
  {
    return get_new_type_name ().as_string ();
  }

protected:
  /* Use covariance to implement clone function as returning this object
   * rather than base */
  TypeAlias *clone_item_impl () const override { return new TypeAlias (*this); }

  /* Use covariance to implement clone function as returning this object
   * rather than base */
  TypeAlias *clone_inherent_impl_item_impl () const override
  {
    return new TypeAlias (*this);
  }
};

// Rust base struct declaration HIR node - abstract base class
class Struct : public VisItem
{
protected:
  // protected to enable access by derived classes - allows better as_string
  Identifier struct_name;

  // bool has_generics;
  // Generics generic_params;
  std::vector<std::unique_ptr<GenericParam>> generic_params; // inlined

  // bool has_where_clause;
  WhereClause where_clause;

  location_t locus;

public:
  Identifier get_identifier () const { return struct_name; }

  // Returns whether struct has generic parameters.
  bool has_generics () const { return !generic_params.empty (); }

  // Returns whether struct has a where clause.
  bool has_where_clause () const { return !where_clause.is_empty (); }

  location_t get_locus () const override final { return locus; }
  ItemKind get_item_kind () const override { return ItemKind::Struct; }

  std::vector<std::unique_ptr<GenericParam>> &get_generic_params ()
  {
    return generic_params;
  }

  WhereClause &get_where_clause () { return where_clause; }

protected:
  Struct (Analysis::NodeMapping mappings, Identifier struct_name,
	  std::vector<std::unique_ptr<GenericParam>> generic_params,
	  WhereClause where_clause, Visibility vis, location_t locus,
	  AST::AttrVec outer_attrs = AST::AttrVec ())
    : VisItem (std::move (mappings), std::move (vis), std::move (outer_attrs)),
      struct_name (std::move (struct_name)),
      generic_params (std::move (generic_params)),
      where_clause (std::move (where_clause)), locus (locus)
  {}

  // Copy constructor with vector clone
  Struct (Struct const &other)
    : VisItem (other), struct_name (other.struct_name),
      where_clause (other.where_clause), locus (other.locus)
  {
    generic_params.reserve (other.generic_params.size ());
    for (const auto &e : other.generic_params)
      generic_params.push_back (e->clone_generic_param ());
  }

  // Overloaded assignment operator with vector clone
  Struct &operator= (Struct const &other)
  {
    VisItem::operator= (other);
    struct_name = other.struct_name;
    where_clause = other.where_clause;
    locus = other.locus;

    generic_params.reserve (other.generic_params.size ());
    for (const auto &e : other.generic_params)
      generic_params.push_back (e->clone_generic_param ());

    return *this;
  }

  // move constructors
  Struct (Struct &&other) = default;
  Struct &operator= (Struct &&other) = default;
};

// A single field in a struct
// FIXME can't this be a TupleStruct + field_name?
class StructField
{
public:
  // bool has_outer_attributes;
  AST::AttrVec outer_attrs;

  // bool has_visibility;
  Visibility visibility;

  Identifier field_name;
  std::unique_ptr<Type> field_type;

  Analysis::NodeMapping mappings;

  location_t locus;

  // Returns whether struct field has any outer attributes.
  bool has_outer_attributes () const { return !outer_attrs.empty (); }

  // Returns whether struct field has a non-private (non-default) visibility.
  bool has_visibility () const { return !visibility.is_error (); }

  StructField (Analysis::NodeMapping mappings, Identifier field_name,
	       std::unique_ptr<Type> field_type, Visibility vis,
	       location_t locus, AST::AttrVec outer_attrs = AST::AttrVec ())
    : outer_attrs (std::move (outer_attrs)), visibility (std::move (vis)),
      field_name (std::move (field_name)), field_type (std::move (field_type)),
      mappings (mappings), locus (locus)
  {}

  // Copy constructor
  StructField (StructField const &other)
    : outer_attrs (other.outer_attrs), visibility (other.visibility),
      field_name (other.field_name),
      field_type (other.field_type->clone_type ()), mappings (other.mappings)
  {}

  ~StructField () = default;

  // Overloaded assignment operator to clone
  StructField &operator= (StructField const &other)
  {
    field_name = other.field_name;
    field_type = other.field_type->clone_type ();
    visibility = other.visibility;
    outer_attrs = other.outer_attrs;
    mappings = other.mappings;

    return *this;
  }

  // move constructors
  StructField (StructField &&other) = default;
  StructField &operator= (StructField &&other) = default;

  std::string as_string () const;

  Identifier get_field_name () const { return field_name; }

  std::unique_ptr<Type> &get_field_type () { return field_type; }

  Analysis::NodeMapping get_mappings () const { return mappings; }

  location_t get_locus () { return locus; }
  AST::AttrVec &get_outer_attrs () { return outer_attrs; }
  Visibility &get_visibility () { return visibility; }
};

// Rust struct declaration with true struct type HIR node
class StructStruct : public Struct
{
public:
  std::vector<StructField> fields;
  bool is_unit;

  std::string as_string () const override;

  // Mega-constructor with all possible fields
  StructStruct (Analysis::NodeMapping mappings, std::vector<StructField> fields,
		Identifier struct_name,
		std::vector<std::unique_ptr<GenericParam>> generic_params,
		WhereClause where_clause, bool is_unit, Visibility vis,
		AST::AttrVec outer_attrs, location_t locus)
    : Struct (std::move (mappings), std::move (struct_name),
	      std::move (generic_params), std::move (where_clause),
	      std::move (vis), locus, std::move (outer_attrs)),
      fields (std::move (fields)), is_unit (is_unit)
  {}

  // Unit struct constructor
  StructStruct (Analysis::NodeMapping mappings, Identifier struct_name,
		std::vector<std::unique_ptr<GenericParam>> generic_params,
		WhereClause where_clause, Visibility vis,
		AST::AttrVec outer_attrs, location_t locus)
    : Struct (std::move (mappings), std::move (struct_name),
	      std::move (generic_params), std::move (where_clause),
	      std::move (vis), locus, std::move (outer_attrs)),
      is_unit (true)
  {}
  // TODO: can a unit struct have generic fields? assuming yes for now.

  /* Returns whether the struct is a unit struct - struct defined without
   * fields. This is important because it also means an implicit constant of its
   * type is defined. */
  bool is_unit_struct () const { return is_unit; }

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRStmtVisitor &vis) override;
  void accept_vis (HIRVisItemVisitor &vis) override;

  std::vector<StructField> &get_fields () { return fields; }

protected:
  /* Use covariance to implement clone function as returning this object
   * rather than base */
  StructStruct *clone_item_impl () const override
  {
    return new StructStruct (*this);
  }

  /* Use covariance to implement clone function as returning this object
   * rather than base */
  /*virtual StructStruct* clone_statement_impl() const override {
      return new StructStruct(*this);
  }*/
};

// A single field in a tuple
class TupleField
{
private:
  // bool has_outer_attributes;
  AST::AttrVec outer_attrs;

  // bool has_visibility;
  Visibility visibility;

  std::unique_ptr<Type> field_type;

  location_t locus;

  Analysis::NodeMapping mappings;

public:
  // Returns whether tuple field has outer attributes.
  bool has_outer_attributes () const { return !outer_attrs.empty (); }

  /* Returns whether tuple field has a non-default visibility (i.e. a public
   * one) */
  bool has_visibility () const { return !visibility.is_error (); }

  // Complete constructor
  TupleField (Analysis::NodeMapping mapping, std::unique_ptr<Type> field_type,
	      Visibility vis, location_t locus,
	      AST::AttrVec outer_attrs = AST::AttrVec ())
    : outer_attrs (std::move (outer_attrs)), visibility (std::move (vis)),
      field_type (std::move (field_type)), locus (locus), mappings (mapping)
  {}

  // Copy constructor with clone
  TupleField (TupleField const &other)
    : outer_attrs (other.outer_attrs), visibility (other.visibility),
      field_type (other.field_type->clone_type ()), locus (other.locus),
      mappings (other.mappings)
  {}

  ~TupleField () = default;

  // Overloaded assignment operator to clone
  TupleField &operator= (TupleField const &other)
  {
    field_type = other.field_type->clone_type ();
    visibility = other.visibility;
    outer_attrs = other.outer_attrs;
    locus = other.locus;
    mappings = other.mappings;

    return *this;
  }

  // move constructors
  TupleField (TupleField &&other) = default;
  TupleField &operator= (TupleField &&other) = default;

  // Returns whether tuple field is in an error state.
  bool is_error () const { return field_type == nullptr; }

  std::string as_string () const;

  Analysis::NodeMapping get_mappings () const { return mappings; }

  Visibility &get_visibility () { return visibility; }

  location_t get_locus () const { return locus; }

  AST::AttrVec &get_outer_attrs () { return outer_attrs; }
  std::unique_ptr<HIR::Type> &get_field_type () { return field_type; }
};

// Rust tuple declared using struct keyword HIR node
class TupleStruct : public Struct
{
  std::vector<TupleField> fields;

public:
  std::string as_string () const override;

  // Mega-constructor with all possible fields
  TupleStruct (Analysis::NodeMapping mappings, std::vector<TupleField> fields,
	       Identifier struct_name,
	       std::vector<std::unique_ptr<GenericParam>> generic_params,
	       WhereClause where_clause, Visibility vis,
	       AST::AttrVec outer_attrs, location_t locus)
    : Struct (std::move (mappings), std::move (struct_name),
	      std::move (generic_params), std::move (where_clause),
	      std::move (vis), locus, std::move (outer_attrs)),
      fields (std::move (fields))
  {}

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRStmtVisitor &vis) override;
  void accept_vis (HIRVisItemVisitor &vis) override;

  std::vector<TupleField> &get_fields () { return fields; }
  const std::vector<TupleField> &get_fields () const { return fields; }

protected:
  /* Use covariance to implement clone function as returning this object
   * rather than base */
  TupleStruct *clone_item_impl () const override
  {
    return new TupleStruct (*this);
  }

  /* Use covariance to implement clone function as returning this object
   * rather than base */
  /*virtual TupleStruct* clone_statement_impl() const override {
      return new TupleStruct(*this);
  }*/
};

/* An item used in an "enum" tagged union - not abstract: base represents a
   name-only enum. Syntactically EnumItem's can have a Visibility. But not
   Semantically. So check there is no Visibility when lowering and make this
   an Item, not an VisItem.  */
class EnumItem : public Item
{
  Identifier variant_name;
  location_t locus;

public:
  virtual ~EnumItem () {}

  enum EnumItemKind
  {
    Named,
    Tuple,
    Struct,
    Discriminant,
  };

  EnumItem (Analysis::NodeMapping mappings, Identifier variant_name,
	    AST::AttrVec outer_attrs, location_t locus)
    : Item (std::move (mappings), std::move (outer_attrs)),
      variant_name (std::move (variant_name)), locus (locus)
  {}

  // Unique pointer custom clone function
  std::unique_ptr<EnumItem> clone_enum_item () const
  {
    return std::unique_ptr<EnumItem> (clone_item_impl ());
  }

  virtual std::string as_string () const override;
  virtual EnumItemKind get_enum_item_kind () const { return Named; };

  // not pure virtual as not abstract
  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRStmtVisitor &vis) override;
  // void accept_vis (HIRVisItemVisitor &vis) override;

  location_t get_locus () const override { return locus; }

  Identifier get_identifier () const { return variant_name; }

  ItemKind get_item_kind () const override { return ItemKind::EnumItem; }

protected:
  EnumItem *clone_item_impl () const override { return new EnumItem (*this); }
};

// A tuple item used in an "enum" tagged union
class EnumItemTuple : public EnumItem
{
  // bool has_tuple_fields;
  std::vector<TupleField> tuple_fields;

public:
  // Returns whether tuple enum item has tuple fields.
  bool has_tuple_fields () const { return !tuple_fields.empty (); }

  EnumItemKind get_enum_item_kind () const override
  {
    return EnumItemKind::Tuple;
  }

  EnumItemTuple (Analysis::NodeMapping mappings, Identifier variant_name,
		 std::vector<TupleField> tuple_fields, AST::AttrVec outer_attrs,
		 location_t locus)
    : EnumItem (std::move (mappings), std::move (variant_name),
		std::move (outer_attrs), locus),
      tuple_fields (std::move (tuple_fields))
  {}

  std::string as_string () const override;

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRStmtVisitor &vis) override;

  std::vector<TupleField> &get_tuple_fields () { return tuple_fields; }

protected:
  // Clone function implementation as (not pure) virtual method
  EnumItemTuple *clone_item_impl () const override
  {
    return new EnumItemTuple (*this);
  }
};

// A struct item used in an "enum" tagged union
class EnumItemStruct : public EnumItem
{
  // bool has_struct_fields;
  std::vector<StructField> struct_fields;

public:
  // Returns whether struct enum item has struct fields.
  bool has_struct_fields () const { return !struct_fields.empty (); }

  EnumItemKind get_enum_item_kind () const override
  {
    return EnumItemKind::Struct;
  }

  EnumItemStruct (Analysis::NodeMapping mappings, Identifier variant_name,
		  std::vector<StructField> struct_fields,
		  AST::AttrVec outer_attrs, location_t locus)
    : EnumItem (std::move (mappings), std::move (variant_name),
		std::move (outer_attrs), locus),
      struct_fields (std::move (struct_fields))
  {}

  std::string as_string () const override;

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRStmtVisitor &vis) override;

  std::vector<StructField> &get_struct_fields () { return struct_fields; }

protected:
  // Clone function implementation as (not pure) virtual method
  EnumItemStruct *clone_item_impl () const override
  {
    return new EnumItemStruct (*this);
  }
};

// A discriminant (numbered enum) item used in an "enum" tagged union
class EnumItemDiscriminant : public EnumItem
{
  std::unique_ptr<Expr> expression;

public:
  EnumItemDiscriminant (Analysis::NodeMapping mappings, Identifier variant_name,
			std::unique_ptr<Expr> expr, AST::AttrVec outer_attrs,
			location_t locus)
    : EnumItem (std::move (mappings), std::move (variant_name),
		std::move (outer_attrs), locus),
      expression (std::move (expr))
  {}

  // Copy constructor with clone
  EnumItemDiscriminant (EnumItemDiscriminant const &other)
    : EnumItem (other), expression (other.expression->clone_expr ())
  {}

  // Overloaded assignment operator to clone
  EnumItemDiscriminant &operator= (EnumItemDiscriminant const &other)
  {
    EnumItem::operator= (other);
    expression = other.expression->clone_expr ();
    // variant_name = other.variant_name;
    // outer_attrs = other.outer_attrs;

    return *this;
  }

  // move constructors
  EnumItemDiscriminant (EnumItemDiscriminant &&other) = default;
  EnumItemDiscriminant &operator= (EnumItemDiscriminant &&other) = default;

  EnumItemKind get_enum_item_kind () const override
  {
    return EnumItemKind::Discriminant;
  }

  std::string as_string () const override;

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRStmtVisitor &vis) override;

  std::unique_ptr<Expr> &get_discriminant_expression () { return expression; }

protected:
  // Clone function implementation as (not pure) virtual method
  EnumItemDiscriminant *clone_item_impl () const override
  {
    return new EnumItemDiscriminant (*this);
  }
};

// HIR node for Rust "enum" - tagged union
class Enum : public VisItem
{
  Identifier enum_name;

  // bool has_generics;
  // Generics generic_params;
  std::vector<std::unique_ptr<GenericParam>> generic_params; // inlined

  // bool has_where_clause;
  WhereClause where_clause;

  std::vector<std::unique_ptr<EnumItem>> items;

  location_t locus;

public:
  std::string as_string () const override;

  // Returns whether "enum" has generic parameters.
  bool has_generics () const { return !generic_params.empty (); }

  // Returns whether "enum" has a where clause.
  bool has_where_clause () const { return !where_clause.is_empty (); }

  /* Returns whether enum is a "zero-variant" (no possible variant) enum,
   * which cannot be instantiated. */
  bool is_zero_variant () const { return items.empty (); }

  // Mega-constructor
  Enum (Analysis::NodeMapping mappings, Identifier enum_name, Visibility vis,
	std::vector<std::unique_ptr<GenericParam>> generic_params,
	WhereClause where_clause, std::vector<std::unique_ptr<EnumItem>> items,
	AST::AttrVec outer_attrs, location_t locus)
    : VisItem (std::move (mappings), std::move (vis), std::move (outer_attrs)),
      enum_name (std::move (enum_name)),
      generic_params (std::move (generic_params)),
      where_clause (std::move (where_clause)), items (std::move (items)),
      locus (locus)
  {}

  // TODO: constructor with less arguments

  // Copy constructor with vector clone
  Enum (Enum const &other)
    : VisItem (other), enum_name (other.enum_name),
      where_clause (other.where_clause), locus (other.locus)
  {
    generic_params.reserve (other.generic_params.size ());
    for (const auto &e : other.generic_params)
      generic_params.push_back (e->clone_generic_param ());

    items.reserve (other.items.size ());
    for (const auto &e : other.items)
      items.push_back (e->clone_enum_item ());
  }

  // Overloaded assignment operator with vector clone
  Enum &operator= (Enum const &other)
  {
    VisItem::operator= (other);
    enum_name = other.enum_name;
    where_clause = other.where_clause;
    locus = other.locus;

    generic_params.reserve (other.generic_params.size ());
    for (const auto &e : other.generic_params)
      generic_params.push_back (e->clone_generic_param ());

    items.reserve (other.items.size ());
    for (const auto &e : other.items)
      items.push_back (e->clone_enum_item ());

    return *this;
  }

  // Move constructors
  Enum (Enum &&other) = default;
  Enum &operator= (Enum &&other) = default;

  location_t get_locus () const override final { return locus; }

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRStmtVisitor &vis) override;
  void accept_vis (HIRVisItemVisitor &vis) override;

  Identifier get_identifier () const { return enum_name; }
  ItemKind get_item_kind () const override { return ItemKind::Enum; }

  std::vector<std::unique_ptr<GenericParam>> &get_generic_params ()
  {
    return generic_params;
  }

  const std::vector<std::unique_ptr<EnumItem>> &get_variants () const
  {
    return items;
  }

  std::vector<std::unique_ptr<EnumItem>> &get_variants () { return items; }
  WhereClause &get_where_clause () { return where_clause; }

protected:
  /* Use covariance to implement clone function as returning this object
   * rather than base */
  Enum *clone_item_impl () const override { return new Enum (*this); }

  /* Use covariance to implement clone function as returning this object
   * rather than base */
  /*virtual Enum* clone_statement_impl() const override {
      return new Enum(*this);
  }*/
};

// Rust untagged union used for C compat HIR node
class Union : public VisItem
{
  Identifier union_name;

  // bool has_generics;
  // Generics generic_params;
  std::vector<std::unique_ptr<GenericParam>> generic_params; // inlined

  // bool has_where_clause;
  WhereClause where_clause;

  std::vector<StructField> variants;

  location_t locus;

public:
  std::string as_string () const override;

  // Returns whether union has generic params.
  bool has_generics () const { return !generic_params.empty (); }

  // Returns whether union has where clause.
  bool has_where_clause () const { return !where_clause.is_empty (); }

  Union (Analysis::NodeMapping mappings, Identifier union_name, Visibility vis,
	 std::vector<std::unique_ptr<GenericParam>> generic_params,
	 WhereClause where_clause, std::vector<StructField> variants,
	 AST::AttrVec outer_attrs, location_t locus)
    : VisItem (std::move (mappings), std::move (vis), std::move (outer_attrs)),
      union_name (std::move (union_name)),
      generic_params (std::move (generic_params)),
      where_clause (std::move (where_clause)), variants (std::move (variants)),
      locus (locus)
  {}

  // copy constructor with vector clone
  Union (Union const &other)
    : VisItem (other), union_name (other.union_name),
      where_clause (other.where_clause), variants (other.variants),
      locus (other.locus)
  {
    generic_params.reserve (other.generic_params.size ());
    for (const auto &e : other.generic_params)
      generic_params.push_back (e->clone_generic_param ());
  }

  // overloaded assignment operator with vector clone
  Union &operator= (Union const &other)
  {
    VisItem::operator= (other);
    union_name = other.union_name;
    where_clause = other.where_clause;
    variants = other.variants;
    locus = other.locus;

    generic_params.reserve (other.generic_params.size ());
    for (const auto &e : other.generic_params)
      generic_params.push_back (e->clone_generic_param ());

    return *this;
  }

  // move constructors
  Union (Union &&other) = default;
  Union &operator= (Union &&other) = default;

  std::vector<std::unique_ptr<GenericParam>> &get_generic_params ()
  {
    return generic_params;
  }

  Identifier get_identifier () const { return union_name; }

  location_t get_locus () const override final { return locus; }

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRStmtVisitor &vis) override;
  void accept_vis (HIRVisItemVisitor &vis) override;

  std::vector<StructField> &get_variants () { return variants; }

  WhereClause &get_where_clause () { return where_clause; }

  ItemKind get_item_kind () const override { return ItemKind::Union; }

protected:
  /* Use covariance to implement clone function as returning this object
   * rather than base */
  Union *clone_item_impl () const override { return new Union (*this); }
};

class ConstantItem : public VisItem, public ImplItem
{
  Identifier identifier;
  std::unique_ptr<Type> type;
  std::unique_ptr<Expr> const_expr;
  location_t locus;

public:
  std::string as_string () const override;

  ConstantItem (Analysis::NodeMapping mappings, Identifier ident,
		Visibility vis, std::unique_ptr<Type> type,
		std::unique_ptr<Expr> const_expr, AST::AttrVec outer_attrs,
		location_t locus)
    : VisItem (std::move (mappings), std::move (vis), std::move (outer_attrs)),
      identifier (std::move (ident)), type (std::move (type)),
      const_expr (std::move (const_expr)), locus (locus)
  {}

  ConstantItem (ConstantItem const &other)
    : VisItem (other), identifier (other.identifier),
      type (other.type->clone_type ()),
      const_expr (other.const_expr->clone_expr ()), locus (other.locus)
  {}

  // Overload assignment operator to clone
  ConstantItem &operator= (ConstantItem const &other)
  {
    VisItem::operator= (other);
    identifier = other.identifier;
    type = other.type->clone_type ();
    const_expr = other.const_expr->clone_expr ();
    locus = other.locus;

    return *this;
  }

  // move constructors
  ConstantItem (ConstantItem &&other) = default;
  ConstantItem &operator= (ConstantItem &&other) = default;

  // Returns whether constant item is an "unnamed" (wildcard underscore used
  // as identifier) constant.
  bool is_unnamed () const
  {
    return identifier.as_string () == std::string ("_");
  }

  location_t get_locus () const override final { return locus; }

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRStmtVisitor &vis) override;
  void accept_vis (HIRImplVisitor &vis) override;
  void accept_vis (HIRVisItemVisitor &vis) override;

  std::unique_ptr<Type> &get_type () { return type; }

  std::unique_ptr<Expr> &get_expr () { return const_expr; }

  Identifier get_identifier () const { return identifier; }

  Analysis::NodeMapping get_impl_mappings () const override
  {
    return get_mappings ();
  };

  ImplItemType get_impl_item_type () const override final
  {
    return ImplItem::ImplItemType::CONSTANT;
  }

  ItemKind get_item_kind () const override { return ItemKind::Constant; }

  std::string get_impl_item_name () const override final
  {
    return get_identifier ().as_string ();
  }

protected:
  /* Use covariance to implement clone function as returning this object
   * rather than base */
  ConstantItem *clone_item_impl () const override
  {
    return new ConstantItem (*this);
  }

  /* Use covariance to implement clone function as returning this object
   * rather than base */
  ConstantItem *clone_inherent_impl_item_impl () const override
  {
    return new ConstantItem (*this);
  }
};

/* Static item HIR node - items within module scope with fixed storage
 * duration? */
class StaticItem : public VisItem
{
  Mutability mut;
  Identifier name;
  std::unique_ptr<Type> type;
  std::unique_ptr<Expr> expr;
  location_t locus;

public:
  std::string as_string () const override;

  StaticItem (Analysis::NodeMapping mappings, Identifier name, Mutability mut,
	      std::unique_ptr<Type> type, std::unique_ptr<Expr> expr,
	      Visibility vis, AST::AttrVec outer_attrs, location_t locus)
    : VisItem (std::move (mappings), std::move (vis), std::move (outer_attrs)),
      mut (mut), name (std::move (name)), type (std::move (type)),
      expr (std::move (expr)), locus (locus)
  {}

  // Copy constructor with clone
  StaticItem (StaticItem const &other)
    : VisItem (other), mut (other.mut), name (other.name),
      type (other.type->clone_type ()), expr (other.expr->clone_expr ()),
      locus (other.locus)
  {}

  // Overloaded assignment operator to clone
  StaticItem &operator= (StaticItem const &other)
  {
    VisItem::operator= (other);
    name = other.name;
    mut = other.mut;
    type = other.type->clone_type ();
    expr = other.expr->clone_expr ();
    locus = other.locus;

    return *this;
  }

  // move constructors
  StaticItem (StaticItem &&other) = default;
  StaticItem &operator= (StaticItem &&other) = default;

  location_t get_locus () const override final { return locus; }

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRStmtVisitor &vis) override;
  void accept_vis (HIRVisItemVisitor &vis) override;

  Identifier get_identifier () const { return name; }

  Mutability get_mut () const { return mut; }

  bool is_mut () const { return mut == Mutability::Mut; }

  std::unique_ptr<Expr> &get_expr () { return expr; }

  std::unique_ptr<Type> &get_type () { return type; }

  ItemKind get_item_kind () const override { return ItemKind::Static; }

protected:
  StaticItem *clone_item_impl () const override
  {
    return new StaticItem (*this);
  }
};

// Function declaration in traits
class TraitFunctionDecl
{
private:
  FunctionQualifiers qualifiers;
  Identifier function_name;
  std::vector<std::unique_ptr<GenericParam>> generic_params;
  std::vector<FunctionParam> function_params;
  std::unique_ptr<Type> return_type;
  WhereClause where_clause;
  SelfParam self;

public:
  // Mega-constructor
  TraitFunctionDecl (Identifier function_name, FunctionQualifiers qualifiers,
		     std::vector<std::unique_ptr<GenericParam>> generic_params,
		     SelfParam self, std::vector<FunctionParam> function_params,
		     std::unique_ptr<Type> return_type,
		     WhereClause where_clause)
    : qualifiers (std::move (qualifiers)),
      function_name (std::move (function_name)),
      generic_params (std::move (generic_params)),
      function_params (std::move (function_params)),
      return_type (std::move (return_type)),
      where_clause (std::move (where_clause)), self (std::move (self))
  {}

  // Copy constructor with clone
  TraitFunctionDecl (TraitFunctionDecl const &other)
    : qualifiers (other.qualifiers), function_name (other.function_name),
      function_params (other.function_params),
      return_type (other.return_type->clone_type ()),
      where_clause (other.where_clause), self (other.self)
  {
    generic_params.reserve (other.generic_params.size ());
    for (const auto &e : other.generic_params)
      generic_params.push_back (e->clone_generic_param ());
  }

  ~TraitFunctionDecl () = default;

  // Overloaded assignment operator with clone
  TraitFunctionDecl &operator= (TraitFunctionDecl const &other)
  {
    function_name = other.function_name;
    qualifiers = other.qualifiers;
    function_params = other.function_params;
    return_type = other.return_type->clone_type ();
    where_clause = other.where_clause;
    self = other.self;

    generic_params.reserve (other.generic_params.size ());
    for (const auto &e : other.generic_params)
      generic_params.push_back (e->clone_generic_param ());

    return *this;
  }

  // move constructors
  TraitFunctionDecl (TraitFunctionDecl &&other) = default;
  TraitFunctionDecl &operator= (TraitFunctionDecl &&other) = default;

  std::string as_string () const;

  // Returns whether function decl has generic parameters.
  bool has_generics () const { return !generic_params.empty (); }

  // Returns whether function decl has regular parameters.
  bool has_params () const { return !function_params.empty (); }

  // Returns whether function has return type (otherwise is void).
  bool has_return_type () const { return return_type != nullptr; }

  // Returns whether function has a where clause.
  bool has_where_clause () const { return !where_clause.is_empty (); }

  WhereClause &get_where_clause () { return where_clause; }

  bool is_method () const { return !self.is_error (); }

  SelfParam &get_self () { return self; }

  Identifier get_function_name () const { return function_name; }

  std::vector<std::unique_ptr<GenericParam>> &get_generic_params ()
  {
    return generic_params;
  }

  std::unique_ptr<Type> &get_return_type () { return return_type; }

  std::vector<FunctionParam> &get_function_params () { return function_params; }

  const FunctionQualifiers &get_qualifiers () const { return qualifiers; }
};

// Actual trait item function declaration within traits
class TraitItemFunc : public TraitItem
{
  AST::AttrVec outer_attrs;
  TraitFunctionDecl decl;
  std::unique_ptr<BlockExpr> block_expr;
  location_t locus;

public:
  // Returns whether function has a definition or is just a declaration.
  bool has_definition () const { return block_expr != nullptr; }

  TraitItemFunc (Analysis::NodeMapping mappings, TraitFunctionDecl decl,
		 std::unique_ptr<BlockExpr> block_expr,
		 AST::AttrVec outer_attrs, location_t locus)
    : TraitItem (mappings), outer_attrs (std::move (outer_attrs)),
      decl (std::move (decl)), block_expr (std::move (block_expr)),
      locus (locus)
  {}

  // Copy constructor with clone
  TraitItemFunc (TraitItemFunc const &other)
    : TraitItem (other.mappings), outer_attrs (other.outer_attrs),
      decl (other.decl), locus (other.locus)
  {
    if (other.block_expr != nullptr)
      block_expr = other.block_expr->clone_block_expr ();
  }

  // Overloaded assignment operator to clone
  TraitItemFunc &operator= (TraitItemFunc const &other)
  {
    TraitItem::operator= (other);
    outer_attrs = other.outer_attrs;
    decl = other.decl;
    locus = other.locus;
    mappings = other.mappings;
    if (other.block_expr != nullptr)
      block_expr = other.block_expr->clone_block_expr ();

    return *this;
  }

  // move constructors
  TraitItemFunc (TraitItemFunc &&other) = default;
  TraitItemFunc &operator= (TraitItemFunc &&other) = default;

  std::string as_string () const override;

  location_t get_locus () const { return locus; }

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRTraitItemVisitor &vis) override;

  TraitFunctionDecl &get_decl () { return decl; }

  const TraitFunctionDecl &get_decl () const { return decl; }

  bool has_block_defined () const { return block_expr != nullptr; }

  std::unique_ptr<BlockExpr> &get_block_expr () { return block_expr; }

  const std::string trait_identifier () const override final
  {
    return decl.get_function_name ().as_string ();
  }

  TraitItemKind get_item_kind () const override final
  {
    return TraitItemKind::FUNC;
  }

  AST::AttrVec &get_outer_attrs () override final { return outer_attrs; }
  const AST::AttrVec &get_outer_attrs () const override final
  {
    return outer_attrs;
  }

  location_t get_trait_locus () const override { return get_locus (); }

protected:
  // Clone function implementation as (not pure) virtual method
  TraitItemFunc *clone_trait_item_impl () const override
  {
    return new TraitItemFunc (*this);
  }
};

// Constant item within traits
class TraitItemConst : public TraitItem
{
  AST::AttrVec outer_attrs;
  Identifier name;
  std::unique_ptr<Type> type;
  std::unique_ptr<Expr> expr;
  location_t locus;

public:
  // Whether the constant item has an associated expression.
  bool has_expression () const { return expr != nullptr; }

  TraitItemConst (Analysis::NodeMapping mappings, Identifier name,
		  std::unique_ptr<Type> type, std::unique_ptr<Expr> expr,
		  AST::AttrVec outer_attrs, location_t locus)
    : TraitItem (mappings), outer_attrs (std::move (outer_attrs)),
      name (std::move (name)), type (std::move (type)), expr (std::move (expr)),
      locus (locus)
  {}

  // Copy constructor with clones
  TraitItemConst (TraitItemConst const &other)
    : TraitItem (other.mappings), outer_attrs (other.outer_attrs),
      name (other.name), type (other.type->clone_type ()),
      expr (other.expr->clone_expr ()), locus (other.locus)
  {}

  // Overloaded assignment operator to clone
  TraitItemConst &operator= (TraitItemConst const &other)
  {
    TraitItem::operator= (other);
    outer_attrs = other.outer_attrs;
    name = other.name;
    type = other.type->clone_type ();
    expr = other.expr->clone_expr ();
    locus = other.locus;
    mappings = other.mappings;

    return *this;
  }

  // move constructors
  TraitItemConst (TraitItemConst &&other) = default;
  TraitItemConst &operator= (TraitItemConst &&other) = default;

  std::string as_string () const override;

  location_t get_locus () const { return locus; }

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRTraitItemVisitor &vis) override;

  Identifier get_name () const { return name; }

  bool has_expr () const { return expr != nullptr; }

  std::unique_ptr<Type> &get_type () { return type; }

  std::unique_ptr<Expr> &get_expr () { return expr; }

  const std::string trait_identifier () const override final
  {
    return name.as_string ();
  }

  TraitItemKind get_item_kind () const override final
  {
    return TraitItemKind::CONST;
  }

  AST::AttrVec &get_outer_attrs () override final { return outer_attrs; }
  const AST::AttrVec &get_outer_attrs () const override final
  {
    return outer_attrs;
  }

  location_t get_trait_locus () const override { return get_locus (); }

protected:
  // Clone function implementation as (not pure) virtual method
  TraitItemConst *clone_trait_item_impl () const override
  {
    return new TraitItemConst (*this);
  }
};

// Type items within traits
class TraitItemType : public TraitItem
{
  AST::AttrVec outer_attrs;

  Identifier name;
  std::vector<std::unique_ptr<TypeParamBound>>
    type_param_bounds; // inlined form
  location_t locus;

public:
  // Returns whether trait item type has type param bounds.
  bool has_type_param_bounds () const { return !type_param_bounds.empty (); }

  TraitItemType (Analysis::NodeMapping mappings, Identifier name,
		 std::vector<std::unique_ptr<TypeParamBound>> type_param_bounds,
		 AST::AttrVec outer_attrs, location_t locus)
    : TraitItem (mappings), outer_attrs (std::move (outer_attrs)),
      name (std::move (name)),
      type_param_bounds (std::move (type_param_bounds)), locus (locus)
  {}

  // Copy constructor with vector clone
  TraitItemType (TraitItemType const &other)
    : TraitItem (other.mappings), outer_attrs (other.outer_attrs),
      name (other.name), locus (other.locus)
  {
    type_param_bounds.reserve (other.type_param_bounds.size ());
    for (const auto &e : other.type_param_bounds)
      type_param_bounds.push_back (e->clone_type_param_bound ());
  }

  // Overloaded assignment operator with vector clone
  TraitItemType &operator= (TraitItemType const &other)
  {
    TraitItem::operator= (other);
    outer_attrs = other.outer_attrs;
    name = other.name;
    locus = other.locus;
    mappings = other.mappings;

    type_param_bounds.reserve (other.type_param_bounds.size ());
    for (const auto &e : other.type_param_bounds)
      type_param_bounds.push_back (e->clone_type_param_bound ());

    return *this;
  }

  // default move constructors
  TraitItemType (TraitItemType &&other) = default;
  TraitItemType &operator= (TraitItemType &&other) = default;

  std::string as_string () const override;

  location_t get_locus () const { return locus; }

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRTraitItemVisitor &vis) override;

  Identifier get_name () const { return name; }

  std::vector<std::unique_ptr<TypeParamBound>> &get_type_param_bounds ()
  {
    return type_param_bounds;
  }

  const std::string trait_identifier () const override final
  {
    return name.as_string ();
  }

  TraitItemKind get_item_kind () const override final
  {
    return TraitItemKind::TYPE;
  }

  AST::AttrVec &get_outer_attrs () override final { return outer_attrs; }
  const AST::AttrVec &get_outer_attrs () const override final
  {
    return outer_attrs;
  }

  location_t get_trait_locus () const override { return get_locus (); }

protected:
  // Clone function implementation as (not pure) virtual method
  TraitItemType *clone_trait_item_impl () const override
  {
    return new TraitItemType (*this);
  }
};

// Rust trait item declaration HIR node
class Trait : public VisItem
{
  Unsafety unsafety;
  Identifier name;
  std::vector<std::unique_ptr<GenericParam>> generic_params;
  std::vector<std::unique_ptr<TypeParamBound>> type_param_bounds;
  WhereClause where_clause;
  std::vector<std::unique_ptr<TraitItem>> trait_items;
  location_t locus;

public:
  std::string as_string () const override;

  // Returns whether trait has generic parameters.
  bool has_generics () const { return !generic_params.empty (); }

  // Returns whether trait has type parameter bounds.
  bool has_type_param_bounds () const { return !type_param_bounds.empty (); }

  // Returns whether trait has where clause.
  bool has_where_clause () const { return !where_clause.is_empty (); }

  // Returns whether trait has trait items.
  bool has_trait_items () const { return !trait_items.empty (); }

  std::vector<std::unique_ptr<TraitItem>> &get_trait_items ()
  {
    return trait_items;
  }

  WhereClause &get_where_clause () { return where_clause; }

  Identifier get_name () const { return name; }
  bool is_unsafe () const { return unsafety == Unsafety::Unsafe; }

  // Mega-constructor
  Trait (Analysis::NodeMapping mappings, Identifier name, Unsafety unsafety,
	 std::vector<std::unique_ptr<GenericParam>> generic_params,
	 std::vector<std::unique_ptr<TypeParamBound>> type_param_bounds,
	 WhereClause where_clause,
	 std::vector<std::unique_ptr<TraitItem>> trait_items, Visibility vis,
	 AST::AttrVec outer_attrs, location_t locus)
    : VisItem (std::move (mappings), std::move (vis), std::move (outer_attrs)),
      unsafety (unsafety), name (std::move (name)),
      generic_params (std::move (generic_params)),
      type_param_bounds (std::move (type_param_bounds)),
      where_clause (std::move (where_clause)),
      trait_items (std::move (trait_items)), locus (locus)
  {}

  // Copy constructor with vector clone
  Trait (Trait const &other)
    : VisItem (other), unsafety (other.unsafety), name (other.name),
      where_clause (other.where_clause), locus (other.locus)
  {
    generic_params.reserve (other.generic_params.size ());
    for (const auto &e : other.generic_params)
      generic_params.push_back (e->clone_generic_param ());

    type_param_bounds.reserve (other.type_param_bounds.size ());
    for (const auto &e : other.type_param_bounds)
      type_param_bounds.push_back (e->clone_type_param_bound ());

    trait_items.reserve (other.trait_items.size ());
    for (const auto &e : other.trait_items)
      trait_items.push_back (e->clone_trait_item ());
  }

  // Overloaded assignment operator with vector clone
  Trait &operator= (Trait const &other)
  {
    VisItem::operator= (other);
    name = other.name;
    unsafety = other.unsafety;
    where_clause = other.where_clause;
    locus = other.locus;

    generic_params.reserve (other.generic_params.size ());
    for (const auto &e : other.generic_params)
      generic_params.push_back (e->clone_generic_param ());

    type_param_bounds.reserve (other.type_param_bounds.size ());
    for (const auto &e : other.type_param_bounds)
      type_param_bounds.push_back (e->clone_type_param_bound ());

    trait_items.reserve (other.trait_items.size ());
    for (const auto &e : other.trait_items)
      trait_items.push_back (e->clone_trait_item ());

    return *this;
  }

  // default move constructors
  Trait (Trait &&other) = default;
  Trait &operator= (Trait &&other) = default;

  location_t get_locus () const override final { return locus; }

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRStmtVisitor &vis) override;
  void accept_vis (HIRVisItemVisitor &vis) override;

  std::vector<std::unique_ptr<GenericParam>> &get_generic_params ()
  {
    return generic_params;
  }

  const std::vector<std::unique_ptr<GenericParam>> &get_generic_params () const
  {
    return generic_params;
  }

  std::vector<std::unique_ptr<TypeParamBound>> &get_type_param_bounds ()
  {
    return type_param_bounds;
  }

  const std::vector<std::unique_ptr<TypeParamBound>> &
  get_type_param_bounds () const
  {
    return type_param_bounds;
  }

  ItemKind get_item_kind () const override { return ItemKind::Trait; }

protected:
  /* Use covariance to implement clone function as returning this object
   * rather than base */
  Trait *clone_item_impl () const override { return new Trait (*this); }
};

class ImplBlock : public VisItem, public WithInnerAttrs
{
  std::vector<std::unique_ptr<GenericParam>> generic_params;
  std::unique_ptr<Type> impl_type;
  std::unique_ptr<TypePath> trait_ref;
  WhereClause where_clause;
  BoundPolarity polarity;
  location_t locus;
  std::vector<std::unique_ptr<ImplItem>> impl_items;

public:
  ImplBlock (Analysis::NodeMapping mappings,
	     std::vector<std::unique_ptr<ImplItem>> impl_items,
	     std::vector<std::unique_ptr<GenericParam>> generic_params,
	     std::unique_ptr<Type> impl_type,
	     std::unique_ptr<TypePath> trait_ref, WhereClause where_clause,
	     BoundPolarity polarity, Visibility vis, AST::AttrVec inner_attrs,
	     AST::AttrVec outer_attrs, location_t locus)
    : VisItem (std::move (mappings), std::move (vis), std::move (outer_attrs)),
      WithInnerAttrs (std::move (inner_attrs)),
      generic_params (std::move (generic_params)),
      impl_type (std::move (impl_type)), trait_ref (std::move (trait_ref)),
      where_clause (std::move (where_clause)), polarity (polarity),
      locus (locus), impl_items (std::move (impl_items))
  {}

  ImplBlock (ImplBlock const &other)
    : VisItem (other), WithInnerAttrs (other.inner_attrs),
      impl_type (other.impl_type->clone_type ()),
      where_clause (other.where_clause), polarity (other.polarity),
      locus (other.locus)
  {
    generic_params.reserve (other.generic_params.size ());
    for (const auto &e : other.generic_params)
      generic_params.push_back (e->clone_generic_param ());

    impl_items.reserve (other.impl_items.size ());
    for (const auto &e : other.impl_items)
      impl_items.push_back (e->clone_inherent_impl_item ());
  }

  ImplBlock &operator= (ImplBlock const &other)
  {
    VisItem::operator= (other);
    impl_type = other.impl_type->clone_type ();
    where_clause = other.where_clause;
    polarity = other.polarity;
    inner_attrs = other.inner_attrs;
    locus = other.locus;

    generic_params.reserve (other.generic_params.size ());
    for (const auto &e : other.generic_params)
      generic_params.push_back (e->clone_generic_param ());

    impl_items.reserve (other.impl_items.size ());
    for (const auto &e : other.impl_items)
      impl_items.push_back (e->clone_inherent_impl_item ());

    return *this;
  }

  ImplBlock (ImplBlock &&other) = default;
  ImplBlock &operator= (ImplBlock &&other) = default;

  std::string as_string () const override;

  // Returns whether inherent impl block has inherent impl items.
  bool has_impl_items () const { return !impl_items.empty (); }

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRStmtVisitor &vis) override;
  void accept_vis (HIRVisItemVisitor &vis) override;

  std::vector<std::unique_ptr<ImplItem>> &get_impl_items ()
  {
    return impl_items;
  };

  const std::vector<std::unique_ptr<ImplItem>> &get_impl_items () const
  {
    return impl_items;
  };

  // Returns whether impl has generic parameters.
  bool has_generics () const { return !generic_params.empty (); }

  // Returns whether impl has where clause.
  bool has_where_clause () const { return !where_clause.is_empty (); }

  // Returns the polarity of the impl.
  BoundPolarity get_polarity () const { return polarity; }

  location_t get_locus () const override final { return locus; }

  std::unique_ptr<Type> &get_type () { return impl_type; };

  std::vector<std::unique_ptr<GenericParam>> &get_generic_params ()
  {
    return generic_params;
  }

  bool has_trait_ref () const { return trait_ref != nullptr; }

  std::unique_ptr<TypePath> &get_trait_ref () { return trait_ref; }

  WhereClause &get_where_clause () { return where_clause; }

  ItemKind get_item_kind () const override { return ItemKind::Impl; }

protected:
  ImplBlock *clone_item_impl () const override { return new ImplBlock (*this); }
};

// Abstract base class for an item used inside an extern block
class ExternalItem : public Node
{
  Analysis::NodeMapping mappings;
  AST::AttrVec outer_attrs;
  Visibility visibility;
  Identifier item_name;
  location_t locus;

public:
  enum class ExternKind
  {
    Static,
    Function,
  };

  virtual ~ExternalItem () {}

  BaseKind get_hir_kind () override final { return EXTERNAL; }

  virtual ExternKind get_extern_kind () = 0;

  // Returns whether item has outer attributes.
  bool has_outer_attrs () const { return !outer_attrs.empty (); }

  // Returns whether item has non-default visibility.
  bool has_visibility () const { return !visibility.is_error (); }

  // Unique pointer custom clone function
  std::unique_ptr<ExternalItem> clone_external_item () const
  {
    return std::unique_ptr<ExternalItem> (clone_external_item_impl ());
  }

  virtual std::string as_string () const;

  location_t get_locus () const { return locus; }

  virtual void accept_vis (HIRFullVisitor &vis) = 0;
  virtual void accept_vis (HIRExternalItemVisitor &vis) = 0;

  Visibility &get_visibility () { return visibility; }
  Analysis::NodeMapping get_mappings () const { return mappings; }

  Identifier get_item_name () const { return item_name; }

  AST::AttrVec &get_outer_attrs () { return outer_attrs; }

protected:
  ExternalItem (Analysis::NodeMapping mappings, Identifier item_name,
		Visibility vis, AST::AttrVec outer_attrs, location_t locus)
    : mappings (mappings), outer_attrs (std::move (outer_attrs)),
      visibility (std::move (vis)), item_name (std::move (item_name)),
      locus (locus)
  {}

  // Copy constructor
  ExternalItem (ExternalItem const &other)
    : mappings (other.mappings), outer_attrs (other.outer_attrs),
      visibility (other.visibility), item_name (other.item_name),
      locus (other.locus)
  {}

  // Overloaded assignment operator to clone
  ExternalItem &operator= (ExternalItem const &other)
  {
    mappings = other.mappings;
    item_name = other.item_name;
    visibility = other.visibility;
    outer_attrs = other.outer_attrs;
    locus = other.locus;

    return *this;
  }

  // move constructors
  ExternalItem (ExternalItem &&other) = default;
  ExternalItem &operator= (ExternalItem &&other) = default;

  // Clone function implementation as pure virtual method
  virtual ExternalItem *clone_external_item_impl () const = 0;
};

// A static item used in an extern block
class ExternalStaticItem : public ExternalItem
{
  Mutability mut;
  std::unique_ptr<Type> item_type;

public:
  ExternalStaticItem (Analysis::NodeMapping mappings, Identifier item_name,
		      std::unique_ptr<Type> item_type, Mutability mut,
		      Visibility vis, AST::AttrVec outer_attrs,
		      location_t locus)
    : ExternalItem (std::move (mappings), std::move (item_name),
		    std::move (vis), std::move (outer_attrs), locus),
      mut (mut), item_type (std::move (item_type))
  {}

  // Copy constructor
  ExternalStaticItem (ExternalStaticItem const &other)
    : ExternalItem (other), mut (other.mut),
      item_type (other.item_type->clone_type ())
  {}

  // Overloaded assignment operator to clone
  ExternalStaticItem &operator= (ExternalStaticItem const &other)
  {
    ExternalItem::operator= (other);
    item_type = other.item_type->clone_type ();
    mut = other.mut;

    return *this;
  }

  // move constructors
  ExternalStaticItem (ExternalStaticItem &&other) = default;
  ExternalStaticItem &operator= (ExternalStaticItem &&other) = default;

  std::string as_string () const override;

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExternalItemVisitor &vis) override;

  bool is_mut () const { return mut == Mutability::Mut; }

  Mutability get_mut () { return mut; }

  std::unique_ptr<Type> &get_item_type () { return item_type; }

  ExternKind get_extern_kind () override { return ExternKind::Static; }

protected:
  /* Use covariance to implement clone function as returning this object
   * rather than base */
  ExternalStaticItem *clone_external_item_impl () const override
  {
    return new ExternalStaticItem (*this);
  }
};

// A named function parameter used in external functions
struct NamedFunctionParam
{
private:
  Identifier name;
  std::unique_ptr<Type> param_type;
  Analysis::NodeMapping mappings;

public:
  bool has_name () const { return name.as_string () != "_"; }

  NamedFunctionParam (Analysis::NodeMapping mappings, Identifier name,
		      std::unique_ptr<Type> param_type)
    : name (std::move (name)), param_type (std::move (param_type)),
      mappings (std::move (mappings))
  {}

  // Copy constructor
  NamedFunctionParam (NamedFunctionParam const &other)
    : name (other.name), param_type (other.param_type->clone_type ()),
      mappings (other.mappings)
  {}

  ~NamedFunctionParam () = default;

  // Overloaded assignment operator to clone
  NamedFunctionParam &operator= (NamedFunctionParam const &other)
  {
    mappings = other.mappings;
    name = other.name;
    param_type = other.param_type->clone_type ();
    // has_name = other.has_name;

    return *this;
  }

  // move constructors
  NamedFunctionParam (NamedFunctionParam &&other) = default;
  NamedFunctionParam &operator= (NamedFunctionParam &&other) = default;

  std::string as_string () const;

  Identifier get_param_name () const { return name; }

  std::unique_ptr<Type> &get_type () { return param_type; }

  Analysis::NodeMapping get_mappings () const { return mappings; }
};

// A function item used in an extern block
class ExternalFunctionItem : public ExternalItem
{
  // bool has_generics;
  // Generics generic_params;
  std::vector<std::unique_ptr<GenericParam>> generic_params; // inlined

  // bool has_return_type;
  // FunctionReturnType return_type;
  std::unique_ptr<Type> return_type; // inlined

  // bool has_where_clause;
  WhereClause where_clause;

  std::vector<NamedFunctionParam> function_params;
  bool has_variadics;

public:
  // Returns whether item has generic parameters.
  bool has_generics () const { return !generic_params.empty (); }

  // Returns whether item has a return type (otherwise void).
  bool has_return_type () const { return return_type != nullptr; }

  // Returns whether item has a where clause.
  bool has_where_clause () const { return !where_clause.is_empty (); }

  WARN_UNUSED_RESULT const WhereClause &get_where_clause () const
  {
    return where_clause;
  }

  ExternalFunctionItem (
    Analysis::NodeMapping mappings, Identifier item_name,
    std::vector<std::unique_ptr<GenericParam>> generic_params,
    std::unique_ptr<Type> return_type, WhereClause where_clause,
    std::vector<NamedFunctionParam> function_params, bool has_variadics,
    Visibility vis, AST::AttrVec outer_attrs, location_t locus)
    : ExternalItem (std::move (mappings), std::move (item_name),
		    std::move (vis), std::move (outer_attrs), locus),
      generic_params (std::move (generic_params)),
      return_type (std::move (return_type)),
      where_clause (std::move (where_clause)),
      function_params (std::move (function_params)),
      has_variadics (has_variadics)
  {}

  // Copy constructor with clone
  ExternalFunctionItem (ExternalFunctionItem const &other)
    : ExternalItem (other), return_type (other.return_type->clone_type ()),
      where_clause (other.where_clause),
      function_params (other.function_params),
      has_variadics (other.has_variadics)
  {
    generic_params.reserve (other.generic_params.size ());
    for (const auto &e : other.generic_params)
      generic_params.push_back (e->clone_generic_param ());
  }

  // Overloaded assignment operator with clone
  ExternalFunctionItem &operator= (ExternalFunctionItem const &other)
  {
    ExternalItem::operator= (other);
    return_type = other.return_type->clone_type ();
    where_clause = other.where_clause;
    function_params = other.function_params;
    has_variadics = other.has_variadics;

    generic_params.reserve (other.generic_params.size ());
    for (const auto &e : other.generic_params)
      generic_params.push_back (e->clone_generic_param ());

    return *this;
  }

  // move constructors
  ExternalFunctionItem (ExternalFunctionItem &&other) = default;
  ExternalFunctionItem &operator= (ExternalFunctionItem &&other) = default;

  std::string as_string () const override;

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRExternalItemVisitor &vis) override;

  std::vector<std::unique_ptr<GenericParam>> &get_generic_params ()
  {
    return generic_params;
  }

  std::unique_ptr<Type> &get_return_type () { return return_type; }

  std::vector<NamedFunctionParam> &get_function_params ()
  {
    return function_params;
  }

  bool is_variadic () const { return has_variadics; }

  ExternKind get_extern_kind () override { return ExternKind::Function; }

protected:
  /* Use covariance to implement clone function as returning this object
   * rather than base */
  ExternalFunctionItem *clone_external_item_impl () const override
  {
    return new ExternalFunctionItem (*this);
  }
};

// An extern block HIR node
class ExternBlock : public VisItem, public WithInnerAttrs
{
  ABI abi;
  std::vector<std::unique_ptr<ExternalItem>> extern_items;
  location_t locus;

public:
  std::string as_string () const override;

  // Returns whether extern block has extern items.
  bool has_extern_items () const { return !extern_items.empty (); }

  ABI get_abi () const { return abi; }

  ExternBlock (Analysis::NodeMapping mappings, ABI abi,
	       std::vector<std::unique_ptr<ExternalItem>> extern_items,
	       Visibility vis, AST::AttrVec inner_attrs,
	       AST::AttrVec outer_attrs, location_t locus)
    : VisItem (std::move (mappings), std::move (vis), std::move (outer_attrs)),
      WithInnerAttrs (std::move (inner_attrs)), abi (abi),
      extern_items (std::move (extern_items)), locus (locus)
  {}

  // Copy constructor with vector clone
  ExternBlock (ExternBlock const &other)
    : VisItem (other), WithInnerAttrs (other.inner_attrs), abi (other.abi),
      locus (other.locus)
  {
    extern_items.reserve (other.extern_items.size ());
    for (const auto &e : other.extern_items)
      extern_items.push_back (e->clone_external_item ());
  }

  // Overloaded assignment operator with vector clone
  ExternBlock &operator= (ExternBlock const &other)
  {
    VisItem::operator= (other);
    abi = other.abi;
    inner_attrs = other.inner_attrs;
    locus = other.locus;

    extern_items.reserve (other.extern_items.size ());
    for (const auto &e : other.extern_items)
      extern_items.push_back (e->clone_external_item ());

    return *this;
  }

  // move constructors
  ExternBlock (ExternBlock &&other) = default;
  ExternBlock &operator= (ExternBlock &&other) = default;

  location_t get_locus () const override final { return locus; }

  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRStmtVisitor &vis) override;
  void accept_vis (HIRVisItemVisitor &vis) override;

  std::vector<std::unique_ptr<ExternalItem>> &get_extern_items ()
  {
    return extern_items;
  }

  ItemKind get_item_kind () const override { return ItemKind::ExternBlock; }

protected:
  /* Use covariance to implement clone function as returning this object
   * rather than base */
  ExternBlock *clone_item_impl () const override
  {
    return new ExternBlock (*this);
  }

  /* Use covariance to implement clone function as returning this object
   * rather than base */
  /*virtual ExternBlock* clone_statement_impl() const override {
      return new ExternBlock(*this);
  }*/
};

} // namespace HIR
} // namespace Rust

#endif
