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

#ifndef RUST_AST_ITEM_H
#define RUST_AST_ITEM_H

#include "rust-ast.h"
#include "rust-hir-map.h"
#include "rust-mapping-common.h"
#include "rust-path.h"
#include "rust-common.h"
#include "rust-expr.h"

namespace Rust {
namespace AST {
// forward decls
class TypePath;

// TODO: inline?
/*struct AbiName {
    std::string abi_name;
    // Technically is meant to be STRING_LITERAL

  public:
    // Returns whether abi name is empty, i.e. doesn't exist.
    bool is_empty() const {
	return abi_name.empty();
    }

    AbiName(std::string name) : abi_name(std::move(name)) {}

    // Empty AbiName constructor
    AbiName() {}
};*/

// A type generic parameter (as opposed to a lifetime generic parameter)
class TypeParam : public GenericParam
{
  // bool has_outer_attribute;
  // std::unique_ptr<Attribute> outer_attr;
  Attribute outer_attr;

  Identifier type_representation;

  // bool has_type_param_bounds;
  // TypeParamBounds type_param_bounds;
  std::vector<std::unique_ptr<TypeParamBound>>
    type_param_bounds; // inlined form

  // bool has_type;
  std::unique_ptr<Type> type;

  location_t locus;

public:
  Identifier get_type_representation () const { return type_representation; }

  // Returns whether the type of the type param has been specified.
  bool has_type () const { return type != nullptr; }

  // Returns whether the type param has type param bounds.
  bool has_type_param_bounds () const { return !type_param_bounds.empty (); }

  // Returns whether the type param has an outer attribute.
  bool has_outer_attribute () const { return !outer_attr.is_empty (); }

  Attribute &get_outer_attribute () { return outer_attr; }

  TypeParam (Identifier type_representation, location_t locus = UNDEF_LOCATION,
	     std::vector<std::unique_ptr<TypeParamBound>> type_param_bounds
	     = std::vector<std::unique_ptr<TypeParamBound>> (),
	     std::unique_ptr<Type> type = nullptr,
	     Attribute outer_attr = Attribute::create_empty ())
    : GenericParam (Analysis::Mappings::get ()->get_next_node_id ()),
      outer_attr (std::move (outer_attr)),
      type_representation (std::move (type_representation)),
      type_param_bounds (std::move (type_param_bounds)),
      type (std::move (type)), locus (locus)
  {}

  // Copy constructor uses clone
  TypeParam (TypeParam const &other)
    : GenericParam (other.node_id), outer_attr (other.outer_attr),
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
    node_id = other.node_id;

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

  Kind get_kind () const override final { return Kind::Type; }

  void accept_vis (ASTVisitor &vis) override;

  // TODO: is this better? Or is a "vis_block" better?
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

  // TODO: mutable getter seems kinda dodgy
  std::vector<std::unique_ptr<TypeParamBound>> &get_type_param_bounds ()
  {
    return type_param_bounds;
  }
  const std::vector<std::unique_ptr<TypeParamBound>> &
  get_type_param_bounds () const
  {
    return type_param_bounds;
  }

protected:
  // Clone function implementation as virtual method
  TypeParam *clone_generic_param_impl () const override
  {
    return new TypeParam (*this);
  }
};

/* "where" clause item base. Abstract - use LifetimeWhereClauseItem,
 * TypeBoundWhereClauseItem */
class WhereClauseItem
{
public:
  virtual ~WhereClauseItem () {}

  // Unique pointer custom clone function
  std::unique_ptr<WhereClauseItem> clone_where_clause_item () const
  {
    return std::unique_ptr<WhereClauseItem> (clone_where_clause_item_impl ());
  }

  virtual std::string as_string () const = 0;

  virtual void accept_vis (ASTVisitor &vis) = 0;

  virtual NodeId get_node_id () const = 0;

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
  NodeId node_id;

public:
  LifetimeWhereClauseItem (Lifetime lifetime,
			   std::vector<Lifetime> lifetime_bounds,
			   location_t locus)
    : lifetime (std::move (lifetime)),
      lifetime_bounds (std::move (lifetime_bounds)), locus (locus),
      node_id (Analysis::Mappings::get ()->get_next_node_id ())
  {}

  std::string as_string () const override;

  void accept_vis (ASTVisitor &vis) override;

  NodeId get_node_id () const override final { return node_id; }

  Lifetime &get_lifetime () { return lifetime; }

  std::vector<Lifetime> &get_lifetime_bounds () { return lifetime_bounds; }

  location_t get_locus () const { return locus; }

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
  NodeId node_id;
  location_t locus;

public:
  // Returns whether the item has ForLifetimes
  bool has_for_lifetimes () const { return !for_lifetimes.empty (); }

  std::vector<LifetimeParam> &get_for_lifetimes () { return for_lifetimes; }

  // Returns whether the item has type param bounds
  bool has_type_param_bounds () const { return !type_param_bounds.empty (); }

  TypeBoundWhereClauseItem (
    std::vector<LifetimeParam> for_lifetimes, std::unique_ptr<Type> bound_type,
    std::vector<std::unique_ptr<TypeParamBound>> type_param_bounds,
    location_t locus)
    : for_lifetimes (std::move (for_lifetimes)),
      bound_type (std::move (bound_type)),
      type_param_bounds (std::move (type_param_bounds)),
      node_id (Analysis::Mappings::get ()->get_next_node_id ()), locus (locus)
  {}

  // Copy constructor requires clone
  TypeBoundWhereClauseItem (TypeBoundWhereClauseItem const &other)
    : for_lifetimes (other.for_lifetimes),
      bound_type (other.bound_type->clone_type ())
  {
    node_id = other.node_id;
    type_param_bounds.reserve (other.type_param_bounds.size ());
    for (const auto &e : other.type_param_bounds)
      type_param_bounds.push_back (e->clone_type_param_bound ());
  }

  // Overload assignment operator to clone
  TypeBoundWhereClauseItem &operator= (TypeBoundWhereClauseItem const &other)
  {
    node_id = other.node_id;
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

  std::string as_string () const override;

  void accept_vis (ASTVisitor &vis) override;

  Type &get_type ()
  {
    rust_assert (bound_type != nullptr);
    return *bound_type;
  }

  std::unique_ptr<Type> &get_type_ptr ()
  {
    rust_assert (bound_type != nullptr);
    return bound_type;
  }

  // TODO: this mutable getter seems really dodgy. Think up better way.
  std::vector<std::unique_ptr<TypeParamBound>> &get_type_param_bounds ()
  {
    return type_param_bounds;
  }

  const std::vector<std::unique_ptr<TypeParamBound>> &
  get_type_param_bounds () const
  {
    return type_param_bounds;
  }

  NodeId get_node_id () const override final { return node_id; }

  location_t get_locus () const { return locus; }

protected:
  // Clone function implementation as (not pure) virtual method
  TypeBoundWhereClauseItem *clone_where_clause_item_impl () const override
  {
    return new TypeBoundWhereClauseItem (*this);
  }
};

// A where clause
class WhereClause
{
  std::vector<std::unique_ptr<WhereClauseItem>> where_clause_items;
  NodeId node_id;

public:
  WhereClause (std::vector<std::unique_ptr<WhereClauseItem>> where_clause_items)
    : where_clause_items (std::move (where_clause_items)),
      node_id (Analysis::Mappings::get ()->get_next_node_id ())
  {}

  // copy constructor with vector clone
  WhereClause (WhereClause const &other)
  {
    node_id = other.node_id;
    where_clause_items.reserve (other.where_clause_items.size ());
    for (const auto &e : other.where_clause_items)
      where_clause_items.push_back (e->clone_where_clause_item ());
  }

  // overloaded assignment operator with vector clone
  WhereClause &operator= (WhereClause const &other)
  {
    node_id = other.node_id;
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

  NodeId get_node_id () const { return node_id; }

  // TODO: this mutable getter seems kinda dodgy
  std::vector<std::unique_ptr<WhereClauseItem>> &get_items ()
  {
    return where_clause_items;
  }
  const std::vector<std::unique_ptr<WhereClauseItem>> &get_items () const
  {
    return where_clause_items;
  }
};

// Abstract class Param
class Param : public Visitable
{
public:
  Param (std::vector<Attribute> outer_attrs, location_t locus)
    : outer_attrs (std::move (outer_attrs)), locus (locus),
      node_id (Analysis::Mappings::get ()->get_next_node_id ())
  {}

  virtual ~Param () = default;

  std::unique_ptr<Param> clone_param () const
  {
    return std::unique_ptr<Param> (clone_param_impl ());
  }

  virtual bool is_variadic () const { return false; }

  virtual bool is_self () const { return false; }

  NodeId get_node_id () const { return node_id; }

  location_t get_locus () const { return locus; }

  std::vector<Attribute> get_outer_attrs () const { return outer_attrs; }

  std::vector<Attribute> &get_outer_attrs () { return outer_attrs; }

  virtual Param *clone_param_impl () const = 0;

  virtual std::string as_string () const = 0;

protected:
  std::vector<Attribute> outer_attrs;
  location_t locus;
  NodeId node_id;
};

// A self parameter in a method
class SelfParam : public Param
{
  bool has_ref;
  bool is_mut;
  // bool has_lifetime; // only possible if also ref
  Lifetime lifetime;

  // bool has_type; // only possible if not ref
  std::unique_ptr<Type> type;

  // Unrestricted constructor used for error state
  SelfParam (Lifetime lifetime, bool has_ref, bool is_mut, Type *type)
    : Param ({}, UNDEF_LOCATION), has_ref (has_ref), is_mut (is_mut),
      lifetime (std::move (lifetime)), type (type)
  {}
  // this is ok as no outside classes can ever call this

  // TODO: self param can have outer attributes

public:
  // Returns whether the self-param has a type field.
  bool has_type () const { return type != nullptr; }

  // Returns whether the self-param has a valid lifetime.
  bool has_lifetime () const { return !lifetime.is_error (); }

  // Returns whether the self-param is in an error state.
  bool is_error () const
  {
    return (has_type () && has_lifetime ()) || (has_lifetime () && !has_ref);
    // not having either is not an error
  }

  // Creates an error state self-param.
  static SelfParam create_error ()
  {
    // cannot have no ref but have a lifetime at the same time
    return SelfParam (Lifetime (Lifetime::STATIC), false, false, nullptr);
  }

  // Type-based self parameter (not ref, no lifetime)
  SelfParam (std::unique_ptr<Type> type, bool is_mut, location_t locus)
    : Param ({}, locus), has_ref (false), is_mut (is_mut),
      lifetime (Lifetime::error ()), type (std::move (type))
  {}

  // Lifetime-based self parameter (is ref, no type)
  SelfParam (Lifetime lifetime, bool is_mut, location_t locus)
    : Param ({}, locus), has_ref (true), is_mut (is_mut),
      lifetime (std::move (lifetime))
  {}

  // Copy constructor requires clone
  SelfParam (SelfParam const &other)
    : Param (other.get_outer_attrs (), other.get_locus ()),
      has_ref (other.has_ref), is_mut (other.is_mut), lifetime (other.lifetime)
  {
    node_id = other.node_id;
    if (other.type != nullptr)
      type = other.type->clone_type ();
  }

  // Overload assignment operator to use clone
  SelfParam &operator= (SelfParam const &other)
  {
    is_mut = other.is_mut;
    has_ref = other.has_ref;
    lifetime = other.lifetime;
    locus = other.locus;
    node_id = other.node_id;
    outer_attrs = other.outer_attrs;

    if (other.type != nullptr)
      type = other.type->clone_type ();
    else
      type = nullptr;

    return *this;
  }

  // move constructors
  SelfParam (SelfParam &&other) = default;
  SelfParam &operator= (SelfParam &&other) = default;

  std::string as_string () const override;

  location_t get_locus () const { return locus; }

  bool is_self () const override { return true; }

  bool get_has_ref () const { return has_ref; };
  bool get_is_mut () const { return is_mut; }

  Lifetime get_lifetime () const { return lifetime; }
  Lifetime &get_lifetime () { return lifetime; }

  NodeId get_node_id () const { return node_id; }

  // TODO: is this better? Or is a "vis_block" better?
  Type &get_type ()
  {
    rust_assert (has_type ());
    return *type;
  }

  std::unique_ptr<Type> &get_type_ptr ()
  {
    rust_assert (has_type ());
    return type;
  }

  void accept_vis (ASTVisitor &vis) override;

  SelfParam *clone_param_impl () const override
  {
    return new SelfParam (*this);
  }
};

// Qualifiers for function, i.e. const, unsafe, extern etc.
class FunctionQualifiers
{
  Async async_status;
  Const const_status;
  Unsafety unsafe_status;
  bool has_extern;
  std::string extern_abi;
  location_t locus;

public:
  FunctionQualifiers (location_t locus, Async async_status, Const const_status,
		      Unsafety unsafe_status, bool has_extern = false,
		      std::string extern_abi = std::string ())
    : async_status (async_status), const_status (const_status),
      unsafe_status (unsafe_status), has_extern (has_extern),
      extern_abi (std::move (extern_abi)), locus (locus)
  {
    if (!this->extern_abi.empty ())
      {
	// having extern is required; not having it is an implementation error
	rust_assert (has_extern);
      }
  }

  std::string as_string () const;

  bool is_unsafe () const { return unsafe_status == Unsafety::Unsafe; }
  bool is_extern () const { return has_extern; }
  bool is_const () const { return const_status == Const::Yes; }
  bool is_async () const { return async_status == Async::Yes; }
  std::string get_extern_abi () const { return extern_abi; }
  bool has_abi () const { return !extern_abi.empty (); }
  Const get_const_status () const { return const_status; }
  Async get_async_status () const { return async_status; }

  location_t get_locus () const { return locus; }
};

class VariadicParam : public Param
{
  std::unique_ptr<Pattern> param_name;

public:
  VariadicParam (std::unique_ptr<Pattern> param_name,
		 std::vector<Attribute> outer_attrs, location_t locus)
    : Param (std::move (outer_attrs), std::move (locus)),
      param_name (std::move (param_name))
  {}

  VariadicParam (std::vector<Attribute> outer_attrs, location_t locus)
    : Param (std::move (outer_attrs), std::move (locus)), param_name (nullptr)
  {}

  VariadicParam (VariadicParam const &other)
    : Param (other.get_outer_attrs (), other.locus)
  {
    if (other.param_name != nullptr)
      param_name = other.param_name->clone_pattern ();
  }

  VariadicParam &operator= (VariadicParam const &other)
  {
    outer_attrs = other.outer_attrs;
    locus = other.locus;
    node_id = other.node_id;
    if (other.param_name != nullptr)
      param_name = other.param_name->clone_pattern ();
    else
      param_name = nullptr;

    return *this;
  }

  bool is_variadic () const override { return true; }

  VariadicParam *clone_param_impl () const override
  {
    return new VariadicParam (*this);
  }

  Pattern &get_pattern ()
  {
    rust_assert (param_name != nullptr);
    return *param_name;
  }

  const Pattern &get_pattern () const
  {
    rust_assert (param_name != nullptr);
    return *param_name;
  }

  bool has_pattern () const { return param_name != nullptr; }

  void accept_vis (ASTVisitor &vis) override;

  std::string as_string () const override;
};

// A function parameter
class FunctionParam : public Param
{
  std::unique_ptr<Pattern> param_name;
  std::unique_ptr<Type> type;

public:
  FunctionParam (std::unique_ptr<Pattern> param_name,
		 std::unique_ptr<Type> param_type,
		 std::vector<Attribute> outer_attrs, location_t locus)
    : Param (std::move (outer_attrs), locus),
      param_name (std::move (param_name)), type (std::move (param_type))
  {}

  // Copy constructor uses clone
  FunctionParam (FunctionParam const &other)
    : Param (other.get_outer_attrs (), other.locus)
  {
    // guard to prevent nullptr dereference
    if (other.param_name != nullptr)
      param_name = other.param_name->clone_pattern ();
    if (other.type != nullptr)
      type = other.type->clone_type ();
  }

  // Overload assignment operator to use clone
  FunctionParam &operator= (FunctionParam const &other)
  {
    locus = other.locus;
    node_id = other.node_id;

    // guard to prevent nullptr dereference
    if (other.param_name != nullptr)
      param_name = other.param_name->clone_pattern ();
    else
      param_name = nullptr;
    if (other.type != nullptr)
      type = other.type->clone_type ();
    else
      type = nullptr;

    return *this;
  }

  // move constructors
  FunctionParam (FunctionParam &&other) = default;
  FunctionParam &operator= (FunctionParam &&other) = default;

  // Returns whether FunctionParam is in an invalid state.
  bool is_error () const { return param_name == nullptr || type == nullptr; }

  // Creates an error FunctionParam.
  static FunctionParam create_error ()
  {
    return FunctionParam (nullptr, nullptr, {}, UNDEF_LOCATION);
  }

  std::string as_string () const override;

  // TODO: seems kinda dodgy. Think of better way.
  std::vector<Attribute> &get_outer_attrs () { return outer_attrs; }
  const std::vector<Attribute> &get_outer_attrs () const { return outer_attrs; }

  // TODO: is this better? Or is a "vis_block" better?
  Pattern &get_pattern ()
  {
    rust_assert (param_name != nullptr);
    return *param_name;
  }

  bool has_name () const { return param_name != nullptr; }

  // TODO: is this better? Or is a "vis_block" better?
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

  FunctionParam *clone_param_impl () const override
  {
    return new FunctionParam (*this);
  }

  void accept_vis (ASTVisitor &vis) override;
};

// Rust module item - abstract base class
class Module : public VisItem
{
public:
  // Type of the current module. A module can be either loaded or unloaded,
  // meaning that the items of the module can already be present or not. For
  // example, the following module would be loaded: `mod foo { fn bar() {} }`.
  // However, the module would be unloaded if it refers to an external file (i.e
  // `mod foo;`) and then become loaded upon expansion.
  enum ModuleKind
  {
    LOADED,
    UNLOADED,
  };

  Identifier get_name () const { return module_name; }

  AST::Kind get_ast_kind () const override { return AST::Kind::MODULE; }

private:
  Identifier module_name;
  location_t locus;
  ModuleKind kind;
  Unsafety safety;

  // Name of the file including the module
  std::string outer_filename;
  // bool has_inner_attrs;
  std::vector<Attribute> inner_attrs;
  // bool has_items;
  std::vector<std::unique_ptr<Item>> items;
  // Names of including inline modules (immediate parent is last in the list)
  std::vector<std::string> module_scope;

  // Filename the module refers to. Empty string on LOADED modules or if an
  // error occured when dealing with UNLOADED modules
  std::string module_file;

  void clone_items (const std::vector<std::unique_ptr<Item>> &other_items)
  {
    items.reserve (other_items.size ());
    for (const auto &e : other_items)
      items.push_back (e->clone_item ());
  }

public:
  // Returns whether the module has items in its body.
  bool has_items () const { return !items.empty (); }

  // Returns whether the module has any inner attributes.
  bool has_inner_attrs () const { return !inner_attrs.empty (); }

  // Unloaded module constructor
  Module (Identifier module_name, Visibility visibility,
	  std::vector<Attribute> outer_attrs, location_t locus, Unsafety safety,
	  std::string outer_filename, std::vector<std::string> module_scope)
    : VisItem (std::move (visibility), std::move (outer_attrs)),
      module_name (module_name), locus (locus), kind (ModuleKind::UNLOADED),
      safety (safety), outer_filename (outer_filename),
      inner_attrs (std::vector<Attribute> ()),
      items (std::vector<std::unique_ptr<Item>> ()),
      module_scope (std::move (module_scope))
  {}

  // Loaded module constructor, with items
  Module (Identifier name, location_t locus,
	  std::vector<std::unique_ptr<Item>> items,
	  Visibility visibility = Visibility::create_error (),
	  Unsafety safety = Unsafety::Normal,
	  std::vector<Attribute> inner_attrs = std::vector<Attribute> (),
	  std::vector<Attribute> outer_attrs = std::vector<Attribute> ())
    : VisItem (std::move (visibility), std::move (outer_attrs)),
      module_name (name), locus (locus), kind (ModuleKind::LOADED),
      safety (safety), outer_filename (std::string ()),
      inner_attrs (std::move (inner_attrs)), items (std::move (items))
  {}

  // Copy constructor with vector clone
  Module (Module const &other)
    : VisItem (other), module_name (other.module_name), locus (other.locus),
      kind (other.kind), safety (other.safety), inner_attrs (other.inner_attrs),
      module_scope (other.module_scope)
  {
    // We need to check whether we are copying a loaded module or an unloaded
    // one. In the second case, clear the `items` vector.
    if (other.kind == LOADED)
      clone_items (other.items);
    else
      items.clear ();
  }

  // Overloaded assignment operator with vector clone
  Module &operator= (Module const &other)
  {
    VisItem::operator= (other);

    module_name = other.module_name;
    locus = other.locus;
    kind = other.kind;
    inner_attrs = other.inner_attrs;
    module_scope = other.module_scope;

    // Likewise, we need to clear the `items` vector in case the other module is
    // unloaded
    if (kind == LOADED)
      clone_items (other.items);
    else
      items.clear ();

    return *this;
  }

  // Search for the filename associated with an external module, storing it in
  // module_file
  void process_file_path ();
  // Load the items contained in an external module
  void load_items ();

  void accept_vis (ASTVisitor &vis) override;

  /* Override that runs the function recursively on all items contained within
   * the module. */
  void add_crate_name (std::vector<std::string> &names) const override;

  // Returns the kind of the module
  enum ModuleKind get_kind () const { return kind; }

  Unsafety get_unsafety () const { return safety; }

  // TODO: think of better way to do this - mutable getter seems dodgy
  const std::vector<Attribute> &get_inner_attrs () const { return inner_attrs; }
  std::vector<Attribute> &get_inner_attrs () { return inner_attrs; }

  const std::vector<std::unique_ptr<Item>> &get_items () const { return items; }
  std::vector<std::unique_ptr<Item>> &get_items () { return items; }

  std::vector<std::unique_ptr<AST::Item>> take_items ()
  {
    return std::move (items);
  }

  void set_items (std::vector<std::unique_ptr<AST::Item>> &&new_items)
  {
    items = std::move (new_items);
  }

  // move constructors
  Module (Module &&other) = default;
  Module &operator= (Module &&other) = default;

  std::string as_string () const override;

  location_t get_locus () const override final { return locus; }

  // Invalid if name is empty, so base stripping on that.
  void mark_for_strip () override { module_name = {""}; }
  bool is_marked_for_strip () const override { return module_name.empty (); }

protected:
  /* Use covariance to implement clone function as returning this object
   * rather than base */
  Module *clone_item_impl () const override { return new Module (*this); }
};

// Rust extern crate declaration AST node
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
  ExternCrate (std::string referenced_crate, Visibility visibility,
	       std::vector<Attribute> outer_attrs, location_t locus,
	       std::string as_clause_name = std::string ())
    : VisItem (std::move (visibility), std::move (outer_attrs)),
      referenced_crate (std::move (referenced_crate)),
      as_clause_name (std::move (as_clause_name)), locus (locus)
  {}

  location_t get_locus () const override final { return locus; }

  void accept_vis (ASTVisitor &vis) override;

  const std::string &get_referenced_crate () const { return referenced_crate; }
  const std::string &get_as_clause () const { return as_clause_name; }

  // Override that adds extern crate name in decl to passed list of names.
  void add_crate_name (std::vector<std::string> &names) const override
  {
    names.push_back (referenced_crate);
  }

  // Invalid if crate name is empty, so base stripping on that.
  void mark_for_strip () override { referenced_crate = ""; }
  bool is_marked_for_strip () const override
  {
    return referenced_crate.empty ();
  }

protected:
  /* Use covariance to implement clone function as returning this object
   * rather than base */
  ExternCrate *clone_item_impl () const override
  {
    return new ExternCrate (*this);
  }
};

// The path-ish thing referred to in a use declaration - abstract base class
class UseTree
{
  location_t locus;
  NodeId node_id;

public:
  enum Kind
  {
    Glob,
    Rebind,
    List,
  };

  virtual ~UseTree () {}

  // Overload assignment operator to clone
  UseTree &operator= (UseTree const &other)
  {
    locus = other.locus;

    return *this;
  }

  UseTree (const UseTree &other) = default;

  // move constructors
  UseTree (UseTree &&other) = default;
  UseTree &operator= (UseTree &&other) = default;

  // Unique pointer custom clone function
  std::unique_ptr<UseTree> clone_use_tree () const
  {
    return std::unique_ptr<UseTree> (clone_use_tree_impl ());
  }

  virtual std::string as_string () const = 0;
  virtual Kind get_kind () const = 0;

  location_t get_locus () const { return locus; }
  NodeId get_node_id () const { return node_id; }

  virtual void accept_vis (ASTVisitor &vis) = 0;

protected:
  // Clone function implementation as pure virtual method
  virtual UseTree *clone_use_tree_impl () const = 0;

  UseTree (location_t locus)
    : locus (locus), node_id (Analysis::Mappings::get ()->get_next_node_id ())
  {}
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
  SimplePath path;

public:
  UseTreeGlob (PathType glob_type, SimplePath path, location_t locus)
    : UseTree (locus), glob_type (glob_type), path (std::move (path))
  {
    if (this->glob_type != PATH_PREFIXED)
      {
	// compiler implementation error if there is a path with a
	// non-path-prefixed use tree glob
	rust_assert (!has_path ());
      }
    // TODO: do path-prefixed paths also have to have a path? If so, have an
    // assert for that too.
  }

  /* Returns whether has path. Should be made redundant by PathType
   * PATH_PREFIXED. */
  bool has_path () const { return !path.is_empty (); }

  std::string as_string () const override;

  void accept_vis (ASTVisitor &vis) override;

  PathType get_glob_type () { return glob_type; }

  Kind get_kind () const override { return Glob; }

  SimplePath get_path () const
  {
    rust_assert (has_path ());
    return path;
  }

  SimplePath &get_path () { return path; }

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
  SimplePath path;

  std::vector<std::unique_ptr<UseTree>> trees;

public:
  UseTreeList (PathType path_type, SimplePath path,
	       std::vector<std::unique_ptr<UseTree>> trees, location_t locus)
    : UseTree (locus), path_type (path_type), path (std::move (path)),
      trees (std::move (trees))
  {
    if (this->path_type != PATH_PREFIXED)
      {
	// compiler implementation error if there is a path with a
	// non-path-prefixed use tree glob
	rust_assert (!has_path ());
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

  PathType get_path_type () { return path_type; }

  void accept_vis (ASTVisitor &vis) override;

  Kind get_kind () const override { return List; }
  SimplePath get_path () const
  {
    rust_assert (has_path ());
    return path;
  }

  SimplePath &get_path () { return path; }

  std::vector<std::unique_ptr<UseTree>> &get_trees () { return trees; }

  const std::vector<std::unique_ptr<UseTree>> &get_trees () const
  {
    return trees;
  }

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
  SimplePath path;

  NewBindType bind_type;
  Identifier identifier; // only if NewBindType is IDENTIFIER

public:
  UseTreeRebind (NewBindType bind_type, SimplePath path, location_t locus,
		 Identifier identifier = std::string ())
    : UseTree (locus), path (std::move (path)), bind_type (bind_type),
      identifier (std::move (identifier))
  {}

  // Returns whether has path (this should always be true).
  bool has_path () const { return !path.is_empty (); }

  // Returns whether has identifier (or, rather, is allowed to).
  bool has_identifier () const { return bind_type == IDENTIFIER; }

  std::string as_string () const override;

  NewBindType get_new_bind_type () { return bind_type; }

  void accept_vis (ASTVisitor &vis) override;

  Kind get_kind () const override { return Rebind; }

  const SimplePath &get_path () const
  {
    rust_assert (has_path ());
    return path;
  }

  SimplePath &get_path () { return path; }

  const Identifier &get_identifier () const
  {
    rust_assert (has_identifier ());
    return identifier;
  }

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

// Rust use declaration (i.e. for modules) AST node
class UseDeclaration : public VisItem
{
  std::unique_ptr<UseTree> use_tree;
  location_t locus;

public:
  std::string as_string () const override;

  UseDeclaration (std::unique_ptr<UseTree> use_tree, Visibility visibility,
		  std::vector<Attribute> outer_attrs, location_t locus)
    : VisItem (std::move (visibility), std::move (outer_attrs)),
      use_tree (std::move (use_tree)), locus (locus)
  {}

  // Copy constructor with clone
  UseDeclaration (UseDeclaration const &other)
    : VisItem (other), locus (other.locus)
  {
    // guard to prevent null dereference (only required if error state)
    if (other.use_tree != nullptr)
      use_tree = other.use_tree->clone_use_tree ();
  }

  // Overloaded assignment operator to clone
  UseDeclaration &operator= (UseDeclaration const &other)
  {
    VisItem::operator= (other);
    // visibility = other.visibility->clone_visibility();
    // outer_attrs = other.outer_attrs;
    locus = other.locus;

    // guard to prevent null dereference (only required if error state)
    if (other.use_tree != nullptr)
      use_tree = other.use_tree->clone_use_tree ();
    else
      use_tree = nullptr;

    return *this;
  }

  // move constructors
  UseDeclaration (UseDeclaration &&other) = default;
  UseDeclaration &operator= (UseDeclaration &&other) = default;

  location_t get_locus () const override final { return locus; }

  std::unique_ptr<UseTree> &get_tree () { return use_tree; }

  const std::unique_ptr<UseTree> &get_tree () const { return use_tree; }

  void accept_vis (ASTVisitor &vis) override;

  // Invalid if use tree is null, so base stripping on that.
  void mark_for_strip () override { use_tree = nullptr; }
  bool is_marked_for_strip () const override { return use_tree == nullptr; }

protected:
  /* Use covariance to implement clone function as returning this object
   * rather than base */
  UseDeclaration *clone_item_impl () const override
  {
    return new UseDeclaration (*this);
  }
};

class LetStmt;

// Rust function declaration AST node
class Function : public VisItem, public AssociatedItem, public ExternalItem
{
  FunctionQualifiers qualifiers;
  Identifier function_name;
  std::vector<std::unique_ptr<GenericParam>> generic_params;
  std::vector<std::unique_ptr<Param>> function_params;
  std::unique_ptr<Type> return_type;
  WhereClause where_clause;
  tl::optional<std::unique_ptr<BlockExpr>> function_body;
  location_t locus;
  bool is_default;
  bool is_external_function;

public:
  std::string as_string () const override;

  // Returns whether function has generic parameters.
  bool has_generics () const { return !generic_params.empty (); }

  // Returns whether function has regular parameters.
  bool has_function_params () const { return !function_params.empty (); }

  // Returns whether function has return type - if not, it is void.
  bool has_return_type () const { return return_type != nullptr; }

  // Returns whether function has a where clause.
  bool has_where_clause () const { return !where_clause.is_empty (); }

  bool has_self_param () const
  {
    return function_params.size () > 0 && function_params[0]->is_self ();
  }

  bool has_body () const { return function_body.has_value (); }

  // Mega-constructor with all possible fields
  Function (Identifier function_name, FunctionQualifiers qualifiers,
	    std::vector<std::unique_ptr<GenericParam>> generic_params,
	    std::vector<std::unique_ptr<Param>> function_params,
	    std::unique_ptr<Type> return_type, WhereClause where_clause,
	    tl::optional<std::unique_ptr<BlockExpr>> function_body,
	    Visibility vis, std::vector<Attribute> outer_attrs,
	    location_t locus, bool is_default = false,
	    bool is_external_function = false)
    : VisItem (std::move (vis), std::move (outer_attrs)),
      ExternalItem (Stmt::node_id), qualifiers (std::move (qualifiers)),
      function_name (std::move (function_name)),
      generic_params (std::move (generic_params)),
      function_params (std::move (function_params)),
      return_type (std::move (return_type)),
      where_clause (std::move (where_clause)),
      function_body (std::move (function_body)), locus (locus),
      is_default (is_default), is_external_function (is_external_function)
  {}

  // TODO: add constructor with less fields

  // Copy constructor with clone
  Function (Function const &other);

  // Overloaded assignment operator to clone
  Function &operator= (Function const &other);

  // move constructors
  Function (Function &&other) = default;
  Function &operator= (Function &&other) = default;

  location_t get_locus () const override final { return locus; }

  void accept_vis (ASTVisitor &vis) override;

  bool is_variadic () const
  {
    return function_params.size () != 0
	   && function_params.back ()->is_variadic ();
  }

  bool is_external () const { return is_external_function; }

  // Invalid if block is null, so base stripping on that.
  void mark_for_strip () override { function_body = nullptr; }
  bool is_marked_for_strip () const override
  {
    return function_body == nullptr;
  }

  std::vector<std::unique_ptr<Param>> &get_function_params ()
  {
    return function_params;
  }
  const std::vector<std::unique_ptr<Param>> &get_function_params () const
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
  tl::optional<std::unique_ptr<BlockExpr>> &get_definition ()
  {
    return function_body;
  }

  const FunctionQualifiers &get_qualifiers () const { return qualifiers; }

  FunctionQualifiers &get_qualifiers () { return qualifiers; }

  Identifier get_function_name () const { return function_name; }

  // TODO: is this better? Or is a "vis_block" better?
  WhereClause &get_where_clause () { return where_clause; }

  // TODO: is this better? Or is a "vis_block" better?
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

  Param &get_self_param ()
  {
    rust_assert (has_self_param ());
    return *function_params[0];
  }
  const Param &get_self_param () const
  {
    rust_assert (has_self_param ());
    return *function_params[0];
  }

  // ExternalItem::node_id is same as Stmt::node_id
  NodeId get_node_id () const override { return Stmt::node_id; }

protected:
  /* Use covariance to implement clone function as returning this object
   * rather than base */
  Function *clone_item_impl () const override { return new Function (*this); }

  /* Use covariance to implement clone function as returning this object
   * rather than base */
  Function *clone_associated_item_impl () const override
  {
    return new Function (*this);
  }

  /* Use covariance to implement clone function as returning this object
   * rather than base */
  Function *clone_external_item_impl () const override
  {
    return new Function (*this);
  }
};

// Rust type alias (i.e. typedef) AST node
class TypeAlias : public VisItem, public AssociatedItem
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

  // Mega-constructor with all possible fields
  TypeAlias (Identifier new_type_name,
	     std::vector<std::unique_ptr<GenericParam>> generic_params,
	     WhereClause where_clause, std::unique_ptr<Type> existing_type,
	     Visibility vis, std::vector<Attribute> outer_attrs,
	     location_t locus)
    : VisItem (std::move (vis), std::move (outer_attrs)),
      new_type_name (std::move (new_type_name)),
      generic_params (std::move (generic_params)),
      where_clause (std::move (where_clause)),
      existing_type (std::move (existing_type)), locus (locus)
  {}

  // Copy constructor
  TypeAlias (TypeAlias const &other)
    : VisItem (other), new_type_name (other.new_type_name),
      where_clause (other.where_clause), locus (other.locus)
  {
    // guard to prevent null dereference (only required if error state)
    if (other.existing_type != nullptr)
      existing_type = other.existing_type->clone_type ();

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
    // visibility = other.visibility->clone_visibility();
    // outer_attrs = other.outer_attrs;
    locus = other.locus;

    // guard to prevent null dereference (only required if error state)
    if (other.existing_type != nullptr)
      existing_type = other.existing_type->clone_type ();
    else
      existing_type = nullptr;

    generic_params.reserve (other.generic_params.size ());
    for (const auto &e : other.generic_params)
      generic_params.push_back (e->clone_generic_param ());

    return *this;
  }

  // move constructors
  TypeAlias (TypeAlias &&other) = default;
  TypeAlias &operator= (TypeAlias &&other) = default;

  location_t get_locus () const override final { return locus; }

  void accept_vis (ASTVisitor &vis) override;

  // Invalid if existing type is null, so base stripping on that.
  void mark_for_strip () override { existing_type = nullptr; }
  bool is_marked_for_strip () const override
  {
    return existing_type == nullptr;
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
  WhereClause &get_where_clause () { return where_clause; }

  // TODO: is this better? Or is a "vis_block" better?
  Type &get_type_aliased ()
  {
    rust_assert (existing_type != nullptr);
    return *existing_type;
  }

  Identifier get_new_type_name () const { return new_type_name; }

protected:
  /* Use covariance to implement clone function as returning this object
   * rather than base */
  TypeAlias *clone_item_impl () const override { return new TypeAlias (*this); }

  /* Use covariance to implement clone function as returning this object
   * rather than base */
  TypeAlias *clone_associated_item_impl () const override
  {
    return new TypeAlias (*this);
  }
};

// Rust base struct declaration AST node - abstract base class
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

private:
  location_t locus;

public:
  // Returns whether struct has generic parameters.
  bool has_generics () const { return !generic_params.empty (); }

  // Returns whether struct has a where clause.
  bool has_where_clause () const { return !where_clause.is_empty (); }

  location_t get_locus () const override final { return locus; }

  // Invalid if name is empty, so base stripping on that.
  void mark_for_strip () override { struct_name = {""}; }
  bool is_marked_for_strip () const override { return struct_name.empty (); }

  Identifier get_struct_name () const { return struct_name; }

  std::vector<std::unique_ptr<GenericParam>> &get_generic_params ()
  {
    return generic_params;
  }
  const std::vector<std::unique_ptr<GenericParam>> &get_generic_params () const
  {
    return generic_params;
  }

  // TODO: is this better? Or is a "vis_block" better?
  WhereClause &get_where_clause () { return where_clause; }

  Identifier get_identifier () const { return struct_name; }

protected:
  Struct (Identifier struct_name,
	  std::vector<std::unique_ptr<GenericParam>> generic_params,
	  WhereClause where_clause, Visibility vis, location_t locus,
	  std::vector<Attribute> outer_attrs = std::vector<Attribute> ())
    : VisItem (std::move (vis), std::move (outer_attrs)),
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
class StructField
{
  // bool has_outer_attributes;
  std::vector<Attribute> outer_attrs;

  // bool has_visibility;
  Visibility visibility;

  Identifier field_name;
  std::unique_ptr<Type> field_type;

  NodeId node_id;

  location_t locus;

public:
  // Returns whether struct field has any outer attributes.
  bool has_outer_attributes () const { return !outer_attrs.empty (); }

  // Returns whether struct field has a non-private (non-default) visibility.
  bool has_visibility () const { return !visibility.is_error (); }

  StructField (Identifier field_name, std::unique_ptr<Type> field_type,
	       Visibility vis, location_t locus,
	       std::vector<Attribute> outer_attrs = std::vector<Attribute> ())
    : outer_attrs (std::move (outer_attrs)), visibility (std::move (vis)),
      field_name (std::move (field_name)), field_type (std::move (field_type)),
      node_id (Analysis::Mappings::get ()->get_next_node_id ()), locus (locus)
  {}

  // Copy constructor
  StructField (StructField const &other)
    : outer_attrs (other.outer_attrs), visibility (other.visibility),
      field_name (other.field_name), node_id (other.node_id),
      locus (other.locus)
  {
    // guard to prevent null dereference
    if (other.field_type != nullptr)
      field_type = other.field_type->clone_type ();
  }

  ~StructField () = default;

  // Overloaded assignment operator to clone
  StructField &operator= (StructField const &other)
  {
    field_name = other.field_name;
    visibility = other.visibility;
    outer_attrs = other.outer_attrs;
    node_id = other.node_id;

    // guard to prevent null dereference
    if (other.field_type != nullptr)
      field_type = other.field_type->clone_type ();
    else
      field_type = nullptr;

    return *this;
  }

  // move constructors
  StructField (StructField &&other) = default;
  StructField &operator= (StructField &&other) = default;

  // Returns whether struct field is in an error state.
  bool is_error () const
  {
    return field_name.empty () && field_type == nullptr;
    // this should really be an or since neither are allowed
  }

  // Creates an error state struct field.
  static StructField create_error ()
  {
    return StructField (std::string (""), nullptr, Visibility::create_error (),
			UNDEF_LOCATION);
  }

  std::string as_string () const;

  // TODO: this mutable getter seems really dodgy. Think up better way.
  std::vector<Attribute> &get_outer_attrs () { return outer_attrs; }
  const std::vector<Attribute> &get_outer_attrs () const { return outer_attrs; }

  Identifier get_field_name () const { return field_name; }

  location_t get_locus () const { return locus; }

  // TODO: is this better? Or is a "vis_block" better?
  Type &get_field_type ()
  {
    rust_assert (field_type != nullptr);
    return *field_type;
  }

  std::unique_ptr<Type> &get_field_type_ptr ()
  {
    rust_assert (field_type != nullptr);
    return field_type;
  }

  Visibility &get_visibility () { return visibility; }
  const Visibility &get_visibility () const { return visibility; }

  NodeId get_node_id () const { return node_id; }
};

// Rust struct declaration with true struct type AST node
class StructStruct : public Struct
{
  std::vector<StructField> fields;
  bool is_unit;

public:
  std::string as_string () const override;

  // Mega-constructor with all possible fields
  StructStruct (std::vector<StructField> fields, Identifier struct_name,
		std::vector<std::unique_ptr<GenericParam>> generic_params,
		WhereClause where_clause, bool is_unit, Visibility vis,
		std::vector<Attribute> outer_attrs, location_t locus)
    : Struct (std::move (struct_name), std::move (generic_params),
	      std::move (where_clause), std::move (vis), locus,
	      std::move (outer_attrs)),
      fields (std::move (fields)), is_unit (is_unit)
  {}

  // Unit struct constructor
  StructStruct (Identifier struct_name,
		std::vector<std::unique_ptr<GenericParam>> generic_params,
		WhereClause where_clause, Visibility vis,
		std::vector<Attribute> outer_attrs, location_t locus)
    : Struct (std::move (struct_name), std::move (generic_params),
	      std::move (where_clause), std::move (vis), locus,
	      std::move (outer_attrs)),
      is_unit (true)
  {}

  /* Returns whether the struct is a unit struct - struct defined without
   * fields. This is important because it also means an implicit constant of its
   * type is defined. */
  bool is_unit_struct () const { return is_unit; }

  void accept_vis (ASTVisitor &vis) override;

  // TODO: this mutable getter seems really dodgy. Think up better way.
  std::vector<StructField> &get_fields () { return fields; }
  const std::vector<StructField> &get_fields () const { return fields; }

protected:
  /* Use covariance to implement clone function as returning this object
   * rather than base */
  StructStruct *clone_item_impl () const override
  {
    return new StructStruct (*this);
  }
};

// A single field in a tuple
class TupleField
{
  // bool has_outer_attributes;
  std::vector<Attribute> outer_attrs;

  // bool has_visibility;
  Visibility visibility;

  std::unique_ptr<Type> field_type;

  NodeId node_id;

  location_t locus;

public:
  // Returns whether tuple field has outer attributes.
  bool has_outer_attributes () const { return !outer_attrs.empty (); }

  /* Returns whether tuple field has a non-default visibility (i.e. a public
   * one) */
  bool has_visibility () const { return !visibility.is_error (); }

  // Complete constructor
  TupleField (std::unique_ptr<Type> field_type, Visibility vis,
	      location_t locus,
	      std::vector<Attribute> outer_attrs = std::vector<Attribute> ())
    : outer_attrs (std::move (outer_attrs)), visibility (std::move (vis)),
      field_type (std::move (field_type)),
      node_id (Analysis::Mappings::get ()->get_next_node_id ()), locus (locus)
  {}

  // Copy constructor with clone
  TupleField (TupleField const &other)
    : outer_attrs (other.outer_attrs), visibility (other.visibility),
      node_id (other.node_id), locus (other.locus)
  {
    // guard to prevent null dereference (only required if error)
    if (other.field_type != nullptr)
      field_type = other.field_type->clone_type ();
  }

  ~TupleField () = default;

  // Overloaded assignment operator to clone
  TupleField &operator= (TupleField const &other)
  {
    visibility = other.visibility;
    outer_attrs = other.outer_attrs;
    node_id = other.node_id;
    locus = other.locus;

    // guard to prevent null dereference (only required if error)
    if (other.field_type != nullptr)
      field_type = other.field_type->clone_type ();
    else
      field_type = nullptr;

    return *this;
  }

  // move constructors
  TupleField (TupleField &&other) = default;
  TupleField &operator= (TupleField &&other) = default;

  // Returns whether tuple field is in an error state.
  bool is_error () const { return field_type == nullptr; }

  // Creates an error state tuple field.
  static TupleField create_error ()
  {
    return TupleField (nullptr, Visibility::create_error (), UNDEF_LOCATION);
  }

  std::string as_string () const;

  NodeId get_node_id () const { return node_id; }

  Visibility &get_visibility () { return visibility; }
  const Visibility &get_visibility () const { return visibility; }

  location_t get_locus () const { return locus; }

  // TODO: this mutable getter seems really dodgy. Think up better way.
  std::vector<Attribute> &get_outer_attrs () { return outer_attrs; }
  const std::vector<Attribute> &get_outer_attrs () const { return outer_attrs; }

  // TODO: is this better? Or is a "vis_block" better?
  Type &get_field_type ()
  {
    rust_assert (field_type != nullptr);
    return *field_type;
  }

  std::unique_ptr<Type> &get_field_type_ptr ()
  {
    rust_assert (field_type != nullptr);
    return field_type;
  }
};

// Rust tuple declared using struct keyword AST node
class TupleStruct : public Struct
{
  std::vector<TupleField> fields;

public:
  std::string as_string () const override;

  // Mega-constructor with all possible fields
  TupleStruct (std::vector<TupleField> fields, Identifier struct_name,
	       std::vector<std::unique_ptr<GenericParam>> generic_params,
	       WhereClause where_clause, Visibility vis,
	       std::vector<Attribute> outer_attrs, location_t locus)
    : Struct (std::move (struct_name), std::move (generic_params),
	      std::move (where_clause), std::move (vis), locus,
	      std::move (outer_attrs)),
      fields (std::move (fields))
  {}

  void accept_vis (ASTVisitor &vis) override;

  // TODO: this mutable getter seems really dodgy. Think up better way.
  std::vector<TupleField> &get_fields () { return fields; }
  const std::vector<TupleField> &get_fields () const { return fields; }

protected:
  /* Use covariance to implement clone function as returning this object
   * rather than base */
  TupleStruct *clone_item_impl () const override
  {
    return new TupleStruct (*this);
  }
};

/* An item used in an "enum" tagged union - not abstract: base represents a
 * name-only enum. EnumItems (variants) syntactically allow a Visibility
 * annotation. */
class EnumItem : public VisItem
{
  Identifier variant_name;

  location_t locus;

public:
  virtual ~EnumItem () {}

  EnumItem (Identifier variant_name, Visibility vis,
	    std::vector<Attribute> outer_attrs, location_t locus)
    : VisItem (std::move (vis), std::move (outer_attrs)),
      variant_name (std::move (variant_name)), locus (locus)
  {}

  // Unique pointer custom clone function
  std::unique_ptr<EnumItem> clone_enum_item () const
  {
    return std::unique_ptr<EnumItem> (clone_item_impl ());
  }

  virtual std::string as_string () const override;

  // not pure virtual as not abstract
  virtual void accept_vis (ASTVisitor &vis) override;

  location_t get_locus () const override { return locus; }

  Identifier get_identifier () const { return variant_name; }

  // Based on idea that name is never empty.
  void mark_for_strip () override { variant_name = {""}; }
  bool is_marked_for_strip () const override { return variant_name.empty (); }

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

  EnumItemTuple (Identifier variant_name, Visibility vis,
		 std::vector<TupleField> tuple_fields,
		 std::vector<Attribute> outer_attrs, location_t locus)
    : EnumItem (std::move (variant_name), std::move (vis),
		std::move (outer_attrs), locus),
      tuple_fields (std::move (tuple_fields))
  {}

  std::string as_string () const override;

  void accept_vis (ASTVisitor &vis) override;

  // TODO: this mutable getter seems really dodgy. Think up better way.
  std::vector<TupleField> &get_tuple_fields () { return tuple_fields; }
  const std::vector<TupleField> &get_tuple_fields () const
  {
    return tuple_fields;
  }

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

  EnumItemStruct (Identifier variant_name, Visibility vis,
		  std::vector<StructField> struct_fields,
		  std::vector<Attribute> outer_attrs, location_t locus)
    : EnumItem (std::move (variant_name), std::move (vis),
		std::move (outer_attrs), locus),
      struct_fields (std::move (struct_fields))
  {}

  std::string as_string () const override;

  void accept_vis (ASTVisitor &vis) override;

  // TODO: this mutable getter seems really dodgy. Think up better way.
  std::vector<StructField> &get_struct_fields () { return struct_fields; }
  const std::vector<StructField> &get_struct_fields () const
  {
    return struct_fields;
  }

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
  EnumItemDiscriminant (Identifier variant_name, Visibility vis,
			std::unique_ptr<Expr> expr,
			std::vector<Attribute> outer_attrs, location_t locus)
    : EnumItem (std::move (variant_name), std::move (vis),
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

  std::string as_string () const override;

  void accept_vis (ASTVisitor &vis) override;

  bool has_expr () { return expression != nullptr; }

  // TODO: is this better? Or is a "vis_block" better?
  Expr &get_expr ()
  {
    rust_assert (expression != nullptr);
    return *expression;
  }

  std::unique_ptr<Expr> &get_expr_ptr ()
  {
    rust_assert (expression != nullptr);
    return expression;
  }

protected:
  // Clone function implementation as (not pure) virtual method
  EnumItemDiscriminant *clone_item_impl () const override
  {
    return new EnumItemDiscriminant (*this);
  }
};

// AST node for Rust "enum" - tagged union
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
  Enum (Identifier enum_name, Visibility vis,
	std::vector<std::unique_ptr<GenericParam>> generic_params,
	WhereClause where_clause, std::vector<std::unique_ptr<EnumItem>> items,
	std::vector<Attribute> outer_attrs, location_t locus)
    : VisItem (std::move (vis), std::move (outer_attrs)),
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

  void accept_vis (ASTVisitor &vis) override;

  Identifier get_identifier () const { return enum_name; }

  // Invalid if name is empty, so base stripping on that.
  void mark_for_strip () override { enum_name = {""}; }
  bool is_marked_for_strip () const override { return enum_name.empty (); }

  // TODO: this mutable getter seems really dodgy. Think up better way.
  std::vector<std::unique_ptr<EnumItem>> &get_variants () { return items; }
  const std::vector<std::unique_ptr<EnumItem>> &get_variants () const
  {
    return items;
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
  WhereClause &get_where_clause () { return where_clause; }

protected:
  /* Use covariance to implement clone function as returning this object
   * rather than base */
  Enum *clone_item_impl () const override { return new Enum (*this); }
};

// Rust untagged union used for C compat AST node
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

  Union (Identifier union_name, Visibility vis,
	 std::vector<std::unique_ptr<GenericParam>> generic_params,
	 WhereClause where_clause, std::vector<StructField> variants,
	 std::vector<Attribute> outer_attrs, location_t locus)
    : VisItem (std::move (vis), std::move (outer_attrs)),
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

  location_t get_locus () const override final { return locus; }

  void accept_vis (ASTVisitor &vis) override;

  // Invalid if name is empty, so base stripping on that.
  void mark_for_strip () override { union_name = {""}; }
  bool is_marked_for_strip () const override { return union_name.empty (); }

  // TODO: this mutable getter seems really dodgy. Think up better way.
  std::vector<StructField> &get_variants () { return variants; }
  const std::vector<StructField> &get_variants () const { return variants; }

  std::vector<std::unique_ptr<GenericParam>> &get_generic_params ()
  {
    return generic_params;
  }
  const std::vector<std::unique_ptr<GenericParam>> &get_generic_params () const
  {
    return generic_params;
  }

  // TODO: is this better? Or is a "vis_block" better?
  WhereClause &get_where_clause () { return where_clause; }

  Identifier get_identifier () const { return union_name; }

protected:
  /* Use covariance to implement clone function as returning this object
   * rather than base */
  Union *clone_item_impl () const override { return new Union (*this); }
};

/* "Constant item" AST node - used for constant, compile-time expressions
 * within module scope (like constexpr) */
class ConstantItem : public VisItem, public AssociatedItem
{
  // either has an identifier or "_" - maybe handle in identifier?
  // bool identifier_is_underscore;
  // if no identifier declared, identifier will be "_"
  std::string identifier;

  std::unique_ptr<Type> type;
  std::unique_ptr<Expr> const_expr;

  location_t locus;

public:
  std::string as_string () const override;

  ConstantItem (std::string ident, Visibility vis, std::unique_ptr<Type> type,
		std::unique_ptr<Expr> const_expr,
		std::vector<Attribute> outer_attrs, location_t locus)
    : VisItem (std::move (vis), std::move (outer_attrs)),
      identifier (std::move (ident)), type (std::move (type)),
      const_expr (std::move (const_expr)), locus (locus)
  {}

  ConstantItem (std::string ident, Visibility vis, std::unique_ptr<Type> type,
		std::vector<Attribute> outer_attrs, location_t locus)
    : VisItem (std::move (vis), std::move (outer_attrs)),
      identifier (std::move (ident)), type (std::move (type)),
      const_expr (nullptr), locus (locus)
  {}

  ConstantItem (ConstantItem const &other)
    : VisItem (other), identifier (other.identifier), locus (other.locus)
  {
    // guard to prevent null dereference (only required if error state)
    if (other.type != nullptr)
      type = other.type->clone_type ();
    if (other.const_expr != nullptr)
      const_expr = other.const_expr->clone_expr ();
  }

  // Overload assignment operator to clone
  ConstantItem &operator= (ConstantItem const &other)
  {
    VisItem::operator= (other);
    identifier = other.identifier;
    locus = other.locus;

    // guard to prevent null dereference (only required if error state)
    if (other.type != nullptr)
      type = other.type->clone_type ();
    else
      type = nullptr;
    if (other.const_expr != nullptr)
      const_expr = other.const_expr->clone_expr ();
    else
      const_expr = nullptr;

    return *this;
  }

  // move constructors
  ConstantItem (ConstantItem &&other) = default;
  ConstantItem &operator= (ConstantItem &&other) = default;

  /* Returns whether constant item is an "unnamed" (wildcard underscore used
   * as identifier) constant. */
  bool is_unnamed () const { return identifier == "_"; }

  location_t get_locus () const override final { return locus; }

  void accept_vis (ASTVisitor &vis) override;

  // Invalid if type or expression are null, so base stripping on that.
  void mark_for_strip () override
  {
    type = nullptr;
    const_expr = nullptr;
  }
  bool is_marked_for_strip () const override
  {
    return type == nullptr && const_expr == nullptr;
  }

  bool has_expr () { return const_expr != nullptr; }

  // TODO: is this better? Or is a "vis_block" better?
  Expr &get_expr ()
  {
    rust_assert (const_expr != nullptr);
    return *const_expr;
  }

  std::unique_ptr<Expr> &get_expr_ptr ()
  {
    rust_assert (const_expr != nullptr);
    return const_expr;
  }

  // TODO: is this better? Or is a "vis_block" better?
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

  std::string get_identifier () const { return identifier; }

protected:
  /* Use covariance to implement clone function as returning this object
   * rather than base */
  ConstantItem *clone_item_impl () const override
  {
    return new ConstantItem (*this);
  }

  /* Use covariance to implement clone function as returning this object
   * rather than base */
  ConstantItem *clone_associated_item_impl () const override
  {
    return new ConstantItem (*this);
  }
};

/* Static item AST node - items within module scope with fixed storage
 * duration? */
class StaticItem : public VisItem
{
  bool has_mut;
  Identifier name;
  std::unique_ptr<Type> type;
  std::unique_ptr<Expr> expr;
  location_t locus;

public:
  std::string as_string () const override;

  StaticItem (Identifier name, bool is_mut, std::unique_ptr<Type> type,
	      std::unique_ptr<Expr> expr, Visibility vis,
	      std::vector<Attribute> outer_attrs, location_t locus)
    : VisItem (std::move (vis), std::move (outer_attrs)), has_mut (is_mut),
      name (std::move (name)), type (std::move (type)), expr (std::move (expr)),
      locus (locus)
  {}

  // Copy constructor with clone
  StaticItem (StaticItem const &other)
    : VisItem (other), has_mut (other.has_mut), name (other.name),
      locus (other.locus)
  {
    // guard to prevent null dereference (only required if error state)
    if (other.type != nullptr)
      type = other.type->clone_type ();
    if (other.expr != nullptr)
      expr = other.expr->clone_expr ();
  }

  // Overloaded assignment operator to clone
  StaticItem &operator= (StaticItem const &other)
  {
    VisItem::operator= (other);
    name = other.name;
    has_mut = other.has_mut;
    locus = other.locus;

    // guard to prevent null dereference (only required if error state)
    if (other.type != nullptr)
      type = other.type->clone_type ();
    else
      type = nullptr;
    if (other.expr != nullptr)
      expr = other.expr->clone_expr ();
    else
      expr = nullptr;

    return *this;
  }

  // move constructors
  StaticItem (StaticItem &&other) = default;
  StaticItem &operator= (StaticItem &&other) = default;

  location_t get_locus () const override final { return locus; }

  void accept_vis (ASTVisitor &vis) override;

  // Invalid if type or expression are null, so base stripping on that.
  void mark_for_strip () override
  {
    type = nullptr;
    expr = nullptr;
  }
  bool is_marked_for_strip () const override
  {
    return type == nullptr && expr == nullptr;
  }

  bool has_expr () { return expr != nullptr; }

  // TODO: is this better? Or is a "vis_block" better?
  Expr &get_expr ()
  {
    rust_assert (expr != nullptr);
    return *expr;
  }

  std::unique_ptr<Expr> &get_expr_ptr ()
  {
    rust_assert (expr != nullptr);
    return expr;
  }

  // TODO: is this better? Or is a "vis_block" better?
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

  bool is_mutable () const { return has_mut; }

  Identifier get_identifier () const { return name; }

protected:
  /* Use covariance to implement clone function as returning this object
   * rather than base */
  StaticItem *clone_item_impl () const override
  {
    return new StaticItem (*this);
  }
};

// Constant item within traits
class TraitItemConst : public TraitItem
{
  std::vector<Attribute> outer_attrs;
  Identifier name;
  std::unique_ptr<Type> type;

  // bool has_expression;
  std::unique_ptr<Expr> expr;

public:
  // Whether the constant item has an associated expression.
  bool has_expression () const { return expr != nullptr; }

  TraitItemConst (Identifier name, std::unique_ptr<Type> type,
		  std::unique_ptr<Expr> expr,
		  std::vector<Attribute> outer_attrs, location_t locus)
    : TraitItem (locus), outer_attrs (std::move (outer_attrs)),
      name (std::move (name)), type (std::move (type)), expr (std::move (expr))
  {}

  // Copy constructor with clones
  TraitItemConst (TraitItemConst const &other)
    : TraitItem (other.locus), outer_attrs (other.outer_attrs),
      name (other.name)
  {
    node_id = other.node_id;

    // guard to prevent null dereference
    if (other.expr != nullptr)
      expr = other.expr->clone_expr ();

    // guard to prevent null dereference (only for error state)
    if (other.type != nullptr)
      type = other.type->clone_type ();
  }

  // Overloaded assignment operator to clone
  TraitItemConst &operator= (TraitItemConst const &other)
  {
    TraitItem::operator= (other);
    outer_attrs = other.outer_attrs;
    name = other.name;
    locus = other.locus;
    node_id = other.node_id;

    // guard to prevent null dereference
    if (other.expr != nullptr)
      expr = other.expr->clone_expr ();
    else
      expr = nullptr;

    // guard to prevent null dereference (only for error state)
    if (other.type != nullptr)
      type = other.type->clone_type ();
    else
      type = nullptr;

    return *this;
  }

  // move constructors
  TraitItemConst (TraitItemConst &&other) = default;
  TraitItemConst &operator= (TraitItemConst &&other) = default;

  std::string as_string () const override;

  location_t get_locus () const override { return locus; }

  void accept_vis (ASTVisitor &vis) override;

  // Invalid if type is null, so base stripping on that.
  void mark_for_strip () override { type = nullptr; }
  bool is_marked_for_strip () const override { return type == nullptr; }

  // TODO: this mutable getter seems really dodgy. Think up better way.
  std::vector<Attribute> &get_outer_attrs () { return outer_attrs; }
  const std::vector<Attribute> &get_outer_attrs () const { return outer_attrs; }

  bool has_expr () const { return expr != nullptr; }

  // TODO: is this better? Or is a "vis_block" better?
  Expr &get_expr ()
  {
    rust_assert (has_expr ());
    return *expr;
  }

  std::unique_ptr<Expr> &get_expr_ptr ()
  {
    rust_assert (has_expr ());
    return expr;
  }

  // TODO: is this better? Or is a "vis_block" better?
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

  Identifier get_identifier () const { return name; }

protected:
  // Clone function implementation as (not pure) virtual method
  TraitItemConst *clone_associated_item_impl () const override
  {
    return new TraitItemConst (*this);
  }
};

// Type items within traits
class TraitItemType : public TraitItem
{
  std::vector<Attribute> outer_attrs;

  Identifier name;

  // bool has_type_param_bounds;
  // TypeParamBounds type_param_bounds;
  std::vector<std::unique_ptr<TypeParamBound>>
    type_param_bounds; // inlined form

public:
  // Returns whether trait item type has type param bounds.
  bool has_type_param_bounds () const { return !type_param_bounds.empty (); }

  TraitItemType (Identifier name,
		 std::vector<std::unique_ptr<TypeParamBound>> type_param_bounds,
		 std::vector<Attribute> outer_attrs, Visibility vis,
		 location_t locus)
    : TraitItem (vis, locus), outer_attrs (std::move (outer_attrs)),
      name (std::move (name)), type_param_bounds (std::move (type_param_bounds))
  {}

  // Copy constructor with vector clone
  TraitItemType (TraitItemType const &other)
    : TraitItem (other.locus), outer_attrs (other.outer_attrs),
      name (other.name)
  {
    node_id = other.node_id;
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
    node_id = other.node_id;

    type_param_bounds.reserve (other.type_param_bounds.size ());
    for (const auto &e : other.type_param_bounds)
      type_param_bounds.push_back (e->clone_type_param_bound ());

    return *this;
  }

  // default move constructors
  TraitItemType (TraitItemType &&other) = default;
  TraitItemType &operator= (TraitItemType &&other) = default;

  std::string as_string () const override;

  void accept_vis (ASTVisitor &vis) override;

  // Invalid if name is empty, so base stripping on that.
  void mark_for_strip () override { name = {""}; }
  bool is_marked_for_strip () const override { return name.empty (); }

  // TODO: this mutable getter seems really dodgy. Think up better way.
  std::vector<Attribute> &get_outer_attrs () { return outer_attrs; }
  const std::vector<Attribute> &get_outer_attrs () const { return outer_attrs; }

  // TODO: mutable getter seems kinda dodgy
  std::vector<std::unique_ptr<TypeParamBound>> &get_type_param_bounds ()
  {
    return type_param_bounds;
  }
  const std::vector<std::unique_ptr<TypeParamBound>> &
  get_type_param_bounds () const
  {
    return type_param_bounds;
  }

  Identifier get_identifier () const { return name; }

protected:
  // Clone function implementation as (not pure) virtual method
  TraitItemType *clone_associated_item_impl () const override
  {
    return new TraitItemType (*this);
  }
};

// Rust trait item declaration AST node
class Trait : public VisItem
{
  bool has_unsafe;
  bool has_auto;
  Identifier name;
  std::vector<std::unique_ptr<GenericParam>> generic_params;
  std::vector<std::unique_ptr<TypeParamBound>> type_param_bounds;
  WhereClause where_clause;
  std::vector<Attribute> inner_attrs;
  std::vector<std::unique_ptr<AssociatedItem>> trait_items;
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

  // Returns whether trait has inner attributes.
  bool has_inner_attrs () const { return !inner_attrs.empty (); }

  Identifier get_identifier () const { return name; }

  bool is_unsafe () const { return has_unsafe; }
  bool is_auto () const { return has_auto; }

  // Mega-constructor
  Trait (Identifier name, bool is_unsafe, bool is_auto,
	 std::vector<std::unique_ptr<GenericParam>> generic_params,
	 std::vector<std::unique_ptr<TypeParamBound>> type_param_bounds,
	 WhereClause where_clause,
	 std::vector<std::unique_ptr<AssociatedItem>> trait_items,
	 Visibility vis, std::vector<Attribute> outer_attrs,
	 std::vector<Attribute> inner_attrs, location_t locus)
    : VisItem (std::move (vis), std::move (outer_attrs)),
      has_unsafe (is_unsafe), has_auto (is_auto), name (std::move (name)),
      generic_params (std::move (generic_params)),
      type_param_bounds (std::move (type_param_bounds)),
      where_clause (std::move (where_clause)),
      inner_attrs (std::move (inner_attrs)),
      trait_items (std::move (trait_items)), locus (locus)
  {}

  // Copy constructor with vector clone
  Trait (Trait const &other)
    : VisItem (other), has_unsafe (other.has_unsafe), has_auto (other.has_auto),
      name (other.name), where_clause (other.where_clause),
      inner_attrs (other.inner_attrs), locus (other.locus)
  {
    generic_params.reserve (other.generic_params.size ());
    for (const auto &e : other.generic_params)
      generic_params.push_back (e->clone_generic_param ());

    type_param_bounds.reserve (other.type_param_bounds.size ());
    for (const auto &e : other.type_param_bounds)
      type_param_bounds.push_back (e->clone_type_param_bound ());

    trait_items.reserve (other.trait_items.size ());
    for (const auto &e : other.trait_items)
      trait_items.push_back (e->clone_associated_item ());
  }

  // Overloaded assignment operator with vector clone
  Trait &operator= (Trait const &other)
  {
    VisItem::operator= (other);
    name = other.name;
    has_unsafe = other.has_unsafe;
    has_auto = other.has_auto;
    where_clause = other.where_clause;
    inner_attrs = other.inner_attrs;
    locus = other.locus;

    generic_params.reserve (other.generic_params.size ());
    for (const auto &e : other.generic_params)
      generic_params.push_back (e->clone_generic_param ());

    type_param_bounds.reserve (other.type_param_bounds.size ());
    for (const auto &e : other.type_param_bounds)
      type_param_bounds.push_back (e->clone_type_param_bound ());

    trait_items.reserve (other.trait_items.size ());
    for (const auto &e : other.trait_items)
      trait_items.push_back (e->clone_associated_item ());

    return *this;
  }

  // default move constructors
  Trait (Trait &&other) = default;
  Trait &operator= (Trait &&other) = default;

  location_t get_locus () const override final { return locus; }

  void accept_vis (ASTVisitor &vis) override;

  // Invalid if trait name is empty, so base stripping on that.
  void mark_for_strip () override { name = {""}; }
  bool is_marked_for_strip () const override { return name.empty (); }

  // TODO: think of better way to do this
  const std::vector<Attribute> &get_inner_attrs () const { return inner_attrs; }
  std::vector<Attribute> &get_inner_attrs () { return inner_attrs; }

  const std::vector<std::unique_ptr<AssociatedItem>> &get_trait_items () const
  {
    return trait_items;
  }
  std::vector<std::unique_ptr<AssociatedItem>> &get_trait_items ()
  {
    return trait_items;
  }

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

  WhereClause &get_where_clause () { return where_clause; }

  void insert_implict_self (std::unique_ptr<AST::GenericParam> &&param)
  {
    std::vector<std::unique_ptr<GenericParam>> new_list;
    new_list.reserve (generic_params.size () + 1);

    new_list.push_back (std::move (param));
    for (auto &p : generic_params)
      {
	new_list.push_back (std::move (p));
      }

    generic_params = std::move (new_list);
  }

protected:
  /* Use covariance to implement clone function as returning this object
   * rather than base */
  Trait *clone_item_impl () const override { return new Trait (*this); }
};

// Implementation item declaration AST node - abstract base class
class Impl : public VisItem
{
  // must be protected to allow subclasses to access them properly
protected:
  // bool has_generics;
  // Generics generic_params;
  std::vector<std::unique_ptr<GenericParam>> generic_params; // inlined

  std::unique_ptr<Type> trait_type;

  // bool has_where_clause;
  WhereClause where_clause;

  // bool has_inner_attrs;
  std::vector<Attribute> inner_attrs;

private:
  // doesn't really need to be protected as write access probably not needed
  location_t locus;

public:
  // Returns whether impl has generic parameters.
  bool has_generics () const { return !generic_params.empty (); }

  // Returns whether impl has where clause.
  bool has_where_clause () const { return !where_clause.is_empty (); }

  // Returns whether impl has inner attributes.
  bool has_inner_attrs () const { return !inner_attrs.empty (); }

  location_t get_locus () const override final { return locus; }

  // Invalid if trait type is null, so base stripping on that.
  void mark_for_strip () override { trait_type = nullptr; }
  bool is_marked_for_strip () const override { return trait_type == nullptr; }

  // TODO: think of better way to do this
  const std::vector<Attribute> &get_inner_attrs () const { return inner_attrs; }
  std::vector<Attribute> &get_inner_attrs () { return inner_attrs; }

  std::vector<std::unique_ptr<GenericParam>> &get_generic_params ()
  {
    return generic_params;
  }
  const std::vector<std::unique_ptr<GenericParam>> &get_generic_params () const
  {
    return generic_params;
  }

  // TODO: is this better? Or is a "vis_block" better?
  WhereClause &get_where_clause () { return where_clause; }

  // TODO: is this better? Or is a "vis_block" better?
  Type &get_type ()
  {
    rust_assert (trait_type != nullptr);
    return *trait_type;
  }

  std::unique_ptr<Type> &get_type_ptr ()
  {
    rust_assert (trait_type != nullptr);
    return trait_type;
  }

protected:
  // Mega-constructor
  Impl (std::vector<std::unique_ptr<GenericParam>> generic_params,
	std::unique_ptr<Type> trait_type, WhereClause where_clause,
	Visibility vis, std::vector<Attribute> inner_attrs,
	std::vector<Attribute> outer_attrs, location_t locus)
    : VisItem (std::move (vis), std::move (outer_attrs)),
      generic_params (std::move (generic_params)),
      trait_type (std::move (trait_type)),
      where_clause (std::move (where_clause)),
      inner_attrs (std::move (inner_attrs)), locus (locus)
  {}

  // Copy constructor
  Impl (Impl const &other)
    : VisItem (other), where_clause (other.where_clause),
      inner_attrs (other.inner_attrs), locus (other.locus)
  {
    // guard to prevent null dereference (only required if error state)
    if (other.trait_type != nullptr)
      trait_type = other.trait_type->clone_type ();

    generic_params.reserve (other.generic_params.size ());
    for (const auto &e : other.generic_params)
      generic_params.push_back (e->clone_generic_param ());
  }

  // Assignment operator overload with cloning
  Impl &operator= (Impl const &other)
  {
    VisItem::operator= (other);
    where_clause = other.where_clause;
    inner_attrs = other.inner_attrs;
    locus = other.locus;

    // guard to prevent null dereference (only required if error state)
    if (other.trait_type != nullptr)
      trait_type = other.trait_type->clone_type ();
    else
      trait_type = nullptr;

    generic_params.reserve (other.generic_params.size ());
    for (const auto &e : other.generic_params)
      generic_params.push_back (e->clone_generic_param ());

    return *this;
  }

  // move constructors
  Impl (Impl &&other) = default;
  Impl &operator= (Impl &&other) = default;
};

// Regular "impl foo" impl block declaration AST node
class InherentImpl : public Impl
{
  // bool has_impl_items;
  std::vector<std::unique_ptr<AssociatedItem>> impl_items;

public:
  std::string as_string () const override;

  // Returns whether inherent impl block has inherent impl items.
  bool has_impl_items () const { return !impl_items.empty (); }

  // Mega-constructor
  InherentImpl (std::vector<std::unique_ptr<AssociatedItem>> impl_items,
		std::vector<std::unique_ptr<GenericParam>> generic_params,
		std::unique_ptr<Type> trait_type, WhereClause where_clause,
		Visibility vis, std::vector<Attribute> inner_attrs,
		std::vector<Attribute> outer_attrs, location_t locus)
    : Impl (std::move (generic_params), std::move (trait_type),
	    std::move (where_clause), std::move (vis), std::move (inner_attrs),
	    std::move (outer_attrs), locus),
      impl_items (std::move (impl_items))
  {}

  // Copy constructor with vector clone
  InherentImpl (InherentImpl const &other) : Impl (other)
  {
    impl_items.reserve (other.impl_items.size ());
    for (const auto &e : other.impl_items)
      impl_items.push_back (e->clone_associated_item ());
  }

  // Overloaded assignment operator with vector clone
  InherentImpl &operator= (InherentImpl const &other)
  {
    Impl::operator= (other);

    impl_items.reserve (other.impl_items.size ());
    for (const auto &e : other.impl_items)
      impl_items.push_back (e->clone_associated_item ());

    return *this;
  }

  // default move constructors
  InherentImpl (InherentImpl &&other) = default;
  InherentImpl &operator= (InherentImpl &&other) = default;

  void accept_vis (ASTVisitor &vis) override;

  // TODO: think of better way to do this
  const std::vector<std::unique_ptr<AssociatedItem>> &get_impl_items () const
  {
    return impl_items;
  }
  std::vector<std::unique_ptr<AssociatedItem>> &get_impl_items ()
  {
    return impl_items;
  }

protected:
  /* Use covariance to implement clone function as returning this object
   * rather than base */
  InherentImpl *clone_item_impl () const override
  {
    return new InherentImpl (*this);
  }
};

// The "impl footrait for foo" impl block declaration AST node
class TraitImpl : public Impl
{
  bool has_unsafe;
  bool has_exclam;
  TypePath trait_path;

  // bool has_impl_items;
  std::vector<std::unique_ptr<AssociatedItem>> impl_items;

public:
  std::string as_string () const override;

  // Returns whether trait impl has impl items.
  bool has_impl_items () const { return !impl_items.empty (); }

  // Mega-constructor
  TraitImpl (TypePath trait_path, bool is_unsafe, bool has_exclam,
	     std::vector<std::unique_ptr<AssociatedItem>> impl_items,
	     std::vector<std::unique_ptr<GenericParam>> generic_params,
	     std::unique_ptr<Type> trait_type, WhereClause where_clause,
	     Visibility vis, std::vector<Attribute> inner_attrs,
	     std::vector<Attribute> outer_attrs, location_t locus)
    : Impl (std::move (generic_params), std::move (trait_type),
	    std::move (where_clause), std::move (vis), std::move (inner_attrs),
	    std::move (outer_attrs), locus),
      has_unsafe (is_unsafe), has_exclam (has_exclam),
      trait_path (std::move (trait_path)), impl_items (std::move (impl_items))
  {}

  // Copy constructor with vector clone
  TraitImpl (TraitImpl const &other)
    : Impl (other), has_unsafe (other.has_unsafe),
      has_exclam (other.has_exclam), trait_path (other.trait_path)
  {
    impl_items.reserve (other.impl_items.size ());
    for (const auto &e : other.impl_items)
      impl_items.push_back (e->clone_associated_item ());
  }

  // Overloaded assignment operator with vector clone
  TraitImpl &operator= (TraitImpl const &other)
  {
    Impl::operator= (other);
    trait_path = other.trait_path;
    has_unsafe = other.has_unsafe;
    has_exclam = other.has_exclam;

    impl_items.reserve (other.impl_items.size ());
    for (const auto &e : other.impl_items)
      impl_items.push_back (e->clone_associated_item ());

    return *this;
  }

  // move constructors
  TraitImpl (TraitImpl &&other) = default;
  TraitImpl &operator= (TraitImpl &&other) = default;

  void accept_vis (ASTVisitor &vis) override;

  bool is_unsafe () const { return has_unsafe; };
  bool is_exclam () const { return has_exclam; }

  // TODO: think of better way to do this
  const std::vector<std::unique_ptr<AssociatedItem>> &get_impl_items () const
  {
    return impl_items;
  }
  std::vector<std::unique_ptr<AssociatedItem>> &get_impl_items ()
  {
    return impl_items;
  }

  // TODO: is this better? Or is a "vis_block" better?
  TypePath &get_trait_path ()
  {
    // TODO: assert that trait path is not empty?
    return trait_path;
  }

protected:
  /* Use covariance to implement clone function as returning this object
   * rather than base */
  TraitImpl *clone_item_impl () const override { return new TraitImpl (*this); }
};

#if 0
// Abstract base class for an item used inside an extern block
class ExternalItem
{
  // bool has_outer_attrs;
  std::vector<Attribute> outer_attrs;

  // bool has_visibility;
  Visibility visibility;

  Identifier item_name;
  location_t locus;

public:
  virtual ~ExternalItem () {}

  /* TODO: spec syntax rules state that "MacroInvocationSemi" can be used as
   * ExternalItem, but text body isn't so clear. Adding MacroInvocationSemi
   * support would require a lot of refactoring. */

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

  location_t get_locus () const override final { return locus; }

  virtual void accept_vis (ASTVisitor &vis) = 0;

  // TODO: make virtual? Would be more flexible.
  // Based on idea that name should never be empty.
  void mark_for_strip () { item_name = ""; };
  bool is_marked_for_strip () const { return item_name.empty (); };

protected:
  ExternalItem (Identifier item_name, Visibility vis,
		std::vector<Attribute> outer_attrs, location_t locus)
    : outer_attrs (std::move (outer_attrs)), visibility (std::move (vis)),
      item_name (std::move (item_name)), locus (locus)
  {}

  // Copy constructor
  ExternalItem (ExternalItem const &other)
    : outer_attrs (other.outer_attrs), visibility (other.visibility),
      item_name (other.item_name), locus (other.locus)
  {}

  // Overloaded assignment operator to clone
  ExternalItem &operator= (ExternalItem const &other)
  {
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

  // possibly make this public if required
  std::string get_item_name () const { return item_name; }
};
#endif

// A foreign type defined outside the current crate.
// https://rust-lang.github.io/rfcs/1861-extern-types.html
class ExternalTypeItem : public ExternalItem
{
  std::vector<Attribute> outer_attrs;

  Visibility visibility;
  Identifier item_name;
  location_t locus;

  bool marked_for_strip;

public:
  ExternalTypeItem (Identifier item_name, Visibility vis,
		    std::vector<Attribute> outer_attrs, location_t locus)
    : ExternalItem (), outer_attrs (std::move (outer_attrs)), visibility (vis),
      item_name (std::move (item_name)), locus (locus), marked_for_strip (false)
  {}

  // copy constructor
  ExternalTypeItem (ExternalTypeItem const &other)
    : ExternalItem (other.get_node_id ()), outer_attrs (other.outer_attrs),
      visibility (other.visibility), item_name (other.item_name),
      locus (other.locus), marked_for_strip (other.marked_for_strip)
  {
    node_id = other.node_id;
  }

  ExternalTypeItem &operator= (ExternalTypeItem const &other)
  {
    node_id = other.node_id;
    outer_attrs = other.outer_attrs;
    visibility = other.visibility;
    item_name = other.item_name;
    locus = other.locus;
    marked_for_strip = other.marked_for_strip;

    return *this;
  }

  // move constructors
  ExternalTypeItem (ExternalTypeItem &&other) = default;
  ExternalTypeItem &operator= (ExternalTypeItem &&other) = default;

  std::string as_string () const override;

  void accept_vis (ASTVisitor &vis) override;

  // Returns whether item has outer attributes.
  bool has_outer_attrs () const { return !outer_attrs.empty (); }

  // Returns whether item has non-default visibility.
  bool has_visibility () const { return !visibility.is_error (); }

  location_t get_locus () const { return locus; }

  void mark_for_strip () override { marked_for_strip = true; };
  bool is_marked_for_strip () const override { return marked_for_strip; };

  // TODO: this mutable getter seems really dodgy. Think up better way.
  std::vector<Attribute> &get_outer_attrs () { return outer_attrs; }
  const std::vector<Attribute> &get_outer_attrs () const { return outer_attrs; }

  Identifier get_identifier () const { return item_name; }

  Visibility &get_visibility () { return visibility; }
  const Visibility &get_visibility () const { return visibility; }

protected:
  /* Use covariance to implement clone function as returning this object
   * rather than base */
  ExternalTypeItem *clone_external_item_impl () const override
  {
    return new ExternalTypeItem (*this);
  }
};

// A static item used in an extern block
class ExternalStaticItem : public ExternalItem
{
  // bool has_outer_attrs;
  std::vector<Attribute> outer_attrs;

  // bool has_visibility;
  Visibility visibility;

  Identifier item_name;
  location_t locus;

  bool has_mut;
  std::unique_ptr<Type> item_type;

public:
  ExternalStaticItem (Identifier item_name, std::unique_ptr<Type> item_type,
		      bool is_mut, Visibility vis,
		      std::vector<Attribute> outer_attrs, location_t locus)
    : ExternalItem (), outer_attrs (std::move (outer_attrs)),
      visibility (std::move (vis)), item_name (std::move (item_name)),
      locus (locus), has_mut (is_mut), item_type (std::move (item_type))
  {}

  // Copy constructor
  ExternalStaticItem (ExternalStaticItem const &other)
    : ExternalItem (other.get_node_id ()), outer_attrs (other.outer_attrs),
      visibility (other.visibility), item_name (other.item_name),
      locus (other.locus), has_mut (other.has_mut)
  {
    node_id = other.node_id;
    // guard to prevent null dereference (only required if error state)
    if (other.item_type != nullptr)
      item_type = other.item_type->clone_type ();
  }

  // Overloaded assignment operator to clone
  ExternalStaticItem &operator= (ExternalStaticItem const &other)
  {
    node_id = other.node_id;
    outer_attrs = other.outer_attrs;
    visibility = other.visibility;
    item_name = other.item_name;
    locus = other.locus;
    has_mut = other.has_mut;

    // guard to prevent null dereference (only required if error state)
    if (other.item_type != nullptr)
      item_type = other.item_type->clone_type ();
    else
      item_type = nullptr;

    return *this;
  }

  // move constructors
  ExternalStaticItem (ExternalStaticItem &&other) = default;
  ExternalStaticItem &operator= (ExternalStaticItem &&other) = default;

  std::string as_string () const override;

  void accept_vis (ASTVisitor &vis) override;

  // Returns whether item has outer attributes.
  bool has_outer_attrs () const { return !outer_attrs.empty (); }

  // Returns whether item has non-default visibility.
  bool has_visibility () const { return !visibility.is_error (); }

  location_t get_locus () const { return locus; }

  // Based on idea that type should never be null.
  void mark_for_strip () override { item_type = nullptr; };
  bool is_marked_for_strip () const override { return item_type == nullptr; };

  // TODO: this mutable getter seems really dodgy. Think up better way.
  std::vector<Attribute> &get_outer_attrs () { return outer_attrs; }
  const std::vector<Attribute> &get_outer_attrs () const { return outer_attrs; }

  // TODO: is this better? Or is a "vis_block" better?
  Type &get_type ()
  {
    rust_assert (item_type != nullptr);
    return *item_type;
  }

  std::unique_ptr<Type> &get_type_ptr ()
  {
    rust_assert (item_type != nullptr);
    return item_type;
  }

  Identifier get_identifier () const { return item_name; }

  Visibility &get_visibility () { return visibility; }

  const Visibility &get_visibility () const { return visibility; }

  bool is_mut () const { return has_mut; }

protected:
  /* Use covariance to implement clone function as returning this object
   * rather than base */
  ExternalStaticItem *clone_external_item_impl () const override
  {
    return new ExternalStaticItem (*this);
  }
};

// A named function parameter used in external functions
class NamedFunctionParam
{
  // bool has_name;   // otherwise is _
  std::string name;

  std::unique_ptr<Type> param_type;

  // seemingly new since writing this node
  std::vector<Attribute> outer_attrs;

  NodeId node_id;
  location_t locus;
  bool variadic;

public:
  /* Returns whether the named function parameter has a name (i.e. name is not
   * '_'). */
  bool has_name () const { return name != "_" && name != ""; }

  bool has_outer_attrs () const { return !outer_attrs.empty (); }

  // Returns whether the named function parameter is in an error state.
  bool is_error () const
  {
    // also if identifier is "" but that is probably more costly to compute
    return param_type == nullptr && !variadic;
  }

  bool is_variadic () const { return variadic; }

  std::string get_name () const { return name; }

  location_t get_locus () { return locus; }

  // Creates an error state named function parameter.
  static NamedFunctionParam create_error ()
  {
    return NamedFunctionParam ("", nullptr, {}, UNDEF_LOCATION);
  }

  NamedFunctionParam (std::string name, std::unique_ptr<Type> param_type,
		      std::vector<Attribute> outer_attrs, location_t locus)
    : name (std::move (name)), param_type (std::move (param_type)),
      outer_attrs (std::move (outer_attrs)),
      node_id (Analysis::Mappings::get ()->get_next_node_id ()), locus (locus),
      variadic (false)
  {}

  NamedFunctionParam (std::string name, std::vector<Attribute> outer_attrs,
		      location_t locus)
    : name (std::move (name)), param_type (nullptr),
      outer_attrs (std::move (outer_attrs)),
      node_id (Analysis::Mappings::get ()->get_next_node_id ()), locus (locus),
      variadic (true)
  {}

  NamedFunctionParam (std::vector<Attribute> outer_attrs, location_t locus)
    : name (""), param_type (nullptr), outer_attrs (std::move (outer_attrs)),
      node_id (Analysis::Mappings::get ()->get_next_node_id ()), locus (locus),
      variadic (true)
  {}

  // Copy constructor
  NamedFunctionParam (NamedFunctionParam const &other)
    : name (other.name), outer_attrs (other.outer_attrs),
      variadic (other.variadic)
  {
    node_id = other.node_id;
    // guard to prevent null dereference (only required if error state)
    if (other.param_type != nullptr)
      param_type = other.param_type->clone_type ();
    else
      param_type = nullptr;
  }

  ~NamedFunctionParam () = default;

  // Overloaded assignment operator to clone
  NamedFunctionParam &operator= (NamedFunctionParam const &other)
  {
    node_id = other.node_id;
    name = other.name;
    // has_name = other.has_name;
    outer_attrs = other.outer_attrs;

    // guard to prevent null dereference (only required if error state)
    if (other.param_type != nullptr)
      param_type = other.param_type->clone_type ();
    else
      param_type = nullptr;

    return *this;
  }

  // move constructors
  NamedFunctionParam (NamedFunctionParam &&other) = default;
  NamedFunctionParam &operator= (NamedFunctionParam &&other) = default;

  std::string as_string () const;

  // Based on idea that nane should never be empty.
  void mark_for_strip () { param_type = nullptr; };
  bool is_marked_for_strip () const { return is_error (); };

  // TODO: this mutable getter seems really dodgy. Think up better way.
  std::vector<Attribute> &get_outer_attrs () { return outer_attrs; }
  const std::vector<Attribute> &get_outer_attrs () const { return outer_attrs; }

  // TODO: is this better? Or is a "vis_block" better?
  Type &get_type ()
  {
    rust_assert (param_type != nullptr);
    return *param_type;
  }

  std::unique_ptr<Type> &get_type_ptr ()
  {
    rust_assert (param_type != nullptr);
    return param_type;
  }

  NodeId get_node_id () const { return node_id; }
};

// A function item used in an extern block
class ExternalFunctionItem : public ExternalItem
{
  // bool has_outer_attrs;
  std::vector<Attribute> outer_attrs;

  // bool has_visibility;
  Visibility visibility;

  Identifier item_name;
  location_t locus;

  // bool has_generics;
  // Generics generic_params;
  std::vector<std::unique_ptr<GenericParam>> generic_params; // inlined

  // bool has_return_type;
  // FunctionReturnType return_type;
  std::unique_ptr<Type> return_type; // inlined

  // bool has_where_clause;
  WhereClause where_clause;

  std::vector<NamedFunctionParam> function_params;

public:
  // Returns whether item has generic parameters.
  bool has_generics () const { return !generic_params.empty (); }

  // Returns whether item has a return type (otherwise void).
  bool has_return_type () const { return return_type != nullptr; }

  // Returns whether item has a where clause.
  bool has_where_clause () const { return !where_clause.is_empty (); }

  // Returns whether item has outer attributes.
  bool has_outer_attrs () const { return !outer_attrs.empty (); }

  // Returns whether item has non-default visibility.
  bool has_visibility () const { return !visibility.is_error (); }

  // Returns whether item has variadic parameters.
  bool is_variadic () const
  {
    return function_params.size () != 0
	   && function_params.back ().is_variadic ();
  }

  location_t get_locus () const { return locus; }

  Visibility &get_visibility () { return visibility; }
  const Visibility &get_visibility () const { return visibility; }

  ExternalFunctionItem (
    Identifier item_name,
    std::vector<std::unique_ptr<GenericParam>> generic_params,
    std::unique_ptr<Type> return_type, WhereClause where_clause,
    std::vector<NamedFunctionParam> function_params, Visibility vis,
    std::vector<Attribute> outer_attrs, location_t locus)
    : ExternalItem (), outer_attrs (std::move (outer_attrs)),
      visibility (std::move (vis)), item_name (std::move (item_name)),
      locus (locus), generic_params (std::move (generic_params)),
      return_type (std::move (return_type)),
      where_clause (std::move (where_clause)),
      function_params (std::move (function_params))
  {
    // TODO: assert that if has variadic outer attrs, then has_variadics is
    // true?
  }

  // Copy constructor with clone
  ExternalFunctionItem (ExternalFunctionItem const &other)
    : ExternalItem (other.get_node_id ()), outer_attrs (other.outer_attrs),
      visibility (other.visibility), item_name (other.item_name),
      locus (other.locus), where_clause (other.where_clause),
      function_params (other.function_params)
  {
    node_id = other.node_id;
    // guard to prevent null pointer dereference
    if (other.return_type != nullptr)
      return_type = other.return_type->clone_type ();

    generic_params.reserve (other.generic_params.size ());
    for (const auto &e : other.generic_params)
      generic_params.push_back (e->clone_generic_param ());
  }

  // Overloaded assignment operator with clone
  ExternalFunctionItem &operator= (ExternalFunctionItem const &other)
  {
    outer_attrs = other.outer_attrs;
    visibility = other.visibility;
    item_name = other.item_name;
    locus = other.locus;
    where_clause = other.where_clause;
    function_params = other.function_params;
    node_id = other.node_id;

    // guard to prevent null pointer dereference
    if (other.return_type != nullptr)
      return_type = other.return_type->clone_type ();
    else
      return_type = nullptr;

    generic_params.reserve (other.generic_params.size ());
    for (const auto &e : other.generic_params)
      generic_params.push_back (e->clone_generic_param ());

    return *this;
  }

  // move constructors
  ExternalFunctionItem (ExternalFunctionItem &&other) = default;
  ExternalFunctionItem &operator= (ExternalFunctionItem &&other) = default;

  std::string as_string () const override;

  void accept_vis (ASTVisitor &vis) override;

  // Based on idea that nane should never be empty.
  void mark_for_strip () override { item_name = {""}; };
  bool is_marked_for_strip () const override { return item_name.empty (); };

  // TODO: this mutable getter seems really dodgy. Think up better way.
  std::vector<Attribute> &get_outer_attrs () { return outer_attrs; }
  const std::vector<Attribute> &get_outer_attrs () const { return outer_attrs; }

  std::vector<NamedFunctionParam> &get_function_params ()
  {
    return function_params;
  }
  const std::vector<NamedFunctionParam> &get_function_params () const
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
  WhereClause &get_where_clause () { return where_clause; }

  // TODO: is this better? Or is a "vis_block" better?
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

  Identifier get_identifier () const { return item_name; };

protected:
  /* Use covariance to implement clone function as returning this object
   * rather than base */
  ExternalFunctionItem *clone_external_item_impl () const override
  {
    return new ExternalFunctionItem (*this);
  }
};

// An extern block AST node
class ExternBlock : public VisItem
{
  // bool has_abi;
  std::string abi;

  // bool has_inner_attrs;
  std::vector<Attribute> inner_attrs;

  // bool has_extern_items;
  std::vector<std::unique_ptr<ExternalItem>> extern_items;

  location_t locus;

  // TODO: find another way to store this to save memory?
  bool marked_for_strip = false;

public:
  std::string as_string () const override;

  // Returns whether extern block has inner attributes.
  bool has_inner_attrs () const { return !inner_attrs.empty (); }

  // Returns whether extern block has extern items.
  bool has_extern_items () const { return !extern_items.empty (); }

  // Returns whether extern block has ABI name.
  bool has_abi () const { return !abi.empty (); }

  std::string get_abi () const { return abi; }

  ExternBlock (std::string abi,
	       std::vector<std::unique_ptr<ExternalItem>> extern_items,
	       Visibility vis, std::vector<Attribute> inner_attrs,
	       std::vector<Attribute> outer_attrs, location_t locus)
    : VisItem (std::move (vis), std::move (outer_attrs)), abi (std::move (abi)),
      inner_attrs (std::move (inner_attrs)),
      extern_items (std::move (extern_items)), locus (locus)
  {}

  // Copy constructor with vector clone
  ExternBlock (ExternBlock const &other)
    : VisItem (other), abi (other.abi), inner_attrs (other.inner_attrs),
      locus (other.locus), marked_for_strip (other.marked_for_strip)
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
    marked_for_strip = other.marked_for_strip;

    extern_items.reserve (other.extern_items.size ());
    for (const auto &e : other.extern_items)
      extern_items.push_back (e->clone_external_item ());

    return *this;
  }

  // move constructors
  ExternBlock (ExternBlock &&other) = default;
  ExternBlock &operator= (ExternBlock &&other) = default;

  location_t get_locus () const override final { return locus; }

  void accept_vis (ASTVisitor &vis) override;

  // Can't think of any invalid invariants, so store boolean.
  void mark_for_strip () override { marked_for_strip = true; }
  bool is_marked_for_strip () const override { return marked_for_strip; }

  // TODO: think of better way to do this
  const std::vector<std::unique_ptr<ExternalItem>> &get_extern_items () const
  {
    return extern_items;
  }
  std::vector<std::unique_ptr<ExternalItem>> &get_extern_items ()
  {
    return extern_items;
  }

  // TODO: think of better way to do this
  const std::vector<Attribute> &get_inner_attrs () const { return inner_attrs; }
  std::vector<Attribute> &get_inner_attrs () { return inner_attrs; }

protected:
  /* Use covariance to implement clone function as returning this object
   * rather than base */
  ExternBlock *clone_item_impl () const override
  {
    return new ExternBlock (*this);
  }
};

class MacroRulesDefinition;
} // namespace AST
} // namespace Rust

#endif
