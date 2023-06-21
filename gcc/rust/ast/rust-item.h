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

#ifndef RUST_AST_ITEM_H
#define RUST_AST_ITEM_H

#include "rust-ast.h"
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

  Location locus;

public:
  Identifier get_type_representation () const { return type_representation; }

  // Returns whether the type of the type param has been specified.
  bool has_type () const { return type != nullptr; }

  // Returns whether the type param has type param bounds.
  bool has_type_param_bounds () const { return !type_param_bounds.empty (); }

  // Returns whether the type param has an outer attribute.
  bool has_outer_attribute () const { return !outer_attr.is_empty (); }

  TypeParam (Identifier type_representation, Location locus = Location (),
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

  Location get_locus () const override final { return locus; }

  Kind get_kind () const override final { return Kind::Type; }

  void accept_vis (ASTVisitor &vis) override;

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<Type> &get_type ()
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
  Location locus;
  NodeId node_id;

public:
  LifetimeWhereClauseItem (Lifetime lifetime,
			   std::vector<Lifetime> lifetime_bounds,
			   Location locus)
    : lifetime (std::move (lifetime)),
      lifetime_bounds (std::move (lifetime_bounds)), locus (locus),
      node_id (Analysis::Mappings::get ()->get_next_node_id ())
  {}

  std::string as_string () const override;

  void accept_vis (ASTVisitor &vis) override;

  NodeId get_node_id () const override final { return node_id; }

  Lifetime &get_lifetime () { return lifetime; }

  std::vector<Lifetime> &get_lifetime_bounds () { return lifetime_bounds; }

  Location get_locus () const { return locus; }

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
  Location locus;

public:
  // Returns whether the item has ForLifetimes
  bool has_for_lifetimes () const { return !for_lifetimes.empty (); }

  std::vector<LifetimeParam> &get_for_lifetimes () { return for_lifetimes; }

  // Returns whether the item has type param bounds
  bool has_type_param_bounds () const { return !type_param_bounds.empty (); }

  TypeBoundWhereClauseItem (
    std::vector<LifetimeParam> for_lifetimes, std::unique_ptr<Type> bound_type,
    std::vector<std::unique_ptr<TypeParamBound>> type_param_bounds,
    Location locus)
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

  std::unique_ptr<Type> &get_type ()
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

  Location get_locus () const { return locus; }

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

// A self parameter in a method
struct SelfParam
{
private:
  bool has_ref;
  bool is_mut;
  // bool has_lifetime; // only possible if also ref
  Lifetime lifetime;

  // bool has_type; // only possible if not ref
  std::unique_ptr<Type> type;

  NodeId node_id;

  Location locus;

  // Unrestricted constructor used for error state
  SelfParam (Lifetime lifetime, bool has_ref, bool is_mut, Type *type)
    : has_ref (has_ref), is_mut (is_mut), lifetime (std::move (lifetime)),
      type (type), node_id (Analysis::Mappings::get ()->get_next_node_id ())
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
  SelfParam (std::unique_ptr<Type> type, bool is_mut, Location locus)
    : has_ref (false), is_mut (is_mut), lifetime (Lifetime::error ()),
      type (std::move (type)),
      node_id (Analysis::Mappings::get ()->get_next_node_id ()), locus (locus)
  {}

  // Lifetime-based self parameter (is ref, no type)
  SelfParam (Lifetime lifetime, bool is_mut, Location locus)
    : has_ref (true), is_mut (is_mut), lifetime (std::move (lifetime)),
      node_id (Analysis::Mappings::get ()->get_next_node_id ()), locus (locus)
  {}

  // Copy constructor requires clone
  SelfParam (SelfParam const &other)
    : has_ref (other.has_ref), is_mut (other.is_mut), lifetime (other.lifetime),
      node_id (Analysis::Mappings::get ()->get_next_node_id ()),
      locus (other.locus)
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

    if (other.type != nullptr)
      type = other.type->clone_type ();
    else
      type = nullptr;

    return *this;
  }

  // move constructors
  SelfParam (SelfParam &&other) = default;
  SelfParam &operator= (SelfParam &&other) = default;

  std::string as_string () const;

  Location get_locus () const { return locus; }

  bool get_has_ref () const { return has_ref; };
  bool get_is_mut () const { return is_mut; }

  Lifetime get_lifetime () const { return lifetime; }

  NodeId get_node_id () const { return node_id; }

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<Type> &get_type ()
  {
    rust_assert (has_type ());
    return type;
  }
};

// Qualifiers for function, i.e. const, unsafe, extern etc.
struct FunctionQualifiers
{
private:
  AsyncConstStatus const_status;
  bool has_unsafe;
  bool has_extern;
  std::string extern_abi;
  Location locus;

public:
  FunctionQualifiers (Location locus, AsyncConstStatus const_status,
		      bool has_unsafe, bool has_extern = false,
		      std::string extern_abi = std::string ())
    : const_status (const_status), has_unsafe (has_unsafe),
      has_extern (has_extern), extern_abi (std::move (extern_abi)),
      locus (locus)
  {
    if (!this->extern_abi.empty ())
      {
	// having extern is required; not having it is an implementation error
	rust_assert (has_extern);
      }
  }

  std::string as_string () const;

  AsyncConstStatus get_const_status () const { return const_status; }
  bool is_unsafe () const { return has_unsafe; }
  bool is_extern () const { return has_extern; }
  std::string get_extern_abi () const { return extern_abi; }
  bool has_abi () const { return !extern_abi.empty (); }

  Location get_locus () const { return locus; }
};

// A function parameter
struct FunctionParam
{
private:
  std::vector<Attribute> outer_attrs;
  Location locus;
  std::unique_ptr<Pattern> param_name;
  std::unique_ptr<Type> type;

public:
  FunctionParam (std::unique_ptr<Pattern> param_name,
		 std::unique_ptr<Type> param_type,
		 std::vector<Attribute> outer_attrs, Location locus)
    : outer_attrs (std::move (outer_attrs)), locus (locus),
      param_name (std::move (param_name)), type (std::move (param_type)),
      node_id (Analysis::Mappings::get ()->get_next_node_id ())
  {}

  // Copy constructor uses clone
  FunctionParam (FunctionParam const &other)
    : locus (other.locus), node_id (other.node_id)
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
    return FunctionParam (nullptr, nullptr, {}, Location ());
  }

  std::string as_string () const;

  Location get_locus () const { return locus; }

  // TODO: seems kinda dodgy. Think of better way.
  std::vector<Attribute> &get_outer_attrs () { return outer_attrs; }
  const std::vector<Attribute> &get_outer_attrs () const { return outer_attrs; }

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<Pattern> &get_pattern ()
  {
    rust_assert (param_name != nullptr);
    return param_name;
  }

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<Type> &get_type ()
  {
    rust_assert (type != nullptr);
    return type;
  }
  NodeId get_node_id () const { return node_id; }

protected:
  NodeId node_id;
};

// Visibility of item - if the item has it, then it is some form of public
struct Visibility
{
public:
  enum VisType
  {
    PRIV,
    PUB,
    PUB_CRATE,
    PUB_SELF,
    PUB_SUPER,
    PUB_IN_PATH
  };

private:
  VisType vis_type;
  // Only assigned if vis_type is IN_PATH
  SimplePath in_path;
  Location locus;

  // should this store location info?

public:
  // Creates a Visibility - TODO make constructor protected or private?
  Visibility (VisType vis_type, SimplePath in_path, Location locus)
    : vis_type (vis_type), in_path (std::move (in_path)), locus (locus)
  {}

  VisType get_vis_type () const { return vis_type; }

  // Returns whether visibility is in an error state.
  bool is_error () const
  {
    return vis_type == PUB_IN_PATH && in_path.is_empty ();
  }

  // Returns whether a visibility has a path
  bool has_path () const { return !(is_error ()) && vis_type == PUB_IN_PATH; }

  // Returns whether visibility is public or not.
  bool is_public () const { return vis_type != PRIV && !is_error (); }

  Location get_locus () const { return locus; }

  // empty?
  // Creates an error visibility.
  static Visibility create_error ()
  {
    return Visibility (PUB_IN_PATH, SimplePath::create_empty (), Location ());
  }

  // Unique pointer custom clone function
  /*std::unique_ptr<Visibility> clone_visibility() const {
      return std::unique_ptr<Visibility>(clone_visibility_impl());
  }*/

  /* TODO: think of a way to only allow valid Visibility states - polymorphism
   * is one idea but may be too resource-intensive. */

  // Creates a public visibility with no further features/arguments.
  // empty?
  static Visibility create_public (Location pub_vis_location)
  {
    return Visibility (PUB, SimplePath::create_empty (), pub_vis_location);
  }

  // Creates a public visibility with crate-relative paths
  static Visibility create_crate (Location crate_tok_location,
				  Location crate_vis_location)
  {
    return Visibility (PUB_CRATE,
		       SimplePath::from_str ("crate", crate_tok_location),
		       crate_vis_location);
  }

  // Creates a public visibility with self-relative paths
  static Visibility create_self (Location self_tok_location,
				 Location self_vis_location)
  {
    return Visibility (PUB_SELF,
		       SimplePath::from_str ("self", self_tok_location),
		       self_vis_location);
  }

  // Creates a public visibility with parent module-relative paths
  static Visibility create_super (Location super_tok_location,
				  Location super_vis_location)
  {
    return Visibility (PUB_SUPER,
		       SimplePath::from_str ("super", super_tok_location),
		       super_vis_location);
  }

  // Creates a private visibility
  static Visibility create_private ()
  {
    return Visibility (PRIV, SimplePath::create_empty (), Location ());
  }

  // Creates a public visibility with a given path or whatever.
  static Visibility create_in_path (SimplePath in_path,
				    Location in_path_vis_location)
  {
    return Visibility (PUB_IN_PATH, std::move (in_path), in_path_vis_location);
  }

  std::string as_string () const;
  const SimplePath &get_path () const { return in_path; }
  SimplePath &get_path () { return in_path; }

protected:
  // Clone function implementation - not currently virtual but may be if
  // polymorphism used
  /*virtual*/ Visibility *clone_visibility_impl () const
  {
    return new Visibility (*this);
  }
};

// A method (function belonging to a type)
class Method : public InherentImplItem, public TraitImplItem
{
  std::vector<Attribute> outer_attrs;
  Visibility vis;
  FunctionQualifiers qualifiers;
  Identifier method_name;
  std::vector<std::unique_ptr<GenericParam>> generic_params;
  SelfParam self_param;
  std::vector<FunctionParam> function_params;
  std::unique_ptr<Type> return_type;
  WhereClause where_clause;
  std::unique_ptr<BlockExpr> function_body;
  Location locus;
  NodeId node_id;

public:
  // Returns whether the method is in an error state.
  bool is_error () const
  {
    return function_body == nullptr || method_name.empty ()
	   || self_param.is_error ();
  }

  // Creates an error state method.
  static Method create_error ()
  {
    return Method ("", FunctionQualifiers (Location (), NONE, true),
		   std::vector<std::unique_ptr<GenericParam>> (),
		   SelfParam::create_error (), std::vector<FunctionParam> (),
		   nullptr, WhereClause::create_empty (), nullptr,
		   Visibility::create_error (), std::vector<Attribute> (), {});
  }

  // Returns whether the method has generic parameters.
  bool has_generics () const { return !generic_params.empty (); }

  // Returns whether the method has parameters.
  bool has_params () const { return !function_params.empty (); }

  // Returns whether the method has a return type (void otherwise).
  bool has_return_type () const { return return_type != nullptr; }

  // Returns whether the where clause exists (i.e. has items)
  bool has_where_clause () const { return !where_clause.is_empty (); }

  // Returns whether method has a non-default visibility.
  bool has_visibility () const { return !vis.is_error (); }

  // Mega-constructor with all possible fields
  Method (Identifier method_name, FunctionQualifiers qualifiers,
	  std::vector<std::unique_ptr<GenericParam>> generic_params,
	  SelfParam self_param, std::vector<FunctionParam> function_params,
	  std::unique_ptr<Type> return_type, WhereClause where_clause,
	  std::unique_ptr<BlockExpr> function_body, Visibility vis,
	  std::vector<Attribute> outer_attrs, Location locus)
    : outer_attrs (std::move (outer_attrs)), vis (std::move (vis)),
      qualifiers (std::move (qualifiers)),
      method_name (std::move (method_name)),
      generic_params (std::move (generic_params)),
      self_param (std::move (self_param)),
      function_params (std::move (function_params)),
      return_type (std::move (return_type)),
      where_clause (std::move (where_clause)),
      function_body (std::move (function_body)), locus (locus),
      node_id (Analysis::Mappings::get ()->get_next_node_id ())
  {}

  // TODO: add constructor with less fields

  // Copy constructor with clone
  Method (Method const &other)
    : outer_attrs (other.outer_attrs), vis (other.vis),
      qualifiers (other.qualifiers), method_name (other.method_name),
      self_param (other.self_param), function_params (other.function_params),
      where_clause (other.where_clause), locus (other.locus)
  {
    // guard to prevent null dereference (always required)
    if (other.return_type != nullptr)
      return_type = other.return_type->clone_type ();

    // guard to prevent null dereference (only required if error state)
    if (other.function_body != nullptr)
      function_body = other.function_body->clone_block_expr ();

    generic_params.reserve (other.generic_params.size ());
    for (const auto &e : other.generic_params)
      generic_params.push_back (e->clone_generic_param ());

    node_id = other.node_id;
  }

  // Overloaded assignment operator to clone
  Method &operator= (Method const &other)
  {
    method_name = other.method_name;
    outer_attrs = other.outer_attrs;
    vis = other.vis;
    qualifiers = other.qualifiers;
    self_param = other.self_param;
    function_params = other.function_params;
    where_clause = other.where_clause;
    locus = other.locus;

    // guard to prevent null dereference (always required)
    if (other.return_type != nullptr)
      return_type = other.return_type->clone_type ();
    else
      return_type = nullptr;

    // guard to prevent null dereference (only required if error state)
    if (other.function_body != nullptr)
      function_body = other.function_body->clone_block_expr ();
    else
      function_body = nullptr;

    generic_params.reserve (other.generic_params.size ());
    for (const auto &e : other.generic_params)
      generic_params.push_back (e->clone_generic_param ());

    node_id = other.node_id;

    return *this;
  }

  // move constructors
  Method (Method &&other) = default;
  Method &operator= (Method &&other) = default;

  std::string as_string () const override;

  void accept_vis (ASTVisitor &vis) override;

  // Invalid if block is null, so base stripping on that.
  void mark_for_strip () override { function_body = nullptr; }
  bool is_marked_for_strip () const override
  {
    return function_body == nullptr;
  }

  // TODO: this mutable getter seems really dodgy. Think up better way.
  std::vector<Attribute> &get_outer_attrs () { return outer_attrs; }
  const std::vector<Attribute> &get_outer_attrs () const { return outer_attrs; }

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
  std::unique_ptr<BlockExpr> &get_definition ()
  {
    rust_assert (function_body != nullptr);
    return function_body;
  }

  SelfParam &get_self_param () { return self_param; }
  const SelfParam &get_self_param () const { return self_param; }

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<Type> &get_return_type ()
  {
    rust_assert (has_return_type ());
    return return_type;
  }

  // TODO: is this better? Or is a "vis_block" better?
  WhereClause &get_where_clause () { return where_clause; }

  Identifier get_method_name () const { return method_name; }

  NodeId get_node_id () const { return node_id; }

  Location get_locus () const override final { return locus; }

  FunctionQualifiers get_qualifiers () { return qualifiers; }

  Visibility &get_visibility () { return vis; }
  const Visibility &get_visibility () const { return vis; }

protected:
  /* Use covariance to implement clone function as returning this object
   * rather than base */
  Method *clone_inherent_impl_item_impl () const final override
  {
    return clone_method_impl ();
  }

  /* Use covariance to implement clone function as returning this object
   * rather than base */
  Method *clone_trait_impl_item_impl () const final override
  {
    return clone_method_impl ();
  }

  /*virtual*/ Method *clone_method_impl () const { return new Method (*this); }
};

// Item that supports visibility - abstract base class
class VisItem : public Item
{
  Visibility visibility;
  std::vector<Attribute> outer_attrs;

protected:
  // Visibility constructor
  VisItem (Visibility visibility,
	   std::vector<Attribute> outer_attrs = std::vector<Attribute> ())
    : visibility (std::move (visibility)), outer_attrs (std::move (outer_attrs))
  {}

  // Visibility copy constructor
  VisItem (VisItem const &other)
    : visibility (other.visibility), outer_attrs (other.outer_attrs)
  {}

  // Overload assignment operator to clone
  VisItem &operator= (VisItem const &other)
  {
    visibility = other.visibility;
    outer_attrs = other.outer_attrs;

    return *this;
  }

  // move constructors
  VisItem (VisItem &&other) = default;
  VisItem &operator= (VisItem &&other) = default;

public:
  /* Does the item have some kind of public visibility (non-default
   * visibility)? */
  bool has_visibility () const { return visibility.is_public (); }

  std::string as_string () const override;

  // TODO: this mutable getter seems really dodgy. Think up better way.
  Visibility &get_visibility () { return visibility; }
  const Visibility &get_visibility () const { return visibility; }

  std::vector<Attribute> &get_outer_attrs () { return outer_attrs; }
  const std::vector<Attribute> &get_outer_attrs () const { return outer_attrs; }
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

private:
  Identifier module_name;
  Location locus;
  ModuleKind kind;

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
	  std::vector<Attribute> outer_attrs, Location locus,
	  std::string outer_filename, std::vector<std::string> module_scope)
    : VisItem (std::move (visibility), std::move (outer_attrs)),
      module_name (module_name), locus (locus), kind (ModuleKind::UNLOADED),
      outer_filename (outer_filename), inner_attrs (std::vector<Attribute> ()),
      items (std::vector<std::unique_ptr<Item>> ()),
      module_scope (std::move (module_scope))
  {}

  // Loaded module constructor, with items
  Module (Identifier name, Location locus,
	  std::vector<std::unique_ptr<Item>> items,
	  Visibility visibility = Visibility::create_error (),
	  std::vector<Attribute> inner_attrs = std::vector<Attribute> (),
	  std::vector<Attribute> outer_attrs = std::vector<Attribute> ())
    : VisItem (std::move (visibility), std::move (outer_attrs)),
      module_name (name), locus (locus), kind (ModuleKind::LOADED),
      outer_filename (std::string ()), inner_attrs (std::move (inner_attrs)),
      items (std::move (items))
  {}

  // Copy constructor with vector clone
  Module (Module const &other)
    : VisItem (other), module_name (other.module_name), locus (other.locus),
      kind (other.kind), inner_attrs (other.inner_attrs),
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

  // TODO: think of better way to do this - mutable getter seems dodgy
  const std::vector<Attribute> &get_inner_attrs () const { return inner_attrs; }
  std::vector<Attribute> &get_inner_attrs () { return inner_attrs; }

  const std::vector<std::unique_ptr<Item>> &get_items () const { return items; }
  std::vector<std::unique_ptr<Item>> &get_items () { return items; }

  // move constructors
  Module (Module &&other) = default;
  Module &operator= (Module &&other) = default;

  std::string as_string () const override;

  Location get_locus () const override final { return locus; }

  // Invalid if name is empty, so base stripping on that.
  void mark_for_strip () override { module_name = ""; }
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

  Location locus;

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
	       std::vector<Attribute> outer_attrs, Location locus,
	       std::string as_clause_name = std::string ())
    : VisItem (std::move (visibility), std::move (outer_attrs)),
      referenced_crate (std::move (referenced_crate)),
      as_clause_name (std::move (as_clause_name)), locus (locus)
  {}

  Location get_locus () const override final { return locus; }

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
  Location locus;

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

  Location get_locus () const { return locus; }

  virtual void accept_vis (ASTVisitor &vis) = 0;

protected:
  // Clone function implementation as pure virtual method
  virtual UseTree *clone_use_tree_impl () const = 0;

  UseTree (Location locus) : locus (locus) {}
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
  UseTreeGlob (PathType glob_type, SimplePath path, Location locus)
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

  Kind get_kind () const override { return Glob; }

  SimplePath get_path () const
  {
    rust_assert (has_path ());
    return path;
  }

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
	       std::vector<std::unique_ptr<UseTree>> trees, Location locus)
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

  void accept_vis (ASTVisitor &vis) override;

  Kind get_kind () const override { return List; }
  SimplePath get_path () const
  {
    rust_assert (has_path ());
    return path;
  }

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
  UseTreeRebind (NewBindType bind_type, SimplePath path, Location locus,
		 Identifier identifier = std::string ())
    : UseTree (locus), path (std::move (path)), bind_type (bind_type),
      identifier (std::move (identifier))
  {}

  // Returns whether has path (this should always be true).
  bool has_path () const { return !path.is_empty (); }

  // Returns whether has identifier (or, rather, is allowed to).
  bool has_identifier () const { return bind_type == IDENTIFIER; }

  std::string as_string () const override;

  void accept_vis (ASTVisitor &vis) override;

  Kind get_kind () const override { return Rebind; }

  SimplePath get_path () const
  {
    rust_assert (has_path ());
    return path;
  }

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
  Location locus;

public:
  std::string as_string () const override;

  UseDeclaration (std::unique_ptr<UseTree> use_tree, Visibility visibility,
		  std::vector<Attribute> outer_attrs, Location locus)
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

  Location get_locus () const override final { return locus; }
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
class Function : public VisItem, public InherentImplItem, public TraitImplItem
{
  FunctionQualifiers qualifiers;
  Identifier function_name;
  std::vector<std::unique_ptr<GenericParam>> generic_params;
  std::vector<FunctionParam> function_params;
  std::unique_ptr<Type> return_type;
  WhereClause where_clause;
  std::unique_ptr<BlockExpr> function_body;
  Location locus;

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

  // Mega-constructor with all possible fields
  Function (Identifier function_name, FunctionQualifiers qualifiers,
	    std::vector<std::unique_ptr<GenericParam>> generic_params,
	    std::vector<FunctionParam> function_params,
	    std::unique_ptr<Type> return_type, WhereClause where_clause,
	    std::unique_ptr<BlockExpr> function_body, Visibility vis,
	    std::vector<Attribute> outer_attrs, Location locus)
    : VisItem (std::move (vis), std::move (outer_attrs)),
      qualifiers (std::move (qualifiers)),
      function_name (std::move (function_name)),
      generic_params (std::move (generic_params)),
      function_params (std::move (function_params)),
      return_type (std::move (return_type)),
      where_clause (std::move (where_clause)),
      function_body (std::move (function_body)), locus (locus)
  {}

  // TODO: add constructor with less fields

  // Copy constructor with clone
  Function (Function const &other)
    : VisItem (other), qualifiers (other.qualifiers),
      function_name (other.function_name),
      function_params (other.function_params),
      where_clause (other.where_clause), locus (other.locus)
  {
    // guard to prevent null dereference (always required)
    if (other.return_type != nullptr)
      return_type = other.return_type->clone_type ();

    // guard to prevent null dereference (only required if error state)
    if (other.function_body != nullptr)
      function_body = other.function_body->clone_block_expr ();

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
    where_clause = other.where_clause;
    // visibility = other.visibility->clone_visibility();
    // outer_attrs = other.outer_attrs;
    locus = other.locus;

    // guard to prevent null dereference (always required)
    if (other.return_type != nullptr)
      return_type = other.return_type->clone_type ();
    else
      return_type = nullptr;

    // guard to prevent null dereference (only required if error state)
    if (other.function_body != nullptr)
      function_body = other.function_body->clone_block_expr ();
    else
      function_body = nullptr;

    generic_params.reserve (other.generic_params.size ());
    for (const auto &e : other.generic_params)
      generic_params.push_back (e->clone_generic_param ());

    return *this;
  }

  // move constructors
  Function (Function &&other) = default;
  Function &operator= (Function &&other) = default;

  Location get_locus () const override final { return locus; }

  void accept_vis (ASTVisitor &vis) override;

  // Invalid if block is null, so base stripping on that.
  void mark_for_strip () override { function_body = nullptr; }
  bool is_marked_for_strip () const override
  {
    return function_body == nullptr;
  }

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
  std::unique_ptr<BlockExpr> &get_definition ()
  {
    rust_assert (function_body != nullptr);
    return function_body;
  }

  const FunctionQualifiers &get_qualifiers () const { return qualifiers; }

  Identifier get_function_name () const { return function_name; }

  // TODO: is this better? Or is a "vis_block" better?
  WhereClause &get_where_clause () { return where_clause; }

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<Type> &get_return_type ()
  {
    rust_assert (has_return_type ());
    return return_type;
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

  /* Use covariance to implement clone function as returning this object
   * rather than base */
  Function *clone_trait_impl_item_impl () const override
  {
    return new Function (*this);
  }
};

// Rust type alias (i.e. typedef) AST node
class TypeAlias : public VisItem, public TraitImplItem
{
  Identifier new_type_name;

  // bool has_generics;
  // Generics generic_params;
  std::vector<std::unique_ptr<GenericParam>> generic_params; // inlined

  // bool has_where_clause;
  WhereClause where_clause;

  std::unique_ptr<Type> existing_type;

  Location locus;

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
	     Visibility vis, std::vector<Attribute> outer_attrs, Location locus)
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

  Location get_locus () const override final { return locus; }

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
  std::unique_ptr<Type> &get_type_aliased ()
  {
    rust_assert (existing_type != nullptr);
    return existing_type;
  }

  Identifier get_new_type_name () const { return new_type_name; }

protected:
  /* Use covariance to implement clone function as returning this object
   * rather than base */
  TypeAlias *clone_item_impl () const override { return new TypeAlias (*this); }

  /* Use covariance to implement clone function as returning this object
   * rather than base */
  TypeAlias *clone_trait_impl_item_impl () const override
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
  Location locus;

public:
  // Returns whether struct has generic parameters.
  bool has_generics () const { return !generic_params.empty (); }

  // Returns whether struct has a where clause.
  bool has_where_clause () const { return !where_clause.is_empty (); }

  Location get_locus () const override final { return locus; }

  // Invalid if name is empty, so base stripping on that.
  void mark_for_strip () override { struct_name = ""; }
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
	  WhereClause where_clause, Visibility vis, Location locus,
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
struct StructField
{
private:
  // bool has_outer_attributes;
  std::vector<Attribute> outer_attrs;

  // bool has_visibility;
  Visibility visibility;

  Identifier field_name;
  std::unique_ptr<Type> field_type;

  NodeId node_id;

  Location locus;

public:
  // Returns whether struct field has any outer attributes.
  bool has_outer_attributes () const { return !outer_attrs.empty (); }

  // Returns whether struct field has a non-private (non-default) visibility.
  bool has_visibility () const { return !visibility.is_error (); }

  StructField (Identifier field_name, std::unique_ptr<Type> field_type,
	       Visibility vis, Location locus,
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
			Location ());
  }

  std::string as_string () const;

  // TODO: this mutable getter seems really dodgy. Think up better way.
  std::vector<Attribute> &get_outer_attrs () { return outer_attrs; }
  const std::vector<Attribute> &get_outer_attrs () const { return outer_attrs; }

  Identifier get_field_name () const { return field_name; }

  Location get_locus () const { return locus; }

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<Type> &get_field_type ()
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
		std::vector<Attribute> outer_attrs, Location locus)
    : Struct (std::move (struct_name), std::move (generic_params),
	      std::move (where_clause), std::move (vis), locus,
	      std::move (outer_attrs)),
      fields (std::move (fields)), is_unit (is_unit)
  {}

  // Unit struct constructor
  StructStruct (Identifier struct_name,
		std::vector<std::unique_ptr<GenericParam>> generic_params,
		WhereClause where_clause, Visibility vis,
		std::vector<Attribute> outer_attrs, Location locus)
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
struct TupleField
{
private:
  // bool has_outer_attributes;
  std::vector<Attribute> outer_attrs;

  // bool has_visibility;
  Visibility visibility;

  std::unique_ptr<Type> field_type;

  NodeId node_id;

  Location locus;

public:
  // Returns whether tuple field has outer attributes.
  bool has_outer_attributes () const { return !outer_attrs.empty (); }

  /* Returns whether tuple field has a non-default visibility (i.e. a public
   * one) */
  bool has_visibility () const { return !visibility.is_error (); }

  // Complete constructor
  TupleField (std::unique_ptr<Type> field_type, Visibility vis, Location locus,
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
    return TupleField (nullptr, Visibility::create_error (), Location ());
  }

  std::string as_string () const;

  NodeId get_node_id () const { return node_id; }

  Visibility &get_visibility () { return visibility; }
  const Visibility &get_visibility () const { return visibility; }

  Location get_locus () const { return locus; }

  // TODO: this mutable getter seems really dodgy. Think up better way.
  std::vector<Attribute> &get_outer_attrs () { return outer_attrs; }
  const std::vector<Attribute> &get_outer_attrs () const { return outer_attrs; }

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<Type> &get_field_type ()
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
	       std::vector<Attribute> outer_attrs, Location locus)
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

  Location locus;

public:
  virtual ~EnumItem () {}

  EnumItem (Identifier variant_name, Visibility vis,
	    std::vector<Attribute> outer_attrs, Location locus)
    : VisItem (std::move (vis), std::move (outer_attrs)),
      variant_name (std::move (variant_name)), locus (locus)
  {}

  // Unique pointer custom clone function
  std::unique_ptr<EnumItem> clone_enum_item () const
  {
    return std::unique_ptr<EnumItem> (clone_item_impl ());
  }

  virtual std::string as_string () const;

  // not pure virtual as not abstract
  virtual void accept_vis (ASTVisitor &vis);

  Location get_locus () const { return locus; }

  Identifier get_identifier () const { return variant_name; }

  // Based on idea that name is never empty.
  void mark_for_strip () { variant_name = ""; }
  bool is_marked_for_strip () const { return variant_name.empty (); }

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
		 std::vector<Attribute> outer_attrs, Location locus)
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
		  std::vector<Attribute> outer_attrs, Location locus)
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
			std::vector<Attribute> outer_attrs, Location locus)
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

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<Expr> &get_expr ()
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

  Location locus;

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
	std::vector<Attribute> outer_attrs, Location locus)
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

  Location get_locus () const override final { return locus; }

  void accept_vis (ASTVisitor &vis) override;

  Identifier get_identifier () const { return enum_name; }

  // Invalid if name is empty, so base stripping on that.
  void mark_for_strip () override { enum_name = ""; }
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

  Location locus;

public:
  std::string as_string () const override;

  // Returns whether union has generic params.
  bool has_generics () const { return !generic_params.empty (); }

  // Returns whether union has where clause.
  bool has_where_clause () const { return !where_clause.is_empty (); }

  Union (Identifier union_name, Visibility vis,
	 std::vector<std::unique_ptr<GenericParam>> generic_params,
	 WhereClause where_clause, std::vector<StructField> variants,
	 std::vector<Attribute> outer_attrs, Location locus)
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

  Location get_locus () const override final { return locus; }

  void accept_vis (ASTVisitor &vis) override;

  // Invalid if name is empty, so base stripping on that.
  void mark_for_strip () override { union_name = ""; }
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
class ConstantItem : public VisItem,
		     public InherentImplItem,
		     public TraitImplItem
{
  // either has an identifier or "_" - maybe handle in identifier?
  // bool identifier_is_underscore;
  // if no identifier declared, identifier will be "_"
  std::string identifier;

  std::unique_ptr<Type> type;
  std::unique_ptr<Expr> const_expr;

  Location locus;

public:
  std::string as_string () const override;

  ConstantItem (std::string ident, Visibility vis, std::unique_ptr<Type> type,
		std::unique_ptr<Expr> const_expr,
		std::vector<Attribute> outer_attrs, Location locus)
    : VisItem (std::move (vis), std::move (outer_attrs)),
      identifier (std::move (ident)), type (std::move (type)),
      const_expr (std::move (const_expr)), locus (locus)
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

  Location get_locus () const override final { return locus; }

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

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<Expr> &get_expr ()
  {
    rust_assert (const_expr != nullptr);
    return const_expr;
  }

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<Type> &get_type ()
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
  ConstantItem *clone_inherent_impl_item_impl () const override
  {
    return new ConstantItem (*this);
  }

  /* Use covariance to implement clone function as returning this object
   * rather than base */
  ConstantItem *clone_trait_impl_item_impl () const override
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
  Location locus;

public:
  std::string as_string () const override;

  StaticItem (Identifier name, bool is_mut, std::unique_ptr<Type> type,
	      std::unique_ptr<Expr> expr, Visibility vis,
	      std::vector<Attribute> outer_attrs, Location locus)
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

  Location get_locus () const override final { return locus; }

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

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<Expr> &get_expr ()
  {
    rust_assert (expr != nullptr);
    return expr;
  }

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<Type> &get_type ()
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

// Function declaration in traits
struct TraitFunctionDecl
{
private:
  // TODO: delete and replace with Function decl item? no as no body in this.
  FunctionQualifiers qualifiers;
  Identifier function_name;

  // bool has_generics;
  // Generics generic_params;
  std::vector<std::unique_ptr<GenericParam>> generic_params; // inlined

  // bool has_params;
  // FunctionParams function_params;
  std::vector<FunctionParam> function_params; // inlined

  // bool has_return_type;
  std::unique_ptr<Type> return_type;

  // bool has_where_clause;
  WhereClause where_clause;

  // should this store location info?

public:
  // Returns whether function decl has generic parameters.
  bool has_generics () const { return !generic_params.empty (); }

  // Returns whether function decl has regular parameters.
  bool has_params () const { return !function_params.empty (); }

  // Returns whether function has return type (otherwise is void).
  bool has_return_type () const { return return_type != nullptr; }

  // Returns whether function has a where clause.
  bool has_where_clause () const { return !where_clause.is_empty (); }

  Identifier get_identifier () const { return function_name; }

  // Mega-constructor
  TraitFunctionDecl (Identifier function_name, FunctionQualifiers qualifiers,
		     std::vector<std::unique_ptr<GenericParam>> generic_params,
		     std::vector<FunctionParam> function_params,
		     std::unique_ptr<Type> return_type,
		     WhereClause where_clause)
    : qualifiers (std::move (qualifiers)),
      function_name (std::move (function_name)),
      generic_params (std::move (generic_params)),
      function_params (std::move (function_params)),
      return_type (std::move (return_type)),
      where_clause (std::move (where_clause))
  {}

  // Copy constructor with clone
  TraitFunctionDecl (TraitFunctionDecl const &other)
    : qualifiers (other.qualifiers), function_name (other.function_name),
      function_params (other.function_params), where_clause (other.where_clause)
  {
    // guard to prevent nullptr dereference
    if (other.return_type != nullptr)
      return_type = other.return_type->clone_type ();

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
    where_clause = other.where_clause;

    // guard to prevent nullptr dereference
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
  TraitFunctionDecl (TraitFunctionDecl &&other) = default;
  TraitFunctionDecl &operator= (TraitFunctionDecl &&other) = default;

  std::string as_string () const;

  // Invalid if function name is empty, so base stripping on that.
  void mark_for_strip () { function_name = ""; }
  bool is_marked_for_strip () const { return function_name.empty (); }

  // TODO: this mutable getter seems really dodgy. Think up better way.
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
  std::unique_ptr<Type> &get_return_type () { return return_type; }

  // TODO: is this better? Or is a "vis_block" better?
  WhereClause &get_where_clause () { return where_clause; }

  FunctionQualifiers get_qualifiers () { return qualifiers; }
};

// Actual trait item function declaration within traits
class TraitItemFunc : public TraitItem
{
  std::vector<Attribute> outer_attrs;
  TraitFunctionDecl decl;
  std::unique_ptr<BlockExpr> block_expr;

public:
  // Returns whether function has a definition or is just a declaration.
  bool has_definition () const { return block_expr != nullptr; }

  TraitItemFunc (TraitFunctionDecl decl, std::unique_ptr<BlockExpr> block_expr,
		 std::vector<Attribute> outer_attrs, Location locus)
    : TraitItem (locus), outer_attrs (std::move (outer_attrs)),
      decl (std::move (decl)), block_expr (std::move (block_expr))
  {}

  // Copy constructor with clone
  TraitItemFunc (TraitItemFunc const &other)
    : TraitItem (other.locus), outer_attrs (other.outer_attrs),
      decl (other.decl)
  {
    node_id = other.node_id;

    // guard to prevent null dereference
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
    node_id = other.node_id;

    // guard to prevent null dereference
    if (other.block_expr != nullptr)
      block_expr = other.block_expr->clone_block_expr ();
    else
      block_expr = nullptr;

    return *this;
  }

  // move constructors
  TraitItemFunc (TraitItemFunc &&other) = default;
  TraitItemFunc &operator= (TraitItemFunc &&other) = default;

  std::string as_string () const override;

  void accept_vis (ASTVisitor &vis) override;

  // Invalid if trait decl is empty, so base stripping on that.
  void mark_for_strip () override { decl.mark_for_strip (); }
  bool is_marked_for_strip () const override
  {
    return decl.is_marked_for_strip ();
  }

  // TODO: this mutable getter seems really dodgy. Think up better way.
  std::vector<Attribute> &get_outer_attrs () { return outer_attrs; }
  const std::vector<Attribute> &get_outer_attrs () const { return outer_attrs; }

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<BlockExpr> &get_definition () { return block_expr; }

  // TODO: is this better? Or is a "vis_block" better?
  TraitFunctionDecl &get_trait_function_decl ()
  {
    // TODO: maybe only allow access if not marked for strip?
    return decl;
  }

protected:
  // Clone function implementation as (not pure) virtual method
  TraitItemFunc *clone_trait_item_impl () const override
  {
    return new TraitItemFunc (*this);
  }
};

// Method declaration within traits
struct TraitMethodDecl
{
private:
  // TODO: delete and replace with Function decl item? no as no body.
  FunctionQualifiers qualifiers;
  Identifier function_name;

  // bool has_generics;
  // Generics generic_params;
  std::vector<std::unique_ptr<GenericParam>> generic_params; // inlined

  SelfParam self_param;

  // bool has_params;
  // FunctionParams function_params;
  std::vector<FunctionParam> function_params; // inlined

  // bool has_return_type;
  std::unique_ptr<Type> return_type;

  // bool has_where_clause;
  WhereClause where_clause;

  // should this store location info?

public:
  // Returns whether method decl has generic parameters.
  bool has_generics () const { return !generic_params.empty (); }

  // Returns whether method decl has regular parameters.
  bool has_params () const { return !function_params.empty (); }

  // Returns whether method has return type (otherwise is void).
  bool has_return_type () const { return return_type != nullptr; }

  // Returns whether method has a where clause.
  bool has_where_clause () const { return !where_clause.is_empty (); }

  Identifier get_identifier () const { return function_name; }

  // Mega-constructor
  TraitMethodDecl (Identifier function_name, FunctionQualifiers qualifiers,
		   std::vector<std::unique_ptr<GenericParam>> generic_params,
		   SelfParam self_param,
		   std::vector<FunctionParam> function_params,
		   std::unique_ptr<Type> return_type, WhereClause where_clause)
    : qualifiers (std::move (qualifiers)),
      function_name (std::move (function_name)),
      generic_params (std::move (generic_params)),
      self_param (std::move (self_param)),
      function_params (std::move (function_params)),
      return_type (std::move (return_type)),
      where_clause (std::move (where_clause))
  {}

  // Copy constructor with clone
  TraitMethodDecl (TraitMethodDecl const &other)
    : qualifiers (other.qualifiers), function_name (other.function_name),
      self_param (other.self_param), function_params (other.function_params),
      where_clause (other.where_clause)
  {
    // guard to prevent nullptr dereference
    if (other.return_type != nullptr)
      return_type = other.return_type->clone_type ();

    generic_params.reserve (other.generic_params.size ());
    for (const auto &e : other.generic_params)
      generic_params.push_back (e->clone_generic_param ());
  }

  ~TraitMethodDecl () = default;

  // Overloaded assignment operator with clone
  TraitMethodDecl &operator= (TraitMethodDecl const &other)
  {
    function_name = other.function_name;
    qualifiers = other.qualifiers;
    self_param = other.self_param;
    function_params = other.function_params;
    where_clause = other.where_clause;

    // guard to prevent nullptr dereference
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
  TraitMethodDecl (TraitMethodDecl &&other) = default;
  TraitMethodDecl &operator= (TraitMethodDecl &&other) = default;

  std::string as_string () const;

  // Invalid if method name is empty, so base stripping on that.
  void mark_for_strip () { function_name = ""; }
  bool is_marked_for_strip () const { return function_name.empty (); }

  // TODO: this mutable getter seems really dodgy. Think up better way.
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
  std::unique_ptr<Type> &get_return_type () { return return_type; }

  // TODO: is this better? Or is a "vis_block" better?
  WhereClause &get_where_clause () { return where_clause; }

  SelfParam &get_self_param () { return self_param; }
  const SelfParam &get_self_param () const { return self_param; }

  FunctionQualifiers get_qualifiers () { return qualifiers; }
};

// Actual trait item method declaration within traits
class TraitItemMethod : public TraitItem
{
  std::vector<Attribute> outer_attrs;
  TraitMethodDecl decl;
  std::unique_ptr<BlockExpr> block_expr;

public:
  // Returns whether method has a definition or is just a declaration.
  bool has_definition () const { return block_expr != nullptr; }

  TraitItemMethod (TraitMethodDecl decl, std::unique_ptr<BlockExpr> block_expr,
		   std::vector<Attribute> outer_attrs, Location locus)
    : TraitItem (locus), outer_attrs (std::move (outer_attrs)),
      decl (std::move (decl)), block_expr (std::move (block_expr))
  {}

  // Copy constructor with clone
  TraitItemMethod (TraitItemMethod const &other)
    : TraitItem (other.locus), outer_attrs (other.outer_attrs),
      decl (other.decl)
  {
    node_id = other.node_id;

    // guard to prevent null dereference
    if (other.block_expr != nullptr)
      block_expr = other.block_expr->clone_block_expr ();
  }

  // Overloaded assignment operator to clone
  TraitItemMethod &operator= (TraitItemMethod const &other)
  {
    TraitItem::operator= (other);
    outer_attrs = other.outer_attrs;
    decl = other.decl;
    locus = other.locus;
    node_id = other.node_id;

    // guard to prevent null dereference
    if (other.block_expr != nullptr)
      block_expr = other.block_expr->clone_block_expr ();
    else
      block_expr = nullptr;

    return *this;
  }

  // move constructors
  TraitItemMethod (TraitItemMethod &&other) = default;
  TraitItemMethod &operator= (TraitItemMethod &&other) = default;

  std::string as_string () const override;

  void accept_vis (ASTVisitor &vis) override;

  // Invalid if trait decl is empty, so base stripping on that.
  void mark_for_strip () override { decl.mark_for_strip (); }
  bool is_marked_for_strip () const override
  {
    return decl.is_marked_for_strip ();
  }

  // TODO: this mutable getter seems really dodgy. Think up better way.
  std::vector<Attribute> &get_outer_attrs () { return outer_attrs; }
  const std::vector<Attribute> &get_outer_attrs () const { return outer_attrs; }

  // TODO: is this better? Or is a "vis_block" better?
  TraitMethodDecl &get_trait_method_decl ()
  {
    // TODO: maybe only allow access if not marked for strip?
    return decl;
  }

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<BlockExpr> &get_definition () { return block_expr; }

protected:
  // Clone function implementation as (not pure) virtual method
  TraitItemMethod *clone_trait_item_impl () const override
  {
    return new TraitItemMethod (*this);
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
		  std::vector<Attribute> outer_attrs, Location locus)
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

  Location get_locus () const { return locus; }

  void accept_vis (ASTVisitor &vis) override;

  // Invalid if type is null, so base stripping on that.
  void mark_for_strip () override { type = nullptr; }
  bool is_marked_for_strip () const override { return type == nullptr; }

  // TODO: this mutable getter seems really dodgy. Think up better way.
  std::vector<Attribute> &get_outer_attrs () { return outer_attrs; }
  const std::vector<Attribute> &get_outer_attrs () const { return outer_attrs; }

  bool has_expr () const { return expr != nullptr; }

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<Expr> &get_expr ()
  {
    rust_assert (has_expr ());
    return expr;
  }

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<Type> &get_type ()
  {
    rust_assert (type != nullptr);
    return type;
  }

  Identifier get_identifier () const { return name; }

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
		 std::vector<Attribute> outer_attrs, Location locus)
    : TraitItem (locus), outer_attrs (std::move (outer_attrs)),
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
  void mark_for_strip () override { name = ""; }
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
  TraitItemType *clone_trait_item_impl () const override
  {
    return new TraitItemType (*this);
  }
};

// Rust trait item declaration AST node
class Trait : public VisItem
{
  bool has_unsafe;
  Identifier name;
  std::vector<std::unique_ptr<GenericParam>> generic_params;
  std::vector<std::unique_ptr<TypeParamBound>> type_param_bounds;
  WhereClause where_clause;
  std::vector<Attribute> inner_attrs;
  std::vector<std::unique_ptr<TraitItem>> trait_items;
  Location locus;

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

  // Mega-constructor
  Trait (Identifier name, bool is_unsafe,
	 std::vector<std::unique_ptr<GenericParam>> generic_params,
	 std::vector<std::unique_ptr<TypeParamBound>> type_param_bounds,
	 WhereClause where_clause,
	 std::vector<std::unique_ptr<TraitItem>> trait_items, Visibility vis,
	 std::vector<Attribute> outer_attrs, std::vector<Attribute> inner_attrs,
	 Location locus)
    : VisItem (std::move (vis), std::move (outer_attrs)),
      has_unsafe (is_unsafe), name (std::move (name)),
      generic_params (std::move (generic_params)),
      type_param_bounds (std::move (type_param_bounds)),
      where_clause (std::move (where_clause)),
      inner_attrs (std::move (inner_attrs)),
      trait_items (std::move (trait_items)), locus (locus)
  {}

  // Copy constructor with vector clone
  Trait (Trait const &other)
    : VisItem (other), has_unsafe (other.has_unsafe), name (other.name),
      where_clause (other.where_clause), inner_attrs (other.inner_attrs),
      locus (other.locus)
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
    has_unsafe = other.has_unsafe;
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
      trait_items.push_back (e->clone_trait_item ());

    return *this;
  }

  // default move constructors
  Trait (Trait &&other) = default;
  Trait &operator= (Trait &&other) = default;

  Location get_locus () const override final { return locus; }

  void accept_vis (ASTVisitor &vis) override;

  // Invalid if trait name is empty, so base stripping on that.
  void mark_for_strip () override { name = ""; }
  bool is_marked_for_strip () const override { return name.empty (); }

  // TODO: think of better way to do this
  const std::vector<Attribute> &get_inner_attrs () const { return inner_attrs; }
  std::vector<Attribute> &get_inner_attrs () { return inner_attrs; }

  const std::vector<std::unique_ptr<TraitItem>> &get_trait_items () const
  {
    return trait_items;
  }
  std::vector<std::unique_ptr<TraitItem>> &get_trait_items ()
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
  Location locus;

public:
  // Returns whether impl has generic parameters.
  bool has_generics () const { return !generic_params.empty (); }

  // Returns whether impl has where clause.
  bool has_where_clause () const { return !where_clause.is_empty (); }

  // Returns whether impl has inner attributes.
  bool has_inner_attrs () const { return !inner_attrs.empty (); }

  Location get_locus () const override final { return locus; }

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
  std::unique_ptr<Type> &get_type ()
  {
    rust_assert (trait_type != nullptr);
    return trait_type;
  }

protected:
  // Mega-constructor
  Impl (std::vector<std::unique_ptr<GenericParam>> generic_params,
	std::unique_ptr<Type> trait_type, WhereClause where_clause,
	Visibility vis, std::vector<Attribute> inner_attrs,
	std::vector<Attribute> outer_attrs, Location locus)
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
  std::vector<std::unique_ptr<InherentImplItem>> impl_items;

public:
  std::string as_string () const override;

  // Returns whether inherent impl block has inherent impl items.
  bool has_impl_items () const { return !impl_items.empty (); }

  // Mega-constructor
  InherentImpl (std::vector<std::unique_ptr<InherentImplItem>> impl_items,
		std::vector<std::unique_ptr<GenericParam>> generic_params,
		std::unique_ptr<Type> trait_type, WhereClause where_clause,
		Visibility vis, std::vector<Attribute> inner_attrs,
		std::vector<Attribute> outer_attrs, Location locus)
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
      impl_items.push_back (e->clone_inherent_impl_item ());
  }

  // Overloaded assignment operator with vector clone
  InherentImpl &operator= (InherentImpl const &other)
  {
    Impl::operator= (other);

    impl_items.reserve (other.impl_items.size ());
    for (const auto &e : other.impl_items)
      impl_items.push_back (e->clone_inherent_impl_item ());

    return *this;
  }

  // default move constructors
  InherentImpl (InherentImpl &&other) = default;
  InherentImpl &operator= (InherentImpl &&other) = default;

  void accept_vis (ASTVisitor &vis) override;

  // TODO: think of better way to do this
  const std::vector<std::unique_ptr<InherentImplItem>> &get_impl_items () const
  {
    return impl_items;
  }
  std::vector<std::unique_ptr<InherentImplItem>> &get_impl_items ()
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
  std::vector<std::unique_ptr<TraitImplItem>> impl_items;

public:
  std::string as_string () const override;

  // Returns whether trait impl has impl items.
  bool has_impl_items () const { return !impl_items.empty (); }

  // Mega-constructor
  TraitImpl (TypePath trait_path, bool is_unsafe, bool has_exclam,
	     std::vector<std::unique_ptr<TraitImplItem>> impl_items,
	     std::vector<std::unique_ptr<GenericParam>> generic_params,
	     std::unique_ptr<Type> trait_type, WhereClause where_clause,
	     Visibility vis, std::vector<Attribute> inner_attrs,
	     std::vector<Attribute> outer_attrs, Location locus)
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
      impl_items.push_back (e->clone_trait_impl_item ());
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
      impl_items.push_back (e->clone_trait_impl_item ());

    return *this;
  }

  // move constructors
  TraitImpl (TraitImpl &&other) = default;
  TraitImpl &operator= (TraitImpl &&other) = default;

  void accept_vis (ASTVisitor &vis) override;

  bool is_unsafe () const { return has_unsafe; };
  bool is_exclam () const { return has_exclam; }

  // TODO: think of better way to do this
  const std::vector<std::unique_ptr<TraitImplItem>> &get_impl_items () const
  {
    return impl_items;
  }
  std::vector<std::unique_ptr<TraitImplItem>> &get_impl_items ()
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
  Location locus;

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

  Location get_locus () const override final { return locus; }

  virtual void accept_vis (ASTVisitor &vis) = 0;

  // TODO: make virtual? Would be more flexible.
  // Based on idea that name should never be empty.
  void mark_for_strip () { item_name = ""; };
  bool is_marked_for_strip () const { return item_name.empty (); };

protected:
  ExternalItem (Identifier item_name, Visibility vis,
		std::vector<Attribute> outer_attrs, Location locus)
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

// A static item used in an extern block
class ExternalStaticItem : public ExternalItem
{
  // bool has_outer_attrs;
  std::vector<Attribute> outer_attrs;

  // bool has_visibility;
  Visibility visibility;

  Identifier item_name;
  Location locus;

  bool has_mut;
  std::unique_ptr<Type> item_type;

public:
  ExternalStaticItem (Identifier item_name, std::unique_ptr<Type> item_type,
		      bool is_mut, Visibility vis,
		      std::vector<Attribute> outer_attrs, Location locus)
    : ExternalItem (), outer_attrs (std::move (outer_attrs)),
      visibility (std::move (vis)), item_name (std::move (item_name)),
      locus (locus), has_mut (is_mut), item_type (std::move (item_type))
  {}

  // Copy constructor
  ExternalStaticItem (ExternalStaticItem const &other)
    : outer_attrs (other.outer_attrs), visibility (other.visibility),
      item_name (other.item_name), locus (other.locus), has_mut (other.has_mut)
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

  Location get_locus () const { return locus; }

  // Based on idea that type should never be null.
  void mark_for_strip () override { item_type = nullptr; };
  bool is_marked_for_strip () const override { return item_type == nullptr; };

  // TODO: this mutable getter seems really dodgy. Think up better way.
  std::vector<Attribute> &get_outer_attrs () { return outer_attrs; }
  const std::vector<Attribute> &get_outer_attrs () const { return outer_attrs; }

  // TODO: is this better? Or is a "vis_block" better?
  std::unique_ptr<Type> &get_type ()
  {
    rust_assert (item_type != nullptr);
    return item_type;
  }

  Identifier get_identifier () const { return item_name; }

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
struct NamedFunctionParam
{
private:
  // bool has_name;   // otherwise is _
  std::string name;

  std::unique_ptr<Type> param_type;

  // seemingly new since writing this node
  std::vector<Attribute> outer_attrs;

  NodeId node_id;
  Location locus;

public:
  /* Returns whether the named function parameter has a name (i.e. name is not
   * '_'). */
  bool has_name () const { return name != "_"; }

  bool has_outer_attrs () const { return !outer_attrs.empty (); }

  // Returns whether the named function parameter is in an error state.
  bool is_error () const
  {
    // also if identifier is "" but that is probably more costly to compute
    return param_type == nullptr;
  }

  std::string get_name () const { return name; }

  // Creates an error state named function parameter.
  static NamedFunctionParam create_error ()
  {
    return NamedFunctionParam ("", nullptr, {}, Location ());
  }

  NamedFunctionParam (std::string name, std::unique_ptr<Type> param_type,
		      std::vector<Attribute> outer_attrs, Location locus)
    : name (std::move (name)), param_type (std::move (param_type)),
      outer_attrs (std::move (outer_attrs)),
      node_id (Analysis::Mappings::get ()->get_next_node_id ()), locus (locus)
  {}

  // Copy constructor
  NamedFunctionParam (NamedFunctionParam const &other)
    : name (other.name), outer_attrs (other.outer_attrs)
  {
    node_id = other.node_id;
    // guard to prevent null dereference (only required if error state)
    if (other.param_type != nullptr)
      param_type = other.param_type->clone_type ();
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
  std::unique_ptr<Type> &get_type ()
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
  Location locus;

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
  std::vector<Attribute> variadic_outer_attrs;

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
  bool is_variadic () const { return has_variadics; }

  // Returns whether item has outer attributes on its variadic parameters.
  bool has_variadic_outer_attrs () const
  {
    return !variadic_outer_attrs.empty ();
  }

  Location get_locus () const { return locus; }

  Visibility &get_visibility () { return visibility; }
  const Visibility &get_visibility () const { return visibility; }

  ExternalFunctionItem (
    Identifier item_name,
    std::vector<std::unique_ptr<GenericParam>> generic_params,
    std::unique_ptr<Type> return_type, WhereClause where_clause,
    std::vector<NamedFunctionParam> function_params, bool has_variadics,
    std::vector<Attribute> variadic_outer_attrs, Visibility vis,
    std::vector<Attribute> outer_attrs, Location locus)
    : ExternalItem (), outer_attrs (std::move (outer_attrs)),
      visibility (std::move (vis)), item_name (std::move (item_name)),
      locus (locus), generic_params (std::move (generic_params)),
      return_type (std::move (return_type)),
      where_clause (std::move (where_clause)),
      function_params (std::move (function_params)),
      has_variadics (has_variadics),
      variadic_outer_attrs (std::move (variadic_outer_attrs))
  {
    // TODO: assert that if has variadic outer attrs, then has_variadics is
    // true?
  }

  // Copy constructor with clone
  ExternalFunctionItem (ExternalFunctionItem const &other)
    : outer_attrs (other.outer_attrs), visibility (other.visibility),
      item_name (other.item_name), locus (other.locus),
      where_clause (other.where_clause),
      function_params (other.function_params),
      has_variadics (other.has_variadics),
      variadic_outer_attrs (other.variadic_outer_attrs)
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
    has_variadics = other.has_variadics;
    variadic_outer_attrs = other.variadic_outer_attrs;
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
  void mark_for_strip () override { item_name = ""; };
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
  std::unique_ptr<Type> &get_return_type ()
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

  Location locus;

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
	       std::vector<Attribute> outer_attrs, Location locus)
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

  Location get_locus () const override final { return locus; }

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
