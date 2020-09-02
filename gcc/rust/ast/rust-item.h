#ifndef RUST_AST_ITEM_H
#define RUST_AST_ITEM_H
/*
Copyright (C) 2009-2020 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>. */

#include "rust-ast.h"
#include "rust-path.h"

namespace Rust {
namespace AST {
// forward decls
class BlockExpr;
class TypePath;
class MacroInvocationSemi;

// TODO: inline?
/*struct AbiName {
    std::string abi_name;
    // Technically is meant to be STRING_LITERAL or RAW_STRING_LITERAL

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
    : outer_attr (std::move (outer_attr)),
      type_representation (std::move (type_representation)),
      type_param_bounds (std::move (type_param_bounds)),
      type (std::move (type)), locus (locus)
  {}

  // Copy constructor uses clone
  TypeParam (TypeParam const &other)
    : outer_attr (other.outer_attr),
      type_representation (other.type_representation),
      type (other.type->clone_type ()), locus (other.locus)
  {
    type_param_bounds.reserve (other.type_param_bounds.size ());
    for (const auto &e : other.type_param_bounds)
      type_param_bounds.push_back (e->clone_type_param_bound ());
  }

  // Overloaded assignment operator to clone
  TypeParam &operator= (TypeParam const &other)
  {
    type_representation = other.type_representation;
    // type_param_bounds = other.type_param_bounds;
    type = other.type->clone_type ();
    outer_attr = other.outer_attr;
    locus = other.locus;

    type_param_bounds.reserve (other.type_param_bounds.size ());
    for (const auto &e : other.type_param_bounds)
      type_param_bounds.push_back (e->clone_type_param_bound ());

    return *this;
  }

  // move constructors
  TypeParam (TypeParam &&other) = default;
  TypeParam &operator= (TypeParam &&other) = default;

  std::string as_string () const override;

  Location get_locus () const { return locus; }

  void accept_vis (ASTVisitor &vis) override;

protected:
  // Clone function implementation as (not pure) virtual method
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

protected:
  // Clone function implementation as pure virtual method
  virtual WhereClauseItem *clone_where_clause_item_impl () const = 0;
};

// A lifetime where clause item
class LifetimeWhereClauseItem : public WhereClauseItem
{
  Lifetime lifetime;

  // LifetimeBounds lifetime_bounds;
  std::vector<Lifetime> lifetime_bounds; // inlined lifetime bounds

  // should this store location info?

public:
  LifetimeWhereClauseItem (Lifetime lifetime,
			   std::vector<Lifetime> lifetime_bounds)
    : lifetime (std::move (lifetime)),
      lifetime_bounds (std::move (lifetime_bounds))
  {}

  std::string as_string () const override;

  void accept_vis (ASTVisitor &vis) override;

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
  // bool has_for_lifetimes;
  // LifetimeParams for_lifetimes;
  std::vector<LifetimeParam> for_lifetimes; // inlined

  std::unique_ptr<Type> bound_type;

  // bool has_type_param_bounds;
  // TypeParamBounds type_param_bounds;
  std::vector<std::unique_ptr<TypeParamBound>>
    type_param_bounds; // inlined form

  // should this store location info?

public:
  // Returns whether the item has ForLifetimes
  bool has_for_lifetimes () const { return !for_lifetimes.empty (); }

  // Returns whether the item has type param bounds
  bool has_type_param_bounds () const { return !type_param_bounds.empty (); }

  TypeBoundWhereClauseItem (
    std::vector<LifetimeParam> for_lifetimes, std::unique_ptr<Type> bound_type,
    std::vector<std::unique_ptr<TypeParamBound>> type_param_bounds)
    : for_lifetimes (std::move (for_lifetimes)),
      bound_type (std::move (bound_type)),
      type_param_bounds (std::move (type_param_bounds))
  {}

  // Copy constructor requires clone
  TypeBoundWhereClauseItem (TypeBoundWhereClauseItem const &other)
    : for_lifetimes (other.for_lifetimes),
      bound_type (other.bound_type->clone_type ())
  {
    type_param_bounds.reserve (other.type_param_bounds.size ());
    for (const auto &e : other.type_param_bounds)
      type_param_bounds.push_back (e->clone_type_param_bound ());
  }

  // Overload assignment operator to clone
  TypeBoundWhereClauseItem &operator= (TypeBoundWhereClauseItem const &other)
  {
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
  WhereClause (
    std::vector<std::unique_ptr<WhereClauseItem>> where_clause_items)
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

  Location locus;

  // Unrestricted constructor used for error state
  SelfParam (Lifetime lifetime, bool has_ref, bool is_mut, Type *type)
    : has_ref (has_ref), is_mut (is_mut), lifetime (std::move (lifetime)),
      type (type)
  {}
  // this is ok as no outside classes can ever call this

public:
  // Returns whether the self-param has a type field.
  bool has_type () const { return type != nullptr; }

  // Returns whether the self-param has a valid lifetime.
  bool has_lifetime () const { return !lifetime.is_error (); }

  // Returns whether the self-param is in an error state.
  bool is_error () const
  {
    return has_type () && has_lifetime ();
    // not having either is not an error
  }

  // Creates an error state self-param.
  static SelfParam create_error ()
  {
    /* HACK: creates a dummy type. Since it's a unique pointer, it should
     * clean it up, but it still allocates memory, which is not ideal. */
    return SelfParam (Lifetime (Lifetime::STATIC), false, false,
		      new QualifiedPathInType (
			QualifiedPathInType::create_error ()));
    /* FIXME: is there a reason why I didn't just create a null pointer? Is it
     * due to error being having both a type and a lifetime? If it is, wouldn't
     * something like "not has_ref and has lifetime" for error be better? */
  }

  // Type-based self parameter (not ref, no lifetime)
  SelfParam (std::unique_ptr<Type> type, bool is_mut, Location locus)
    : has_ref (false), is_mut (is_mut), lifetime (Lifetime::error ()),
      type (std::move (type)), locus (locus)
  {}

  // Lifetime-based self parameter (is ref, no type)
  SelfParam (Lifetime lifetime, bool is_mut, Location locus)
    : has_ref (true), is_mut (is_mut), lifetime (std::move (lifetime)),
      locus (locus)
  {}

  // Copy constructor requires clone
  SelfParam (SelfParam const &other)
    : has_ref (other.has_ref), is_mut (other.is_mut), lifetime (other.lifetime),
      locus (other.locus)
  {
    if (other.type != nullptr)
      type = other.type->clone_type ();
  }

  // Overload assignment operator to use clone
  SelfParam &operator= (SelfParam const &other)
  {
    if (other.type != nullptr)
      type = other.type->clone_type ();
    is_mut = other.is_mut;
    has_ref = other.has_ref;
    lifetime = other.lifetime;
    locus = other.locus;

    return *this;
  }

  // move constructors
  SelfParam (SelfParam &&other) = default;
  SelfParam &operator= (SelfParam &&other) = default;

  std::string as_string () const;

  Location get_locus () const { return locus; }
};

// Qualifiers for function, i.e. const, unsafe, extern etc.
struct FunctionQualifiers
{
public:
  /* Whether the function is neither const nor async, const only, or async
   * only. */
  enum AsyncConstStatus
  {
    NONE,
    CONST,
    ASYNC
  };

private:
  AsyncConstStatus const_status;
  bool has_unsafe;
  bool has_extern;
  std::string extern_abi; // e.g. extern "C" fn() -> i32 {}
  // TODO: maybe ensure that extern_abi only exists if extern exists?

  // should this store location info?

public:
  FunctionQualifiers (AsyncConstStatus const_status, bool has_unsafe,
		      bool has_extern = false,
		      std::string extern_abi = std::string ())
    : const_status (const_status), has_unsafe (has_unsafe),
      has_extern (has_extern), extern_abi (std::move (extern_abi))
  {
    if (!this->extern_abi.empty ())
      {
	// having extern is required; not having it is an implementation error
	gcc_assert (has_extern);
      }
  }

  std::string as_string () const;
};

// A function parameter
struct FunctionParam
{
public:
  std::unique_ptr<Pattern> param_name;
  std::unique_ptr<Type> type;

  Location locus;

  FunctionParam (std::unique_ptr<Pattern> param_name,
		 std::unique_ptr<Type> param_type, Location locus)
    : param_name (std::move (param_name)), type (std::move (param_type)),
      locus (locus)
  {}

  // Copy constructor uses clone
  FunctionParam (FunctionParam const &other)
    : param_name (other.param_name->clone_pattern ()),
      type (other.type->clone_type ()), locus (other.locus)
  {}

  // Overload assignment operator to use clone
  FunctionParam &operator= (FunctionParam const &other)
  {
    param_name = other.param_name->clone_pattern ();
    type = other.type->clone_type ();
    locus = other.locus;

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
    return FunctionParam (nullptr, nullptr, Location ());
  }

  std::string as_string () const;

  Location get_locus () const { return locus; }
};

// Visibility of item - if the item has it, then it is some form of public
struct Visibility
{
public:
  enum PublicVisType
  {
    NONE,
    CRATE,
    SELF,
    SUPER,
    IN_PATH
  };

private:
  // if vis is public, one of these
  PublicVisType public_vis_type;
  // Only assigned if public_vis_type is IN_PATH
  SimplePath in_path;

  // should this store location info?

public:
  // Creates a Visibility - TODO make constructor protected or private?
  Visibility (PublicVisType public_vis_type, SimplePath in_path)
    : public_vis_type (public_vis_type), in_path (std::move (in_path))
  {}

  // Returns whether visibility is in an error state.
  bool is_error () const
  {
    return public_vis_type == IN_PATH && in_path.is_empty ();
  }

  // Creates an error visibility.
  static Visibility create_error ()
  {
    return Visibility (IN_PATH, SimplePath::create_empty ());
  }

  // Unique pointer custom clone function
  /*std::unique_ptr<Visibility> clone_visibility() const {
      return std::unique_ptr<Visibility>(clone_visibility_impl());
  }*/

  /* TODO: think of a way to only allow valid Visibility states - polymorphism
   * is one idea but may be too resource-intensive. */

  // Creates a public visibility with no further features/arguments.
  static Visibility create_public ()
  {
    return Visibility (NONE, SimplePath::create_empty ());
  }

  // Creates a public visibility with crate-relative paths or whatever.
  static Visibility create_crate ()
  {
    return Visibility (CRATE, SimplePath::create_empty ());
  }

  // Creates a public visibility with self-relative paths or whatever.
  static Visibility create_self ()
  {
    return Visibility (SELF, SimplePath::create_empty ());
  }

  // Creates a public visibility with parent module-relative paths or
  // whatever.
  static Visibility create_super ()
  {
    return Visibility (SUPER, SimplePath::create_empty ());
  }

  // Creates a public visibility with a given path or whatever.
  static Visibility create_in_path (SimplePath in_path)
  {
    return Visibility (IN_PATH, std::move (in_path));
  }

  std::string as_string () const;

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
  // moved from impl items for consistency
  std::vector<Attribute> outer_attrs;
  Visibility vis;

  FunctionQualifiers qualifiers;
  Identifier method_name;

  // bool has_generics;
  // Generics generic_params;
  std::vector<std::unique_ptr<GenericParam>> generic_params; // inlined

  SelfParam self_param;

  // bool has_params;
  // FunctionParams function_params;
  std::vector<FunctionParam> function_params; // inlined

  // bool has_return_type;
  // FunctionReturnType return_type;
  std::unique_ptr<Type> return_type; // inlined

  // bool has_where_clause;
  WhereClause where_clause;

  std::unique_ptr<BlockExpr> expr;

  Location locus;

public:
  // Returns whether the method is in an error state.
  bool is_error () const
  {
    return expr == nullptr || method_name.empty () || self_param.is_error ();
  }

  // Creates an error state method.
  static Method create_error ()
  {
    return Method ("", FunctionQualifiers (FunctionQualifiers::NONE, true),
		   std::vector<std::unique_ptr<GenericParam>> (),
		   SelfParam::create_error (), std::vector<FunctionParam> (),
		   nullptr, WhereClause::create_empty (), nullptr,
		   Visibility::create_error (), std::vector<Attribute> ());
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
	  std::vector<Attribute> outer_attrs, Location locus = Location ())
    : outer_attrs (std::move (outer_attrs)), vis (std::move (vis)),
      qualifiers (std::move (qualifiers)),
      method_name (std::move (method_name)),
      generic_params (std::move (generic_params)),
      self_param (std::move (self_param)),
      function_params (std::move (function_params)),
      return_type (std::move (return_type)),
      where_clause (std::move (where_clause)), expr (std::move (function_body)),
      locus (locus)
  {}

  // TODO: add constructor with less fields

  // Copy constructor with clone
  Method (Method const &other)
    : outer_attrs (other.outer_attrs), vis (other.vis),
      qualifiers (other.qualifiers), method_name (other.method_name),
      self_param (other.self_param), function_params (other.function_params),
      return_type (other.return_type->clone_type ()),
      where_clause (other.where_clause), expr (other.expr->clone_block_expr ()),
      locus (other.locus)
  {
    generic_params.reserve (other.generic_params.size ());
    for (const auto &e : other.generic_params)
      generic_params.push_back (e->clone_generic_param ());
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
    return_type = other.return_type->clone_type ();
    where_clause = other.where_clause;
    expr = other.expr->clone_block_expr ();
    locus = other.locus;

    generic_params.reserve (other.generic_params.size ());
    for (const auto &e : other.generic_params)
      generic_params.push_back (e->clone_generic_param ());

    return *this;
  }

  // move constructors
  Method (Method &&other) = default;
  Method &operator= (Method &&other) = default;

  std::string as_string () const override;

  void accept_vis (ASTVisitor &vis) override;

protected:
  /* Use covariance to implement clone function as returning this object
   * rather than base */
  Method *clone_inherent_impl_item_impl () const override
  {
    return new Method (*this);
  }

  /* Use covariance to implement clone function as returning this object
   * rather than base */
  Method *clone_trait_impl_item_impl () const override
  {
    return new Method (*this);
  }
};

// Item that supports visibility - abstract base class
class VisItem : public Item
{
  Visibility visibility;

protected:
  // Visibility constructor
  VisItem (Visibility visibility,
	   std::vector<Attribute> outer_attrs = std::vector<Attribute> ())
    : Item (std::move (outer_attrs)), visibility (std::move (visibility))
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
  /* Does the item have some kind of public visibility (non-default
   * visibility)? */
  bool has_visibility () const { return !visibility.is_error (); }

  std::string as_string () const override;
};

// Rust module item - abstract base class
class Module : public VisItem
{
  Identifier module_name;
  Location locus;

protected:
  // Protected constructor
  Module (Identifier module_name, Visibility visibility, Location locus,
	  std::vector<Attribute> outer_attrs = std::vector<Attribute> ())
    : VisItem (std::move (visibility), std::move (outer_attrs)),
      module_name (module_name), locus (locus)
  {}

public:
  std::string as_string () const override;

  Location get_locus () const { return locus; }
};

// Module with a body, defined in file
class ModuleBodied : public Module
{
  // bool has_inner_attrs;
  std::vector<Attribute> inner_attrs;
  // bool has_items;
  std::vector<std::unique_ptr<Item>> items;

public:
  std::string as_string () const override;

  // Returns whether the module has items in its body.
  bool has_items () const { return !items.empty (); }

  // Returns whether the module has any inner attributes.
  bool has_inner_attrs () const { return !inner_attrs.empty (); }

  // Full constructor
  ModuleBodied (Identifier name, Location locus,
		std::vector<std::unique_ptr<Item>> items
		= std::vector<std::unique_ptr<Item>> (),
		Visibility visibility = Visibility::create_error (),
		std::vector<Attribute> inner_attrs = std::vector<Attribute> (),
		std::vector<Attribute> outer_attrs = std::vector<Attribute> ())
    : Module (std::move (name), std::move (visibility), locus,
	      std::move (outer_attrs)),
      inner_attrs (std::move (inner_attrs)), items (std::move (items))
  {}

  // Copy constructor with vector clone
  ModuleBodied (ModuleBodied const &other)
    : Module (other), inner_attrs (other.inner_attrs)
  {
    items.reserve (other.items.size ());
    for (const auto &e : other.items)
      items.push_back (e->clone_item ());
  }

  // Overloaded assignment operator with vector clone
  ModuleBodied &operator= (ModuleBodied const &other)
  {
    Module::operator= (other);
    inner_attrs = other.inner_attrs;

    items.reserve (other.items.size ());
    for (const auto &e : other.items)
      items.push_back (e->clone_item ());

    return *this;
  }

  // move constructors
  ModuleBodied (ModuleBodied &&other) = default;
  ModuleBodied &operator= (ModuleBodied &&other) = default;

  void accept_vis (ASTVisitor &vis) override;

  /* Override that runs the function recursively on all items contained within
   * the module. */
  void add_crate_name (std::vector<std::string> &names) const override;

protected:
  /* Use covariance to implement clone function as returning this object
   * rather than base */
  ModuleBodied *clone_item_impl () const override
  {
    return new ModuleBodied (*this);
  }

  /* Use covariance to implement clone function as returning this object
   * rather than base */
  /*virtual ModuleBodied* clone_statement_impl() const override {
      return new ModuleBodied(*this);
  }*/
};

// Module without a body, loaded from external file
class ModuleNoBody : public Module
{
public:
  std::string as_string () const override;

  // Full constructor
  ModuleNoBody (Identifier name, Visibility visibility,
		std::vector<Attribute> outer_attrs, Location locus)
    : Module (std::move (name), std::move (visibility), locus,
	      std::move (outer_attrs))
  {}

  void accept_vis (ASTVisitor &vis) override;

protected:
  /* Use covariance to implement clone function as returning this object
   * rather than base */
  ModuleNoBody *clone_item_impl () const override
  {
    return new ModuleNoBody (*this);
  }

  /* Use covariance to implement clone function as returning this object
   * rather than base */
  /*virtual ModuleNoBody* clone_statement_impl() const override {
      return new ModuleNoBody(*this);
  }*/
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

  Location get_locus () const { return locus; }

  void accept_vis (ASTVisitor &vis) override;

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
class UseTree
{
  Location locus;

public:
  virtual ~UseTree () {}

  // Unique pointer custom clone function
  std::unique_ptr<UseTree> clone_use_tree () const
  {
    return std::unique_ptr<UseTree> (clone_use_tree_impl ());
  }

  virtual std::string as_string () const = 0;

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
	gcc_assert (!has_path ());
      }
    // TODO: do path-prefixed paths also have to have a path? If so, have an
    // assert for that too.
  }

  /* Returns whether has path. Should be made redundant by PathType
   * PATH_PREFIXED. */
  bool has_path () const { return !path.is_empty (); }

  std::string as_string () const override;

  void accept_vis (ASTVisitor &vis) override;

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

  void accept_vis (ASTVisitor &vis) override;

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

  Location get_locus () const { return locus; }

  void accept_vis (ASTVisitor &vis) override;

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

// Parameters used in a function - TODO inline?
/*struct FunctionParams {
    std::vector<FunctionParam> function_params;
};*/

class LetStmt;

// Rust function declaration AST node
class Function : public VisItem, public InherentImplItem, public TraitImplItem
{
public:
  FunctionQualifiers qualifiers;
  Identifier function_name;

  // bool has_generics;
  // Generics generic_params;
  std::vector<std::unique_ptr<GenericParam>> generic_params; // inlined

  // bool has_function_params;
  // FunctionParams function_params;
  std::vector<FunctionParam> function_params; // inlined

  // bool has_function_return_type;
  std::unique_ptr<Type> return_type;

  // bool has_where_clause;
  WhereClause where_clause;

  std::unique_ptr<BlockExpr> function_body;

  Location locus;

  std::vector<LetStmt *> locals;

  std::string as_string () const override;

  // Returns whether function has generic parameters.
  bool has_generics () const { return !generic_params.empty (); }

  // Returns whether function has regular parameters.
  bool has_function_params () const { return !function_params.empty (); }

  // Returns whether function has return type - if not, it is void.
  bool has_function_return_type () const { return return_type != nullptr; }

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
      return_type (other.return_type->clone_type ()),
      where_clause (other.where_clause),
      function_body (other.function_body->clone_block_expr ()),
      locus (other.locus)
  {
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
    // generic_params = other.generic_params;
    function_params = other.function_params;
    return_type = other.return_type->clone_type ();
    where_clause = other.where_clause;
    function_body = other.function_body->clone_block_expr ();
    // visibility = other.visibility->clone_visibility();
    // outer_attrs = other.outer_attrs;
    locus = other.locus;

    generic_params.reserve (other.generic_params.size ());
    for (const auto &e : other.generic_params)
      generic_params.push_back (e->clone_generic_param ());

    return *this;
  }

  // move constructors
  Function (Function &&other) = default;
  Function &operator= (Function &&other) = default;

  Location get_locus () const { return locus; }

  void accept_vis (ASTVisitor &vis) override;

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

  /* Use covariance to implement clone function as returning this object
   * rather than base */
  /*virtual Function* clone_statement_impl() const override {
      return new Function(*this);
  }*/
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
    // generic_params = other.generic_params;
    where_clause = other.where_clause;
    existing_type = other.existing_type->clone_type ();
    // visibility = other.visibility->clone_visibility();
    // outer_attrs = other.outer_attrs;
    locus = other.locus;

    generic_params.reserve (other.generic_params.size ());
    for (const auto &e : other.generic_params)
      generic_params.push_back (e->clone_generic_param ());

    return *this;
  }

  // move constructors
  TypeAlias (TypeAlias &&other) = default;
  TypeAlias &operator= (TypeAlias &&other) = default;

  Location get_locus () const { return locus; }

  void accept_vis (ASTVisitor &vis) override;

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

  /* Use covariance to implement clone function as returning this object
   * rather than base */
  /*virtual TypeAlias* clone_statement_impl() const override {
      return new TypeAlias(*this);
  }*/
};

// Rust base struct declaration AST node - abstract base class
class Struct : public VisItem
{
public:
  // protected to enable access by derived classes - allows better as_string
  Identifier struct_name;

  // bool has_generics;
  // Generics generic_params;
  std::vector<std::unique_ptr<GenericParam>> generic_params; // inlined

  // bool has_where_clause;
  WhereClause where_clause;

  Location locus;

  // Returns whether struct has generic parameters.
  bool has_generics () const { return !generic_params.empty (); }

  // Returns whether struct has a where clause.
  bool has_where_clause () const { return !where_clause.is_empty (); }

  Location get_locus () const { return locus; }

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
public:
  // bool has_outer_attributes;
  std::vector<Attribute> outer_attrs;

  // bool has_visibility;
  Visibility visibility;

  Identifier field_name;
  std::unique_ptr<Type> field_type;

  // should this store location info?

  // Returns whether struct field has any outer attributes.
  bool has_outer_attributes () const { return !outer_attrs.empty (); }

  // Returns whether struct field has a non-private (non-default) visibility.
  bool has_visibility () const { return !visibility.is_error (); }

  StructField (Identifier field_name, std::unique_ptr<Type> field_type,
	       Visibility vis,
	       std::vector<Attribute> outer_attrs = std::vector<Attribute> ())
    : outer_attrs (std::move (outer_attrs)), visibility (std::move (vis)),
      field_name (std::move (field_name)), field_type (std::move (field_type))
  {}

  // Copy constructor
  StructField (StructField const &other)
    : outer_attrs (other.outer_attrs), visibility (other.visibility),
      field_name (other.field_name),
      field_type (other.field_type->clone_type ())
  {}

  ~StructField () = default;

  // Overloaded assignment operator to clone
  StructField &operator= (StructField const &other)
  {
    field_name = other.field_name;
    field_type = other.field_type->clone_type ();
    visibility = other.visibility;
    outer_attrs = other.outer_attrs;

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
    return StructField (std::string (""), nullptr, Visibility::create_error ());
  }

  std::string as_string () const;
};

// Rust struct declaration with true struct type AST node
class StructStruct : public Struct
{
public:
  std::vector<StructField> fields;
  bool is_unit;

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
  // TODO: can a unit struct have generic fields? assuming yes for now.

  /* Returns whether the struct is a unit struct - struct defined without
   * fields. This is important because it also means an implicit constant of its
   * type is defined. */
  bool is_unit_struct () const { return is_unit; }

  void accept_vis (ASTVisitor &vis) override;

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
struct TupleField
{
private:
  // bool has_outer_attributes;
  std::vector<Attribute> outer_attrs;

  // bool has_visibility;
  Visibility visibility;

  std::unique_ptr<Type> field_type;

  // should this store location info?

public:
  // Returns whether tuple field has outer attributes.
  bool has_outer_attributes () const { return !outer_attrs.empty (); }

  /* Returns whether tuple field has a non-default visibility (i.e. a public
   * one) */
  bool has_visibility () const { return !visibility.is_error (); }

  // Complete constructor
  TupleField (std::unique_ptr<Type> field_type, Visibility vis,
	      std::vector<Attribute> outer_attrs = std::vector<Attribute> ())
    : outer_attrs (std::move (outer_attrs)), visibility (std::move (vis)),
      field_type (std::move (field_type))
  {}

  // Copy constructor with clone
  TupleField (TupleField const &other)
    : outer_attrs (other.outer_attrs), visibility (other.visibility),
      field_type (other.field_type->clone_type ())
  {}

  ~TupleField () = default;

  // Overloaded assignment operator to clone
  TupleField &operator= (TupleField const &other)
  {
    field_type = other.field_type->clone_type ();
    visibility = other.visibility;
    outer_attrs = other.outer_attrs;

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
    return TupleField (nullptr, Visibility::create_error ());
  }

  std::string as_string () const;
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
 * name-only enum */
class EnumItem
{
  // bool has_attrs;
  std::vector<Attribute> outer_attrs;

  Identifier variant_name;

  Location locus;

public:
  virtual ~EnumItem () {}

  // Returns whether enum item has outer attributes.
  bool has_outer_attrs () const { return !outer_attrs.empty (); }

  EnumItem (Identifier variant_name, std::vector<Attribute> outer_attrs,
	    Location locus)
    : outer_attrs (std::move (outer_attrs)),
      variant_name (std::move (variant_name)), locus (locus)
  {}

  // Unique pointer custom clone function
  std::unique_ptr<EnumItem> clone_enum_item () const
  {
    return std::unique_ptr<EnumItem> (clone_enum_item_impl ());
  }

  virtual std::string as_string () const;

  // not pure virtual as not abstract
  virtual void accept_vis (ASTVisitor &vis);

protected:
  // Clone function implementation as (not pure) virtual method
  virtual EnumItem *clone_enum_item_impl () const
  {
    return new EnumItem (*this);
  }
};

// A tuple item used in an "enum" tagged union
class EnumItemTuple : public EnumItem
{
  // bool has_tuple_fields;
  std::vector<TupleField> tuple_fields;

public:
  // Returns whether tuple enum item has tuple fields.
  bool has_tuple_fields () const { return !tuple_fields.empty (); }

  EnumItemTuple (Identifier variant_name, std::vector<TupleField> tuple_fields,
		 std::vector<Attribute> outer_attrs, Location locus)
    : EnumItem (std::move (variant_name), std::move (outer_attrs), locus),
      tuple_fields (std::move (tuple_fields))
  {}

  std::string as_string () const override;

  void accept_vis (ASTVisitor &vis) override;

protected:
  // Clone function implementation as (not pure) virtual method
  EnumItemTuple *clone_enum_item_impl () const override
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

  EnumItemStruct (Identifier variant_name,
		  std::vector<StructField> struct_fields,
		  std::vector<Attribute> outer_attrs, Location locus)
    : EnumItem (std::move (variant_name), std::move (outer_attrs), locus),
      struct_fields (std::move (struct_fields))
  {}

  std::string as_string () const override;

  void accept_vis (ASTVisitor &vis) override;

protected:
  // Clone function implementation as (not pure) virtual method
  EnumItemStruct *clone_enum_item_impl () const override
  {
    return new EnumItemStruct (*this);
  }
};

// A discriminant (numbered enum) item used in an "enum" tagged union
class EnumItemDiscriminant : public EnumItem
{
  std::unique_ptr<Expr> expression;

public:
  EnumItemDiscriminant (Identifier variant_name, std::unique_ptr<Expr> expr,
			std::vector<Attribute> outer_attrs, Location locus)
    : EnumItem (std::move (variant_name), std::move (outer_attrs), locus),
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

protected:
  // Clone function implementation as (not pure) virtual method
  EnumItemDiscriminant *clone_enum_item_impl () const override
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

  Location get_locus () const { return locus; }

  void accept_vis (ASTVisitor &vis) override;

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

  Location get_locus () const { return locus; }

  void accept_vis (ASTVisitor &vis) override;

protected:
  /* Use covariance to implement clone function as returning this object
   * rather than base */
  Union *clone_item_impl () const override { return new Union (*this); }

  /* Use covariance to implement clone function as returning this object
   * rather than base */
  /*virtual Union* clone_statement_impl() const override {
      return new Union(*this);
  }*/
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
  Identifier identifier;

  std::unique_ptr<Type> type;
  std::unique_ptr<Expr> const_expr;

  Location locus;

public:
  std::string as_string () const override;

  ConstantItem (Identifier ident, Visibility vis, std::unique_ptr<Type> type,
		std::unique_ptr<Expr> const_expr,
		std::vector<Attribute> outer_attrs, Location locus)
    : VisItem (std::move (vis), std::move (outer_attrs)),
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

  /* Returns whether constant item is an "unnamed" (wildcard underscore used
   * as identifier) constant. */
  bool is_unnamed () const { return identifier == std::string ("_"); }

  Location get_locus () const { return locus; }

  void accept_vis (ASTVisitor &vis) override;

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

  /* Use covariance to implement clone function as returning this object
   * rather than base */
  /*virtual ConstantItem* clone_statement_impl() const override {
      return new ConstantItem(*this);
  }*/
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
      type (other.type->clone_type ()), expr (other.expr->clone_expr ()),
      locus (other.locus)
  {}

  // Overloaded assignment operator to clone
  StaticItem &operator= (StaticItem const &other)
  {
    VisItem::operator= (other);
    name = other.name;
    has_mut = other.has_mut;
    type = other.type->clone_type ();
    expr = other.expr->clone_expr ();
    locus = other.locus;

    return *this;
  }

  // move constructors
  StaticItem (StaticItem &&other) = default;
  StaticItem &operator= (StaticItem &&other) = default;

  Location get_locus () const { return locus; }

  void accept_vis (ASTVisitor &vis) override;

protected:
  /* Use covariance to implement clone function as returning this object
   * rather than base */
  StaticItem *clone_item_impl () const override
  {
    return new StaticItem (*this);
  }

  /* Use covariance to implement clone function as returning this object
   * rather than base */
  /*virtual StaticItem* clone_statement_impl() const override {
      return new StaticItem(*this);
  }*/
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
      function_params (other.function_params),
      return_type (other.return_type->clone_type ()),
      where_clause (other.where_clause)
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

    generic_params.reserve (other.generic_params.size ());
    for (const auto &e : other.generic_params)
      generic_params.push_back (e->clone_generic_param ());

    return *this;
  }

  // move constructors
  TraitFunctionDecl (TraitFunctionDecl &&other) = default;
  TraitFunctionDecl &operator= (TraitFunctionDecl &&other) = default;

  std::string as_string () const;
};

// Actual trait item function declaration within traits
class TraitItemFunc : public TraitItem
{
  std::vector<Attribute> outer_attrs;
  TraitFunctionDecl decl;
  std::unique_ptr<BlockExpr> block_expr;
  Location locus;

public:
  // Returns whether function has a definition or is just a declaration.
  bool has_definition () const { return block_expr != nullptr; }

  TraitItemFunc (TraitFunctionDecl decl, std::unique_ptr<BlockExpr> block_expr,
		 std::vector<Attribute> outer_attrs, Location locus)
    : outer_attrs (std::move (outer_attrs)), decl (std::move (decl)),
      block_expr (std::move (block_expr)), locus (locus)
  {}

  // Copy constructor with clone
  TraitItemFunc (TraitItemFunc const &other)
    : outer_attrs (other.outer_attrs), decl (other.decl), locus (other.locus)
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
    if (other.block_expr != nullptr)
      block_expr = other.block_expr->clone_block_expr ();

    return *this;
  }

  // move constructors
  TraitItemFunc (TraitItemFunc &&other) = default;
  TraitItemFunc &operator= (TraitItemFunc &&other) = default;

  std::string as_string () const override;

  Location get_locus () const { return locus; }

  void accept_vis (ASTVisitor &vis) override;

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
      return_type (other.return_type->clone_type ()),
      where_clause (other.where_clause)
  {
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
    return_type = other.return_type->clone_type ();
    where_clause = other.where_clause;

    generic_params.reserve (other.generic_params.size ());
    for (const auto &e : other.generic_params)
      generic_params.push_back (e->clone_generic_param ());

    return *this;
  }

  // move constructors
  TraitMethodDecl (TraitMethodDecl &&other) = default;
  TraitMethodDecl &operator= (TraitMethodDecl &&other) = default;

  std::string as_string () const;
};

// Actual trait item method declaration within traits
class TraitItemMethod : public TraitItem
{
  std::vector<Attribute> outer_attrs;
  TraitMethodDecl decl;
  std::unique_ptr<BlockExpr> block_expr;
  Location locus;

public:
  // Returns whether method has a definition or is just a declaration.
  bool has_definition () const { return block_expr != nullptr; }

  TraitItemMethod (TraitMethodDecl decl, std::unique_ptr<BlockExpr> block_expr,
		   std::vector<Attribute> outer_attrs, Location locus)
    : outer_attrs (std::move (outer_attrs)), decl (std::move (decl)),
      block_expr (std::move (block_expr)), locus (locus)
  {}

  // Copy constructor with clone
  TraitItemMethod (TraitItemMethod const &other)
    : outer_attrs (other.outer_attrs), decl (other.decl),
      block_expr (other.block_expr->clone_block_expr ()), locus (other.locus)
  {}

  // Overloaded assignment operator to clone
  TraitItemMethod &operator= (TraitItemMethod const &other)
  {
    TraitItem::operator= (other);
    outer_attrs = other.outer_attrs;
    decl = other.decl;
    block_expr = other.block_expr->clone_block_expr ();
    locus = other.locus;

    return *this;
  }

  // move constructors
  TraitItemMethod (TraitItemMethod &&other) = default;
  TraitItemMethod &operator= (TraitItemMethod &&other) = default;

  std::string as_string () const override;

  Location get_locus () const { return locus; }

  void accept_vis (ASTVisitor &vis) override;

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

  Location locus;

public:
  // Whether the constant item has an associated expression.
  bool has_expression () const { return expr != nullptr; }

  TraitItemConst (Identifier name, std::unique_ptr<Type> type,
		  std::unique_ptr<Expr> expr,
		  std::vector<Attribute> outer_attrs, Location locus)
    : outer_attrs (std::move (outer_attrs)), name (std::move (name)),
      type (std::move (type)), expr (std::move (expr)), locus (locus)
  {}

  // Copy constructor with clones
  TraitItemConst (TraitItemConst const &other)
    : outer_attrs (other.outer_attrs), name (other.name),
      type (other.type->clone_type ()), expr (other.expr->clone_expr ()),
      locus (other.locus)
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

    return *this;
  }

  // move constructors
  TraitItemConst (TraitItemConst &&other) = default;
  TraitItemConst &operator= (TraitItemConst &&other) = default;

  std::string as_string () const override;

  Location get_locus () const { return locus; }

  void accept_vis (ASTVisitor &vis) override;

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

  Location locus;

public:
  // Returns whether trait item type has type param bounds.
  bool has_type_param_bounds () const { return !type_param_bounds.empty (); }

  TraitItemType (
    Identifier name,
    std::vector<std::unique_ptr<TypeParamBound>> type_param_bounds,
    std::vector<Attribute> outer_attrs, Location locus)
    : outer_attrs (std::move (outer_attrs)), name (std::move (name)),
      type_param_bounds (std::move (type_param_bounds)), locus (locus)
  {}

  // Copy constructor with vector clone
  TraitItemType (TraitItemType const &other)
    : outer_attrs (other.outer_attrs), name (other.name), locus (other.locus)
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

    type_param_bounds.reserve (other.type_param_bounds.size ());
    for (const auto &e : other.type_param_bounds)
      type_param_bounds.push_back (e->clone_type_param_bound ());

    return *this;
  }

  // default move constructors
  TraitItemType (TraitItemType &&other) = default;
  TraitItemType &operator= (TraitItemType &&other) = default;

  std::string as_string () const override;

  Location get_locus () const { return locus; }

  void accept_vis (ASTVisitor &vis) override;

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

  // bool has_generics;
  // Generics generic_params;
  std::vector<std::unique_ptr<GenericParam>> generic_params; // inlined

  // bool has_type_param_bounds;
  // TypeParamBounds type_param_bounds;
  std::vector<std::unique_ptr<TypeParamBound>>
    type_param_bounds; // inlined form

  // bool has_where_clause;
  WhereClause where_clause;

  // bool has_trait_items;
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

  // Mega-constructor
  Trait (Identifier name, bool is_unsafe,
	 std::vector<std::unique_ptr<GenericParam>> generic_params,
	 std::vector<std::unique_ptr<TypeParamBound>> type_param_bounds,
	 WhereClause where_clause,
	 std::vector<std::unique_ptr<TraitItem>> trait_items, Visibility vis,
	 std::vector<Attribute> outer_attrs, Location locus)
    : VisItem (std::move (vis), std::move (outer_attrs)),
      has_unsafe (is_unsafe), name (std::move (name)),
      generic_params (std::move (generic_params)),
      type_param_bounds (std::move (type_param_bounds)),
      where_clause (std::move (where_clause)),
      trait_items (std::move (trait_items)), locus (locus)
  {}

  // Copy constructor with vector clone
  Trait (Trait const &other)
    : VisItem (other), has_unsafe (other.has_unsafe), name (other.name),
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
    has_unsafe = other.has_unsafe;
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

  Location get_locus () const { return locus; }

  void accept_vis (ASTVisitor &vis) override;

protected:
  /* Use covariance to implement clone function as returning this object
   * rather than base */
  Trait *clone_item_impl () const override { return new Trait (*this); }

  /* Use covariance to implement clone function as returning this object
   * rather than base */
  /*virtual Trait* clone_statement_impl() const override {
      return new Trait(*this);
  }*/
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

  Location get_locus () const { return locus; }

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
    : VisItem (other), trait_type (other.trait_type->clone_type ()),
      where_clause (other.where_clause), inner_attrs (other.inner_attrs),
      locus (other.locus)
  {
    generic_params.reserve (other.generic_params.size ());
    for (const auto &e : other.generic_params)
      generic_params.push_back (e->clone_generic_param ());
  }

  // Assignment operator overload with cloning
  Impl &operator= (Impl const &other)
  {
    VisItem::operator= (other);
    trait_type = other.trait_type->clone_type ();
    where_clause = other.where_clause;
    inner_attrs = other.inner_attrs;
    locus = other.locus;

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

protected:
  /* Use covariance to implement clone function as returning this object
   * rather than base */
  InherentImpl *clone_item_impl () const override
  {
    return new InherentImpl (*this);
  }

  /* Use covariance to implement clone function as returning this object
   * rather than base */
  /*virtual InherentImpl* clone_statement_impl() const override {
      return new InherentImpl(*this);
  }*/
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

  // TODO: constructors with less params

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

protected:
  /* Use covariance to implement clone function as returning this object
   * rather than base */
  TraitImpl *clone_item_impl () const override { return new TraitImpl (*this); }

  /* Use covariance to implement clone function as returning this object
   * rather than base */
  /*virtual TraitImpl* clone_statement_impl() const override {
      return new TraitImpl(*this);
  }*/
};

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

  Location get_locus () const { return locus; }

  virtual void accept_vis (ASTVisitor &vis) = 0;

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

// A static item used in an extern block
class ExternalStaticItem : public ExternalItem
{
  bool has_mut;
  std::unique_ptr<Type> item_type;

public:
  ExternalStaticItem (Identifier item_name, std::unique_ptr<Type> item_type,
		      bool is_mut, Visibility vis,
		      std::vector<Attribute> outer_attrs, Location locus)
    : ExternalItem (std::move (item_name), std::move (vis),
		    std::move (outer_attrs), locus),
      has_mut (is_mut), item_type (std::move (item_type))
  {}

  // Copy constructor
  ExternalStaticItem (ExternalStaticItem const &other)
    : ExternalItem (other), has_mut (other.has_mut),
      item_type (other.item_type->clone_type ())
  {}

  // Overloaded assignment operator to clone
  ExternalStaticItem &operator= (ExternalStaticItem const &other)
  {
    ExternalItem::operator= (other);
    item_type = other.item_type->clone_type ();
    has_mut = other.has_mut;

    return *this;
  }

  // move constructors
  ExternalStaticItem (ExternalStaticItem &&other) = default;
  ExternalStaticItem &operator= (ExternalStaticItem &&other) = default;

  std::string as_string () const override;

  void accept_vis (ASTVisitor &vis) override;

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
  Identifier name; // TODO: handle wildcard in identifier?

  std::unique_ptr<Type> param_type;

  // TODO: should this store location data?

public:
  // Returns whether the named function parameter has a name (i.e. name is not
  // '_').
  bool has_name () const { return name != "_"; }

  // Returns whether the named function parameter is in an error state.
  bool is_error () const
  {
    // also if identifier is "" but that is probably more costly to compute
    return param_type == nullptr;
  }

  // Creates an error state named function parameter.
  static NamedFunctionParam create_error ()
  {
    return NamedFunctionParam ("", nullptr);
  }

  NamedFunctionParam (Identifier name, std::unique_ptr<Type> param_type)
    : name (std::move (name)), param_type (std::move (param_type))
  {}

  // Copy constructor
  NamedFunctionParam (NamedFunctionParam const &other)
    : name (other.name), param_type (other.param_type->clone_type ())
  {}

  ~NamedFunctionParam () = default;

  // Overloaded assignment operator to clone
  NamedFunctionParam &operator= (NamedFunctionParam const &other)
  {
    name = other.name;
    param_type = other.param_type->clone_type ();
    // has_name = other.has_name;

    return *this;
  }

  // move constructors
  NamedFunctionParam (NamedFunctionParam &&other) = default;
  NamedFunctionParam &operator= (NamedFunctionParam &&other) = default;

  std::string as_string () const;
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

  ExternalFunctionItem (
    Identifier item_name,
    std::vector<std::unique_ptr<GenericParam>> generic_params,
    std::unique_ptr<Type> return_type, WhereClause where_clause,
    std::vector<NamedFunctionParam> function_params, bool has_variadics,
    Visibility vis, std::vector<Attribute> outer_attrs, Location locus)
    : ExternalItem (std::move (item_name), std::move (vis),
		    std::move (outer_attrs), locus),
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

  void accept_vis (ASTVisitor &vis) override;

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

public:
  std::string as_string () const override;

  // Returns whether extern block has inner attributes.
  bool has_inner_attrs () const { return !inner_attrs.empty (); }

  // Returns whether extern block has extern items.
  bool has_extern_items () const { return !extern_items.empty (); }

  // Returns whether extern block has ABI name.
  bool has_abi () const { return !abi.empty (); }

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

  Location get_locus () const { return locus; }

  void accept_vis (ASTVisitor &vis) override;

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

// Replaced with forward decls - defined in "rust-macro.h"
class MacroItem;
class MacroInvocationSemi;
class MacroRulesDefinition;
} // namespace AST
} // namespace Rust

#endif
