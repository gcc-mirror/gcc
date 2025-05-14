
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

#ifndef RUST_HIR_GENERIC_PARAM_H
#define RUST_HIR_GENERIC_PARAM_H

#include "rust-hir-visitable.h"
#include "rust-hir-bound.h"

namespace Rust {
namespace HIR {

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

  virtual AST::AttrVec &get_outer_attrs () = 0;
  virtual bool has_outer_attribute () const = 0;

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
		enum GenericKind kind = GenericKind::TYPE);
};

// A lifetime generic parameter (as opposed to a type generic parameter)
class LifetimeParam : public GenericParam
{
  Lifetime lifetime;

  // bool has_lifetime_bounds;
  // LifetimeBounds lifetime_bounds;
  std::vector<Lifetime> lifetime_bounds; // inlined LifetimeBounds

  AST::AttrVec outer_attrs;

  location_t locus;

public:
  Lifetime get_lifetime () { return lifetime; }

  // Returns whether the lifetime param has any lifetime bounds.
  bool has_lifetime_bounds () const { return !lifetime_bounds.empty (); }

  std::vector<Lifetime> &get_lifetime_bounds () { return lifetime_bounds; }

  // Returns whether the lifetime param has an outer attribute.
  bool has_outer_attribute () const override { return outer_attrs.size () > 1; }

  AST::AttrVec &get_outer_attrs () override { return outer_attrs; }

  // Constructor
  LifetimeParam (Analysis::NodeMapping mappings, Lifetime lifetime,
		 location_t locus = UNDEF_LOCATION,
		 std::vector<Lifetime> lifetime_bounds
		 = std::vector<Lifetime> (),
		 AST::AttrVec outer_attrs = std::vector<AST::Attribute> ());

  // TODO: remove copy and assignment operator definitions - not required

  // Copy constructor with clone
  LifetimeParam (LifetimeParam const &other);

  // Overloaded assignment operator to clone attribute
  LifetimeParam &operator= (LifetimeParam const &other);

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
		     Analysis::NodeMapping mapping, location_t locus);

  ConstGenericParam (const ConstGenericParam &other);

  bool has_outer_attribute () const override { return false; }

  AST::AttrVec &get_outer_attrs () override { return outer_attrs; }

  std::string as_string () const override final;

  void accept_vis (HIRFullVisitor &vis) override final;

  location_t get_locus () const override final { return locus; };

  bool has_default_expression () { return default_expression != nullptr; }

  std::string get_name () { return name; }
  Type &get_type ()
  {
    rust_assert (type);
    return *type;
  }
  Expr &get_default_expression () { return *default_expression; }

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

  /* const params have no outer attrs, should be empty */
  AST::AttrVec outer_attrs = std::vector<AST::Attribute> ();

  /* Optional - can be a null pointer if there is no default expression */
  std::unique_ptr<Expr> default_expression;

  location_t locus;
};

} // namespace HIR
} // namespace Rust

#endif
