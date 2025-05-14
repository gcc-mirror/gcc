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

#include "rust-system.h"
#include "rust-ast.h"
#include "rust-hir-visitable.h"
#include "rust-hir-attrs.h"

#include "rust-token.h"

#include "rust-location.h"

#include "rust-hir-map.h"
#include "rust-diagnostics.h"
#include "rust-hir-bound.h"

namespace Rust {

typedef int TupleIndex;

namespace HIR {

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
	 Analysis::NodeMapping mappings);

  // Copy constructor with vector clone
  Crate (Crate const &other);

  ~Crate () = default;

  // Overloaded assignment operator with vector clone
  Crate &operator= (Crate const &other);

  // Move constructors
  Crate (Crate &&other) = default;
  Crate &operator= (Crate &&other) = default;

  // Get crate representation as string (e.g. for debugging).
  std::string as_string () const;

  const Analysis::NodeMapping &get_mappings () const { return mappings; }
  std::vector<std::unique_ptr<Item>> &get_items () { return items; }
};

} // namespace HIR
} // namespace Rust

#endif
