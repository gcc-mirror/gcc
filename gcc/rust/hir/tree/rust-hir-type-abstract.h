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

#ifndef RUST_HIR_TYPE_ABSTRACT_H
#define RUST_HIR_TYPE_ABSTRACT_H

#include "rust-hir-node.h"
#include "rust-hir-visitable.h"
#include "rust-system.h"
#include "rust-hir-map.h"

namespace Rust {
namespace HIR {

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
  virtual std::unique_ptr<TraitBound>
  to_trait_bound (bool in_parens ATTRIBUTE_UNUSED) const;
  /* as pointer, shouldn't require definition beforehand, only forward
   * declaration. */

  virtual void accept_vis (HIRTypeVisitor &vis) = 0;

  virtual const Analysis::NodeMapping &get_mappings () const
  {
    return mappings;
  }
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

} // namespace HIR
} // namespace Rust

#endif
