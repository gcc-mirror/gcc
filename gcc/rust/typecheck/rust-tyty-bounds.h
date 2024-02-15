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

#ifndef RUST_TYTY_BOUNDS_H
#define RUST_TYTY_BOUNDS_H

#include "rust-location.h"
#include "rust-mapping-common.h"

namespace Rust {

namespace Resolver {
class TraitReference;
class TraitItemReference;
class AssociatedImplTrait;
} // namespace Resolver

namespace TyTy {

class BaseType;
class TypeBoundPredicate;
class TypeBoundPredicateItem
{
public:
  TypeBoundPredicateItem (const TypeBoundPredicate *parent,
			  const Resolver::TraitItemReference *trait_item_ref);

  static TypeBoundPredicateItem error ();

  bool is_error () const;

  BaseType *get_tyty_for_receiver (const TyTy::BaseType *receiver);

  const Resolver::TraitItemReference *get_raw_item () const;

  bool needs_implementation () const;

  const TypeBoundPredicate *get_parent () const;

  location_t get_locus () const;

private:
  const TypeBoundPredicate *parent;
  const Resolver::TraitItemReference *trait_item_ref;
};

class TypeBoundsMappings
{
protected:
  TypeBoundsMappings (std::vector<TypeBoundPredicate> specified_bounds);

public:
  std::vector<TypeBoundPredicate> &get_specified_bounds ();

  const std::vector<TypeBoundPredicate> &get_specified_bounds () const;

  TypeBoundPredicate lookup_predicate (DefId id);

  size_t num_specified_bounds () const;

  std::string raw_bounds_as_string () const;

  std::string bounds_as_string () const;

  std::string raw_bounds_as_name () const;

protected:
  void add_bound (TypeBoundPredicate predicate);

  std::vector<TypeBoundPredicate> specified_bounds;
};

} // namespace TyTy
} // namespace Rust

#endif // RUST_TYTY_BOUNDS_H
