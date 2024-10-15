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

#ifndef RUST_HIR_BOUND_ABSTRACT_H
#define RUST_HIR_BOUND_ABSTRACT_H

#include "rust-hir-visitable.h"
#include "rust-system.h"
#include "rust-hir-map.h"

namespace Rust {
namespace HIR {

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

} // namespace HIR
} // namespace Rust

#endif
