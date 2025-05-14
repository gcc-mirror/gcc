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

#ifndef RUST_HIR_BOUND_H
#define RUST_HIR_BOUND_H

#include "rust-hir-bound-abstract.h"
#include "rust-common.h"
#include "rust-hir-path.h"

namespace Rust {
namespace HIR {

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

} // namespace HIR
} // namespace Rust

#endif
