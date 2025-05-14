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

#ifndef RUST_HIR_TRAIT_BOUND_H
#define RUST_HIR_TRAIT_BOUND_H

#include "rust-hir-bound-abstract.h"
#include "rust-hir-path.h"
#include "rust-hir-generic-param.h"

namespace Rust {
namespace HIR {

// A trait bound
class TraitBound : public TypeParamBound
{
  bool in_parens;
  BoundPolarity polarity;
  std::vector<LifetimeParam> for_lifetimes;
  TypePath type_path;
  location_t locus;

  Analysis::NodeMapping mappings;

public:
  // Returns whether trait bound has "for" lifetimes
  bool has_for_lifetimes () const { return !for_lifetimes.empty (); }

  TraitBound (Analysis::NodeMapping mapping, TypePath type_path,
	      location_t locus, bool in_parens = false,
	      BoundPolarity polarity = BoundPolarity::RegularBound,
	      std::vector<LifetimeParam> for_lifetimes
	      = std::vector<LifetimeParam> ())
    : in_parens (in_parens), polarity (polarity),
      for_lifetimes (std::move (for_lifetimes)),
      type_path (std::move (type_path)), locus (locus), mappings (mapping)
  {}

  std::string as_string () const override;

  location_t get_locus () const override final { return locus; }

  void accept_vis (HIRFullVisitor &vis) override;

  Analysis::NodeMapping get_mappings () const override final
  {
    return mappings;
  }

  std::vector<LifetimeParam> &get_for_lifetimes () { return for_lifetimes; }
  bool get_in_parens () { return in_parens; }
  BoundPolarity get_polarity () { return polarity; }

  BoundType get_bound_type () const final override { return TRAITBOUND; }

  TypePath &get_path () { return type_path; }

  const TypePath &get_path () const { return type_path; }

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  TraitBound *clone_type_param_bound_impl () const override
  {
    return new TraitBound (*this);
  }
};

} // namespace HIR
} // namespace Rust

#endif
