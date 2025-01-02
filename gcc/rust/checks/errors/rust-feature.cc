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

#include "rust-feature.h"
#include "rust-session-manager.h"

namespace Rust {

Feature
Feature::create (Feature::Name name)
{
  switch (name)
    {
    case Feature::Name::ASSOCIATED_TYPE_BOUNDS:
      return Feature (Feature::Name::ASSOCIATED_TYPE_BOUNDS,
		      Feature::State::ACCEPTED, "associated_type_bounds",
		      "1.34.0", 52662, tl::nullopt, "");
    case Feature::Name::INTRINSICS:
      return Feature (Feature::Name::INTRINSICS, Feature::State::ACCEPTED,
		      "intrinsics", "1.0.0", 0, tl::nullopt, "");
    case Feature::Name::RUSTC_ATTRS:
      return Feature (Feature::Name::RUSTC_ATTRS, Feature::State::ACCEPTED,
		      "rustc_attrs", "1.0.0", 0, tl::nullopt, "");
    case Feature::Name::DECL_MACRO:
      return Feature (Feature::Name::DECL_MACRO, Feature::State::ACCEPTED,
		      "decl_macro", "1.0.0", 0, tl::nullopt, "");
    case Feature::Name::EXTERN_TYPES:
      return Feature (Feature::Name::EXTERN_TYPES, Feature::State::ACTIVE,
		      "extern_types", "1.23.0", 43467, tl::nullopt, "");
    default:
      rust_unreachable ();
    }
}

const std::map<std::string, Feature::Name> Feature::name_hash_map = {
  {"associated_type_bounds", Feature::Name::ASSOCIATED_TYPE_BOUNDS},
  {"intrinsics", Feature::Name::INTRINSICS},
  {"rustc_attrs", Feature::Name::RUSTC_ATTRS},
  {"decl_macro", Feature::Name::DECL_MACRO},
  // TODO: Rename to "auto_traits" when supporting
  // later Rust versions
  {"optin_builtin_traits", Feature::Name::AUTO_TRAITS},
  {"extern_types", Feature::Name::EXTERN_TYPES},
  {"lang_items", Feature::Name::LANG_ITEMS},
  {"no_core", Feature::Name::NO_CORE},
}; // namespace Rust

tl::optional<Feature::Name>
Feature::as_name (const std::string &name)
{
  if (Feature::name_hash_map.count (name))
    return Feature::name_hash_map.at (name);

  return tl::nullopt;
}

} // namespace Rust