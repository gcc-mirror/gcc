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

namespace Rust {

Feature
Feature::create (Feature::Name f)
{
  switch (f)
    {
    case Feature::Name::ASSOCIATED_TYPE_BOUNDS:
      return Feature (Feature::Name::ASSOCIATED_TYPE_BOUNDS,
		      Feature::State::ACCEPTED, "associated_type_bounds",
		      "1.34.0", 52662);
    case Feature::Name::INTRINSICS:
      return Feature (f, Feature::State::ACCEPTED, "intrinsics", "1.0.0");
    case Feature::Name::RUSTC_ATTRS:
      return Feature (f, Feature::State::ACCEPTED, "rustc_attrs", "1.0.0");
    case Feature::Name::DECL_MACRO:
      return Feature (f, Feature::State::ACCEPTED, "decl_macro", "1.0.0",
		      39412);
    case Feature::Name::EXTERN_TYPES:
      return Feature (f, Feature::State::ACTIVE, "extern_types", "1.23.0",
		      43467);
    case Feature::Name::NEGATIVE_IMPLS:
      return Feature (f, Feature::State::ACTIVE, "negative_impls", "1.0.0",
		      68318);
    case Feature::Name::BOX_SYNTAX:
      return Feature (f, Feature::State::ACTIVE, "box_syntax", "1.0.0", 49733);
    case Feature::Name::DROPCK_EYEPATCH:
      return Feature (f, Feature::State::ACTIVE, "dropck_eyepatch", "1.10.0",
		      34761);
    case Feature::Name::RAW_REF_OP:
      return Feature (f, Feature::State::ACTIVE, "raw_ref_op", "1.41.0", 64490);
    case Feature::Name::EXCLUSIVE_RANGE_PATTERN:
      return Feature (Feature::Name::EXCLUSIVE_RANGE_PATTERN,
		      Feature::State::ACTIVE, "exclusive_range_pattern",
		      "1.11.0", 37854);
    case Feature::Name::PRELUDE_IMPORT:
      return Feature (f, Feature::State::ACTIVE, "prelude_import", "1.0.0");
    case Feature::Name::MIN_SPECIALIZATION:
      return Feature (f, Feature::State::ACTIVE, "min_specialization",
		      "1.0.0" /* FIXME: What version here? */, 31844);
    case Feature::Name::AUTO_TRAITS:
      return Feature (f, Feature::State::ACTIVE, "optin_builtin_traits",
		      "1.0.0", 13231);
    default:
      rust_unreachable ();
    }
}

const std::map<std::string, Feature::Name> Feature::name_hash_map = {
  {"associated_type_bounds", Feature::Name::ASSOCIATED_TYPE_BOUNDS},
  {"intrinsics", Feature::Name::INTRINSICS},
  {"rustc_attrs", Feature::Name::RUSTC_ATTRS},
  {"decl_macro", Feature::Name::DECL_MACRO},
  {"negative_impls", Feature::Name::NEGATIVE_IMPLS},
  // TODO: Rename to "auto_traits" when supporting
  // later Rust versions
  {"optin_builtin_traits", Feature::Name::AUTO_TRAITS},
  {"extern_types", Feature::Name::EXTERN_TYPES},
  {"lang_items", Feature::Name::LANG_ITEMS},
  {"no_core", Feature::Name::NO_CORE},
  {"box_syntax", Feature::Name::BOX_SYNTAX},
  {"dropck_eyepatch", Feature::Name::DROPCK_EYEPATCH},
  {"raw_ref_op", Feature::Name::RAW_REF_OP},
  {"exclusive_range_pattern", Feature::Name::EXCLUSIVE_RANGE_PATTERN},
  {"prelude_import", Feature::Name::PRELUDE_IMPORT},
  {"min_specialization", Feature::Name::MIN_SPECIALIZATION},
}; // namespace Rust

tl::optional<Feature::Name>
Feature::as_name (const std::string &name)
{
  if (Feature::name_hash_map.count (name))
    return Feature::name_hash_map.at (name);

  return tl::nullopt;
}

} // namespace Rust
