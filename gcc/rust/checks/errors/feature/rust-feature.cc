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

Feature Feature::feature_list[] = {
#define ISSUE_SOME(n) n
#define ISSUE_NONE tl::nullopt
#define EDITION_2018 Edition::E2018
#define EDITION_NONE tl::nullopt
#define REASON_SOME(r) r
#define REASON_NONE tl::nullopt

#define FEATURE_BASE(state, name_str, name, rust_since, issue, ...)            \
  Feature (Feature::Name::name, Feature::State::state, name_str, rust_since,   \
	   issue, __VA_ARGS__),

#define FEATURE_ACTIVE(a, b, c, d, edition)                                    \
  FEATURE_BASE (ACTIVE, a, b, c, d, edition, tl::nullopt)

#define FEATURE_ACCEPTED(a, b, c, d)                                           \
  FEATURE_BASE (ACCEPTED, a, b, c, d, tl::nullopt, tl::nullopt)

#define FEATURE_REMOVED(a, b, c, d, reason)                                    \
  FEATURE_BASE (REMOVED, a, b, c, d, tl::nullopt, reason)

#define FEATURE_STABLE_REMOVED(a, b, c, d)                                     \
  FEATURE_BASE (ACCEPTED, a, b, c, d, tl::nullopt, tl::nullopt)

#include "rust-feature-defs.h"

#undef ISSUE_SOME
#undef ISSUE_NONE
#undef EDITION_2018
#undef EDITION_NONE
#undef REASON_SOME
#undef REASON_NONE

#undef FEATURE_BASE
#undef FEATURE_ACTIVE
#undef FEATURE_ACCEPTED
#undef FEATURE_REMOVED
#undef FEATURE_STABLE_REMOVED
};

const std::map<std::string, Feature::Name> Feature::name_hash_map = {
#define FEATURE(s, name, ...) {s, Feature::Name::name},
#define FEATURE_ACTIVE(...) FEATURE (__VA_ARGS__)
#define FEATURE_ACCEPTED(...) FEATURE (__VA_ARGS__)
#define FEATURE_REMOVED(...) FEATURE (__VA_ARGS__)
#define FEATURE_STABLE_REMOVED(...) FEATURE (__VA_ARGS__)
#include "rust-feature-defs.h"
#undef FEATURE
#undef FEATURE_ACTIVE
#undef FEATURE_ACCEPTED
#undef FEATURE_REMOVED
#undef FEATURE_STABLE_REMOVED
};

tl::optional<Feature::Name>
Feature::as_name (const std::string &name)
{
  if (Feature::name_hash_map.count (name))
    return Feature::name_hash_map.at (name);

  return tl::nullopt;
}

tl::optional<std::reference_wrapper<const Feature>>
Feature::lookup (const std::string &name)
{
  return as_name (name).map (
    [] (Name n) { return std::ref (Feature::lookup (n)); });
}

const Feature &
Feature::lookup (Feature::Name name)
{
  return feature_list[static_cast<size_t> (name)];
}

} // namespace Rust
