// Copyright (C) 2020-2026 Free Software Foundation, Inc.

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

#ifndef RUST_FEATURE_H
#define RUST_FEATURE_H

#include "rust-edition.h"
#include "optional.h"

namespace Rust {

class Feature
{
public:
  enum class State
  {
    ACCEPTED,	// stabilized
    ACTIVE,	// unstable
    REMOVED,	// removed
    STABILIZED, // removed after stabilization
  };

  enum class Name
  {
#define FEATURE_ACTIVE(x, name, ...) name,
#define FEATURE_ACCEPTED(x, name, ...) name,
#define FEATURE_REMOVED(x, name, ...) name,
#define FEATURE_STABLE_REMOVED(x, name, ...) name,
#include "rust-feature-defs.h"
#undef FEATURE_ACTIVE
#undef FEATURE_ACCEPTED
#undef FEATURE_REMOVED
#undef FEATURE_STABLE_REMOVED
  };

  const std::string &as_string () const { return m_name_str; }

  Name name () const { return m_name; }
  State state () const { return m_state; }
  tl::optional<unsigned> issue () const { return m_issue; }

  static tl::optional<Name> as_name (const std::string &name);

  static tl::optional<std::reference_wrapper<const Feature>>
  lookup (const std::string &name);
  static const Feature &lookup (Name name);

private:
  Feature (Name name, State state, const char *name_str, const char *rust_since,
	   tl::optional<unsigned> issue_number, tl::optional<Edition> edition,
	   tl::optional<const char *> reason)
    : m_name (name), m_state (state), m_name_str (name_str),
      m_rust_since (rust_since), m_issue (issue_number), edition (edition),
      m_reason (reason)
  {}

  Name m_name;
  State m_state;
  std::string m_name_str;
  std::string m_rust_since;
  tl::optional<unsigned> m_issue;
  tl::optional<Edition> edition;
  tl::optional<const char *> m_reason;

  static Feature feature_list[];
  static const std::map<std::string, Name> name_hash_map;
};

} // namespace Rust
#endif
