// Copyright (C) 2020-2023 Free Software Foundation, Inc.

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

#include "rust-session-manager.h"
#include "rust-optional.h"

namespace Rust {

class Feature
{
public:
  enum class State
  {
    ACCEPTED,
    ACTIVE,
    REMOVED,
    STABILIZED,
  };

  enum class Name
  {
    ASSOCIATED_TYPE_BOUNDS,
    INTRINSICS,
    RUSTC_ATTRS,
    DECL_MACRO,
  };

  const std::string &as_string () { return m_name_str; }
  Name name () { return m_name; }
  const std::string &description () { return m_description; }
  State state () { return m_state; }
  uint64_t issue () { return m_issue; }

  static Optional<Name> as_name (const std::string &name);
  static Feature create (Name name);

private:
  Feature (Name name, State state, const char *name_str,
	   const char *rustc_since, uint64_t issue_number,
	   const Optional<CompileOptions::Edition> &edition,
	   const char *description)
    : m_state (state), m_name (name), m_name_str (name_str),
      m_rustc_since (rustc_since), m_issue (issue_number), edition (edition),
      m_description (description)
  {}

  State m_state;
  Name m_name;
  std::string m_name_str;
  std::string m_rustc_since;
  uint64_t m_issue;
  Optional<CompileOptions::Edition> edition;
  std::string m_description;

  static const std::map<std::string, Name> name_hash_map;
};

} // namespace Rust
#endif