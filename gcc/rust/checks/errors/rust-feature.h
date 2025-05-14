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
    ACCEPTED,
    ACTIVE,
    REMOVED,
    STABILIZED,
  };

  enum class Name
  {
    ASSOCIATED_TYPE_BOUNDS,
    INTRINSICS,
    NEGATIVE_IMPLS,
    RUSTC_ATTRS,
    DECL_MACRO,
    AUTO_TRAITS,
    EXTERN_TYPES,
    LANG_ITEMS,
    NO_CORE,
    BOX_SYNTAX,
    DROPCK_EYEPATCH,
    RAW_REF_OP,
    EXCLUSIVE_RANGE_PATTERN,
    PRELUDE_IMPORT,
    MIN_SPECIALIZATION,
  };

  const std::string &as_string () { return m_name_str; }
  Name name () { return m_name; }
  const std::string &description () { return m_description; }
  State state () { return m_state; }
  tl::optional<unsigned> issue () { return m_issue; }

  static tl::optional<Name> as_name (const std::string &name);
  static Feature create (Name name);

private:
  Feature (Name name, State state, const char *name_str,
	   const char *rustc_since,
	   tl::optional<unsigned> issue_number = tl::nullopt,
	   const tl::optional<Edition> &edition = tl::nullopt,
	   const char *description = "")
    : m_state (state), m_name (name), m_name_str (name_str),
      m_rustc_since (rustc_since), m_issue (issue_number), edition (edition),
      m_description (description)
  {}

  State m_state;
  Name m_name;
  std::string m_name_str;
  std::string m_rustc_since;
  tl::optional<unsigned> m_issue;
  tl::optional<Edition> edition;
  std::string m_description; // TODO: Switch to optional?

  static const std::map<std::string, Name> name_hash_map;
};

} // namespace Rust
#endif
