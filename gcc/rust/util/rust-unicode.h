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

#ifndef RUST_UNICODE_H
#define RUST_UNICODE_H

#include "optional.h"
#include "rust-system.h"
#include "rust-input-source.h"

namespace Rust {

class Utf8String
{
private:
  std::vector<Codepoint> chars;

public:
  static tl::optional<Utf8String>
  make_utf8_string (const std::string &maybe_utf8)
  {
    BufferInputSource input_source = {maybe_utf8, 0};
    tl::optional<std::vector<Codepoint>> chars_opt = input_source.get_chars ();
    if (chars_opt.has_value ())
      return {Utf8String (chars_opt.value ())};
    else
      return tl::nullopt;
  }

  Utf8String (const std::vector<Codepoint> codepoints) : chars ({codepoints}) {}

  std::string as_string () const
  {
    std::stringstream ss;
    for (Codepoint c : chars)
      ss << c.as_string ();

    return ss.str ();
  };

  // Returns characters
  std::vector<Codepoint> get_chars () const { return chars; }

  Utf8String nfc_normalize () const;
};

bool
is_alphabetic (uint32_t codepoint);

bool
is_ascii_only (const std::string &str);

bool
is_numeric (uint32_t codepoint);

bool
is_nfc_qc_no (uint32_t codepoint);

bool
is_nfc_qc_maybe (uint32_t codepoint);

enum class QuickCheckResult
{
  YES,
  NO,
  MAYBE
};

QuickCheckResult
nfc_quick_check (const std::vector<Codepoint> &s);

} // namespace Rust

#if CHECKING_P

namespace selftest {

void
rust_nfc_qc_test ();

void
rust_utf8_normalize_test ();

void
rust_utf8_property_test ();

} // namespace selftest

#endif // CHECKING_P

#endif
