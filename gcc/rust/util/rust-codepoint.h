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

#ifndef RUST_CODEPOINT_H
#define RUST_CODEPOINT_H

#include "rust-system.h"

namespace Rust {

constexpr uint32_t MAX_ASCII_CODEPOINT = 0x7F;
constexpr uint32_t CODEPOINT_INVALID = 0xFFFE;

// FIXME: move this to rust-unicode.h?
struct Codepoint
{
  uint32_t value;

  // Creates a zero codepoint.
  Codepoint () : value (0) {}

  // Creates a codepoint from an encoded UTF-8 value.
  Codepoint (uint32_t value) : value (value) {}

  static Codepoint eof () { return Codepoint (UINT32_MAX); }
  bool is_eof () const { return value == UINT32_MAX; }
  bool is_ascii () const { return value <= MAX_ASCII_CODEPOINT; }
  bool is_supplementary_character () const { return value > 0xFFFF; }

  // Returns a C++ string containing string value of codepoint.
  std::string as_string ();

  bool operator== (Codepoint other) const { return value == other.value; }
  bool operator!= (Codepoint other) const { return !operator== (other); }
};
} // namespace Rust

#endif
