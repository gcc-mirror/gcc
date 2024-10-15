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

#ifndef RUST_HIR_LITERAL_H
#define RUST_HIR_LITERAL_H

#include "rust-token.h"

namespace Rust {
namespace HIR {
// A literal - value with a type. Used in LiteralExpr and LiteralPattern.
struct Literal
{
public:
  enum LitType
  {
    CHAR,
    STRING,
    BYTE,
    BYTE_STRING,
    INT,
    FLOAT,
    BOOL
  };

private:
  std::string value_as_string;
  LitType type;
  PrimitiveCoreType type_hint;

public:
  std::string as_string () const { return value_as_string; }

  LitType get_lit_type () const { return type; }

  PrimitiveCoreType get_type_hint () const { return type_hint; }

  Literal (std::string value_as_string, LitType type,
	   PrimitiveCoreType type_hint)
    : value_as_string (std::move (value_as_string)), type (type),
      type_hint (type_hint)
  {}

  static Literal create_error ()
  {
    return Literal ("", CHAR, PrimitiveCoreType::CORETYPE_UNKNOWN);
  }

  void set_lit_type (LitType lt) { type = lt; }

  // Returns whether literal is in an invalid state.
  bool is_error () const { return value_as_string == ""; }

  bool is_equal (Literal &other)
  {
    return value_as_string == other.value_as_string && type == other.type
	   && type_hint == other.type_hint;
  }
};
} // namespace HIR
} // namespace Rust

#endif
