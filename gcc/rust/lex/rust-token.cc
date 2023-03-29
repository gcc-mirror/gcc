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

#include "rust-token.h"
#include "rust-diagnostics.h"

namespace Rust {
// Hackily defined way to get token description for enum value using x-macros
const char *
get_token_description (TokenId id)
{
  switch (id)
    {
#define RS_TOKEN(name, descr)                                                  \
  case name:                                                                   \
    return descr;
#define RS_TOKEN_KEYWORD(x, y) RS_TOKEN (x, y)
      RS_TOKEN_LIST
#undef RS_TOKEN_KEYWORD
#undef RS_TOKEN
    default:
      gcc_unreachable ();
    }
}

/* Hackily defined way to get token description as a string for enum value using
 * x-macros */
const char *
token_id_to_str (TokenId id)
{
  switch (id)
    {
#define RS_TOKEN(name, _)                                                      \
  case name:                                                                   \
    return #name;
#define RS_TOKEN_KEYWORD(x, y) RS_TOKEN (x, y)
      RS_TOKEN_LIST
#undef RS_TOKEN_KEYWORD
#undef RS_TOKEN
    default:
      gcc_unreachable ();
    }
}

const char *
get_type_hint_string (PrimitiveCoreType type)
{
  switch (type)
    {
    case CORETYPE_BOOL:
      return "bool";
    case CORETYPE_CHAR:
      return "char";
    case CORETYPE_STR:
      return "str";
    // case CORETYPE_INT:
    case CORETYPE_ISIZE:
      return "isize";
    // case CORETYPE_UINT:
    case CORETYPE_USIZE:
      return "usize";
    case CORETYPE_F32:
      return "f32";
    case CORETYPE_F64:
      return "f64";
    case CORETYPE_I8:
      return "i8";
    case CORETYPE_I16:
      return "i16";
    case CORETYPE_I32:
      return "i32";
    case CORETYPE_I64:
      return "i64";
    case CORETYPE_I128:
      return "i128";
    case CORETYPE_U8:
      return "u8";
    case CORETYPE_U16:
      return "u16";
    case CORETYPE_U32:
      return "u32";
    case CORETYPE_U64:
      return "u64";
    case CORETYPE_U128:
      return "u128";
    case CORETYPE_PURE_DECIMAL:
      return "pure_decimal";
    case CORETYPE_UNKNOWN:
    default:
      return "unknown";
    }
}

const char *
Token::get_type_hint_str () const
{
  return get_type_hint_string (type_hint);
}

const std::string &
Token::get_str () const
{
  // FIXME: attempt to return null again
  // gcc_assert(str != NULL);

  // HACK: allow referencing an empty string
  static const std::string empty = "";

  if (str == NULL)
    {
      rust_error_at (get_locus (),
		     "attempted to get string for %<%s%>, which has no string. "
		     "returning empty string instead",
		     get_token_description ());
      return empty;
    }
  return *str;
}
} // namespace Rust
