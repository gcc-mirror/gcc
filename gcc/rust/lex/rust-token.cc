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

#include "rust-system.h"
#include "rust-token.h"
#include "rust-diagnostics.h"
#include "rust-unicode.h"

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
#define RS_TOKEN_KEYWORD_2015(x, y) RS_TOKEN (x, y)
#define RS_TOKEN_KEYWORD_2018 RS_TOKEN_KEYWORD_2015
      RS_TOKEN_LIST
#undef RS_TOKEN_KEYWORD_2015
#undef RS_TOKEN_KEYWORD_2018
#undef RS_TOKEN
    default:
      rust_unreachable ();
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
#define RS_TOKEN_KEYWORD_2015(x, y) RS_TOKEN (x, y)
#define RS_TOKEN_KEYWORD_2018 RS_TOKEN_KEYWORD_2015
      RS_TOKEN_LIST
#undef RS_TOKEN_KEYWORD_2015
#undef RS_TOKEN_KEYWORD_2018
#undef RS_TOKEN
    default:
      rust_unreachable ();
    }
}

/* checks if a token is a keyword */
bool
token_id_is_keyword (TokenId id)
{
  switch (id)
    {
#define RS_TOKEN_KEYWORD_2015(name, _) case name:
#define RS_TOKEN_KEYWORD_2018 RS_TOKEN_KEYWORD_2015
#define RS_TOKEN(a, b)
      RS_TOKEN_LIST return true;
#undef RS_TOKEN_KEYWORD_2015
#undef RS_TOKEN_KEYWORD_2018
#undef RS_TOKEN
    default:
      return false;
    }
}

/* gets the string associated with a keyword */
const std::string &
token_id_keyword_string (TokenId id)
{
  switch (id)
    {
#define RS_TOKEN_KEYWORD_2015(id, str_ptr)                                     \
    case id: {                                                                 \
      static const std::string str (str_ptr);                                  \
      return str;                                                              \
    }                                                                          \
    rust_unreachable ();
#define RS_TOKEN_KEYWORD_2018 RS_TOKEN_KEYWORD_2015
#define RS_TOKEN(a, b)
      RS_TOKEN_LIST
#undef RS_TOKEN_KEYWORD_2015
#undef RS_TOKEN_KEYWORD_2018
#undef RS_TOKEN
    default:
      rust_unreachable ();
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

std::string
nfc_normalize_token_string (location_t loc, TokenId id, const std::string &str)
{
  if (id == IDENTIFIER || id == LIFETIME)
    {
      tl::optional<Utf8String> ustring = Utf8String::make_utf8_string (str);
      if (ustring.has_value ())
	return ustring.value ().nfc_normalize ().as_string ();
      else
	rust_internal_error_at (loc,
				"identifier '%s' is not a valid UTF-8 string",
				str.c_str ());
    }
  else
    return str;
}

const std::string &
Token::get_str () const
{
  if (token_id_is_keyword (token_id))
    return token_id_keyword_string (token_id);

  // FIXME: attempt to return null again
  // gcc_assert(str != NULL);

  // HACK: allow referencing an empty string
  static const std::string empty = "";

  if (str == NULL)
    {
      rust_error_at (get_locus (),
		     "attempted to get string for %qs, which has no string. "
		     "returning empty string instead",
		     get_token_description ());
      return empty;
    }
  return *str;
}

namespace {
enum class Context
{
  String,
  Char
};

const std::map<char, std::string> matches = {
  {'\t', "\\t"}, {'\n', "\\n"},	 {'\r', "\\r"},
  {'\0', "\\0"}, {'\\', "\\\\"}, {'\v', "\\v"},
};

std::string
escape_special_chars (const std::string &source, Context ctx)
{
  std::stringstream stream;
  decltype (matches)::const_iterator result;
  for (char c : source)
    {
      // FIXME: #2411 Also replace escaped unicode values and \x digits
      if ((result = matches.find (c)) != matches.end ())
	stream << result->second;
      else if (c == '\'' && ctx == Context::Char)
	stream << "\\'";
      else if (c == '"' && ctx == Context::String)
	stream << "\\\"";
      else
	stream << c;
    }

  return stream.str ();
}

} // namespace

std::string
Token::as_string () const
{
  if (should_have_str ())
    {
      switch (get_id ())
	{
	case STRING_LITERAL:
	  return "\"" + escape_special_chars (get_str (), Context::String)
		 + "\"";
	case BYTE_STRING_LITERAL:
	  return "b\"" + escape_special_chars (get_str (), Context::String)
		 + "\"";
	case CHAR_LITERAL:
	  return "'" + escape_special_chars (get_str (), Context::Char) + "'";
	case BYTE_CHAR_LITERAL:
	  return "b'" + escape_special_chars (get_str (), Context::Char) + "'";
	case LIFETIME:
	  return "'" + get_str ();
	case SCOPE_RESOLUTION:
	  return "::";
	case INT_LITERAL:
	  if (get_type_hint () == CORETYPE_UNKNOWN)
	    return get_str ();
	  else
	    return get_str () + get_type_hint_str ();
	case FLOAT_LITERAL:
	  if (get_type_hint () == CORETYPE_UNKNOWN)
	    return get_str ();
	  else
	    return get_str () + get_type_hint_str ();
	default:
	  return get_str ();
	}
    }
  else
    {
      return get_token_description ();
    }
}
} // namespace Rust
