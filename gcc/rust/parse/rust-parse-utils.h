
// Copyright (C) 2025 Free Software Foundation, Inc.

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

#ifndef RUST_PARSE_UTILS_H
#define RUST_PARSE_UTILS_H

#include "rust-ast.h"

namespace Rust {
namespace Parse {
/* Utility structure to return members of an attribute body, was initially a
 * tuple but tuples are ugly*/
struct AttributeBody
{
  AST::SimplePath path;
  std::unique_ptr<AST::AttrInput> input;
  location_t locus;
};

namespace Utils {

/* Returns true if the token id matches the delimiter type. Note that this only
 * operates for END delimiter tokens. */
inline bool
token_id_matches_delims (TokenId token_id, AST::DelimType delim_type)
{
  return ((token_id == RIGHT_PAREN && delim_type == AST::PARENS)
	  || (token_id == RIGHT_SQUARE && delim_type == AST::SQUARE)
	  || (token_id == RIGHT_CURLY && delim_type == AST::CURLY));
}

/* Determines whether token is a valid simple path segment. This does not
 * include scope resolution operators. */
inline bool
is_simple_path_segment (TokenId id)
{
  switch (id)
    {
    case IDENTIFIER:
    case SUPER:
    case SELF:
    case CRATE:
      return true;
    case DOLLAR_SIGN:
      // assume that dollar sign leads to $crate
      return true;
    default:
      return false;
    }
}

/* Returns whether the token id is (or is likely to be) a right angle bracket.
 * i.e. '>', '>>', '>=' and '>>=' tokens. */
inline bool
is_right_angle_tok (TokenId id)
{
  switch (id)
    {
    case RIGHT_ANGLE:
    case RIGHT_SHIFT:
    case GREATER_OR_EQUAL:
    case RIGHT_SHIFT_EQ:
      return true;
    default:
      return false;
    }
}

/* Returns whether the token can start a type (i.e. there is a valid type
 * beginning with the token). */
inline bool
can_tok_start_type (TokenId id)
{
  switch (id)
    {
    case EXCLAM:
    case LEFT_SQUARE:
    case LEFT_ANGLE:
    case UNDERSCORE:
    case ASTERISK:
    case AMP:
    case LIFETIME:
    case IDENTIFIER:
    case SUPER:
    case SELF:
    case SELF_ALIAS:
    case CRATE:
    case DOLLAR_SIGN:
    case SCOPE_RESOLUTION:
    case LEFT_PAREN:
    case FOR:
    case ASYNC:
    case CONST:
    case UNSAFE:
    case EXTERN_KW:
    case FN_KW:
    case IMPL:
    case DYN:
    case QUESTION_MARK:
      return true;
    default:
      return false;
    }
}

} // namespace Utils

} // namespace Parse
} // namespace Rust

#endif /* !RUST_PARSE_UTILS_H */
