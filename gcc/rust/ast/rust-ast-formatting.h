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

#ifndef RUST_AST_FORMATTING_H
#define RUST_AST_FORMATTING_H

namespace Rust {
namespace AST {

enum indent_mode
{
  enter,
  out,
  stay
};

enum AttrMode
{
  OUTER,
  INNER
};

std::string
indent_spaces (enum indent_mode mode);

// Gets a string in a certain delim type.
std::string
get_string_in_delims (std::string str_input, DelimType delim_type);

std::string
get_mode_dump_desc (AttrMode mode);

// Adds lines below adding attributes
std::string
append_attributes (std::vector<Attribute> attrs, AttrMode mode);

// Removes the beginning and end quotes of a quoted string.
std::string
unquote_string (std::string input);

} // namespace AST
} // namespace Rust

#endif /* ! RUST_AST_FORMATTING_H */
