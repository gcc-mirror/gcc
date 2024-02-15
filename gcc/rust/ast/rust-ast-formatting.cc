/* General AST-related method implementations for Rust frontend.
   Copyright (C) 2009-2024 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "rust-ast-full.h"

namespace Rust {
namespace AST {

std::string
indent_spaces (enum indent_mode mode)
{
  static int indent = 0;
  std::string str = "";
  if (out == mode)
    indent--;
  for (int i = 0; i < indent; i++)
    str += " ";
  if (enter == mode)
    indent++;

  return str;
}

std::string
get_string_in_delims (std::string str_input, DelimType delim_type)
{
  switch (delim_type)
    {
    case PARENS:
      return "(" + str_input + ")";
    case SQUARE:
      return "[" + str_input + "]";
    case CURLY:
      return "{" + str_input + "}";
    default:
      return "ERROR-MARK-STRING (delims)";
    }
  rust_unreachable ();
}

std::string
get_mode_dump_desc (AttrMode mode)
{
  switch (mode)
    {
    case OUTER:
      return "outer attributes";
    case INNER:
      return "inner attributes";
    default:
      rust_unreachable ();
      return "";
    }
}

std::string
append_attributes (std::vector<Attribute> attrs, AttrMode mode)
{
  indent_spaces (enter);

  std::string str
    = "\n" + indent_spaces (stay) + get_mode_dump_desc (mode) + ": ";
  // str += "\n" + indent_spaces (stay) + "inner attributes: ";
  if (attrs.empty ())
    {
      str += "none";
    }
  else
    {
      /* note that this does not print them with outer or "inner attribute"
       * syntax - just prints the body */
      for (const auto &attr : attrs)
	str += "\n" + indent_spaces (stay) + attr.as_string ();
    }

  indent_spaces (out);

  return str;
}

std::string
unquote_string (std::string input)
{
  rust_assert (input.front () == '"');
  rust_assert (input.back () == '"');
  return input.substr (1, input.length () - 2);
}

} // namespace AST
} // namespace Rust
