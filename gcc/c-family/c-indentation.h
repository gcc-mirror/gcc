/* Definitions for c-indentation.c.
   Copyright (C) 2015-2017 Free Software Foundation, Inc.

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

#ifndef GCC_C_INDENTATION_H
#define GCC_C_INDENTATION_H

/* Token information used by the -Wmisleading-indentation implementation.  */

struct token_indent_info
{
  location_t location;
  ENUM_BITFIELD (cpp_ttype) type : 8;
  ENUM_BITFIELD (rid) keyword : 8;
};

/* Extract token information from TOKEN, which ought to either be a
   cp_token * or a c_token *.  */

template <typename T>
inline token_indent_info
get_token_indent_info (const T *token)
{
  token_indent_info info;
  info.location = token->location;
  info.type = token->type;
  info.keyword = token->keyword;

  return info;
}

extern void
warn_for_misleading_indentation (const token_indent_info &guard_tinfo,
				 const token_indent_info &body_tinfo,
				 const token_indent_info &next_tinfo);

#endif  /* ! GCC_C_INDENTATION_H  */
