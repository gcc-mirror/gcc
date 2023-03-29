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

// rust-gcc-diagnostics.cc -- GCC implementation of rust diagnostics interface.

#include "rust-system.h"
#include "rust-diagnostics.h"

#include "options.h"

void
rust_be_internal_error_at (const Location location, const std::string &errmsg)
{
  std::string loc_str = Linemap::location_to_string (location);
  if (loc_str.empty ())
    internal_error ("%s", errmsg.c_str ());
  else
    internal_error ("at %s, %s", loc_str.c_str (), errmsg.c_str ());
}

void
rust_be_error_at (const Location location, const std::string &errmsg)
{
  location_t gcc_loc = location.gcc_location ();
  error_at (gcc_loc, "%s", errmsg.c_str ());
}

void
rust_be_warning_at (const Location location, int opt,
		    const std::string &warningmsg)
{
  location_t gcc_loc = location.gcc_location ();
  warning_at (gcc_loc, opt, "%s", warningmsg.c_str ());
}

void
rust_be_fatal_error (const Location location, const std::string &fatalmsg)
{
  location_t gcc_loc = location.gcc_location ();
  fatal_error (gcc_loc, "%s", fatalmsg.c_str ());
}

void
rust_be_inform (const Location location, const std::string &infomsg)
{
  location_t gcc_loc = location.gcc_location ();
  inform (gcc_loc, "%s", infomsg.c_str ());
}

void
rust_be_error_at (const RichLocation &location, const std::string &errmsg)
{
  /* TODO: 'error_at' would like a non-'const' 'rich_location *'.  */
  rich_location &gcc_loc = const_cast<rich_location &> (location.get ());
  error_at (&gcc_loc, "%s", errmsg.c_str ());
}

void
rust_be_get_quotechars (const char **open_qu, const char **close_qu)
{
  *open_qu = open_quote;
  *close_qu = close_quote;
}

bool
rust_be_debug_p (void)
{
  return !!flag_rust_debug;
}
