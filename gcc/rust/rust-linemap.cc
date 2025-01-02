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

// rust-linemap.cc -- GCC implementation of Linemap.

#include "rust-linemap.h"

Linemap *Linemap::instance_ = NULL;

// Start getting locations from a new file.

void
Linemap::start_file (const char *file_name, unsigned line_begin)
{
  if (this->in_file_)
    linemap_add (line_table, LC_LEAVE, 0, NULL, 0);
  linemap_add (line_table, LC_ENTER, 0, file_name, line_begin);
  this->in_file_ = true;
}

// Stringify a location

std::string
Linemap::location_to_string (location_t location)
{
  const line_map_ordinary *lmo;
  location_t resolved_location;

  // Screen out unknown and predeclared locations; produce output
  // only for simple file:line locations.
  resolved_location = linemap_resolve_location (line_table, location,
						LRK_SPELLING_LOCATION, &lmo);
  if (lmo == NULL || resolved_location < RESERVED_LOCATION_COUNT)
    return "";
  const char *path = LINEMAP_FILE (lmo);
  if (!path)
    return "";

  // Strip the source file down to the base file, to reduce clutter.
  std::stringstream ss;
  ss << lbasename (path) << ":" << SOURCE_LINE (lmo, location) << ":"
     << SOURCE_COLUMN (lmo, location);
  return ss.str ();
}

// Stop getting locations.

void
Linemap::stop ()
{
  linemap_add (line_table, LC_LEAVE, 0, NULL, 0);
  this->in_file_ = false;
}

// Return the Linemap to use for the gcc backend.

Linemap *
rust_get_linemap ()
{
  return new Linemap;
}
