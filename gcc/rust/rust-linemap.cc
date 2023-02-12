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

// rust-linemap.cc -- GCC implementation of Linemap.

#include "rust-linemap.h"

// This class implements the Linemap interface defined by the
// frontend.

class Gcc_linemap : public Linemap
{
public:
  Gcc_linemap () : Linemap (), in_file_ (false) {}

  void start_file (const char *file_name, unsigned int line_begin);

  void start_line (unsigned int line_number, unsigned int line_size);

  Location get_location (unsigned int column);

  void stop ();

  std::string to_string (Location);

  std::string location_file (Location);

  int location_line (Location);

  int location_column (Location);

protected:
  Location get_predeclared_location ();

  Location get_unknown_location ();

  bool is_predeclared (Location);

  bool is_unknown (Location);

private:
  // Whether we are currently reading a file.
  bool in_file_;
};

Linemap *Linemap::instance_ = NULL;

// Start getting locations from a new file.

void
Gcc_linemap::start_file (const char *file_name, unsigned line_begin)
{
  if (this->in_file_)
    linemap_add (line_table, LC_LEAVE, 0, NULL, 0);
  linemap_add (line_table, LC_ENTER, 0, file_name, line_begin);
  this->in_file_ = true;
}

// Stringify a location

std::string
Gcc_linemap::to_string (Location location)
{
  const line_map_ordinary *lmo;
  location_t resolved_location;

  // Screen out unknown and predeclared locations; produce output
  // only for simple file:line locations.
  resolved_location
    = linemap_resolve_location (line_table, location.gcc_location (),
				LRK_SPELLING_LOCATION, &lmo);
  if (lmo == NULL || resolved_location < RESERVED_LOCATION_COUNT)
    return "";
  const char *path = LINEMAP_FILE (lmo);
  if (!path)
    return "";

  // Strip the source file down to the base file, to reduce clutter.
  std::stringstream ss;
  ss << lbasename (path) << ":" << SOURCE_LINE (lmo, location.gcc_location ())
     << ":" << SOURCE_COLUMN (lmo, location.gcc_location ());
  return ss.str ();
}

// Return the file name for a given location.

std::string
Gcc_linemap::location_file (Location loc)
{
  return LOCATION_FILE (loc.gcc_location ());
}

// Return the line number for a given location.

int
Gcc_linemap::location_line (Location loc)
{
  return LOCATION_LINE (loc.gcc_location ());
}

// Return the column number for a given location.
int
Gcc_linemap::location_column (Location loc)
{
  return LOCATION_COLUMN (loc.gcc_location ());
}

// Stop getting locations.

void
Gcc_linemap::stop ()
{
  linemap_add (line_table, LC_LEAVE, 0, NULL, 0);
  this->in_file_ = false;
}

// Start a new line.

void
Gcc_linemap::start_line (unsigned lineno, unsigned linesize)
{
  linemap_line_start (line_table, lineno, linesize);
}

// Get a location.

Location
Gcc_linemap::get_location (unsigned column)
{
  return Location (linemap_position_for_column (line_table, column));
}

// Get the unknown location.

Location
Gcc_linemap::get_unknown_location ()
{
  return Location (UNKNOWN_LOCATION);
}

// Get the predeclared location.

Location
Gcc_linemap::get_predeclared_location ()
{
  return Location (BUILTINS_LOCATION);
}

// Return whether a location is the predeclared location.

bool
Gcc_linemap::is_predeclared (Location loc)
{
  return loc.gcc_location () == BUILTINS_LOCATION;
}

// Return whether a location is the unknown location.

bool
Gcc_linemap::is_unknown (Location loc)
{
  return loc.gcc_location () == UNKNOWN_LOCATION;
}

// Return the Linemap to use for the gcc backend.

Linemap *
rust_get_linemap ()
{
  return new Gcc_linemap;
}

RichLocation::RichLocation (Location root)
  : gcc_rich_loc (line_table, root.gcc_location ())
{
  /*rich_location (line_maps *set, location_t loc,
		 const range_label *label = NULL);*/
}

RichLocation::~RichLocation () {}

void
RichLocation::add_range (Location loc)
{
  gcc_rich_loc.add_range (loc.gcc_location ());
}

void
RichLocation::add_fixit_insert_before (const std::string &new_parent)
{
  gcc_rich_loc.add_fixit_insert_before (new_parent.c_str ());
}

void
RichLocation::add_fixit_insert_before (Location where,
				       const std::string &new_parent)
{
  gcc_rich_loc.add_fixit_insert_before (where.gcc_location (),
					new_parent.c_str ());
}

void
RichLocation::add_fixit_insert_after (const std::string &new_parent)
{
  gcc_rich_loc.add_fixit_insert_after (new_parent.c_str ());
}

void
RichLocation::add_fixit_insert_after (Location where,
				      const std::string &new_parent)
{
  gcc_rich_loc.add_fixit_insert_after (where.gcc_location (),
				       new_parent.c_str ());
}
