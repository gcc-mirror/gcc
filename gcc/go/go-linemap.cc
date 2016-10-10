// go-linemap.cc -- GCC implementation of Linemap.

// Copyright 2011 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "go-linemap.h"

// This class implements the Linemap interface defined by the
// frontend.

class Gcc_linemap : public Linemap
{
 public:
  Gcc_linemap()
    : Linemap(),
      in_file_(false)
  { }

  void
  start_file(const char* file_name, unsigned int line_begin);

  void
  start_line(unsigned int line_number, unsigned int line_size);

  Location
  get_location(unsigned int column);

  void
  stop();

  std::string
  to_string(Location);

  int
  location_line(Location);

 protected:
  Location
  get_predeclared_location();

  Location
  get_unknown_location();

  bool
  is_predeclared(Location);

  bool
  is_unknown(Location);

 private:
  // Whether we are currently reading a file.
  bool in_file_;
};

Linemap* Linemap::instance_ = NULL;

// Start getting locations from a new file.

void
Gcc_linemap::start_file(const char *file_name, unsigned line_begin)
{
  if (this->in_file_)
    linemap_add(line_table, LC_LEAVE, 0, NULL, 0);
  linemap_add(line_table, LC_ENTER, 0, file_name, line_begin);
  this->in_file_ = true;
}

// Stringify a location

std::string
Gcc_linemap::to_string(Location location)
{
  const line_map_ordinary *lmo;
  source_location resolved_location;

  // Screen out unknown and predeclared locations; produce output
  // only for simple file:line locations.
  resolved_location =
      linemap_resolve_location (line_table, location.gcc_location(),
                                LRK_SPELLING_LOCATION, &lmo);
  if (lmo == NULL || resolved_location < RESERVED_LOCATION_COUNT)
    return "";
  const char *path = LINEMAP_FILE (lmo);
  if (!path)
    return "";

  // Strip the source file down to the base file, to reduce clutter.
  std::stringstream ss;
  ss << lbasename(path) << ":" << SOURCE_LINE (lmo, location.gcc_location());
  return ss.str();
}

// Return the line number for a given location (for debugging dumps)
int
Gcc_linemap::location_line(Location loc)
{
  return LOCATION_LINE(loc.gcc_location());
}

// Stop getting locations.

void
Gcc_linemap::stop()
{
  linemap_add(line_table, LC_LEAVE, 0, NULL, 0);
  this->in_file_ = false;
}

// Start a new line.

void
Gcc_linemap::start_line(unsigned lineno, unsigned linesize)
{
  linemap_line_start(line_table, lineno, linesize);
}

// Get a location.

Location
Gcc_linemap::get_location(unsigned column)
{
  return Location(linemap_position_for_column(line_table, column));
}

// Get the unknown location.

Location
Gcc_linemap::get_unknown_location()
{
  return Location(UNKNOWN_LOCATION);
}

// Get the predeclared location.

Location
Gcc_linemap::get_predeclared_location()
{
  return Location(BUILTINS_LOCATION);
}

// Return whether a location is the predeclared location.

bool
Gcc_linemap::is_predeclared(Location loc)
{
  return loc.gcc_location() == BUILTINS_LOCATION;
}

// Return whether a location is the unknown location.

bool
Gcc_linemap::is_unknown(Location loc)
{
  return loc.gcc_location() == UNKNOWN_LOCATION;
}

// Return the Linemap to use for the gcc backend.

Linemap*
go_get_linemap()
{
  return new Gcc_linemap;
}
