/* Data and functions related to line maps and input files.
   Copyright (C) 2004, 2007, 2008, 2009, 2010
   Free Software Foundation, Inc.

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "intl.h"
#include "input.h"

/* Current position in real source file.  */

location_t input_location;

struct line_maps *line_table;

expanded_location
expand_location (source_location loc)
{
  expanded_location xloc;
  if (loc <= BUILTINS_LOCATION)
    {
      xloc.file = loc == UNKNOWN_LOCATION ? NULL : _("<built-in>");
      xloc.line = 0;
      xloc.column = 0;
      xloc.sysp = 0;
    }
  else
    {
      const struct line_map *map = linemap_lookup (line_table, loc);
      xloc.file = map->to_file;
      xloc.line = SOURCE_LINE (map, loc);
      xloc.column = SOURCE_COLUMN (map, loc);
      xloc.sysp = map->sysp != 0;
    };
  return xloc;
}
