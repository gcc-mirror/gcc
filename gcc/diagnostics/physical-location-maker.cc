/* Convenient but inefficient creation of location_t values.
   Copyright (C) 2023-2026 Free Software Foundation, Inc.

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
#include "cpplib.h"
#include "diagnostics/physical-location-maker.h"
#include "diagnostics/dumping.h"

namespace diagnostics {

// class physical_location_maker

location_t
physical_location_maker::
new_location_from_file_and_line (const char *filename,
				 int line_num)
{
  ensure_linemap_for_file_and_line (filename, line_num);
  return linemap_position_for_column (m_line_table, 0);
}

location_t
physical_location_maker::
new_location_from_file_line_column (const char *filename,
				    int line_num,
				    int column_num)
{
  ensure_linemap_for_file_and_line (filename, line_num);
  return linemap_position_for_column (m_line_table, column_num);
}

void
physical_location_maker::
ensure_linemap_for_file_and_line (const char *filename,
				  int line_num)
{
  /* Build a simple linemap describing some locations. */
  if (LINEMAPS_ORDINARY_USED (m_line_table) == 0)
    linemap_add (m_line_table, LC_ENTER, false, filename, 0);
  else
    {
      line_map_ordinary *last_map
	= LINEMAPS_LAST_ORDINARY_MAP (m_line_table);
      if (last_map->to_file != filename
	  || (long)line_num < (long)last_map->to_line)
	{
	  line_map *map
	    = const_cast<line_map *>
	    (linemap_add (m_line_table, LC_RENAME_VERBATIM, false,
			  filename, 0));
	  ((line_map_ordinary *)map)->included_from = UNKNOWN_LOCATION;
	}
    }
  linemap_line_start (m_line_table, line_num, 100);
}

} // namespace diagnostics
