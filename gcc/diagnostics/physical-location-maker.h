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

#ifndef GCC_DIAGNOSTICS_PHYSICAL_LOCATION_MAKER_H
#define GCC_DIAGNOSTICS_PHYSICAL_LOCATION_MAKER_H

namespace diagnostics {

/* A class for generating location_t values for arbitrary
   filename/line/column values.
   This is less efficient than working with the line_maps
   directly, but is more convenient and flexible for occasional
   on-demand location_t values.  */

class physical_location_maker
{
public:
  physical_location_maker (line_maps *line_table_)
  : m_line_table (line_table_)
  {
  }

  location_t
  new_location_from_file_and_line (const char *filename,
				   int line_num);

  /* column_num is 1-based.  */
  location_t
  new_location_from_file_line_column (const char *filename,
				      int line_num,
				      int column_num);

private:
  void
  ensure_linemap_for_file_and_line (const char *filename,
				    int linenum);

private:
  line_maps *m_line_table;
};

} // namespace diagnostics

#endif // #ifndef GCC_DIAGNOSTICS_PHYSICAL_LOCATION_MAKER_H
