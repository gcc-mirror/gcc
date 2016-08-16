/* Source locations within string literals.
   Copyright (C) 2016 Free Software Foundation, Inc.

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

#ifndef GCC_SUBSTRING_LOCATIONS_H
#define GCC_SUBSTRING_LOCATIONS_H

extern const char *get_source_location_for_substring (cpp_reader *pfile,
						      string_concat_db *concats,
						      location_t strloc,
						      enum cpp_ttype type,
						      int caret_idx,
						      int start_idx, int end_idx,
						      location_t *out_loc);

#endif /* ! GCC_SUBSTRING_LOCATIONS_H */
