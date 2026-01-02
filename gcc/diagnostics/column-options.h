/* Options relating to the meaning of column numbers.
   Copyright (C) 2000-2026 Free Software Foundation, Inc.

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

#ifndef GCC_DIAGNOSTICS_COLUMN_OPTIONS_H
#define GCC_DIAGNOSTICS_COLUMN_OPTIONS_H

namespace diagnostics {

/* A bundle of options relating to the meaning of column numbers.  */

struct column_options
{
  int convert_column (file_cache &fc,
		      expanded_location s) const;

  /* What units to use when outputting the column number.  */
  enum diagnostics_column_unit m_column_unit;

  /* The origin for the column number (1-based or 0-based typically).  */
  int m_column_origin;

  /* The size of the tabstop for tab expansion.  */
  int m_tabstop;
};

} // namespace diagnostics

#endif /* ! GCC_DIAGNOSTICS_COLUMN_OPTIONS_H */
