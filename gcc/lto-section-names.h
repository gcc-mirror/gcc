/* Definitions for LTO section names.
   Copyright (C) 2013 Free Software Foundation, Inc.

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

/* The string that is the prefix on the section names we make for lto.
   For decls the DECL_ASSEMBLER_NAME is appended to make the section
   name for the functions and static_initializers.  For other types of
   sections a '.' and the section type are appended.  */
#define LTO_SECTION_NAME_PREFIX ".gnu.lto_"

/* Segment name for LTO sections.  This is only used for Mach-O.  */

#define LTO_SEGMENT_NAME "__GNU_LTO"
