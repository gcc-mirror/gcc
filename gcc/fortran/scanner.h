/* Character scanner header.
   Copyright (C) 2013-2014 Free Software Foundation, Inc.
   Contributed by Janne Blomqvist

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


/* Structure for holding module and include file search path.  */
typedef struct gfc_directorylist
{
  char *path;
  bool use_for_modules;
  struct gfc_directorylist *next;
}
gfc_directorylist;

/* List of include file search directories.  */
extern gfc_directorylist *include_dirs, *intrinsic_modules_dirs;
