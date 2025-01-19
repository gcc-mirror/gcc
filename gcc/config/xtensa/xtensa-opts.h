/* Definitions for option handling for Xtensa.
   Copyright (C) 2023-2025 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef XTENSA_OPTS_H
#define XTENSA_OPTS_H

/* Undefined state for the -mstrict-alignment option  */
enum xtensa_strict_alignment_setting {
  XTENSA_STRICT_ALIGNMENT_UNDEFINED = -1,
};

#endif
