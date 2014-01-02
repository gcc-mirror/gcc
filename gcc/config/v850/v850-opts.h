/* Definitions for option handling for NEC V850 series.
   Copyright (C) 1996-2014 Free Software Foundation, Inc.

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

#ifndef V850_OPTS_H
#define V850_OPTS_H

enum small_memory_type {
  /* tiny data area, using EP as base register */
  SMALL_MEMORY_TDA = 0,
  /* small data area using dp as base register */
  SMALL_MEMORY_SDA,
  /* zero data area using r0 as base register */
  SMALL_MEMORY_ZDA,
  SMALL_MEMORY_max
};

#endif
