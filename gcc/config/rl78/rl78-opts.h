/* GCC option-handling definitions for the Renesas RL78 processor.
   Copyright (C) 2011-2021 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#ifndef RL78_OPTS_H
#define RL78_OPTS_H

enum rl78_mul_types
{
  MUL_NONE,
  MUL_G13,
  MUL_G14,
  MUL_UNINIT
};

enum rl78_cpu_types
{
  CPU_G10,
  CPU_G13,
  CPU_G14,
  CPU_UNINIT
};

#endif
