/* Definitions for option handling for IA-64.
   Copyright (C) 1999-2016 Free Software Foundation, Inc.

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

#ifndef IA64_OPTS_H
#define IA64_OPTS_H

/* Which processor to schedule for. The cpu attribute defines a list
   that mirrors this list, so changes to ia64.md must be made at the
   same time.  */

enum processor_type
{
  PROCESSOR_ITANIUM,			/* Original Itanium.  */
  PROCESSOR_ITANIUM2,
  PROCESSOR_max
};

#endif
