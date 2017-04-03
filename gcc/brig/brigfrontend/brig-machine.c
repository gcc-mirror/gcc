/* brig-machine.c -- gccbrig machine queries
   Copyright (C) 2016-2017 Free Software Foundation, Inc.
   Contributed by Pekka Jaaskelainen <pekka.jaaskelainen@parmance.com>
   for General Processor Tech.

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
#include "brig-machine.h"

/* Return the numerical address space id for the segment in the current
   target.  Currently a dummy function that always returns 0, serves as
   a placeholder for multi-AS machines.  */

unsigned
gccbrig_get_target_addr_space_id (BrigSegment8_t)
{
  return 0;
}

/* Return the WAVESIZE for the current target.  For now a dummy placeholder
   returning always 1.  */

unsigned
gccbrig_get_target_wavesize ()
{
  return 1;
}
