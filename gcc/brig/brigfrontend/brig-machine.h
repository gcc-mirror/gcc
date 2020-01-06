/* brig-machine.h -- gccbrig machine queries
   Copyright (C) 2016-2020 Free Software Foundation, Inc.

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

#ifndef GCC_BRIG_MACHINE_H
#define GCC_BRIG_MACHINE_H

#include "hsa-brig-format.h"

/* These functions should be eventually converted to machine info queries and
   redefined at backends.  At that point make these functions delegate to
   those.  */

unsigned gccbrig_get_target_addr_space_id (BrigSegment8_t segment);

unsigned gccbrig_get_target_wavesize ();

#endif
