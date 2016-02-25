/* Functions to support a pool of allocatable objects.
   Copyright (C) 1987-2016 Free Software Foundation, Inc.
   Contributed by Daniel Berlin <dan@cgsoftware.com>

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
#include "alloc-pool.h"

ALLOC_POOL_ID_TYPE last_id;
mem_alloc_description<pool_usage> pool_allocator_usage;
bool after_memory_report = false;

/* Output per-alloc_pool memory usage statistics.  */
void
dump_alloc_pool_statistics (void)
{
  if (! GATHER_STATISTICS)
    return;

  pool_allocator_usage.dump (ALLOC_POOL_ORIGIN);
}
