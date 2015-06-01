/* Functions to support a pool of allocatable objects.
   Copyright (C) 1987-2015 Free Software Foundation, Inc.
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
#include "hash-table.h"
#include "hash-map.h"

ALLOC_POOL_ID_TYPE last_id;

/* Hashtable mapping alloc_pool names to descriptors.  */
hash_map<const char *, alloc_pool_descriptor> *alloc_pool_hash;

struct alloc_pool_descriptor *
allocate_pool_descriptor (const char *name)
{
  if (!alloc_pool_hash)
    alloc_pool_hash = new hash_map<const char *, alloc_pool_descriptor> (10,
									 false,
									 false);

  return &alloc_pool_hash->get_or_insert (name);
}

/* Output per-alloc_pool statistics.  */

/* Used to accumulate statistics about alloc_pool sizes.  */
struct pool_output_info
{
  unsigned long total_created;
  unsigned long total_allocated;
};

/* Called via hash_map.traverse.  Output alloc_pool descriptor pointed out by
   SLOT and update statistics.  */
bool
print_alloc_pool_statistics (const char *const &name,
			     const alloc_pool_descriptor &d,
			     struct pool_output_info *i)
{
  if (d.allocated)
    {
      fprintf (stderr,
	       "%-22s %6d %10lu %10lu(%10lu) %10lu(%10lu) %10lu(%10lu)\n",
	       name, d.elt_size, d.created, d.allocated,
	       d.allocated / d.elt_size, d.peak, d.peak / d.elt_size,
	       d.current, d.current / d.elt_size);
      i->total_allocated += d.allocated;
      i->total_created += d.created;
    }
  return 1;
}

/* Output per-alloc_pool memory usage statistics.  */
void
dump_alloc_pool_statistics (void)
{
  struct pool_output_info info;

  if (! GATHER_STATISTICS)
    return;

  if (!alloc_pool_hash)
    return;

  fprintf (stderr, "\nAlloc-pool Kind         Elt size  Pools  Allocated (elts)            Peak (elts)            Leak (elts)\n");
  fprintf (stderr, "--------------------------------------------------------------------------------------------------------------\n");
  info.total_created = 0;
  info.total_allocated = 0;
  alloc_pool_hash->traverse <struct pool_output_info *,
			     print_alloc_pool_statistics> (&info);
  fprintf (stderr, "--------------------------------------------------------------------------------------------------------------\n");
  fprintf (stderr, "%-22s           %7lu %10lu\n",
	   "Total", info.total_created, info.total_allocated);
  fprintf (stderr, "--------------------------------------------------------------------------------------------------------------\n");
}
