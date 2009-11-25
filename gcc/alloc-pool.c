/* Functions to support a pool of allocatable objects.
   Copyright (C) 1987, 1997, 1998, 1999, 2000, 2001, 2003, 2004, 2005, 2006,
   2007, 2008  Free Software Foundation, Inc.
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
#include "alloc-pool.h"
#include "hashtab.h"

#define align_eight(x) (((x+7) >> 3) << 3)

/* The internal allocation object.  */
typedef struct allocation_object_def
{
#ifdef ENABLE_CHECKING
  /* The ID of alloc pool which the object was allocated from.  */
  ALLOC_POOL_ID_TYPE id;
#endif

  union
    {
      /* The data of the object.  */
      char data[1];

      /* Because we want any type of data to be well aligned after the ID,
	 the following elements are here.  They are never accessed so
	 the allocated object may be even smaller than this structure.
	 We do not care about alignment for floating-point types.  */
      char *align_p;
      HOST_WIDEST_INT align_i;
    } u;
} allocation_object;

/* Convert a pointer to allocation_object from a pointer to user data.  */
#define ALLOCATION_OBJECT_PTR_FROM_USER_PTR(X)				\
   ((allocation_object *) (((char *) (X))				\
			   - offsetof (allocation_object, u.data)))

/* Convert a pointer to user data from a pointer to allocation_object.  */
#define USER_PTR_FROM_ALLOCATION_OBJECT_PTR(X)				\
   ((void *) (((allocation_object *) (X))->u.data))

#ifdef ENABLE_CHECKING
/* Last used ID.  */
static ALLOC_POOL_ID_TYPE last_id;
#endif

#ifdef GATHER_STATISTICS

/* Store information about each particular alloc_pool.  Note that this
   will underestimate the amount the amount of storage used by a small amount:
   1) The overhead in a pool is not accounted for.
   2) The unallocated elements in a block are not accounted for.  Note
   that this can at worst case be one element smaller that the block
   size for that pool.  */
struct alloc_pool_descriptor
{
  const char *name;
  /* Number of pools allocated.  */
  unsigned long created;
  /* Gross allocated storage.  */
  unsigned long allocated;
  /* Amount of currently active storage. */
  unsigned long current;
  /* Peak amount of storage used.  */
  unsigned long peak;
  /* Size of element in the pool.  */
  int elt_size;
};

/* Hashtable mapping alloc_pool names to descriptors.  */
static htab_t alloc_pool_hash;

/* Hashtable helpers.  */
static hashval_t
hash_descriptor (const void *p)
{
  const struct alloc_pool_descriptor *const d =
    (const struct alloc_pool_descriptor * )p;
  return htab_hash_pointer (d->name);
}
static int
eq_descriptor (const void *p1, const void *p2)
{
  const struct alloc_pool_descriptor *const d =
    (const struct alloc_pool_descriptor *) p1;
  return d->name == p2;
}

/* For given name, return descriptor, create new if needed.  */
static struct alloc_pool_descriptor *
alloc_pool_descriptor (const char *name)
{
  struct alloc_pool_descriptor **slot;

  if (!alloc_pool_hash)
    alloc_pool_hash = htab_create (10, hash_descriptor, eq_descriptor, NULL);

  slot = (struct alloc_pool_descriptor **)
    htab_find_slot_with_hash (alloc_pool_hash, name,
			      htab_hash_pointer (name),
			      INSERT);
  if (*slot)
    return *slot;
  *slot = XCNEW (struct alloc_pool_descriptor);
  (*slot)->name = name;
  return *slot;
}
#endif

/* Create a pool of things of size SIZE, with NUM in each block we
   allocate.  */

alloc_pool
create_alloc_pool (const char *name, size_t size, size_t num)
{
  alloc_pool pool;
  size_t header_size;
#ifdef GATHER_STATISTICS
  struct alloc_pool_descriptor *desc;
#endif

  gcc_assert (name);

  /* Make size large enough to store the list header.  */
  if (size < sizeof (alloc_pool_list))
    size = sizeof (alloc_pool_list);

  /* Now align the size to a multiple of 4.  */
  size = align_eight (size);

#ifdef ENABLE_CHECKING
  /* Add the aligned size of ID.  */
  size += offsetof (allocation_object, u.data);
#endif

  /* Um, we can't really allocate 0 elements per block.  */
  gcc_assert (num);

  /* Allocate memory for the pool structure.  */
  pool = XNEW (struct alloc_pool_def);

  /* Now init the various pieces of our pool structure.  */
  pool->name = /*xstrdup (name)*/name;
#ifdef GATHER_STATISTICS
  desc = alloc_pool_descriptor (name);
  desc->elt_size = size;
  desc->created++;
#endif
  pool->elt_size = size;
  pool->elts_per_block = num;

  /* List header size should be a multiple of 8.  */
  header_size = align_eight (sizeof (struct alloc_pool_list_def));

  pool->block_size = (size * num) + header_size;
  pool->returned_free_list = NULL;
  pool->virgin_free_list = NULL;
  pool->virgin_elts_remaining = 0;
  pool->elts_allocated = 0;
  pool->elts_free = 0;
  pool->blocks_allocated = 0;
  pool->block_list = NULL;

#ifdef ENABLE_CHECKING
  /* Increase the last used ID and use it for this pool.
     ID == 0 is used for free elements of pool so skip it.  */
  last_id++;
  if (last_id == 0)
    last_id++;

  pool->id = last_id;
#endif

  return (pool);
}

/* Free all memory allocated for the given memory pool.  */
void
empty_alloc_pool (alloc_pool pool)
{
  alloc_pool_list block, next_block;
#ifdef GATHER_STATISTICS
  struct alloc_pool_descriptor *desc = alloc_pool_descriptor (pool->name);
#endif

  gcc_assert (pool);

  /* Free each block allocated to the pool.  */
  for (block = pool->block_list; block != NULL; block = next_block)
    {
      next_block = block->next;
      free (block);
    }

#ifdef GATHER_STATISTICS
  desc->current -= (pool->elts_allocated - pool->elts_free) * pool->elt_size;
#endif
  pool->returned_free_list = NULL;
  pool->virgin_free_list = NULL;
  pool->virgin_elts_remaining = 0;
  pool->elts_allocated = 0;
  pool->elts_free = 0;
  pool->blocks_allocated = 0;
  pool->block_list = NULL;
}

/* Free all memory allocated for the given memory pool and the pool itself.  */
void
free_alloc_pool (alloc_pool pool)
{
  /* First empty the pool.  */
  empty_alloc_pool (pool);
#ifdef ENABLE_CHECKING
  memset (pool, 0xaf, sizeof (*pool));
#endif
  /* Lastly, free the pool.  */
  free (pool);
}

/* Frees the alloc_pool, if it is empty and zero *POOL in this case.  */
void
free_alloc_pool_if_empty (alloc_pool *pool)
{
  if ((*pool)->elts_free == (*pool)->elts_allocated)
    {
      free_alloc_pool (*pool);
      *pool = NULL;
    }
}

/* Allocates one element from the pool specified.  */
void *
pool_alloc (alloc_pool pool)
{
  alloc_pool_list header;
#ifdef GATHER_STATISTICS
  struct alloc_pool_descriptor *desc = alloc_pool_descriptor (pool->name);

  desc->allocated += pool->elt_size;
  desc->current += pool->elt_size;
  if (desc->peak < desc->current)
    desc->peak = desc->current;
#endif

  gcc_assert (pool);

  /* If there are no more free elements, make some more!.  */
  if (!pool->returned_free_list)
    {
      char *block;
      if (!pool->virgin_elts_remaining)
	{
	  alloc_pool_list block_header;

	  /* Make the block.  */
	  block = XNEWVEC (char, pool->block_size);
	  block_header = (alloc_pool_list) block;
	  block += align_eight (sizeof (struct alloc_pool_list_def));

	  /* Throw it on the block list.  */
	  block_header->next = pool->block_list;
	  pool->block_list = block_header;

	  /* Make the block available for allocation.  */
	  pool->virgin_free_list = block;
	  pool->virgin_elts_remaining = pool->elts_per_block;

	  /* Also update the number of elements we have free/allocated, and
	     increment the allocated block count.  */
	  pool->elts_allocated += pool->elts_per_block;
	  pool->elts_free += pool->elts_per_block;
	  pool->blocks_allocated += 1;
	}


      /* We now know that we can take the first elt off the virgin list and
	 put it on the returned list. */
      block = pool->virgin_free_list;
      header = (alloc_pool_list) USER_PTR_FROM_ALLOCATION_OBJECT_PTR (block);
      header->next = NULL;
#ifdef ENABLE_CHECKING
      /* Mark the element to be free.  */
      ((allocation_object *) block)->id = 0;
#endif
      pool->returned_free_list = header;
      pool->virgin_free_list += pool->elt_size;
      pool->virgin_elts_remaining--;

    }

  /* Pull the first free element from the free list, and return it.  */
  header = pool->returned_free_list;
  pool->returned_free_list = header->next;
  pool->elts_free--;

#ifdef ENABLE_CHECKING
  /* Set the ID for element.  */
  ALLOCATION_OBJECT_PTR_FROM_USER_PTR (header)->id = pool->id;
#endif

  return ((void *) header);
}

/* Puts PTR back on POOL's free list.  */
void
pool_free (alloc_pool pool, void *ptr)
{
  alloc_pool_list header;
#ifdef GATHER_STATISTICS
  struct alloc_pool_descriptor *desc = alloc_pool_descriptor (pool->name);
#endif

  gcc_assert (ptr);

#ifdef ENABLE_CHECKING
  /* Check whether the PTR was allocated from POOL.  */
  gcc_assert (pool->id == ALLOCATION_OBJECT_PTR_FROM_USER_PTR (ptr)->id);

  memset (ptr, 0xaf, pool->elt_size - offsetof (allocation_object, u.data));

  /* Mark the element to be free.  */
  ALLOCATION_OBJECT_PTR_FROM_USER_PTR (ptr)->id = 0;
#else
  /* Check if we free more than we allocated, which is Bad (TM).  */
  gcc_assert (pool->elts_free < pool->elts_allocated);
#endif

  header = (alloc_pool_list) ptr;
  header->next = pool->returned_free_list;
  pool->returned_free_list = header;
  pool->elts_free++;

#ifdef GATHER_STATISTICS
  desc->current -= pool->elt_size;
#endif

}
/* Output per-alloc_pool statistics.  */
#ifdef GATHER_STATISTICS

/* Used to accumulate statistics about alloc_pool sizes.  */
struct output_info
{
  unsigned long total_created;
  unsigned long total_allocated;
};

/* Called via htab_traverse.  Output alloc_pool descriptor pointed out by SLOT
   and update statistics.  */
static int
print_statistics (void **slot, void *b)
{
  struct alloc_pool_descriptor *d = (struct alloc_pool_descriptor *) *slot;
  struct output_info *i = (struct output_info *) b;

  if (d->allocated)
    {
      fprintf (stderr, "%-22s %6d %10lu %10lu(%10lu) %10lu(%10lu) %10lu(%10lu)\n", d->name,
	       d->elt_size, d->created, d->allocated, d->allocated / d->elt_size,
	       d->peak, d->peak / d->elt_size,
	       d->current, d->current / d->elt_size);
      i->total_allocated += d->allocated;
      i->total_created += d->created;
    }
  return 1;
}
#endif

/* Output per-alloc_pool memory usage statistics.  */
void
dump_alloc_pool_statistics (void)
{
#ifdef GATHER_STATISTICS
  struct output_info info;

  if (!alloc_pool_hash)
    return;

  fprintf (stderr, "\nAlloc-pool Kind         Elt size  Pools  Allocated (elts)            Peak (elts)            Leak (elts)\n");
  fprintf (stderr, "--------------------------------------------------------------------------------------------------------------\n");
  info.total_created = 0;
  info.total_allocated = 0;
  htab_traverse (alloc_pool_hash, print_statistics, &info);
  fprintf (stderr, "--------------------------------------------------------------------------------------------------------------\n");
  fprintf (stderr, "%-22s           %7lu %10lu\n",
	   "Total", info.total_created, info.total_allocated);
  fprintf (stderr, "--------------------------------------------------------------------------------------------------------------\n");
#endif
}
