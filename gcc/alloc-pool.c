/* Functions to support a pool of allocatable objects.
   Copyright (C) 1987, 1997, 1998, 1999, 2000, 2001 Free Software Foundation, Inc.
   Contributed by Daniel Berlin <dan@cgsoftware.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

#include "libiberty.h"
#include "config.h"
#include "system.h"
#include "alloc-pool.h"

#define align_four(x) (((x+3) >> 2) << 2)
#define align_eight(x) (((x+7) >> 3) << 3)

/* Create a pool of things of size SIZE, with NUM in each block we
   allocate.  */

alloc_pool
create_alloc_pool (name, size, num)
     const char *name;
     size_t size;
     size_t num;
{
  alloc_pool pool;
  size_t pool_size, header_size;

  if (!name)
    abort ();

  /* Make size large enough to store the list header.  */
  if (size < sizeof (alloc_pool_list))
    size = sizeof (alloc_pool_list);

  /* Now align the size to a multiple of 4.  */
  size = align_four (size);

  /* Um, we can't really allocate 0 elements per block.  */
  if (num == 0)
    abort ();

  /* Find the size of the pool structure, and the name.  */
  pool_size = sizeof (struct alloc_pool_def);

  /* and allocate that much memory.  */
  pool = (alloc_pool) xmalloc (pool_size);

  /* Now init the various pieces of our pool structure.  */
  pool->name = xstrdup (name);
  pool->elt_size = size;
  pool->elts_per_block = num;

  /* List header size should be a multiple of 8 */
  header_size = align_eight (sizeof (struct alloc_pool_list_def));

  pool->block_size = (size * num) + header_size;
  pool->free_list = NULL;
  pool->elts_allocated = 0;
  pool->elts_free = 0;
  pool->blocks_allocated = 0;
  pool->block_list = NULL;

  return (pool);
}

/* Free all memory allocated for the given memory pool.  */
void
free_alloc_pool (pool)
     alloc_pool pool;
{
  alloc_pool_list block, next_block;

  if (!pool)
    abort ();

  /* Free each block allocated to the pool.  */
  for (block = pool->block_list; block != NULL; block = next_block)
    {
      next_block = block->next;
      free (block);
    }
  /* Lastly, free the pool and the name.  */
  free (pool->name);
  free (pool);
}

/* Allocates one element from the pool specified.  */
void *
pool_alloc (pool)
     alloc_pool pool;
{
  alloc_pool_list header;
  char *block;

  if (!pool)
    abort ();

  /* If there are no more free elements, make some more!.  */
  if (!pool->free_list)
    {
      size_t i;
      alloc_pool_list block_header;

      /* Make the block */
      block = (char *) xmalloc (pool->block_size);
      block_header = (alloc_pool_list) block;
      block += align_eight (sizeof (struct alloc_pool_list_def));

      /* Throw it on the block list */
      block_header->next = pool->block_list;
      pool->block_list = block_header;

      /* Now put the actual block pieces onto the free list.  */
      for (i = 0; i < pool->elts_per_block; i++, block += pool->elt_size)
      {
        header = (alloc_pool_list) block;
        header->next = pool->free_list;
        pool->free_list = header;
      }
      /* Also update the number of elements we have free/allocated, and
         increment the allocated block count.  */
      pool->elts_allocated += pool->elts_per_block;
      pool->elts_free += pool->elts_per_block;
      pool->blocks_allocated += 1;
    }

  /* Pull the first free element from the free list, and return it.  */
  header = pool->free_list;
  pool->free_list = header->next;
  pool->elts_free--;
  return ((void *) header);
}

/* Puts PTR back on POOL's free list.  */
void
pool_free (pool, ptr)
     alloc_pool pool;
     void *ptr;
{
  alloc_pool_list header;

  if (!ptr)
    abort ();

  /* Check if we free more than we allocated, which is Bad (TM).  */
  if (pool->elts_free + 1 > pool->elts_allocated)
    abort ();
  header = (alloc_pool_list) ptr;
  header->next = pool->free_list;
  pool->free_list = header;
  pool->elts_free++;
}
