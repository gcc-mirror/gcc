/* Shared pool of memory blocks for pool allocators.
   Copyright (C) 2015-2021 Free Software Foundation, Inc.

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
#include "memory-block.h"
#include "obstack.h"

/* Global singleton-like instance.  */
memory_block_pool memory_block_pool::instance;

memory_block_pool::memory_block_pool () : m_blocks (NULL) {}

/* Reduce free list to NUM blocks and return remaining to malloc.  */
void
memory_block_pool::reduce_free_list (int num)
{
  block_list **blocks = &m_blocks;

  /* First skip NUM blocks.  */

  for (;num > 0 && *blocks; num--)
    blocks = &(*blocks)->m_next;

  if (!*blocks)
    return;

  /* And free the remainder of them.  */

  block_list *to_free = *blocks;
  *blocks = NULL;

  while (to_free)
    {
      block_list *next = to_free->m_next;
      XDELETEVEC (to_free);
      to_free = next;
    }
}

/* Allocate a chunk for obstack.  Use the pool if requested chunk size matches
   the size of blocks in the pool.  */
void *
mempool_obstack_chunk_alloc (size_t size)
{
  if (size == memory_block_pool::block_size)
    return memory_block_pool::allocate ();
  else
    return XNEWVEC (char, size);
}

/* Free previously allocated obstack chunk.  */
void
mempool_obstack_chunk_free (void *chunk)
{
  size_t size = (reinterpret_cast<_obstack_chunk *> (chunk)->limit
		 - reinterpret_cast<char *> (chunk));
  if (size == memory_block_pool::block_size)
    memory_block_pool::release (chunk);
  else
    XDELETEVEC (chunk);
}

/* Return allocated memory back to malloc (and to system).  */
void
memory_block_pool::trim (int num)
{
  instance.reduce_free_list (num);
}
