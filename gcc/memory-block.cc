/* Shared pool of memory blocks for pool allocators.
   Copyright (C) 2015-2016 Free Software Foundation, Inc.

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

/* Return all blocks from free list to the OS.  */
void
memory_block_pool::clear_free_list ()
{
  while (m_blocks)
    {
      block_list *next = m_blocks->m_next;
      XDELETEVEC (m_blocks);
      m_blocks = next;
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
