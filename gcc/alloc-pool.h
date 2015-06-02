/* Functions to support a pool of allocatable objects
   Copyright (C) 1997-2015 Free Software Foundation, Inc.
   Contributed by Daniel Berlin <dan@cgsoftware.com>

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
#ifndef ALLOC_POOL_H
#define ALLOC_POOL_H

#include "hash-map.h"

extern void dump_alloc_pool_statistics (void);

typedef unsigned long ALLOC_POOL_ID_TYPE;

/* Type based memory pool allocator.  */
template <typename T>
class pool_allocator
{
public:
  /* Default constructor for pool allocator called NAME.  Each block
     has NUM elements.  The allocator support EXTRA_SIZE and can
     potentially IGNORE_TYPE_SIZE.  */
  pool_allocator (const char *name, size_t num, size_t extra_size = 0,
		  bool ignore_type_size = false);
  ~pool_allocator ();
  void release ();
  void release_if_empty ();
  T *allocate () ATTRIBUTE_MALLOC;
  void remove (T *object);

private:
  struct allocation_pool_list
  {
    allocation_pool_list *next;
  };

  /* Initialize a pool allocator.  */
  void initialize ();

  template <typename U>
  struct allocation_object
  {
    /* The ID of alloc pool which the object was allocated from.  */
    ALLOC_POOL_ID_TYPE id;

    union
      {
	/* The data of the object.  */
	char data[1];

	/* Because we want any type of data to be well aligned after the ID,
	   the following elements are here.  They are never accessed so
	   the allocated object may be even smaller than this structure.
	   We do not care about alignment for floating-point types.  */
	char *align_p;
	int64_t align_i;
      } u;

    static inline allocation_object<U> *get_instance (void *data_ptr)
    {
      return (allocation_object<U> *)(((char *)(data_ptr))
				      - offsetof (allocation_object<U>,
						  u.data));
    }

    static inline U *get_data (void *instance_ptr)
    {
      return (U*)(((allocation_object<U> *) instance_ptr)->u.data);
    }
  };

  /* Align X to 8.  */
  size_t align_eight (size_t x)
  {
    return (((x+7) >> 3) << 3);
  }

  const char *m_name;
  ALLOC_POOL_ID_TYPE m_id;
  size_t m_elts_per_block;

  /* These are the elements that have been allocated at least once
     and freed.  */
  allocation_pool_list *m_returned_free_list;

  /* These are the elements that have not yet been allocated out of
     the last block obtained from XNEWVEC.  */
  char* m_virgin_free_list;

  /* The number of elements in the virgin_free_list that can be
     allocated before needing another block.  */
  size_t m_virgin_elts_remaining;
  /* The number of elements that are allocated.  */
  size_t m_elts_allocated;
  /* The number of elements that are released.  */
  size_t m_elts_free;
  /* The number of allocated blocks.  */
  size_t m_blocks_allocated;
  /* List of blocks that are used to allocate new objects.  */
  allocation_pool_list *m_block_list;
  /* The number of elements in a block.  */
  size_t m_block_size;
  /* Size of a pool elements in bytes.  */
  size_t m_elt_size;
  /* Flag if we shoul ignore size of a type.  */
  bool m_ignore_type_size;
  /* Extra size in bytes that should be allocated for each element.  */
  size_t m_extra_size;
  /* Flag if a pool allocator is initialized.  */
  bool m_initialized;
};

/* Last used ID.  */
extern ALLOC_POOL_ID_TYPE last_id;

/* Store information about each particular alloc_pool.  Note that this
   will underestimate the amount the amount of storage used by a small amount:
   1) The overhead in a pool is not accounted for.
   2) The unallocated elements in a block are not accounted for.  Note
   that this can at worst case be one element smaller that the block
   size for that pool.  */
struct alloc_pool_descriptor
{
  /* Number of pools allocated.  */
  unsigned long created;
  /* Gross allocated storage.  */
  unsigned long allocated;
  /* Amount of currently active storage.  */
  unsigned long current;
  /* Peak amount of storage used.  */
  unsigned long peak;
  /* Size of element in the pool.  */
  int elt_size;
};


/* Hashtable mapping alloc_pool names to descriptors.  */
extern hash_map<const char *, alloc_pool_descriptor> *alloc_pool_hash;

/* For given name, return descriptor, create new if needed.  */
alloc_pool_descriptor *
allocate_pool_descriptor (const char *name);

template <typename T>
inline
pool_allocator<T>::pool_allocator (const char *name, size_t num,
				   size_t extra_size, bool ignore_type_size):
  m_name (name), m_id (0), m_elts_per_block (num), m_returned_free_list (NULL),
  m_virgin_free_list (NULL), m_virgin_elts_remaining (0), m_elts_allocated (0),
  m_elts_free (0), m_blocks_allocated (0), m_block_list (NULL),
  m_block_size (0), m_ignore_type_size (ignore_type_size),
  m_extra_size (extra_size), m_initialized (false) {}

/* Initialize a pool allocator.  */

template <typename T>
void
pool_allocator<T>::initialize ()
{
  gcc_checking_assert (!m_initialized);
  m_initialized = true;

  size_t header_size;
  size_t size = (m_ignore_type_size ? 0 : sizeof (T)) + m_extra_size;

  gcc_checking_assert (m_name);

  /* Make size large enough to store the list header.  */
  if (size < sizeof (allocation_pool_list*))
    size = sizeof (allocation_pool_list*);

  /* Now align the size to a multiple of 4.  */
  size = align_eight (size);

  /* Add the aligned size of ID.  */
  size += offsetof (allocation_object<T>, u.data);

  /* Um, we can't really allocate 0 elements per block.  */
  gcc_checking_assert (m_elts_per_block);

  m_elt_size = size;

  if (GATHER_STATISTICS)
    {
      alloc_pool_descriptor *desc = allocate_pool_descriptor (m_name);
      desc->elt_size = size;
      desc->created++;
    }

  /* List header size should be a multiple of 8.  */
  header_size = align_eight (sizeof (allocation_pool_list));

  m_block_size = (size * m_elts_per_block) + header_size;

#ifdef ENABLE_CHECKING
  /* Increase the last used ID and use it for this pool.
     ID == 0 is used for free elements of pool so skip it.  */
  last_id++;
  if (last_id == 0)
    last_id++;

  m_id = last_id;
#endif
}

/* Free all memory allocated for the given memory pool.  */
template <typename T>
inline void
pool_allocator<T>::release ()
{
  if (!m_initialized)
    return;

  allocation_pool_list *block, *next_block;

  /* Free each block allocated to the pool.  */
  for (block = m_block_list; block != NULL; block = next_block)
    {
      next_block = block->next;
      free (block);
    }

  if (GATHER_STATISTICS && false)
    {
      alloc_pool_descriptor *desc = allocate_pool_descriptor (m_name);
      desc->current -= (m_elts_allocated - m_elts_free) * m_elt_size;
    }

  m_returned_free_list = NULL;
  m_virgin_free_list = NULL;
  m_virgin_elts_remaining = 0;
  m_elts_allocated = 0;
  m_elts_free = 0;
  m_blocks_allocated = 0;
  m_block_list = NULL;
}

template <typename T>
void
inline pool_allocator<T>::release_if_empty ()
{
  if (m_elts_free == m_elts_allocated)
    release ();
}

template <typename T>
inline pool_allocator<T>::~pool_allocator ()
{
  release ();
}

/* Allocates one element from the pool specified.  */
template <typename T>
inline T *
pool_allocator<T>::allocate ()
{
  if (!m_initialized)
    initialize ();

  allocation_pool_list *header;
#ifdef ENABLE_VALGRIND_ANNOTATIONS
  int size;
#endif

  if (GATHER_STATISTICS)
    {
      alloc_pool_descriptor *desc = allocate_pool_descriptor (m_name);

      desc->allocated += m_elt_size;
      desc->current += m_elt_size;
      if (desc->peak < desc->current)
	desc->peak = desc->current;
    }

#ifdef ENABLE_VALGRIND_ANNOTATIONS
  size = m_elt_size - offsetof (allocation_object<T>, u.data);
#endif

  /* If there are no more free elements, make some more!.  */
  if (!m_returned_free_list)
    {
      char *block;
      if (!m_virgin_elts_remaining)
	{
	  allocation_pool_list *block_header;

	  /* Make the block.  */
	  block = XNEWVEC (char, m_block_size);
	  block_header = (allocation_pool_list*) block;
	  block += align_eight (sizeof (allocation_pool_list));

	  /* Throw it on the block list.  */
	  block_header->next = m_block_list;
	  m_block_list = block_header;

	  /* Make the block available for allocation.  */
	  m_virgin_free_list = block;
	  m_virgin_elts_remaining = m_elts_per_block;

	  /* Also update the number of elements we have free/allocated, and
	     increment the allocated block count.  */
	  m_elts_allocated += m_elts_per_block;
	  m_elts_free += m_elts_per_block;
	  m_blocks_allocated += 1;
	}

      /* We now know that we can take the first elt off the virgin list and
	 put it on the returned list.  */
      block = m_virgin_free_list;
      header = (allocation_pool_list*) allocation_object<T>::get_data (block);
      header->next = NULL;
#ifdef ENABLE_CHECKING
      /* Mark the element to be free.  */
      ((allocation_object<T> *) block)->id = 0;
#endif
      VALGRIND_DISCARD (VALGRIND_MAKE_MEM_NOACCESS (header,size));
      m_returned_free_list = header;
      m_virgin_free_list += m_elt_size;
      m_virgin_elts_remaining--;

    }

  /* Pull the first free element from the free list, and return it.  */
  header = m_returned_free_list;
  VALGRIND_DISCARD (VALGRIND_MAKE_MEM_DEFINED (header, sizeof (*header)));
  m_returned_free_list = header->next;
  m_elts_free--;

#ifdef ENABLE_CHECKING
  /* Set the ID for element.  */
  allocation_object<T>::get_instance (header)->id = m_id;
#endif
  VALGRIND_DISCARD (VALGRIND_MAKE_MEM_UNDEFINED (header, size));

  /* Call default constructor.  */
  return (T *)(header);
}

/* Puts PTR back on POOL's free list.  */
template <typename T>
void
pool_allocator<T>::remove (T *object)
{
  gcc_checking_assert (m_initialized);

  allocation_pool_list *header;
  int size ATTRIBUTE_UNUSED;
  size = m_elt_size - offsetof (allocation_object<T>, u.data);

#ifdef ENABLE_CHECKING
  gcc_assert (object
	      /* Check if we free more than we allocated, which is Bad (TM).  */
	      && m_elts_free < m_elts_allocated
	      /* Check whether the PTR was allocated from POOL.  */
	      && m_id == allocation_object<T>::get_instance (object)->id);

  memset (object, 0xaf, size);

  /* Mark the element to be free.  */
  allocation_object<T>::get_instance (object)->id = 0;
#endif

  header = (allocation_pool_list*) object;
  header->next = m_returned_free_list;
  m_returned_free_list = header;
  VALGRIND_DISCARD (VALGRIND_MAKE_MEM_NOACCESS (object, size));
  m_elts_free++;

  if (GATHER_STATISTICS)
    {
      alloc_pool_descriptor *desc = allocate_pool_descriptor (m_name);
      desc->current -= m_elt_size;
    }
}

#endif
