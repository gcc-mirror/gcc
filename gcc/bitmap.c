/* Functions to support general ended bitmaps.
   Copyright (C) 1997-2012  Free Software Foundation, Inc.

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
#include "obstack.h"
#include "ggc.h"
#include "bitmap.h"
#include "hashtab.h"

/* Store information about each particular bitmap.  */
struct bitmap_descriptor
{
  const char *function;
  const char *file;
  int line;
  int created;
  HOST_WIDEST_INT allocated;
  HOST_WIDEST_INT peak;
  HOST_WIDEST_INT current;
  int nsearches;
  int search_iter;
};

/* Hashtable mapping bitmap names to descriptors.  */
static htab_t bitmap_desc_hash;

/* Hashtable helpers.  */
static hashval_t
hash_descriptor (const void *p)
{
  const struct bitmap_descriptor *const d =
    (const struct bitmap_descriptor *) p;
  return htab_hash_pointer (d->file) + d->line;
}
struct loc
{
  const char *file;
  const char *function;
  int line;
};
static int
eq_descriptor (const void *p1, const void *p2)
{
  const struct bitmap_descriptor *const d =
    (const struct bitmap_descriptor *) p1;
  const struct loc *const l = (const struct loc *) p2;
  return d->file == l->file && d->function == l->function && d->line == l->line;
}

/* For given file and line, return descriptor, create new if needed.  */
static struct bitmap_descriptor *
bitmap_descriptor (const char *file, int line, const char *function)
{
  struct bitmap_descriptor **slot;
  struct loc loc;

  loc.file = file;
  loc.function = function;
  loc.line = line;

  if (!bitmap_desc_hash)
    bitmap_desc_hash = htab_create (10, hash_descriptor, eq_descriptor, NULL);

  slot = (struct bitmap_descriptor **)
    htab_find_slot_with_hash (bitmap_desc_hash, &loc,
			      htab_hash_pointer (file) + line,
			      INSERT);
  if (*slot)
    return *slot;
  *slot = XCNEW (struct bitmap_descriptor);
  (*slot)->file = file;
  (*slot)->function = function;
  (*slot)->line = line;
  return *slot;
}

/* Register new bitmap.  */
void
bitmap_register (bitmap b MEM_STAT_DECL)
{
  b->desc = bitmap_descriptor (ALONE_FINAL_PASS_MEM_STAT);
  b->desc->created++;
}

/* Account the overhead.  */
static void
register_overhead (bitmap b, int amount)
{
  b->desc->current += amount;
  if (amount > 0)
    b->desc->allocated += amount;
  gcc_assert (b->desc->current >= 0);
  if (b->desc->peak < b->desc->current)
    b->desc->peak = b->desc->current;
}

/* Global data */
bitmap_element bitmap_zero_bits;  /* An element of all zero bits.  */
bitmap_obstack bitmap_default_obstack;    /* The default bitmap obstack.  */
static int bitmap_default_obstack_depth;
static GTY((deletable)) bitmap_element *bitmap_ggc_free; /* Freelist of
							    GC'd elements.  */

static void bitmap_elem_to_freelist (bitmap, bitmap_element *);
static void bitmap_element_free (bitmap, bitmap_element *);
static bitmap_element *bitmap_element_allocate (bitmap);
static int bitmap_element_zerop (const bitmap_element *);
static void bitmap_element_link (bitmap, bitmap_element *);
static bitmap_element *bitmap_elt_insert_after (bitmap, bitmap_element *, unsigned int);
static void bitmap_elt_clear_from (bitmap, bitmap_element *);
static bitmap_element *bitmap_find_bit (bitmap, unsigned int);


/* Add ELEM to the appropriate freelist.  */
static inline void
bitmap_elem_to_freelist (bitmap head, bitmap_element *elt)
{
  bitmap_obstack *bit_obstack = head->obstack;

  elt->next = NULL;
  if (bit_obstack)
    {
      elt->prev = bit_obstack->elements;
      bit_obstack->elements = elt;
    }
  else
    {
      elt->prev = bitmap_ggc_free;
      bitmap_ggc_free = elt;
    }
}

/* Free a bitmap element.  Since these are allocated off the
   bitmap_obstack, "free" actually means "put onto the freelist".  */

static inline void
bitmap_element_free (bitmap head, bitmap_element *elt)
{
  bitmap_element *next = elt->next;
  bitmap_element *prev = elt->prev;

  if (prev)
    prev->next = next;

  if (next)
    next->prev = prev;

  if (head->first == elt)
    head->first = next;

  /* Since the first thing we try is to insert before current,
     make current the next entry in preference to the previous.  */
  if (head->current == elt)
    {
      head->current = next != 0 ? next : prev;
      if (head->current)
	head->indx = head->current->indx;
      else
	head->indx = 0;
    }

  if (GATHER_STATISTICS)
    register_overhead (head, -((int)sizeof (bitmap_element)));

  bitmap_elem_to_freelist (head, elt);
}

/* Allocate a bitmap element.  The bits are cleared, but nothing else is.  */

static inline bitmap_element *
bitmap_element_allocate (bitmap head)
{
  bitmap_element *element;
  bitmap_obstack *bit_obstack = head->obstack;

  if (bit_obstack)
    {
      element = bit_obstack->elements;

      if (element)
	/* Use up the inner list first before looking at the next
	   element of the outer list.  */
	if (element->next)
	  {
	    bit_obstack->elements = element->next;
	    bit_obstack->elements->prev = element->prev;
	  }
	else
	  /*  Inner list was just a singleton.  */
	  bit_obstack->elements = element->prev;
      else
	element = XOBNEW (&bit_obstack->obstack, bitmap_element);
    }
  else
    {
      element = bitmap_ggc_free;
      if (element)
	/* Use up the inner list first before looking at the next
	   element of the outer list.  */
	if (element->next)
	  {
	    bitmap_ggc_free = element->next;
	    bitmap_ggc_free->prev = element->prev;
	  }
	else
	  /*  Inner list was just a singleton.  */
	  bitmap_ggc_free = element->prev;
      else
	element = ggc_alloc_bitmap_element_def ();
    }

  if (GATHER_STATISTICS)
    register_overhead (head, sizeof (bitmap_element));

  memset (element->bits, 0, sizeof (element->bits));

  return element;
}

/* Remove ELT and all following elements from bitmap HEAD.  */

void
bitmap_elt_clear_from (bitmap head, bitmap_element *elt)
{
  bitmap_element *prev;
  bitmap_obstack *bit_obstack = head->obstack;

  if (!elt) return;

  if (GATHER_STATISTICS)
    {
      int n = 0;
      for (prev = elt; prev; prev = prev->next)
	n++;
      register_overhead (head, -sizeof (bitmap_element) * n);
    }

  prev = elt->prev;
  if (prev)
    {
      prev->next = NULL;
      if (head->current->indx > prev->indx)
	{
	  head->current = prev;
	  head->indx = prev->indx;
	}
    }
  else
    {
      head->first = NULL;
      head->current = NULL;
      head->indx = 0;
    }

  /* Put the entire list onto the free list in one operation. */
  if (bit_obstack)
    {
      elt->prev = bit_obstack->elements;
      bit_obstack->elements = elt;
    }
  else
    {
      elt->prev = bitmap_ggc_free;
      bitmap_ggc_free = elt;
    }
}

/* Clear a bitmap by freeing the linked list.  */

void
bitmap_clear (bitmap head)
{
  if (head->first)
    bitmap_elt_clear_from (head, head->first);
}

/* Initialize a bitmap obstack.  If BIT_OBSTACK is NULL, initialize
   the default bitmap obstack.  */

void
bitmap_obstack_initialize (bitmap_obstack *bit_obstack)
{
  if (!bit_obstack)
    {
      if (bitmap_default_obstack_depth++)
	return;
      bit_obstack = &bitmap_default_obstack;
    }

#if !defined(__GNUC__) || (__GNUC__ < 2)
#define __alignof__(type) 0
#endif

  bit_obstack->elements = NULL;
  bit_obstack->heads = NULL;
  obstack_specify_allocation (&bit_obstack->obstack, OBSTACK_CHUNK_SIZE,
			      __alignof__ (bitmap_element),
			      obstack_chunk_alloc,
			      obstack_chunk_free);
}

/* Release the memory from a bitmap obstack.  If BIT_OBSTACK is NULL,
   release the default bitmap obstack.  */

void
bitmap_obstack_release (bitmap_obstack *bit_obstack)
{
  if (!bit_obstack)
    {
      if (--bitmap_default_obstack_depth)
	{
	  gcc_assert (bitmap_default_obstack_depth > 0);
	  return;
	}
      bit_obstack = &bitmap_default_obstack;
    }

  bit_obstack->elements = NULL;
  bit_obstack->heads = NULL;
  obstack_free (&bit_obstack->obstack, NULL);
}

/* Create a new bitmap on an obstack.  If BIT_OBSTACK is NULL, create
   it on the default bitmap obstack.  */

bitmap
bitmap_obstack_alloc_stat (bitmap_obstack *bit_obstack MEM_STAT_DECL)
{
  bitmap map;

  if (!bit_obstack)
    bit_obstack = &bitmap_default_obstack;
  map = bit_obstack->heads;
  if (map)
    bit_obstack->heads = (struct bitmap_head_def *) map->first;
  else
    map = XOBNEW (&bit_obstack->obstack, bitmap_head);
  bitmap_initialize_stat (map, bit_obstack PASS_MEM_STAT);

  if (GATHER_STATISTICS)
    register_overhead (map, sizeof (bitmap_head));

  return map;
}

/* Create a new GCd bitmap.  */

bitmap
bitmap_gc_alloc_stat (ALONE_MEM_STAT_DECL)
{
  bitmap map;

  map = ggc_alloc_bitmap_head_def ();
  bitmap_initialize_stat (map, NULL PASS_MEM_STAT);

  if (GATHER_STATISTICS)
    register_overhead (map, sizeof (bitmap_head));

  return map;
}

/* Release an obstack allocated bitmap.  */

void
bitmap_obstack_free (bitmap map)
{
  if (map)
    {
      bitmap_clear (map);
      map->first = (bitmap_element *) map->obstack->heads;

      if (GATHER_STATISTICS)
	register_overhead (map, -((int)sizeof (bitmap_head)));

      map->obstack->heads = map;
    }
}


/* Return nonzero if all bits in an element are zero.  */

static inline int
bitmap_element_zerop (const bitmap_element *element)
{
#if BITMAP_ELEMENT_WORDS == 2
  return (element->bits[0] | element->bits[1]) == 0;
#else
  unsigned i;

  for (i = 0; i < BITMAP_ELEMENT_WORDS; i++)
    if (element->bits[i] != 0)
      return 0;

  return 1;
#endif
}

/* Link the bitmap element into the current bitmap linked list.  */

static inline void
bitmap_element_link (bitmap head, bitmap_element *element)
{
  unsigned int indx = element->indx;
  bitmap_element *ptr;

  /* If this is the first and only element, set it in.  */
  if (head->first == 0)
    {
      element->next = element->prev = 0;
      head->first = element;
    }

  /* If this index is less than that of the current element, it goes someplace
     before the current element.  */
  else if (indx < head->indx)
    {
      for (ptr = head->current;
	   ptr->prev != 0 && ptr->prev->indx > indx;
	   ptr = ptr->prev)
	;

      if (ptr->prev)
	ptr->prev->next = element;
      else
	head->first = element;

      element->prev = ptr->prev;
      element->next = ptr;
      ptr->prev = element;
    }

  /* Otherwise, it must go someplace after the current element.  */
  else
    {
      for (ptr = head->current;
	   ptr->next != 0 && ptr->next->indx < indx;
	   ptr = ptr->next)
	;

      if (ptr->next)
	ptr->next->prev = element;

      element->next = ptr->next;
      element->prev = ptr;
      ptr->next = element;
    }

  /* Set up so this is the first element searched.  */
  head->current = element;
  head->indx = indx;
}

/* Insert a new uninitialized element into bitmap HEAD after element
   ELT.  If ELT is NULL, insert the element at the start.  Return the
   new element.  */

static bitmap_element *
bitmap_elt_insert_after (bitmap head, bitmap_element *elt, unsigned int indx)
{
  bitmap_element *node = bitmap_element_allocate (head);
  node->indx = indx;

  if (!elt)
    {
      if (!head->current)
	{
	  head->current = node;
	  head->indx = indx;
	}
      node->next = head->first;
      if (node->next)
	node->next->prev = node;
      head->first = node;
      node->prev = NULL;
    }
  else
    {
      gcc_checking_assert (head->current);
      node->next = elt->next;
      if (node->next)
	node->next->prev = node;
      elt->next = node;
      node->prev = elt;
    }
  return node;
}

/* Copy a bitmap to another bitmap.  */

void
bitmap_copy (bitmap to, const_bitmap from)
{
  const bitmap_element *from_ptr;
  bitmap_element *to_ptr = 0;

  bitmap_clear (to);

  /* Copy elements in forward direction one at a time.  */
  for (from_ptr = from->first; from_ptr; from_ptr = from_ptr->next)
    {
      bitmap_element *to_elt = bitmap_element_allocate (to);

      to_elt->indx = from_ptr->indx;
      memcpy (to_elt->bits, from_ptr->bits, sizeof (to_elt->bits));

      /* Here we have a special case of bitmap_element_link, for the case
	 where we know the links are being entered in sequence.  */
      if (to_ptr == 0)
	{
	  to->first = to->current = to_elt;
	  to->indx = from_ptr->indx;
	  to_elt->next = to_elt->prev = 0;
	}
      else
	{
	  to_elt->prev = to_ptr;
	  to_elt->next = 0;
	  to_ptr->next = to_elt;
	}

      to_ptr = to_elt;
    }
}

/* Find a bitmap element that would hold a bitmap's bit.
   Update the `current' field even if we can't find an element that
   would hold the bitmap's bit to make eventual allocation
   faster.  */

static inline bitmap_element *
bitmap_find_bit (bitmap head, unsigned int bit)
{
  bitmap_element *element;
  unsigned int indx = bit / BITMAP_ELEMENT_ALL_BITS;

  if (head->current == 0
      || head->indx == indx)
    return head->current;

  if (GATHER_STATISTICS)
    head->desc->nsearches++;

  if (head->indx < indx)
    /* INDX is beyond head->indx.  Search from head->current
       forward.  */
    for (element = head->current;
	 element->next != 0 && element->indx < indx;
	 element = element->next)
      {
	if (GATHER_STATISTICS)
	  head->desc->search_iter++;
      }

  else if (head->indx / 2 < indx)
    /* INDX is less than head->indx and closer to head->indx than to
       0.  Search from head->current backward.  */
    for (element = head->current;
	 element->prev != 0 && element->indx > indx;
	 element = element->prev)
      {
	if (GATHER_STATISTICS)
	  head->desc->search_iter++;
      }

  else
    /* INDX is less than head->indx and closer to 0 than to
       head->indx.  Search from head->first forward.  */
    for (element = head->first;
	 element->next != 0 && element->indx < indx;
	 element = element->next)
      if (GATHER_STATISTICS)
	{
	  head->desc->search_iter++;
	}

  /* `element' is the nearest to the one we want.  If it's not the one we
     want, the one we want doesn't exist.  */
  head->current = element;
  head->indx = element->indx;
  if (element != 0 && element->indx != indx)
    element = 0;

  return element;
}

/* Clear a single bit in a bitmap.  Return true if the bit changed.  */

bool
bitmap_clear_bit (bitmap head, int bit)
{
  bitmap_element *const ptr = bitmap_find_bit (head, bit);

  if (ptr != 0)
    {
      unsigned bit_num  = bit % BITMAP_WORD_BITS;
      unsigned word_num = bit / BITMAP_WORD_BITS % BITMAP_ELEMENT_WORDS;
      BITMAP_WORD bit_val = ((BITMAP_WORD) 1) << bit_num;
      bool res = (ptr->bits[word_num] & bit_val) != 0;
      if (res)
	{
	  ptr->bits[word_num] &= ~bit_val;
	  /* If we cleared the entire word, free up the element.  */
	  if (!ptr->bits[word_num]
	      && bitmap_element_zerop (ptr))
	    bitmap_element_free (head, ptr);
	}

      return res;
    }

  return false;
}

/* Set a single bit in a bitmap.  Return true if the bit changed.  */

bool
bitmap_set_bit (bitmap head, int bit)
{
  bitmap_element *ptr = bitmap_find_bit (head, bit);
  unsigned word_num = bit / BITMAP_WORD_BITS % BITMAP_ELEMENT_WORDS;
  unsigned bit_num  = bit % BITMAP_WORD_BITS;
  BITMAP_WORD bit_val = ((BITMAP_WORD) 1) << bit_num;

  if (ptr == 0)
    {
      ptr = bitmap_element_allocate (head);
      ptr->indx = bit / BITMAP_ELEMENT_ALL_BITS;
      ptr->bits[word_num] = bit_val;
      bitmap_element_link (head, ptr);
      return true;
    }
  else
    {
      bool res = (ptr->bits[word_num] & bit_val) == 0;
      if (res)
	ptr->bits[word_num] |= bit_val;
      return res;
    }
}

/* Return whether a bit is set within a bitmap.  */

int
bitmap_bit_p (bitmap head, int bit)
{
  bitmap_element *ptr;
  unsigned bit_num;
  unsigned word_num;

  ptr = bitmap_find_bit (head, bit);
  if (ptr == 0)
    return 0;

  bit_num = bit % BITMAP_WORD_BITS;
  word_num = bit / BITMAP_WORD_BITS % BITMAP_ELEMENT_WORDS;

  return (ptr->bits[word_num] >> bit_num) & 1;
}

#if GCC_VERSION < 3400
/* Table of number of set bits in a character, indexed by value of char.  */
static const unsigned char popcount_table[] =
{
    0,1,1,2,1,2,2,3,1,2,2,3,2,3,3,4,1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,
    1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
    1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
    2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,
    1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
    2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,
    2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,
    3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,4,5,5,6,5,6,6,7,5,6,6,7,6,7,7,8,
};

static unsigned long
bitmap_popcount (BITMAP_WORD a)
{
  unsigned long ret = 0;
  unsigned i;

  /* Just do this the table way for now  */
  for (i = 0; i < BITMAP_WORD_BITS; i+= 8)
    ret += popcount_table[(a >> i) & 0xff];
  return ret;
}
#endif
/* Count the number of bits set in the bitmap, and return it.  */

unsigned long
bitmap_count_bits (const_bitmap a)
{
  unsigned long count = 0;
  const bitmap_element *elt;
  unsigned ix;

  for (elt = a->first; elt; elt = elt->next)
    {
      for (ix = 0; ix != BITMAP_ELEMENT_WORDS; ix++)
	{
#if GCC_VERSION >= 3400
 	  /* Note that popcountl matches BITMAP_WORD in type, so the actual size
	 of BITMAP_WORD is not material.  */
	  count += __builtin_popcountl (elt->bits[ix]);
#else
	  count += bitmap_popcount (elt->bits[ix]);
#endif
	}
    }
  return count;
}

/* Return true if the bitmap has a single bit set.  Otherwise return
   false.  */

bool
bitmap_single_bit_set_p (const_bitmap a)
{
  unsigned long count = 0;
  const bitmap_element *elt;
  unsigned ix;

  if (bitmap_empty_p (a))
    return false;

  elt = a->first;
  /* As there are no completely empty bitmap elements, a second one
     means we have more than one bit set.  */
  if (elt->next != NULL)
    return false;

  for (ix = 0; ix != BITMAP_ELEMENT_WORDS; ix++)
    {
#if GCC_VERSION >= 3400
      /* Note that popcountl matches BITMAP_WORD in type, so the actual size
	 of BITMAP_WORD is not material.  */
      count += __builtin_popcountl (elt->bits[ix]);
#else
      count += bitmap_popcount (elt->bits[ix]);
#endif
      if (count > 1)
	return false;
    }

  return count == 1;
}


/* Return the bit number of the first set bit in the bitmap.  The
   bitmap must be non-empty.  */

unsigned
bitmap_first_set_bit (const_bitmap a)
{
  const bitmap_element *elt = a->first;
  unsigned bit_no;
  BITMAP_WORD word;
  unsigned ix;

  gcc_checking_assert (elt);
  bit_no = elt->indx * BITMAP_ELEMENT_ALL_BITS;
  for (ix = 0; ix != BITMAP_ELEMENT_WORDS; ix++)
    {
      word = elt->bits[ix];
      if (word)
	goto found_bit;
    }
  gcc_unreachable ();
 found_bit:
  bit_no += ix * BITMAP_WORD_BITS;

#if GCC_VERSION >= 3004
  gcc_assert (sizeof(long) == sizeof (word));
  bit_no += __builtin_ctzl (word);
#else
  /* Binary search for the first set bit.  */
#if BITMAP_WORD_BITS > 64
#error "Fill out the table."
#endif
#if BITMAP_WORD_BITS > 32
  if (!(word & 0xffffffff))
    word >>= 32, bit_no += 32;
#endif
  if (!(word & 0xffff))
    word >>= 16, bit_no += 16;
  if (!(word & 0xff))
    word >>= 8, bit_no += 8;
  if (!(word & 0xf))
    word >>= 4, bit_no += 4;
  if (!(word & 0x3))
    word >>= 2, bit_no += 2;
  if (!(word & 0x1))
    word >>= 1, bit_no += 1;

 gcc_checking_assert (word & 1);
#endif
 return bit_no;
}

/* Return the bit number of the first set bit in the bitmap.  The
   bitmap must be non-empty.  */

unsigned
bitmap_last_set_bit (const_bitmap a)
{
  const bitmap_element *elt = a->current ? a->current : a->first;
  unsigned bit_no;
  BITMAP_WORD word;
  int ix;

  gcc_checking_assert (elt);
  while (elt->next)
    elt = elt->next;
  bit_no = elt->indx * BITMAP_ELEMENT_ALL_BITS;
  for (ix = BITMAP_ELEMENT_WORDS - 1; ix >= 0; ix--)
    {
      word = elt->bits[ix];
      if (word)
	goto found_bit;
    }
  gcc_unreachable ();
 found_bit:
  bit_no += ix * BITMAP_WORD_BITS;

  /* Binary search for the last set bit.  */
#if GCC_VERSION >= 3004
  gcc_assert (sizeof(long) == sizeof (word));
  bit_no += sizeof (long) * 8 - __builtin_ctzl (word);
#else
#if BITMAP_WORD_BITS > 64
#error "Fill out the table."
#endif
#if BITMAP_WORD_BITS > 32
  if ((word & 0xffffffff00000000))
    word >>= 32, bit_no += 32;
#endif
  if (word & 0xffff0000)
    word >>= 16, bit_no += 16;
  if (!(word & 0xff00))
    word >>= 8, bit_no += 8;
  if (!(word & 0xf0))
    word >>= 4, bit_no += 4;
  if (!(word & 12))
    word >>= 2, bit_no += 2;
  if (!(word & 2))
    word >>= 1, bit_no += 1;
#endif

 gcc_checking_assert (word & 1);
 return bit_no;
}


/* DST = A & B.  */

void
bitmap_and (bitmap dst, const_bitmap a, const_bitmap b)
{
  bitmap_element *dst_elt = dst->first;
  const bitmap_element *a_elt = a->first;
  const bitmap_element *b_elt = b->first;
  bitmap_element *dst_prev = NULL;

  gcc_assert (dst != a && dst != b);

  if (a == b)
    {
      bitmap_copy (dst, a);
      return;
    }

  while (a_elt && b_elt)
    {
      if (a_elt->indx < b_elt->indx)
	a_elt = a_elt->next;
      else if (b_elt->indx < a_elt->indx)
	b_elt = b_elt->next;
      else
	{
	  /* Matching elts, generate A & B.  */
	  unsigned ix;
	  BITMAP_WORD ior = 0;

	  if (!dst_elt)
	    dst_elt = bitmap_elt_insert_after (dst, dst_prev, a_elt->indx);
	  else
	    dst_elt->indx = a_elt->indx;
	  for (ix = 0; ix < BITMAP_ELEMENT_WORDS; ix++)
	    {
	      BITMAP_WORD r = a_elt->bits[ix] & b_elt->bits[ix];

	      dst_elt->bits[ix] = r;
	      ior |= r;
	    }
	  if (ior)
	    {
	      dst_prev = dst_elt;
	      dst_elt = dst_elt->next;
	    }
	  a_elt = a_elt->next;
	  b_elt = b_elt->next;
	}
    }
  /* Ensure that dst->current is valid.  */
  dst->current = dst->first;
  bitmap_elt_clear_from (dst, dst_elt);
  gcc_checking_assert (!dst->current == !dst->first);
  if (dst->current)
    dst->indx = dst->current->indx;
}

/* A &= B.  */

void
bitmap_and_into (bitmap a, const_bitmap b)
{
  bitmap_element *a_elt = a->first;
  const bitmap_element *b_elt = b->first;
  bitmap_element *next;

  if (a == b)
    return;

  while (a_elt && b_elt)
    {
      if (a_elt->indx < b_elt->indx)
	{
	  next = a_elt->next;
	  bitmap_element_free (a, a_elt);
	  a_elt = next;
	}
      else if (b_elt->indx < a_elt->indx)
	b_elt = b_elt->next;
      else
	{
	  /* Matching elts, generate A &= B.  */
	  unsigned ix;
	  BITMAP_WORD ior = 0;

	  for (ix = 0; ix < BITMAP_ELEMENT_WORDS; ix++)
	    {
	      BITMAP_WORD r = a_elt->bits[ix] & b_elt->bits[ix];

	      a_elt->bits[ix] = r;
	      ior |= r;
	    }
	  next = a_elt->next;
	  if (!ior)
	    bitmap_element_free (a, a_elt);
	  a_elt = next;
	  b_elt = b_elt->next;
	}
    }
  bitmap_elt_clear_from (a, a_elt);
  gcc_checking_assert (!a->current == !a->first
		       && (!a->current || a->indx == a->current->indx));
}


/* Insert an element equal to SRC_ELT after DST_PREV, overwriting DST_ELT
   if non-NULL.  CHANGED is true if the destination bitmap had already been
   changed; the new value of CHANGED is returned.  */

static inline bool
bitmap_elt_copy (bitmap dst, bitmap_element *dst_elt, bitmap_element *dst_prev,
		 const bitmap_element *src_elt, bool changed)
{
  if (!changed && dst_elt && dst_elt->indx == src_elt->indx)
    {
      unsigned ix;

      for (ix = 0; ix < BITMAP_ELEMENT_WORDS; ix++)
	if (src_elt->bits[ix] != dst_elt->bits[ix])
	  {
	    dst_elt->bits[ix] = src_elt->bits[ix];
	    changed = true;
	  }
    }
  else
    {
      changed = true;
      if (!dst_elt)
	dst_elt = bitmap_elt_insert_after (dst, dst_prev, src_elt->indx);
      else
	dst_elt->indx = src_elt->indx;
      memcpy (dst_elt->bits, src_elt->bits, sizeof (dst_elt->bits));
    }
  return changed;
}



/* DST = A & ~B  */

bool
bitmap_and_compl (bitmap dst, const_bitmap a, const_bitmap b)
{
  bitmap_element *dst_elt = dst->first;
  const bitmap_element *a_elt = a->first;
  const bitmap_element *b_elt = b->first;
  bitmap_element *dst_prev = NULL;
  bitmap_element **dst_prev_pnext = &dst->first;
  bool changed = false;

  gcc_assert (dst != a && dst != b);

  if (a == b)
    {
      changed = !bitmap_empty_p (dst);
      bitmap_clear (dst);
      return changed;
    }

  while (a_elt)
    {
      while (b_elt && b_elt->indx < a_elt->indx)
	b_elt = b_elt->next;

      if (!b_elt || b_elt->indx > a_elt->indx)
	{
	  changed = bitmap_elt_copy (dst, dst_elt, dst_prev, a_elt, changed);
	  dst_prev = *dst_prev_pnext;
	  dst_prev_pnext = &dst_prev->next;
	  dst_elt = *dst_prev_pnext;
	  a_elt = a_elt->next;
	}

      else
	{
	  /* Matching elts, generate A & ~B.  */
	  unsigned ix;
	  BITMAP_WORD ior = 0;

	  if (!changed && dst_elt && dst_elt->indx == a_elt->indx)
	    {
	      for (ix = 0; ix < BITMAP_ELEMENT_WORDS; ix++)
		{
		  BITMAP_WORD r = a_elt->bits[ix] & ~b_elt->bits[ix];

		  if (dst_elt->bits[ix] != r)
		    {
		      changed = true;
		      dst_elt->bits[ix] = r;
		    }
		  ior |= r;
		}
	    }
	  else
	    {
	      bool new_element;
	      if (!dst_elt || dst_elt->indx > a_elt->indx)
		{
		  dst_elt = bitmap_elt_insert_after (dst, dst_prev, a_elt->indx);
		  new_element = true;
		}
	      else
		{
		  dst_elt->indx = a_elt->indx;
		  new_element = false;
		}

	      for (ix = 0; ix < BITMAP_ELEMENT_WORDS; ix++)
		{
		  BITMAP_WORD r = a_elt->bits[ix] & ~b_elt->bits[ix];

		  dst_elt->bits[ix] = r;
		  ior |= r;
		}

	      if (ior)
	        changed = true;
	      else
	        {
	          changed |= !new_element;
		  bitmap_element_free (dst, dst_elt);
		  dst_elt = *dst_prev_pnext;
		}
	    }

	  if (ior)
	    {
	      dst_prev = *dst_prev_pnext;
	      dst_prev_pnext = &dst_prev->next;
	      dst_elt = *dst_prev_pnext;
	    }
	  a_elt = a_elt->next;
	  b_elt = b_elt->next;
	}
    }

  /* Ensure that dst->current is valid.  */
  dst->current = dst->first;

  if (dst_elt)
    {
      changed = true;
      bitmap_elt_clear_from (dst, dst_elt);
    }
  gcc_checking_assert (!dst->current == !dst->first);
  if (dst->current)
    dst->indx = dst->current->indx;

  return changed;
}

/* A &= ~B. Returns true if A changes */

bool
bitmap_and_compl_into (bitmap a, const_bitmap b)
{
  bitmap_element *a_elt = a->first;
  const bitmap_element *b_elt = b->first;
  bitmap_element *next;
  BITMAP_WORD changed = 0;

  if (a == b)
    {
      if (bitmap_empty_p (a))
	return false;
      else
	{
	  bitmap_clear (a);
	  return true;
	}
    }

  while (a_elt && b_elt)
    {
      if (a_elt->indx < b_elt->indx)
	a_elt = a_elt->next;
      else if (b_elt->indx < a_elt->indx)
	b_elt = b_elt->next;
      else
	{
	  /* Matching elts, generate A &= ~B.  */
	  unsigned ix;
	  BITMAP_WORD ior = 0;

	  for (ix = 0; ix < BITMAP_ELEMENT_WORDS; ix++)
	    {
	      BITMAP_WORD cleared = a_elt->bits[ix] & b_elt->bits[ix];
	      BITMAP_WORD r = a_elt->bits[ix] ^ cleared;

	      a_elt->bits[ix] = r;
	      changed |= cleared;
	      ior |= r;
	    }
	  next = a_elt->next;
	  if (!ior)
	    bitmap_element_free (a, a_elt);
	  a_elt = next;
	  b_elt = b_elt->next;
	}
    }
  gcc_checking_assert (!a->current == !a->first
		       && (!a->current || a->indx == a->current->indx));
  return changed != 0;
}

/* Set COUNT bits from START in HEAD.  */
void
bitmap_set_range (bitmap head, unsigned int start, unsigned int count)
{
  unsigned int first_index, end_bit_plus1, last_index;
  bitmap_element *elt, *elt_prev;
  unsigned int i;

  if (!count)
    return;

  first_index = start / BITMAP_ELEMENT_ALL_BITS;
  end_bit_plus1 = start + count;
  last_index = (end_bit_plus1 - 1) / BITMAP_ELEMENT_ALL_BITS;
  elt = bitmap_find_bit (head, start);

  /* If bitmap_find_bit returns zero, the current is the closest block
     to the result.  Otherwise, just use bitmap_element_allocate to
     ensure ELT is set; in the loop below, ELT == NULL means "insert
     at the end of the bitmap".  */
  if (!elt)
    {
      elt = bitmap_element_allocate (head);
      elt->indx = first_index;
      bitmap_element_link (head, elt);
    }

  gcc_checking_assert (elt->indx == first_index);
  elt_prev = elt->prev;
  for (i = first_index; i <= last_index; i++)
    {
      unsigned elt_start_bit = i * BITMAP_ELEMENT_ALL_BITS;
      unsigned elt_end_bit_plus1 = elt_start_bit + BITMAP_ELEMENT_ALL_BITS;

      unsigned int first_word_to_mod;
      BITMAP_WORD first_mask;
      unsigned int last_word_to_mod;
      BITMAP_WORD last_mask;
      unsigned int ix;

      if (!elt || elt->indx != i)
	elt = bitmap_elt_insert_after (head, elt_prev, i);

      if (elt_start_bit <= start)
	{
	  /* The first bit to turn on is somewhere inside this
	     elt.  */
	  first_word_to_mod = (start - elt_start_bit) / BITMAP_WORD_BITS;

	  /* This mask should have 1s in all bits >= start position. */
	  first_mask =
	    (((BITMAP_WORD) 1) << ((start % BITMAP_WORD_BITS))) - 1;
	  first_mask = ~first_mask;
	}
      else
	{
	  /* The first bit to turn on is below this start of this elt.  */
	  first_word_to_mod = 0;
	  first_mask = ~(BITMAP_WORD) 0;
	}

      if (elt_end_bit_plus1 <= end_bit_plus1)
	{
	  /* The last bit to turn on is beyond this elt.  */
	  last_word_to_mod = BITMAP_ELEMENT_WORDS - 1;
	  last_mask = ~(BITMAP_WORD) 0;
	}
      else
	{
	  /* The last bit to turn on is inside to this elt.  */
	  last_word_to_mod =
	    (end_bit_plus1 - elt_start_bit) / BITMAP_WORD_BITS;

	  /* The last mask should have 1s below the end bit.  */
	  last_mask =
	    (((BITMAP_WORD) 1) << ((end_bit_plus1 % BITMAP_WORD_BITS))) - 1;
	}

      if (first_word_to_mod == last_word_to_mod)
	{
	  BITMAP_WORD mask = first_mask & last_mask;
	  elt->bits[first_word_to_mod] |= mask;
	}
      else
	{
	  elt->bits[first_word_to_mod] |= first_mask;
	  if (BITMAP_ELEMENT_WORDS > 2)
	    for (ix = first_word_to_mod + 1; ix < last_word_to_mod; ix++)
	      elt->bits[ix] = ~(BITMAP_WORD) 0;
	  elt->bits[last_word_to_mod] |= last_mask;
	}

      elt_prev = elt;
      elt = elt->next;
    }

  head->current = elt ? elt : elt_prev;
  head->indx = head->current->indx;
}

/* Clear COUNT bits from START in HEAD.  */
void
bitmap_clear_range (bitmap head, unsigned int start, unsigned int count)
{
  unsigned int first_index, end_bit_plus1, last_index;
  bitmap_element *elt;

  if (!count)
    return;

  first_index = start / BITMAP_ELEMENT_ALL_BITS;
  end_bit_plus1 = start + count;
  last_index = (end_bit_plus1 - 1) / BITMAP_ELEMENT_ALL_BITS;
  elt = bitmap_find_bit (head, start);

  /* If bitmap_find_bit returns zero, the current is the closest block
     to the result.  If the current is less than first index, find the
     next one.  Otherwise, just set elt to be current.  */
  if (!elt)
    {
      if (head->current)
	{
	  if (head->indx < first_index)
	    {
	      elt = head->current->next;
	      if (!elt)
		return;
	    }
	  else
	    elt = head->current;
	}
      else
	return;
    }

  while (elt && (elt->indx <= last_index))
    {
      bitmap_element * next_elt = elt->next;
      unsigned elt_start_bit = (elt->indx) * BITMAP_ELEMENT_ALL_BITS;
      unsigned elt_end_bit_plus1 = elt_start_bit + BITMAP_ELEMENT_ALL_BITS;


      if (elt_start_bit >= start && elt_end_bit_plus1 <= end_bit_plus1)
	/* Get rid of the entire elt and go to the next one.  */
	bitmap_element_free (head, elt);
      else
	{
	  /* Going to have to knock out some bits in this elt.  */
	  unsigned int first_word_to_mod;
	  BITMAP_WORD first_mask;
	  unsigned int last_word_to_mod;
	  BITMAP_WORD last_mask;
	  unsigned int i;
	  bool clear = true;

	  if (elt_start_bit <= start)
	    {
	      /* The first bit to turn off is somewhere inside this
		 elt.  */
	      first_word_to_mod = (start - elt_start_bit) / BITMAP_WORD_BITS;

	      /* This mask should have 1s in all bits >= start position. */
	      first_mask =
		(((BITMAP_WORD) 1) << ((start % BITMAP_WORD_BITS))) - 1;
	      first_mask = ~first_mask;
	    }
	  else
	    {
	      /* The first bit to turn off is below this start of this elt.  */
	      first_word_to_mod = 0;
	      first_mask = 0;
	      first_mask = ~first_mask;
	    }

	  if (elt_end_bit_plus1 <= end_bit_plus1)
	    {
	      /* The last bit to turn off is beyond this elt.  */
	      last_word_to_mod = BITMAP_ELEMENT_WORDS - 1;
	      last_mask = 0;
	      last_mask = ~last_mask;
	    }
	  else
	    {
	      /* The last bit to turn off is inside to this elt.  */
	      last_word_to_mod =
		(end_bit_plus1 - elt_start_bit) / BITMAP_WORD_BITS;

	      /* The last mask should have 1s below the end bit.  */
	      last_mask =
		(((BITMAP_WORD) 1) << (((end_bit_plus1) % BITMAP_WORD_BITS))) - 1;
	    }


	  if (first_word_to_mod == last_word_to_mod)
	    {
	      BITMAP_WORD mask = first_mask & last_mask;
	      elt->bits[first_word_to_mod] &= ~mask;
	    }
	  else
	    {
	      elt->bits[first_word_to_mod] &= ~first_mask;
	      if (BITMAP_ELEMENT_WORDS > 2)
	        for (i = first_word_to_mod + 1; i < last_word_to_mod; i++)
		  elt->bits[i] = 0;
	      elt->bits[last_word_to_mod] &= ~last_mask;
	    }
	  for (i = 0; i < BITMAP_ELEMENT_WORDS; i++)
	    if (elt->bits[i])
	      {
		clear = false;
		break;
	      }
	  /* Check to see if there are any bits left.  */
	  if (clear)
	    bitmap_element_free (head, elt);
	}
      elt = next_elt;
    }

  if (elt)
    {
      head->current = elt;
      head->indx = head->current->indx;
    }
}

/* A = ~A & B. */

void
bitmap_compl_and_into (bitmap a, const_bitmap b)
{
  bitmap_element *a_elt = a->first;
  const bitmap_element *b_elt = b->first;
  bitmap_element *a_prev = NULL;
  bitmap_element *next;

  gcc_assert (a != b);

  if (bitmap_empty_p (a))
    {
      bitmap_copy (a, b);
      return;
    }
  if (bitmap_empty_p (b))
    {
      bitmap_clear (a);
      return;
    }

  while (a_elt || b_elt)
    {
      if (!b_elt || (a_elt && a_elt->indx < b_elt->indx))
	{
	  /* A is before B.  Remove A */
	  next = a_elt->next;
	  a_prev = a_elt->prev;
	  bitmap_element_free (a, a_elt);
	  a_elt = next;
	}
      else if (!a_elt || b_elt->indx < a_elt->indx)
	{
	  /* B is before A.  Copy B. */
	  next = bitmap_elt_insert_after (a, a_prev, b_elt->indx);
	  memcpy (next->bits, b_elt->bits, sizeof (next->bits));
	  a_prev = next;
	  b_elt = b_elt->next;
	}
      else
	{
	  /* Matching elts, generate A = ~A & B.  */
	  unsigned ix;
	  BITMAP_WORD ior = 0;

	  for (ix = 0; ix < BITMAP_ELEMENT_WORDS; ix++)
	    {
	      BITMAP_WORD cleared = a_elt->bits[ix] & b_elt->bits[ix];
	      BITMAP_WORD r = b_elt->bits[ix] ^ cleared;

	      a_elt->bits[ix] = r;
	      ior |= r;
	    }
	  next = a_elt->next;
	  if (!ior)
	    bitmap_element_free (a, a_elt);
	  else
	    a_prev = a_elt;
	  a_elt = next;
	  b_elt = b_elt->next;
	}
    }
  gcc_checking_assert (!a->current == !a->first
		       && (!a->current || a->indx == a->current->indx));
  return;
}


/* Insert an element corresponding to A_ELT | B_ELT after DST_PREV,
   overwriting DST_ELT if non-NULL.  CHANGED is true if the destination bitmap
   had already been changed; the new value of CHANGED is returned.  */

static inline bool
bitmap_elt_ior (bitmap dst, bitmap_element *dst_elt, bitmap_element *dst_prev,
		const bitmap_element *a_elt, const bitmap_element *b_elt,
		bool changed)
{
  gcc_assert (a_elt || b_elt);

  if (a_elt && b_elt && a_elt->indx == b_elt->indx)
    {
      /* Matching elts, generate A | B.  */
      unsigned ix;

      if (!changed && dst_elt && dst_elt->indx == a_elt->indx)
	{
	  for (ix = 0; ix < BITMAP_ELEMENT_WORDS; ix++)
	    {
	      BITMAP_WORD r = a_elt->bits[ix] | b_elt->bits[ix];
	      if (r != dst_elt->bits[ix])
		{
		  dst_elt->bits[ix] = r;
		  changed = true;
		}
	    }
	}
      else
	{
	  changed = true;
	  if (!dst_elt)
	    dst_elt = bitmap_elt_insert_after (dst, dst_prev, a_elt->indx);
	  else
	    dst_elt->indx = a_elt->indx;
	  for (ix = 0; ix < BITMAP_ELEMENT_WORDS; ix++)
	    {
	      BITMAP_WORD r = a_elt->bits[ix] | b_elt->bits[ix];
	      dst_elt->bits[ix] = r;
	    }
	}
    }
  else
    {
      /* Copy a single element.  */
      const bitmap_element *src;

      if (!b_elt || (a_elt && a_elt->indx < b_elt->indx))
	src = a_elt;
      else
	src = b_elt;

      gcc_checking_assert (src);
      changed = bitmap_elt_copy (dst, dst_elt, dst_prev, src, changed);
    }
  return changed;
}


/* DST = A | B.  Return true if DST changes.  */

bool
bitmap_ior (bitmap dst, const_bitmap a, const_bitmap b)
{
  bitmap_element *dst_elt = dst->first;
  const bitmap_element *a_elt = a->first;
  const bitmap_element *b_elt = b->first;
  bitmap_element *dst_prev = NULL;
  bitmap_element **dst_prev_pnext = &dst->first;
  bool changed = false;

  gcc_assert (dst != a && dst != b);

  while (a_elt || b_elt)
    {
      changed = bitmap_elt_ior (dst, dst_elt, dst_prev, a_elt, b_elt, changed);

      if (a_elt && b_elt && a_elt->indx == b_elt->indx)
	{
	  a_elt = a_elt->next;
	  b_elt = b_elt->next;
	}
      else
	{
	  if (a_elt && (!b_elt || a_elt->indx <= b_elt->indx))
            a_elt = a_elt->next;
          else if (b_elt && (!a_elt || b_elt->indx <= a_elt->indx))
            b_elt = b_elt->next;
	}

      dst_prev = *dst_prev_pnext;
      dst_prev_pnext = &dst_prev->next;
      dst_elt = *dst_prev_pnext;
    }

  if (dst_elt)
    {
      changed = true;
      bitmap_elt_clear_from (dst, dst_elt);
    }
  gcc_checking_assert (!dst->current == !dst->first);
  if (dst->current)
    dst->indx = dst->current->indx;
  return changed;
}

/* A |= B.  Return true if A changes.  */

bool
bitmap_ior_into (bitmap a, const_bitmap b)
{
  bitmap_element *a_elt = a->first;
  const bitmap_element *b_elt = b->first;
  bitmap_element *a_prev = NULL;
  bitmap_element **a_prev_pnext = &a->first;
  bool changed = false;

  if (a == b)
    return false;

  while (b_elt)
    {
      /* If A lags behind B, just advance it.  */
      if (!a_elt || a_elt->indx == b_elt->indx)
	{
	  changed = bitmap_elt_ior (a, a_elt, a_prev, a_elt, b_elt, changed);
	  b_elt = b_elt->next;
	}
      else if (a_elt->indx > b_elt->indx)
	{
          changed = bitmap_elt_copy (a, NULL, a_prev, b_elt, changed);
	  b_elt = b_elt->next;
	}

      a_prev = *a_prev_pnext;
      a_prev_pnext = &a_prev->next;
      a_elt = *a_prev_pnext;
    }

  gcc_checking_assert (!a->current == !a->first);
  if (a->current)
    a->indx = a->current->indx;
  return changed;
}

/* DST = A ^ B  */

void
bitmap_xor (bitmap dst, const_bitmap a, const_bitmap b)
{
  bitmap_element *dst_elt = dst->first;
  const bitmap_element *a_elt = a->first;
  const bitmap_element *b_elt = b->first;
  bitmap_element *dst_prev = NULL;

  gcc_assert (dst != a && dst != b);
  if (a == b)
    {
      bitmap_clear (dst);
      return;
    }

  while (a_elt || b_elt)
    {
      if (a_elt && b_elt && a_elt->indx == b_elt->indx)
	{
	  /* Matching elts, generate A ^ B.  */
	  unsigned ix;
	  BITMAP_WORD ior = 0;

	  if (!dst_elt)
	    dst_elt = bitmap_elt_insert_after (dst, dst_prev, a_elt->indx);
	  else
	    dst_elt->indx = a_elt->indx;
	  for (ix = 0; ix < BITMAP_ELEMENT_WORDS; ix++)
	    {
	      BITMAP_WORD r = a_elt->bits[ix] ^ b_elt->bits[ix];

	      ior |= r;
	      dst_elt->bits[ix] = r;
	    }
	  a_elt = a_elt->next;
	  b_elt = b_elt->next;
	  if (ior)
	    {
	      dst_prev = dst_elt;
	      dst_elt = dst_elt->next;
	    }
	}
      else
	{
	  /* Copy a single element.  */
	  const bitmap_element *src;

	  if (!b_elt || (a_elt && a_elt->indx < b_elt->indx))
	    {
	      src = a_elt;
	      a_elt = a_elt->next;
	    }
	  else
	    {
	      src = b_elt;
	      b_elt = b_elt->next;
	    }

	  if (!dst_elt)
	    dst_elt = bitmap_elt_insert_after (dst, dst_prev, src->indx);
	  else
	    dst_elt->indx = src->indx;
	  memcpy (dst_elt->bits, src->bits, sizeof (dst_elt->bits));
	  dst_prev = dst_elt;
	  dst_elt = dst_elt->next;
	}
    }
  /* Ensure that dst->current is valid.  */
  dst->current = dst->first;
  bitmap_elt_clear_from (dst, dst_elt);
  gcc_checking_assert (!dst->current == !dst->first);
  if (dst->current)
    dst->indx = dst->current->indx;
}

/* A ^= B */

void
bitmap_xor_into (bitmap a, const_bitmap b)
{
  bitmap_element *a_elt = a->first;
  const bitmap_element *b_elt = b->first;
  bitmap_element *a_prev = NULL;

  if (a == b)
    {
      bitmap_clear (a);
      return;
    }

  while (b_elt)
    {
      if (!a_elt || b_elt->indx < a_elt->indx)
	{
	  /* Copy b_elt.  */
	  bitmap_element *dst = bitmap_elt_insert_after (a, a_prev, b_elt->indx);
	  memcpy (dst->bits, b_elt->bits, sizeof (dst->bits));
	  a_prev = dst;
	  b_elt = b_elt->next;
	}
      else if (a_elt->indx < b_elt->indx)
	{
	  a_prev = a_elt;
	  a_elt = a_elt->next;
	}
      else
	{
	  /* Matching elts, generate A ^= B.  */
	  unsigned ix;
	  BITMAP_WORD ior = 0;
	  bitmap_element *next = a_elt->next;

	  for (ix = 0; ix < BITMAP_ELEMENT_WORDS; ix++)
	    {
	      BITMAP_WORD r = a_elt->bits[ix] ^ b_elt->bits[ix];

	      ior |= r;
	      a_elt->bits[ix] = r;
	    }
	  b_elt = b_elt->next;
	  if (ior)
	    a_prev = a_elt;
	  else
	    bitmap_element_free (a, a_elt);
	  a_elt = next;
	}
    }
  gcc_checking_assert (!a->current == !a->first);
  if (a->current)
    a->indx = a->current->indx;
}

/* Return true if two bitmaps are identical.
   We do not bother with a check for pointer equality, as that never
   occurs in practice.  */

bool
bitmap_equal_p (const_bitmap a, const_bitmap b)
{
  const bitmap_element *a_elt;
  const bitmap_element *b_elt;
  unsigned ix;

  for (a_elt = a->first, b_elt = b->first;
       a_elt && b_elt;
       a_elt = a_elt->next, b_elt = b_elt->next)
    {
      if (a_elt->indx != b_elt->indx)
	return false;
      for (ix = 0; ix < BITMAP_ELEMENT_WORDS; ix++)
	if (a_elt->bits[ix] != b_elt->bits[ix])
	  return false;
    }
  return !a_elt && !b_elt;
}

/* Return true if A AND B is not empty.  */

bool
bitmap_intersect_p (const_bitmap a, const_bitmap b)
{
  const bitmap_element *a_elt;
  const bitmap_element *b_elt;
  unsigned ix;

  for (a_elt = a->first, b_elt = b->first;
       a_elt && b_elt;)
    {
      if (a_elt->indx < b_elt->indx)
	a_elt = a_elt->next;
      else if (b_elt->indx < a_elt->indx)
	b_elt = b_elt->next;
      else
	{
	  for (ix = 0; ix < BITMAP_ELEMENT_WORDS; ix++)
	    if (a_elt->bits[ix] & b_elt->bits[ix])
	      return true;
	  a_elt = a_elt->next;
	  b_elt = b_elt->next;
	}
    }
  return false;
}

/* Return true if A AND NOT B is not empty.  */

bool
bitmap_intersect_compl_p (const_bitmap a, const_bitmap b)
{
  const bitmap_element *a_elt;
  const bitmap_element *b_elt;
  unsigned ix;
  for (a_elt = a->first, b_elt = b->first;
       a_elt && b_elt;)
    {
      if (a_elt->indx < b_elt->indx)
	return true;
      else if (b_elt->indx < a_elt->indx)
	b_elt = b_elt->next;
      else
	{
	  for (ix = 0; ix < BITMAP_ELEMENT_WORDS; ix++)
	    if (a_elt->bits[ix] & ~b_elt->bits[ix])
	      return true;
	  a_elt = a_elt->next;
	  b_elt = b_elt->next;
	}
    }
  return a_elt != NULL;
}


/* DST = A | (FROM1 & ~FROM2).  Return true if DST changes.  */

bool
bitmap_ior_and_compl (bitmap dst, const_bitmap a, const_bitmap b, const_bitmap kill)
{
  bool changed = false;

  bitmap_element *dst_elt = dst->first;
  const bitmap_element *a_elt = a->first;
  const bitmap_element *b_elt = b->first;
  const bitmap_element *kill_elt = kill->first;
  bitmap_element *dst_prev = NULL;
  bitmap_element **dst_prev_pnext = &dst->first;

  gcc_assert (dst != a && dst != b && dst != kill);

  /* Special cases.  We don't bother checking for bitmap_equal_p (b, kill).  */
  if (b == kill || bitmap_empty_p (b))
    {
      changed = !bitmap_equal_p (dst, a);
      if (changed)
	bitmap_copy (dst, a);
      return changed;
    }
  if (bitmap_empty_p (kill))
    return bitmap_ior (dst, a, b);
  if (bitmap_empty_p (a))
    return bitmap_and_compl (dst, b, kill);

  while (a_elt || b_elt)
    {
      bool new_element = false;

      if (b_elt)
	while (kill_elt && kill_elt->indx < b_elt->indx)
	  kill_elt = kill_elt->next;

      if (b_elt && kill_elt && kill_elt->indx == b_elt->indx
	  && (!a_elt || a_elt->indx >= b_elt->indx))
        {
	  bitmap_element tmp_elt;
	  unsigned ix;

	  BITMAP_WORD ior = 0;
	  tmp_elt.indx = b_elt->indx;
	  for (ix = 0; ix < BITMAP_ELEMENT_WORDS; ix++)
            {
              BITMAP_WORD r = b_elt->bits[ix] & ~kill_elt->bits[ix];
              ior |= r;
              tmp_elt.bits[ix] = r;
            }

	  if (ior)
	    {
	      changed = bitmap_elt_ior (dst, dst_elt, dst_prev,
				        a_elt, &tmp_elt, changed);
	      new_element = true;
	      if (a_elt && a_elt->indx == b_elt->indx)
                a_elt = a_elt->next;
	    }

	  b_elt = b_elt->next;
	  kill_elt = kill_elt->next;
	}
      else
	{
	  changed = bitmap_elt_ior (dst, dst_elt, dst_prev,
				    a_elt, b_elt, changed);
	  new_element = true;

          if (a_elt && b_elt && a_elt->indx == b_elt->indx)
	    {
	      a_elt = a_elt->next;
	      b_elt = b_elt->next;
	    }
          else
	    {
	      if (a_elt && (!b_elt || a_elt->indx <= b_elt->indx))
                a_elt = a_elt->next;
              else if (b_elt && (!a_elt || b_elt->indx <= a_elt->indx))
                b_elt = b_elt->next;
	    }
	}

      if (new_element)
	{
	  dst_prev = *dst_prev_pnext;
	  dst_prev_pnext = &dst_prev->next;
	  dst_elt = *dst_prev_pnext;
	}
    }

  if (dst_elt)
    {
      changed = true;
      bitmap_elt_clear_from (dst, dst_elt);
    }
  gcc_checking_assert (!dst->current == !dst->first);
  if (dst->current)
    dst->indx = dst->current->indx;

  return changed;
}

/* A |= (FROM1 & ~FROM2).  Return true if A changes.  */

bool
bitmap_ior_and_compl_into (bitmap a, const_bitmap from1, const_bitmap from2)
{
  bitmap_head tmp;
  bool changed;

  bitmap_initialize (&tmp, &bitmap_default_obstack);
  bitmap_and_compl (&tmp, from1, from2);
  changed = bitmap_ior_into (a, &tmp);
  bitmap_clear (&tmp);

  return changed;
}

/* A |= (B & C).  Return true if A changes.  */

bool
bitmap_ior_and_into (bitmap a, const_bitmap b, const_bitmap c)
{
  bitmap_element *a_elt = a->first;
  const bitmap_element *b_elt = b->first;
  const bitmap_element *c_elt = c->first;
  bitmap_element and_elt;
  bitmap_element *a_prev = NULL;
  bitmap_element **a_prev_pnext = &a->first;
  bool changed = false;
  unsigned ix;

  if (b == c)
    return bitmap_ior_into (a, b);
  if (bitmap_empty_p (b) || bitmap_empty_p (c))
    return false;

  and_elt.indx = -1;
  while (b_elt && c_elt)
    {
      BITMAP_WORD overall;

      /* Find a common item of B and C.  */
      while (b_elt->indx != c_elt->indx)
	{
          if (b_elt->indx < c_elt->indx)
	    {
	      b_elt = b_elt->next;
	      if (!b_elt)
		goto done;
	    }
          else
	    {
	      c_elt = c_elt->next;
	      if (!c_elt)
		goto done;
	    }
	}

      overall = 0;
      and_elt.indx = b_elt->indx;
      for (ix = 0; ix < BITMAP_ELEMENT_WORDS; ix++)
	{
	  and_elt.bits[ix] = b_elt->bits[ix] & c_elt->bits[ix];
	  overall |= and_elt.bits[ix];
	}

      b_elt = b_elt->next;
      c_elt = c_elt->next;
      if (!overall)
	continue;

      /* Now find a place to insert AND_ELT.  */
      do
	{
	  ix = a_elt ? a_elt->indx : and_elt.indx;
          if (ix == and_elt.indx)
	    changed = bitmap_elt_ior (a, a_elt, a_prev, a_elt, &and_elt, changed);
          else if (ix > and_elt.indx)
	    changed = bitmap_elt_copy (a, NULL, a_prev, &and_elt, changed);

          a_prev = *a_prev_pnext;
          a_prev_pnext = &a_prev->next;
          a_elt = *a_prev_pnext;

          /* If A lagged behind B/C, we advanced it so loop once more.  */
	}
      while (ix < and_elt.indx);
    }

 done:
  gcc_checking_assert (!a->current == !a->first);
  if (a->current)
    a->indx = a->current->indx;
  return changed;
}

/* Compute hash of bitmap (for purposes of hashing).  */
hashval_t
bitmap_hash (const_bitmap head)
{
  const bitmap_element *ptr;
  BITMAP_WORD hash = 0;
  int ix;

  for (ptr = head->first; ptr; ptr = ptr->next)
    {
      hash ^= ptr->indx;
      for (ix = 0; ix != BITMAP_ELEMENT_WORDS; ix++)
	hash ^= ptr->bits[ix];
    }
  return (hashval_t)hash;
}


/* Debugging function to print out the contents of a bitmap.  */

DEBUG_FUNCTION void
debug_bitmap_file (FILE *file, const_bitmap head)
{
  const bitmap_element *ptr;

  fprintf (file, "\nfirst = " HOST_PTR_PRINTF
	   " current = " HOST_PTR_PRINTF " indx = %u\n",
	   (void *) head->first, (void *) head->current, head->indx);

  for (ptr = head->first; ptr; ptr = ptr->next)
    {
      unsigned int i, j, col = 26;

      fprintf (file, "\t" HOST_PTR_PRINTF " next = " HOST_PTR_PRINTF
	       " prev = " HOST_PTR_PRINTF " indx = %u\n\t\tbits = {",
	       (const void*) ptr, (const void*) ptr->next,
	       (const void*) ptr->prev, ptr->indx);

      for (i = 0; i < BITMAP_ELEMENT_WORDS; i++)
	for (j = 0; j < BITMAP_WORD_BITS; j++)
	  if ((ptr->bits[i] >> j) & 1)
	    {
	      if (col > 70)
		{
		  fprintf (file, "\n\t\t\t");
		  col = 24;
		}

	      fprintf (file, " %u", (ptr->indx * BITMAP_ELEMENT_ALL_BITS
				     + i * BITMAP_WORD_BITS + j));
	      col += 4;
	    }

      fprintf (file, " }\n");
    }
}

/* Function to be called from the debugger to print the contents
   of a bitmap.  */

DEBUG_FUNCTION void
debug_bitmap (const_bitmap head)
{
  debug_bitmap_file (stdout, head);
}

/* Function to print out the contents of a bitmap.  Unlike debug_bitmap_file,
   it does not print anything but the bits.  */

DEBUG_FUNCTION void
bitmap_print (FILE *file, const_bitmap head, const char *prefix, const char *suffix)
{
  const char *comma = "";
  unsigned i;
  bitmap_iterator bi;

  fputs (prefix, file);
  EXECUTE_IF_SET_IN_BITMAP (head, 0, i, bi)
    {
      fprintf (file, "%s%d", comma, i);
      comma = ", ";
    }
  fputs (suffix, file);
}


/* Used to accumulate statistics about bitmap sizes.  */
struct output_info
{
  HOST_WIDEST_INT size;
  int count;
};

/* Called via htab_traverse.  Output bitmap descriptor pointed out by SLOT
   and update statistics.  */
static int
print_statistics (void **slot, void *b)
{
  struct bitmap_descriptor *d = (struct bitmap_descriptor *) *slot;
  struct output_info *i = (struct output_info *) b;
  char s[4096];

  if (d->allocated)
    {
      const char *s1 = d->file;
      const char *s2;
      while ((s2 = strstr (s1, "gcc/")))
	s1 = s2 + 4;
      sprintf (s, "%s:%i (%s)", s1, d->line, d->function);
      s[41] = 0;
      fprintf (stderr, "%-41s %8d %15"HOST_WIDEST_INT_PRINT"d %15"
	       HOST_WIDEST_INT_PRINT"d %15"HOST_WIDEST_INT_PRINT"d %10d %10d\n",
	       s, d->created, d->allocated, d->peak, d->current, d->nsearches,
	       d->search_iter);
      i->size += d->allocated;
      i->count += d->created;
    }
  return 1;
}

/* Output per-bitmap memory usage statistics.  */
void
dump_bitmap_statistics (void)
{
  struct output_info info;

  if (! GATHER_STATISTICS)
    return;

  if (!bitmap_desc_hash)
    return;

  fprintf (stderr, "\nBitmap                                     Overall "
		   "      Allocated            Peak            Leak   searched "
		   "  search itr\n");
  fprintf (stderr, "---------------------------------------------------------------------------------\n");
  info.count = 0;
  info.size = 0;
  htab_traverse (bitmap_desc_hash, print_statistics, &info);
  fprintf (stderr, "---------------------------------------------------------------------------------\n");
  fprintf (stderr, "%-40s %9d %15"HOST_WIDEST_INT_PRINT"d\n",
	   "Total", info.count, info.size);
  fprintf (stderr, "---------------------------------------------------------------------------------\n");
}

#include "gt-bitmap.h"
