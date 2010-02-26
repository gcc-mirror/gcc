/* Sparse array-based bitmaps.
   Copyright (C) 2007, 2008, 2009, 2010 Free Software Foundation, Inc.
   Contributed by Daniel Berlin <dberlin@dberlin.org>

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
#include "tm.h"
#include "rtl.h"
#include "flags.h"
#include "obstack.h"
#include "ebitmap.h"

/* The ebitmap data structure is a sparse bitmap structure that works
   by having two pieces:
   1. An array of all nonzero words in the structures, organized as
   an array of HOST_WIDE_INT's.
   2. A non-sparse bitmap saying which bitmap words are present in the
   array.

   As a consequence of this representation, testing whether a bit
   exists in the bitmap is faster than linked-list bitmaps.  For bits
   in words that are all zero, the time to test is O(1).  For bits in
   words that exist, it requires O(n/sizeof(word)) time to perform the
   test (ignoring the one element cache).  It also has much better
   locality than linked-list bitmaps.

   To test whether a bit exists, we do the following:
   1. Test whether the word the bit would appear in exists in the
   bitmap (O(1) check of the mask).  If not, fail.

   2. Population count the mask up to the word containing the bit, to
   find the place in the array the word would be (popcount is cached,
   but this is just as slow as the linked-list bitmap)
   3. Test the word to see if the bit exists.

   Just like linked-list bitmaps, we cache the last element that has
   been tested in order to speed up common access patterns.

   Most of the other speedups are simply due to better locality of the
   single contiguous array.

   As would be expected in a structure like this, insertion into an
   empty word in the middle requires moving bits to make room for the
   new word.   As most operations that perform sets perform them in
   order, this is rarely a problem.  We also take advantage of the
   same element cache to make repeated sets to the same word O(1).

   Operations on the entire bitmap are also more efficient than linked
   list bitmaps, as they are all operating on contiguous memory, and
   can be easily vectorized.  They are all O(number of words) +
   O(number of bits that may end up in the destination), as the
   appropriate operation is first performed on the word mask, and then
   only those elements that may generate results are touched.

   Memory wise, the ebitmap starts out using less memory than the
   linked-list bitmap, but its size in memory is technically bounded
   by ((index of the highest bit set) / (size of a word) + (index of
   the highest bit set) / ((size of a word) * (size of a word))) This
   is because the mask is non-sparse.  The mask could be transformed
   into a sparse bitmap at the cost of making bit testing
   *theoretically* slower (real world numbers have not been computed
   yet as to whether it matters or not).  */

/* #define EBITMAP_DEBUGGING  */

/* Find the last set bit in ebitmap MAP.  */

int
ebitmap_last_set_bit (ebitmap map)
{
  unsigned int i = 0;
  ebitmap_iterator ebi;
  bool foundbit = false;

  /* This is not the fastest way to do this, we could simply look for
     the popcount, and start there, but this function is not used
     anywhere speed critical.  */
  EXECUTE_IF_SET_IN_EBITMAP (map, 0, i, ebi)
    {
      foundbit = true;
    }


  if (foundbit)
    return i;
  return -1;
}

/* Grow or shrink the internal word array for MAP to NEWSIZE
   elements.  */

static inline void
ebitmap_array_grow (ebitmap map, unsigned int newsize)
{
  if (newsize <= map->n_elts)
    {
      map->n_elts = newsize;
      return;
    }

  newsize += newsize / 4;

  map->n_elts = newsize;
  map->elts = XRESIZEVEC (EBITMAP_ELT_TYPE, map->elts, newsize);
}

/* Grow the internal word array for MAP so it is at least SIZE
   elements.  Will not shrink the array if it is already big
   enough.  */

static inline void
ebitmap_array_maybe_grow (ebitmap map, unsigned int size)
{
  if (size >= map->n_elts)
    ebitmap_array_grow (map, size + 1);
}

/* Retrieve the IDX'th element from the word array for MAP.  */

static inline EBITMAP_ELT_TYPE
ebitmap_array_get (ebitmap map, unsigned int idx)
{
  gcc_assert (idx < map->n_elts);
  return map->elts[idx];
}

/* Retrieve a pointer to the IDX'th element from the word array for
   MAP.  If the element index is greater than the size of the array,
   the array will be grown to the correct size.  */

static inline EBITMAP_ELT_TYPE *
ebitmap_array_grow_get (ebitmap map, unsigned int idx)
{
  if (idx >= map->n_elts)
    ebitmap_array_grow (map, idx + 1);
  return &map->elts[idx];
}

/* Initialize the internal word array for MAP, so that it is SIZE
   elements.  */

static inline void
ebitmap_array_init (ebitmap map, unsigned int size)
{
  if (size > 0)
    {
      map->elts = XNEWVEC (EBITMAP_ELT_TYPE, size);
      map->n_elts = size;
    }
  else
    {
      map->n_elts = 0;
      map->elts = NULL;
    }
}

/* Free the internal word array for MAP.  */

static inline void
ebitmap_array_clear (ebitmap map)
{
  if (map->elts)
    {
      free (map->elts);
      map->elts = NULL;
    }
  map->n_elts = 0;
}

/* Clear ebitmap MAP.  */

void
ebitmap_clear (ebitmap map)
{
  ebitmap_array_clear (map);
  sbitmap_zero (map->wordmask);
  map->wordmask = sbitmap_resize (map->wordmask, 1, 0);
  map->numwords = 0;
  map->cache = NULL;
  map->cacheindex = 0;
}

/* Allocate an ebitmap with enough room for SIZE bits to start out.  */

ebitmap
ebitmap_alloc (unsigned int size)
{
  ebitmap ret = XNEW (struct ebitmap_def);
  if (size == 0)
    size = EBITMAP_ELT_BITS;
  ebitmap_array_init (ret, (size + EBITMAP_ELT_BITS - 1) / EBITMAP_ELT_BITS);
  ret->wordmask = sbitmap_alloc_with_popcount (size);
  sbitmap_zero (ret->wordmask);
  ret->numwords = 0;
  ret->cache = NULL;
  ret->cacheindex = 0;

  return ret;
}

/* Clear BIT from ebitmap MAP.  */

void
ebitmap_clear_bit (ebitmap map, unsigned int bit)
{
  unsigned int wordindex = bit / EBITMAP_ELT_BITS;
  unsigned int eltwordindex = 0;
  unsigned int bitindex, shift;
  bool have_eltwordindex = false;
  EBITMAP_ELT_TYPE *elt_ptr;

  /* If the bit can't exist in our bitmap, just return.  */
  if (map->numwords == 0)
    return;

  if (wordindex >= map->wordmask->n_bits
      || !TEST_BIT (map->wordmask, wordindex))
    return;

  if (map->cache != NULL && map->cacheindex == wordindex)
    elt_ptr = map->cache;
  else
    {
      eltwordindex = sbitmap_popcount (map->wordmask, wordindex);
      elt_ptr = &map->elts[eltwordindex];
      have_eltwordindex = true;
    }

  bitindex = bit % EBITMAP_ELT_BITS;
  shift = bitindex;

  *(elt_ptr) &= ~(((EBITMAP_ELT_TYPE)1) << shift);

  /* Clear out the empty words.  */
  if (*(elt_ptr) == 0)
    {
      if (!have_eltwordindex)
	eltwordindex = sbitmap_popcount (map->wordmask, wordindex);

      if (map->cache != NULL)
        {
          if (map->cacheindex == wordindex)
            map->cache = NULL;
          else if (map->cacheindex > wordindex)
            map->cache = map->cache - 1;
        }

      RESET_BIT (map->wordmask, wordindex);

      memmove(&map->elts[eltwordindex], &map->elts[eltwordindex + 1],
	      sizeof (EBITMAP_ELT_TYPE) * (map->numwords - eltwordindex));
      map->numwords--;
    }
}

/* Set BIT in ebitmap MAP.  */

void
ebitmap_set_bit (ebitmap map, unsigned int bit)
{
  unsigned int wordindex = bit / EBITMAP_ELT_BITS;
  unsigned int eltwordindex;
  unsigned int bitindex =   bit % EBITMAP_ELT_BITS;

  /* If we have this element cached, just set the bit in it.  */
  if (map->cache && map->cacheindex == wordindex)
    {
      (*map->cache) |= (EBITMAP_ELT_TYPE)1 << bitindex;
      return;
    }

  /* Resize the wordmask if necessary.  */
  if (wordindex >= map->wordmask->n_bits)
    map->wordmask = sbitmap_resize (map->wordmask, wordindex + 1, 0);

  /* Allocate a new word in the array and move whatever is in it's
     place, if necessary. */
  if (!TEST_BIT (map->wordmask, wordindex))
    {
      unsigned long count;
      unsigned int i;

      SET_BIT (map->wordmask, wordindex);
      count = sbitmap_popcount (map->wordmask, wordindex);
      gcc_assert (count <= map->numwords);

      for (i = map->numwords; i > count; i--)
	{
	  EBITMAP_ELT_TYPE *newelt;
	  newelt = ebitmap_array_grow_get (map, i);
	  *newelt = ebitmap_array_get (map, i - 1);
	}
      map->numwords++;
      eltwordindex = count;
      ebitmap_array_maybe_grow (map, eltwordindex);
      map->elts[eltwordindex] = 0;
    }
  else
    {
      eltwordindex = sbitmap_popcount (map->wordmask, wordindex);
    }

  /* Set the actual bit.  */
  bitindex = bit % EBITMAP_ELT_BITS;

  map->elts[eltwordindex] |= (EBITMAP_ELT_TYPE)1 << bitindex;
  map->cache = &map->elts[eltwordindex];
  map->cacheindex = wordindex;
}


/* Return true if MAP contains BIT.  */

bool
ebitmap_bit_p (ebitmap map, unsigned int bit)
{
  unsigned int wordindex = bit / EBITMAP_ELT_BITS;
  unsigned int bitindex= bit % EBITMAP_ELT_BITS;

  /* Trivial empty ebitmap case.  */
  if (map->numwords == 0)
    return false;

  if (map->cache && map->cacheindex == wordindex)
    return ((*map->cache) >> bitindex) & 1;

  /* If the wordindex is greater than the size of the wordmask, *or*
     it's not set in the wordmask, this bit can't exist in our
     ebitmap.  */
  if (wordindex >= map->wordmask->n_bits
      || !TEST_BIT (map->wordmask, wordindex))
    return false;

  /* Find the bit and test it.  */
  map->cacheindex = wordindex;
  wordindex = sbitmap_popcount (map->wordmask, wordindex);
  map->cache = &map->elts[wordindex];

  return (map->elts[wordindex] >> bitindex) & 1;
}

/* Copy ebitmap SRC to DST.  */

void
ebitmap_copy (ebitmap dst, ebitmap src)
{
  /* Blow away any existing wordmask, and copy the new one.  */
  sbitmap_free (dst->wordmask);
  dst->wordmask = sbitmap_alloc_with_popcount (src->wordmask->n_bits);
  sbitmap_copy (dst->wordmask, src->wordmask);

  /* Make sure our destination array is big enough, and then copy the
     actual words.  */
  ebitmap_array_grow (dst, src->numwords);
  memcpy (dst->elts, src->elts,
	  src->numwords * sizeof (EBITMAP_ELT_TYPE));
  dst->numwords = src->numwords;
  dst->cache = NULL;
}

/* Dump ebitmap BMAP to FILE.  */

void
dump_ebitmap (FILE *file, ebitmap bmap)
{
  unsigned int pos;
  unsigned int i;
  int res;
  unsigned int size;

  res = sbitmap_last_set_bit (bmap->wordmask);
  if (res == -1)
    size = 0;
  else
    size = (res + 1) * EBITMAP_ELT_BITS;

  fprintf (file, "n_words = %d, set = {", bmap->numwords);

  for (pos = 30, i = 0; i < size; i++)
    if (ebitmap_bit_p (bmap, i))
      {
	if (pos > 70)
	  {
	    fprintf (file, "\n  ");
	    pos = 0;
	  }

	pos += fprintf (file, "%d ", i);
      }

  fprintf (file, "}\n");
}

/* Dump ebitmap BMAP to stderr.  */

void
debug_ebitmap (ebitmap bmap)
{
  dump_ebitmap (stderr, bmap);
}

/* Perform the operation DST &= SRC.  */

void
ebitmap_and_into (ebitmap dst, ebitmap src)
{
  sbitmap_iterator sbi;
  unsigned int i;
  unsigned int neweltindex = 0;
  unsigned int dsteltindex = 0;
  unsigned int srceltindex = 0;

  gcc_assert (dst != src);

  dst->cache = NULL;

  /* Short circuit the empty bitmap cases.  */
  if (src->numwords == 0 || dst->numwords == 0)
    {
      ebitmap_clear (dst);
      return;
    }

  /* AND the masks, then walk the words that may actually appear in
     the result, AND'ing them.  */
  sbitmap_a_and_b (dst->wordmask, dst->wordmask, src->wordmask);

  EXECUTE_IF_SET_IN_SBITMAP (dst->wordmask, 0, i, sbi)
    {
      EBITMAP_ELT_TYPE tmpword = ebitmap_array_get (src, srceltindex++);
      tmpword &= ebitmap_array_get (dst, dsteltindex++);
      if (tmpword != 0)
	{
	  EBITMAP_ELT_TYPE *dstplace;
	  dstplace = ebitmap_array_grow_get (dst, neweltindex++);
	  *dstplace = tmpword;
	}
      else
	RESET_BIT (dst->wordmask, i);
    }
#ifdef EBITMAP_DEBUGGING
  {
    unsigned int i;

    for (i = 0; i <  dst->numwords; i++)
      gcc_assert (dst->elts[i] != 0);

    sbitmap_verify_popcount (dst->wordmask);
    gcc_assert (sbitmap_popcount (dst->wordmask,
				  dst->wordmask->n_bits) == dst->numwords);
  }
#endif
  dst->numwords = neweltindex;
}

/* Perform the operation DST = SRC1 & SRC2.  */

void
ebitmap_and (ebitmap dst, ebitmap src1, ebitmap src2)
{
  sbitmap_iterator sbi;
  unsigned int i;
  unsigned int neweltindex = 0;
  unsigned int src1eltindex = 0;
  unsigned int src2eltindex = 0;

  dst->cache = NULL;
  if (src1->numwords == 0 || src2->numwords == 0)
    {
      ebitmap_clear (dst);
      return;
    }

  dst->wordmask
    = sbitmap_resize (dst->wordmask,
		      MIN (src1->wordmask->n_bits, src2->wordmask->n_bits),
		      0);
  sbitmap_a_and_b (dst->wordmask, src1->wordmask, src2->wordmask);

  EXECUTE_IF_SET_IN_SBITMAP (dst->wordmask, 0, i, sbi)
    {
      bool src1hasword, src2hasword;

      src1hasword = TEST_BIT (src1->wordmask, i);
      src2hasword = TEST_BIT (src2->wordmask, i);

      if (src1hasword && src2hasword)
	{
	  EBITMAP_ELT_TYPE tmpword = ebitmap_array_get (src1, src1eltindex++);
	  tmpword &= ebitmap_array_get (src2, src2eltindex++);
	  if (tmpword != 0)
	    {
	      EBITMAP_ELT_TYPE *dstplace;
	      dstplace = ebitmap_array_grow_get (dst, neweltindex++);
	      *dstplace = tmpword;
	    }
	  else
	    RESET_BIT (dst->wordmask, i);
	}
      else if (src1hasword)
	src1eltindex++;
      else
	src2eltindex++;
    }
#ifdef EBITMAP_DEBUGGING
  {
    ebitmap_iterator ebi;
    unsigned int i;

    for (i = 0; i <  dst->numwords; i++)
      gcc_assert (dst->elts[i] != 0);

    EXECUTE_IF_SET_IN_EBITMAP (src1, 0, i, ebi)
      if (ebitmap_bit_p (src2, i))
	gcc_assert (ebitmap_bit_p (dst, i));

    for (i = 0; i <  dst->numwords; i++)
      gcc_assert (dst->elts[i] != 0);

    sbitmap_verify_popcount (dst->wordmask);
    gcc_assert (sbitmap_popcount (dst->wordmask,
				  dst->wordmask->n_bits) == dst->numwords);
  }
#endif
  dst->numwords = neweltindex;
}

/* Perform the operation DST |= SRC.  Return true if any bits in DST
   changed.  */

bool
ebitmap_ior_into (ebitmap dst, ebitmap src)
{
  unsigned int dstsize = dst->wordmask->n_bits;
  unsigned int srcsize = src->wordmask->n_bits;
  sbitmap_iterator sbi;
  unsigned int i;
  sbitmap tempmask;
  unsigned int neweltindex = 0;
  unsigned int dsteltindex = 0;
  unsigned int srceltindex = 0;
  bool changed = false;
  EBITMAP_ELT_TYPE *newarray;
  unsigned int newarraysize;
#ifdef EBITMAP_DEBUGGING
  ebitmap dstcopy = ebitmap_alloc (1);
  ebitmap_copy (dstcopy, dst);
#endif

  dst->cache = NULL;
  if (dst == src)
    return false;

  if (dst->numwords == 0 && src->numwords != 0)
    {
      ebitmap_copy (dst, src);
      return true;
    }
  else if (src->numwords == 0)
    return false;

  /* We can do without the temp mask if it's faster, but it would mean
     touching more words in the actual dense vector.  */
  tempmask = sbitmap_alloc (MAX (srcsize, dstsize));
  sbitmap_zero (tempmask);
  if (srcsize == dstsize)
    {
      sbitmap_a_or_b (tempmask, dst->wordmask, src->wordmask);
    }
  else
    {
      dst->wordmask = sbitmap_resize (dst->wordmask, MAX (srcsize, dstsize),
				      0);
      if (srcsize >= dstsize)
	{
	  sbitmap_copy_n (tempmask, dst->wordmask, dst->wordmask->size);
	  sbitmap_a_or_b (tempmask, tempmask, src->wordmask);
	}
      else
	{
	  sbitmap_copy_n (tempmask, src->wordmask, src->wordmask->size);
	  sbitmap_a_or_b (tempmask, tempmask, dst->wordmask);
	}
    }
  newarraysize = src->numwords + dst->numwords;
  newarray = XNEWVEC (EBITMAP_ELT_TYPE, newarraysize);

  EXECUTE_IF_SET_IN_SBITMAP (tempmask, 0, i, sbi)
    {
      bool dsthasword, srchasword;

      dsthasword = (i < dst->wordmask->n_bits
		    && TEST_BIT (dst->wordmask, i));
      srchasword = (i < src->wordmask->n_bits
		    && TEST_BIT (src->wordmask, i));

      if (dsthasword && srchasword)
	{
	  EBITMAP_ELT_TYPE tmpword = ebitmap_array_get (src, srceltindex++);
	  tmpword |= ebitmap_array_get (dst, dsteltindex);
	  if (!changed)
	    changed |= tmpword != ebitmap_array_get (dst, dsteltindex);
	  dsteltindex++;
	  newarray[neweltindex++] = tmpword;
	}
      else if (dsthasword)
	{
	  newarray [neweltindex++] = ebitmap_array_get (dst, dsteltindex++);
	}
      else
	{
	  newarray [neweltindex++] = ebitmap_array_get (src, srceltindex++);
	  gcc_assert (i < dst->wordmask->n_bits);
	  SET_BIT (dst->wordmask, i);
	  changed |= true;
	}
    }

  sbitmap_free (tempmask);
  if (changed)
    {
      dst->numwords = neweltindex;
      free (dst->elts);
      dst->elts = newarray;
      dst->n_elts = newarraysize;
    }
  else
    free (newarray);

#ifdef EBITMAP_DEBUGGING
  {
    ebitmap_iterator ebi;
    unsigned int i;

    for (i = 0; i <  dst->numwords; i++)
      gcc_assert (dst->elts[i] != 0);

    EXECUTE_IF_SET_IN_EBITMAP (src, 0, i, ebi)
      gcc_assert (ebitmap_bit_p (dst, i));
    EXECUTE_IF_SET_IN_EBITMAP (dstcopy, 0, i, ebi)
      gcc_assert (ebitmap_bit_p (dst, i));

    sbitmap_verify_popcount (dst->wordmask);
    gcc_assert (changed == !ebitmap_equal_p (dst, dstcopy));
    gcc_assert (sbitmap_popcount (dst->wordmask,
				  dst->wordmask->n_bits) == dst->numwords);
  }
#endif
  return changed;
}

/* Perform the operation DST = SRC1 | SRC2.  Return true if any bit
   in DST has changed.  */

bool
ebitmap_ior (ebitmap dst, ebitmap src1, ebitmap src2)
{
  unsigned int src1size = src1->wordmask->n_bits;
  unsigned int src2size = src2->wordmask->n_bits;
  sbitmap_iterator sbi;
  unsigned int i;
  sbitmap tempmask;
  unsigned int neweltindex = 0;
  unsigned int src1eltindex = 0;
  unsigned int src2eltindex = 0;
  bool changed = false;
  EBITMAP_ELT_TYPE *newarray;
  unsigned int newarraysize;
#ifdef EBITMAP_DEBUGGING
  ebitmap dstcopy = ebitmap_alloc (1);
  ebitmap_copy (dstcopy, dst);
#endif

  dst->cache = NULL;
  tempmask = sbitmap_alloc_with_popcount (MAX (src1size, src2size));
  sbitmap_zero (tempmask);
  if (src1size == src2size)
    {
      sbitmap_a_or_b (tempmask, src1->wordmask, src2->wordmask);
    }
  else
    {
      if (src1size >= src2size)
	{
	  sbitmap_copy_n (tempmask, src2->wordmask, src2->wordmask->size);
	  sbitmap_a_or_b (tempmask, tempmask, src1->wordmask);
	}
      else
	{
	  sbitmap_copy_n (tempmask, src1->wordmask, src1->wordmask->size);
	  sbitmap_a_or_b (tempmask, tempmask, src2->wordmask);
	}
    }
  newarraysize = src1->numwords + src2->numwords;
  newarray = XNEWVEC (EBITMAP_ELT_TYPE, newarraysize);

  EXECUTE_IF_SET_IN_SBITMAP (tempmask, 0, i, sbi)
    {
      bool src1hasword, src2hasword;
      EBITMAP_ELT_TYPE tmpword;
      src1hasword = (i < src1->wordmask->n_bits
		    && TEST_BIT (src1->wordmask, i));
      src2hasword = (i < src2->wordmask->n_bits
		    && TEST_BIT (src2->wordmask, i));

      if (src1hasword && src2hasword)
	{
	  tmpword = (ebitmap_array_get (src1, src1eltindex++)
		     | ebitmap_array_get (src2, src2eltindex++));
	  newarray[neweltindex++] = tmpword;
	}
      else if (src1hasword)
	{
	  tmpword = ebitmap_array_get (src1, src1eltindex++);
	  newarray [neweltindex++] = tmpword;
	}
      else
	{
	  tmpword = ebitmap_array_get (src2, src2eltindex++);
	  newarray [neweltindex++] = tmpword;
	}

      if (i >= dst->wordmask->n_bits || !TEST_BIT (dst->wordmask, i))
	{
	  changed = true;
	}
      else if (!changed)
	{
	  unsigned int count = sbitmap_popcount (dst->wordmask, i);
	  changed |= ebitmap_array_get (dst, count) != tmpword;
	}
    }

  if (changed)
    {
      sbitmap_free (dst->wordmask);
      dst->wordmask = tempmask;
      dst->numwords = neweltindex;
      free (dst->elts);
      dst->elts = newarray;
      dst->n_elts = newarraysize;
    }
  else
    {
      sbitmap_free (tempmask);
      free (newarray);
    }

#ifdef EBITMAP_DEBUGGING
  {
    ebitmap_iterator ebi;
    unsigned int i;

    for (i = 0; i <  dst->numwords; i++)
      gcc_assert (dst->elts[i] != 0);

    EXECUTE_IF_SET_IN_EBITMAP (src1, 0, i, ebi)
      gcc_assert (ebitmap_bit_p (dst, i));

    EXECUTE_IF_SET_IN_EBITMAP (src2, 0, i, ebi)
      gcc_assert (ebitmap_bit_p (dst, i));
  }
  sbitmap_verify_popcount (dst->wordmask);
  gcc_assert (changed == !ebitmap_equal_p (dst, dstcopy));
  gcc_assert (sbitmap_popcount (dst->wordmask,
				dst->wordmask->n_bits) == dst->numwords);
#endif

  return changed;
}

/* Perform the operation DST &= ~SRC.  Return true if any bit in DST
   has changed.  */

bool
ebitmap_and_compl_into (ebitmap dst, ebitmap src)
{
  bool changed = false;
  unsigned int i;
  unsigned int neweltindex = 0;
  unsigned int dsteltindex = 0;
  sbitmap_iterator sbi;
#ifdef EBITMAP_DEBUGGING
  ebitmap dstcopy = ebitmap_alloc (1);
  ebitmap_copy (dstcopy, dst);
#endif

  gcc_assert (dst != src);
  dst->cache = NULL;
  if (src->numwords == 0)
    return false;

  EXECUTE_IF_SET_IN_SBITMAP (dst->wordmask, 0, i, sbi)
    {
      bool srchasword;

      srchasword = (i < src->wordmask->n_bits
		    && TEST_BIT (src->wordmask, i));

      if (srchasword)
	{
	  unsigned int srccount = sbitmap_popcount (src->wordmask, i);
	  EBITMAP_ELT_TYPE tmpword = ebitmap_array_get (dst, dsteltindex);
	  tmpword &= ~(ebitmap_array_get (src, srccount));
	  if (!changed)
	    changed |= ebitmap_array_get (dst, dsteltindex) != tmpword;
	  dsteltindex++;
	  if (tmpword != 0)
	    {
	      EBITMAP_ELT_TYPE *dstplace;
	      dstplace = ebitmap_array_grow_get (dst, neweltindex++);
	      *dstplace = tmpword;
	    }
	  else
	    RESET_BIT (dst->wordmask, i);
	}
      else
	{
	  dsteltindex++;
	  neweltindex++;
	}
    }
#ifdef EBITMAP_DEBUGGING
  {
    unsigned int i;
    ebitmap_iterator ebi;

    EXECUTE_IF_SET_IN_EBITMAP (dstcopy, 0, i, ebi)
      {
	if (!ebitmap_bit_p (src, i))
	  gcc_assert (ebitmap_bit_p (dst, i));
      }

    for (i = 0; i <  dst->numwords; i++)
      gcc_assert (dst->elts[i] != 0);

    gcc_assert (sbitmap_popcount (dst->wordmask,
				  dst->wordmask->n_bits) == neweltindex);
    sbitmap_verify_popcount (dst->wordmask);
    gcc_assert (changed == !ebitmap_equal_p (dst, dstcopy));
    gcc_assert (sbitmap_popcount (dst->wordmask,
				  dst->wordmask->n_bits) == dst->numwords);
  }
#endif
  dst->numwords = neweltindex;
  /* sbitmap_popcount (dst->wordmask, -1); */

  return changed;
}

/* Perform the operation DST = SRC1 & ~SRC2.  Return true if any bit
   in DST has changed.  */

bool
ebitmap_and_compl (ebitmap dst, ebitmap src1, ebitmap src2)
{
  unsigned int src1size = src1->wordmask->n_bits;
  sbitmap_iterator sbi;
  unsigned int i;
  sbitmap tempmask;
  unsigned int neweltindex = 0;
  unsigned int src1eltindex = 0;
  bool changed = false;
  EBITMAP_ELT_TYPE *newarray;
  unsigned int newarraysize;

  /* XXX: Optimize like the into version.  */
  dst->cache = NULL;
  tempmask = sbitmap_alloc_with_popcount (src1size);
  sbitmap_zero (tempmask);
  sbitmap_copy (tempmask, src1->wordmask);

  newarraysize = src1->numwords;
  newarray = XNEWVEC (EBITMAP_ELT_TYPE, newarraysize);

  EXECUTE_IF_SET_IN_SBITMAP (src1->wordmask, 0, i, sbi)
    {
      bool src2hasword;
      EBITMAP_ELT_TYPE tmpword;

      src2hasword = (i < src2->wordmask->n_bits
		     && TEST_BIT (src2->wordmask, i));

      if (src2hasword)
	{
	  unsigned int src2count = sbitmap_popcount (src2->wordmask, i);
	  tmpword = ebitmap_array_get (src1, src1eltindex++)
	            & ~(ebitmap_array_get (src2, src2count));
	  if (tmpword)
	    {
	      newarray[neweltindex++] = tmpword;
	    }
	  else
	    RESET_BIT (tempmask, i);

	}
      else
	{
	  tmpword = ebitmap_array_get (src1, src1eltindex++);
	  gcc_assert (tmpword != 0);
	  newarray[neweltindex++] = tmpword;
	}

      if (i >= dst->wordmask->n_bits || !TEST_BIT (dst->wordmask, i))
	{
	  changed = true;
	}
      else if (!changed)
	{
	  unsigned int count = sbitmap_popcount (dst->wordmask, i);
	  changed |= ebitmap_array_get (dst, count) != tmpword;
	}
    }
  if (changed)
    {
      sbitmap_free (dst->wordmask);
      dst->wordmask = tempmask;
      dst->numwords = neweltindex;
      free (dst->elts);
      dst->elts = newarray;
      dst->n_elts = newarraysize;
    }
  else
    {
      free (tempmask);
      free (newarray);
    }
#ifdef EBITMAP_DEBUGGING
  {
    unsigned int i;
    ebitmap_iterator ebi;

    EXECUTE_IF_SET_IN_EBITMAP (src1, 0, i, ebi)
      {
	if (!ebitmap_bit_p (src2, i))
	  gcc_assert (ebitmap_bit_p (dst, i));
      }
  for (i = 0; i <  dst->numwords; i++)
    gcc_assert (dst->elts[i] != 0);

  sbitmap_verify_popcount (dst->wordmask);
  gcc_assert (sbitmap_popcount (dst->wordmask,
				dst->wordmask->n_bits) == dst->numwords);
  }
#endif
  return changed;
}

/* Perform the operation DST = A | (B & ~C).  */

bool
ebitmap_ior_and_compl (ebitmap dst, ebitmap a, ebitmap b, ebitmap c)
{
  bool changed;
  ebitmap temp = ebitmap_alloc (1);
#ifdef EBITMAP_DEBUGGING
  ebitmap dstcopy = ebitmap_alloc (1);
  ebitmap_copy (dstcopy, dst);
#endif

  dst->cache = NULL;
  ebitmap_and_compl (temp, b, c);
  changed = ebitmap_ior (dst, a, temp);
#ifdef EBITMAP_DEBUGGING
  {
    ebitmap_iterator ebi;
    unsigned int i;

    EXECUTE_IF_SET_IN_EBITMAP (a, 0, i, ebi)
      gcc_assert (ebitmap_bit_p (dst, i));

    EXECUTE_IF_SET_IN_EBITMAP (b, 0, i, ebi)
      if (!ebitmap_bit_p (c, i))
	gcc_assert (ebitmap_bit_p (dst, i));
    gcc_assert (changed == !ebitmap_equal_p (dst, dstcopy));
  }
#endif
  ebitmap_free (temp);

  return changed;
}

/* Return true if ebitmap DST is equal to ebitmap SRC.  */

bool
ebitmap_equal_p (ebitmap dst, ebitmap src)
{
  unsigned int which = MIN (dst->wordmask->size, src->wordmask->size);

  if (dst->numwords != src->numwords)
    return false;

  /* sbitmap_equal compares up to the size of the first argument, so
     if the two sbitmaps are not equally sized, we need to pass the
     smaller one as the first argument, or it will crash.  */
  if (which == dst->wordmask->size
      && !sbitmap_equal (dst->wordmask, src->wordmask))
    return false;
  else if (which == src->wordmask->size
	   && !sbitmap_equal (src->wordmask, dst->wordmask))
    return false;

  return memcmp (dst->elts, src->elts,
		 dst->numwords * sizeof (EBITMAP_ELT_TYPE)) == 0;
  return true;
}
