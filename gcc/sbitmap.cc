/* Simple bitmaps.
   Copyright (C) 1999-2023 Free Software Foundation, Inc.

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
#include "sbitmap.h"
#include "selftest.h"

typedef SBITMAP_ELT_TYPE *sbitmap_ptr;
typedef const SBITMAP_ELT_TYPE *const_sbitmap_ptr;

/* Return the size in bytes of a bitmap MAP.  */

static inline unsigned int sbitmap_size_bytes (const_sbitmap map)
{
   return map->size * sizeof (SBITMAP_ELT_TYPE);
}


/* Bitmap manipulation routines.  */

/* Allocate a simple bitmap of N_ELMS bits.  */

sbitmap
sbitmap_alloc (unsigned int n_elms)
{
  unsigned int bytes, size, amt;
  sbitmap bmap;

  size = SBITMAP_SET_SIZE (n_elms);
  bytes = size * sizeof (SBITMAP_ELT_TYPE);
  amt = (sizeof (struct simple_bitmap_def)
	 + bytes - sizeof (SBITMAP_ELT_TYPE));
  bmap = (sbitmap) xmalloc (amt);
  bmap->n_bits = n_elms;
  bmap->size = size;
  return bmap;
}

/* Resize a simple bitmap BMAP to N_ELMS bits.  If increasing the
   size of BMAP, clear the new bits to zero if the DEF argument
   is zero, and set them to one otherwise.  */

sbitmap
sbitmap_resize (sbitmap bmap, unsigned int n_elms, int def)
{
  unsigned int bytes, size, amt;
  unsigned int last_bit;

  size = SBITMAP_SET_SIZE (n_elms);
  bytes = size * sizeof (SBITMAP_ELT_TYPE);
  if (bytes > sbitmap_size_bytes (bmap))
    {
      amt = (sizeof (struct simple_bitmap_def)
	    + bytes - sizeof (SBITMAP_ELT_TYPE));
      bmap = (sbitmap) xrealloc (bmap, amt);
    }

  if (n_elms > bmap->n_bits)
    {
      if (def)
	{
	  memset (bmap->elms + bmap->size, -1,
		  bytes - sbitmap_size_bytes (bmap));

	  /* Set the new bits if the original last element.  */
	  last_bit = bmap->n_bits % SBITMAP_ELT_BITS;
	  if (last_bit)
	    bmap->elms[bmap->size - 1]
	      |= ~((SBITMAP_ELT_TYPE)-1 >> (SBITMAP_ELT_BITS - last_bit));

	  /* Clear the unused bit in the new last element.  */
	  last_bit = n_elms % SBITMAP_ELT_BITS;
	  if (last_bit)
	    bmap->elms[size - 1]
	      &= (SBITMAP_ELT_TYPE)-1 >> (SBITMAP_ELT_BITS - last_bit);
	}
      else
	memset (bmap->elms + bmap->size, 0, bytes - sbitmap_size_bytes (bmap));
    }
  else if (n_elms < bmap->n_bits)
    {
      /* Clear the surplus bits in the last word.  */
      last_bit = n_elms % SBITMAP_ELT_BITS;
      if (last_bit)
	bmap->elms[size - 1]
	  &= (SBITMAP_ELT_TYPE)-1 >> (SBITMAP_ELT_BITS - last_bit);
    }

  bmap->n_bits = n_elms;
  bmap->size = size;
  return bmap;
}

/* Re-allocate a simple bitmap of N_ELMS bits. New storage is uninitialized.  */

sbitmap
sbitmap_realloc (sbitmap src, unsigned int n_elms)
{
  unsigned int bytes, size, amt;
  sbitmap bmap;

  size = SBITMAP_SET_SIZE (n_elms);
  bytes = size * sizeof (SBITMAP_ELT_TYPE);
  amt = (sizeof (struct simple_bitmap_def)
	 + bytes - sizeof (SBITMAP_ELT_TYPE));

  if (sbitmap_size_bytes (src)  >= bytes)
    {
      src->n_bits = n_elms;
      return src;
    }

  bmap = (sbitmap) xrealloc (src, amt);
  bmap->n_bits = n_elms;
  bmap->size = size;
  return bmap;
}

/* Allocate a vector of N_VECS bitmaps of N_ELMS bits.  */

sbitmap *
sbitmap_vector_alloc (unsigned int n_vecs, unsigned int n_elms)
{
  unsigned int i, size;
  size_t amt, bytes, vector_bytes, elm_bytes, offset;
  sbitmap *bitmap_vector;

  size = SBITMAP_SET_SIZE (n_elms);
  bytes = size * sizeof (SBITMAP_ELT_TYPE);
  elm_bytes = (sizeof (struct simple_bitmap_def)
	       + bytes - sizeof (SBITMAP_ELT_TYPE));
  vector_bytes = n_vecs * sizeof (sbitmap *);

  /* Round up `vector_bytes' to account for the alignment requirements
     of an sbitmap.  One could allocate the vector-table and set of sbitmaps
     separately, but that requires maintaining two pointers or creating
     a cover struct to hold both pointers (so our result is still just
     one pointer).  Neither is a bad idea, but this is simpler for now.  */
  {
    /* Based on DEFAULT_ALIGNMENT computation in obstack.c.  */
    struct { char x; SBITMAP_ELT_TYPE y; } align;
    int alignment = (char *) & align.y - & align.x;
    vector_bytes = (vector_bytes + alignment - 1) & ~ (alignment - 1);
  }

  amt = vector_bytes + (n_vecs * elm_bytes);
  bitmap_vector = (sbitmap *) xmalloc (amt);

  for (i = 0, offset = vector_bytes; i < n_vecs; i++, offset += elm_bytes)
    {
      sbitmap b = (sbitmap) ((char *) bitmap_vector + offset);

      bitmap_vector[i] = b;
      b->n_bits = n_elms;
      b->size = size;
    }

  return bitmap_vector;
}

/* Copy sbitmap SRC to DST.  */

void
bitmap_copy (sbitmap dst, const_sbitmap src)
{
  gcc_checking_assert (src->size <= dst->size);

  memcpy (dst->elms, src->elms, sizeof (SBITMAP_ELT_TYPE) * dst->size);
}

/* Determine if a == b.  */
int
bitmap_equal_p (const_sbitmap a, const_sbitmap b)
{
  bitmap_check_sizes (a, b);

  return !memcmp (a->elms, b->elms, sizeof (SBITMAP_ELT_TYPE) * a->size);
}

/* Return true if the bitmap is empty.  */

bool
bitmap_empty_p (const_sbitmap bmap)
{
  unsigned int i;
  for (i=0; i<bmap->size; i++)
    if (bmap->elms[i])
      return false;

  return true;
}

/* Clear COUNT bits from START in BMAP.  */

void
bitmap_clear_range (sbitmap bmap, unsigned int start, unsigned int count)
{
  if (count == 0)
    return;

  bitmap_check_index (bmap, start + count - 1);

  unsigned int start_word = start / SBITMAP_ELT_BITS;
  unsigned int start_bitno = start % SBITMAP_ELT_BITS;

  /* Clearing less than a full word, starting at the beginning of a word.  */
  if (start_bitno == 0 && count < SBITMAP_ELT_BITS)
    {
      SBITMAP_ELT_TYPE mask = ((SBITMAP_ELT_TYPE)1 << count) - 1;
      bmap->elms[start_word] &= ~mask;
      return;
    }

  unsigned int end_word = (start + count) / SBITMAP_ELT_BITS;
  unsigned int end_bitno = (start + count) % SBITMAP_ELT_BITS;

  /* Clearing starts somewhere in the middle of the first word.  Clear up to
     the end of the first word or the end of the requested region, whichever
     comes first.  */
  if (start_bitno != 0)
    {
      unsigned int nbits = ((start_word == end_word)
			    ? end_bitno - start_bitno
			    : SBITMAP_ELT_BITS - start_bitno);
      SBITMAP_ELT_TYPE mask = ((SBITMAP_ELT_TYPE)1 << nbits) - 1;
      mask <<= start_bitno;
      bmap->elms[start_word] &= ~mask;
      start_word++;
      count -= nbits;
    }

  if (count == 0)
    return;

  /* Now clear words at a time until we hit a partial word.  */
  unsigned int nwords = (end_word - start_word);
  if (nwords)
    {
      memset (&bmap->elms[start_word], 0, nwords * sizeof (SBITMAP_ELT_TYPE));
      count -= nwords * sizeof (SBITMAP_ELT_TYPE) * BITS_PER_UNIT;
      start_word += nwords;
    }

  if (count == 0)
    return;

  /* Now handle residuals in the last word.  */
  SBITMAP_ELT_TYPE mask = ((SBITMAP_ELT_TYPE)1 << count) - 1;
  bmap->elms[start_word] &= ~mask;
}

/* Set COUNT bits from START in BMAP.  */
void
bitmap_set_range (sbitmap bmap, unsigned int start, unsigned int count)
{
  if (count == 0)
    return;

  bitmap_check_index (bmap, start + count - 1);

  unsigned int start_word = start / SBITMAP_ELT_BITS;
  unsigned int start_bitno = start % SBITMAP_ELT_BITS;

  /* Setting less than a full word, starting at the beginning of a word.  */
  if (start_bitno == 0 && count < SBITMAP_ELT_BITS)
    {
      SBITMAP_ELT_TYPE mask = ((SBITMAP_ELT_TYPE)1 << count) - 1;
      bmap->elms[start_word] |= mask;
      return;
    }

  unsigned int end_word = (start + count) / SBITMAP_ELT_BITS;
  unsigned int end_bitno = (start + count) % SBITMAP_ELT_BITS;

  /* Setting starts somewhere in the middle of the first word.  Set up to
     the end of the first word or the end of the requested region, whichever
     comes first.  */
  if (start_bitno != 0)
    {
      unsigned int nbits = ((start_word == end_word)
			    ? end_bitno - start_bitno
			    : SBITMAP_ELT_BITS - start_bitno);
      SBITMAP_ELT_TYPE mask = ((SBITMAP_ELT_TYPE)1 << nbits) - 1;
      mask <<= start_bitno;
      bmap->elms[start_word] |= mask;
      start_word++;
      count -= nbits;
    }

  if (count == 0)
    return;

  /* Now set words at a time until we hit a partial word.  */
  unsigned int nwords = (end_word - start_word);
  if (nwords)
    {
      memset (&bmap->elms[start_word], 0xff,
	      nwords * sizeof (SBITMAP_ELT_TYPE));
      count -= nwords * sizeof (SBITMAP_ELT_TYPE) * BITS_PER_UNIT;
      start_word += nwords;
    }

  if (count == 0)
    return;

  /* Now handle residuals in the last word.  */
  SBITMAP_ELT_TYPE mask = ((SBITMAP_ELT_TYPE)1 << count) - 1;
  bmap->elms[start_word] |= mask;
}

/* Return TRUE if any bit between START and END inclusive is set within
   the simple bitmap BMAP.  Return FALSE otherwise.  */

bool
bitmap_bit_in_range_p (const_sbitmap bmap, unsigned int start, unsigned int end)
{
  gcc_checking_assert (start <= end);
  bitmap_check_index (bmap, end);

  unsigned int start_word = start / SBITMAP_ELT_BITS;
  unsigned int start_bitno = start % SBITMAP_ELT_BITS;

  unsigned int end_word = end / SBITMAP_ELT_BITS;
  unsigned int end_bitno = end % SBITMAP_ELT_BITS;

  /* Check beginning of first word if different from zero.  */
  if (start_bitno != 0)
    {
      SBITMAP_ELT_TYPE high_mask = ~(SBITMAP_ELT_TYPE)0;
      if (start_word == end_word && end_bitno + 1 < SBITMAP_ELT_BITS)
	high_mask = ((SBITMAP_ELT_TYPE)1 << (end_bitno + 1)) - 1;

      SBITMAP_ELT_TYPE low_mask = ((SBITMAP_ELT_TYPE)1 << start_bitno) - 1;
      SBITMAP_ELT_TYPE mask = high_mask - low_mask;
      if (bmap->elms[start_word] & mask)
	return true;
      start_word++;
    }

  if (start_word > end_word)
    return false;

  /* Now test words at a time until we hit a partial word.  */
  unsigned int nwords = (end_word - start_word);
  while (nwords)
    {
      if (bmap->elms[start_word])
	return true;
      start_word++;
      nwords--;
    }

  /* Now handle residuals in the last word.  */
  SBITMAP_ELT_TYPE mask = ~(SBITMAP_ELT_TYPE)0;
  if (end_bitno + 1 < SBITMAP_ELT_BITS)
    mask = ((SBITMAP_ELT_TYPE)1 << (end_bitno + 1)) - 1;
  return (bmap->elms[start_word] & mask) != 0;
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
sbitmap_popcount (SBITMAP_ELT_TYPE a)
{
  unsigned long ret = 0;
  unsigned i;

  /* Just do this the table way for now  */
  for (i = 0; i < HOST_BITS_PER_WIDEST_FAST_INT; i += 8)
    ret += popcount_table[(a >> i) & 0xff];
  return ret;
}
#endif

/* Count and return the number of bits set in the bitmap BMAP.  */

unsigned int
bitmap_count_bits (const_sbitmap bmap)
{
  unsigned int count = 0;
  for (unsigned int i = 0; i < bmap->size; i++)
    if (bmap->elms[i])
      {
#if GCC_VERSION < 3400
	count += sbitmap_popcount (bmap->elms[i]);
#else
# if HOST_BITS_PER_WIDEST_FAST_INT == HOST_BITS_PER_LONG
	count += __builtin_popcountl (bmap->elms[i]);
# elif HOST_BITS_PER_WIDEST_FAST_INT == HOST_BITS_PER_LONGLONG
	count += __builtin_popcountll (bmap->elms[i]);
# else
	count += __builtin_popcount (bmap->elms[i]);
# endif
#endif
      }
  return count;
}

/* Zero all elements in a bitmap.  */

void
bitmap_clear (sbitmap bmap)
{
  memset (bmap->elms, 0, sbitmap_size_bytes (bmap));
}

/* Set all elements in a bitmap to ones.  */

void
bitmap_ones (sbitmap bmap)
{
  unsigned int last_bit;

  memset (bmap->elms, -1, sbitmap_size_bytes (bmap));

  last_bit = bmap->n_bits % SBITMAP_ELT_BITS;
  if (last_bit)
    bmap->elms[bmap->size - 1]
      = (SBITMAP_ELT_TYPE)-1 >> (SBITMAP_ELT_BITS - last_bit);
}

/* Zero a vector of N_VECS bitmaps.  */

void
bitmap_vector_clear (sbitmap *bmap, unsigned int n_vecs)
{
  unsigned int i;

  for (i = 0; i < n_vecs; i++)
    bitmap_clear (bmap[i]);
}

/* Set a vector of N_VECS bitmaps to ones.  */

void
bitmap_vector_ones (sbitmap *bmap, unsigned int n_vecs)
{
  unsigned int i;

  for (i = 0; i < n_vecs; i++)
    bitmap_ones (bmap[i]);
}

/* Set DST to be A union (B - C).
   DST = A | (B & ~C).
   Returns true if any change is made.  */

bool
bitmap_ior_and_compl (sbitmap dst, const_sbitmap a, const_sbitmap b, const_sbitmap c)
{
  bitmap_check_sizes (a, b);
  bitmap_check_sizes (b, c);

  unsigned int i, n = dst->size;
  sbitmap_ptr dstp = dst->elms;
  const_sbitmap_ptr ap = a->elms;
  const_sbitmap_ptr bp = b->elms;
  const_sbitmap_ptr cp = c->elms;
  SBITMAP_ELT_TYPE changed = 0;

  for (i = 0; i < n; i++)
    {
      const SBITMAP_ELT_TYPE tmp = *ap++ | (*bp++ & ~*cp++);
      changed |= *dstp ^ tmp;
      *dstp++ = tmp;
    }

  return changed != 0;
}

/* Set bitmap DST to the bitwise negation of the bitmap SRC.  */

void
bitmap_not (sbitmap dst, const_sbitmap src)
{
  bitmap_check_sizes (src, dst);

  unsigned int i, n = dst->size;
  sbitmap_ptr dstp = dst->elms;
  const_sbitmap_ptr srcp = src->elms;
  unsigned int last_bit;

  for (i = 0; i < n; i++)
    *dstp++ = ~*srcp++;

  /* Zero all bits past n_bits, by ANDing dst with bitmap_ones.  */
  last_bit = src->n_bits % SBITMAP_ELT_BITS;
  if (last_bit)
    dst->elms[n-1] = dst->elms[n-1]
      & ((SBITMAP_ELT_TYPE)-1 >> (SBITMAP_ELT_BITS - last_bit));
}

/* Set the bits in DST to be the difference between the bits
   in A and the bits in B. i.e. dst = a & (~b).  */

void
bitmap_and_compl (sbitmap dst, const_sbitmap a, const_sbitmap b)
{
  bitmap_check_sizes (a, b);
  bitmap_check_sizes (b, dst);

  unsigned int i, dst_size = dst->size;
  unsigned int min_size = dst->size;
  sbitmap_ptr dstp = dst->elms;
  const_sbitmap_ptr ap = a->elms;
  const_sbitmap_ptr bp = b->elms;

  /* A should be at least as large as DEST, to have a defined source.  */
  gcc_assert (a->size >= dst_size);
  /* If minuend is smaller, we simply pretend it to be zero bits, i.e.
     only copy the subtrahend into dest.  */
  if (b->size < min_size)
    min_size = b->size;
  for (i = 0; i < min_size; i++)
    *dstp++ = *ap++ & (~*bp++);
  /* Now fill the rest of dest from A, if B was too short.
     This makes sense only when destination and A differ.  */
  if (dst != a && i != dst_size)
    for (; i < dst_size; i++)
      *dstp++ = *ap++;
}

/* Return true if there are any bits set in A are also set in B.
   Return false otherwise.  */

bool
bitmap_intersect_p (const_sbitmap a, const_sbitmap b)
{
  bitmap_check_sizes (a, b);

  const_sbitmap_ptr ap = a->elms;
  const_sbitmap_ptr bp = b->elms;
  unsigned int i, n;

  n = MIN (a->size, b->size);
  for (i = 0; i < n; i++)
    if ((*ap++ & *bp++) != 0)
      return true;

  return false;
}

/* Set DST to be (A and B).
   Return nonzero if any change is made.  */

bool
bitmap_and (sbitmap dst, const_sbitmap a, const_sbitmap b)
{
  bitmap_check_sizes (a, b);
  bitmap_check_sizes (b, dst);

  unsigned int i, n = dst->size;
  sbitmap_ptr dstp = dst->elms;
  const_sbitmap_ptr ap = a->elms;
  const_sbitmap_ptr bp = b->elms;
  SBITMAP_ELT_TYPE changed = 0;

  for (i = 0; i < n; i++)
    {
      const SBITMAP_ELT_TYPE tmp = *ap++ & *bp++;
      SBITMAP_ELT_TYPE wordchanged = *dstp ^ tmp;
      *dstp++ = tmp;
      changed |= wordchanged;
    }
  return changed != 0;
}

/* Set DST to be (A xor B)).
   Return nonzero if any change is made.  */

bool
bitmap_xor (sbitmap dst, const_sbitmap a, const_sbitmap b)
{
  bitmap_check_sizes (a, b);
  bitmap_check_sizes (b, dst);

  unsigned int i, n = dst->size;
  sbitmap_ptr dstp = dst->elms;
  const_sbitmap_ptr ap = a->elms;
  const_sbitmap_ptr bp = b->elms;
  SBITMAP_ELT_TYPE changed = 0;

  for (i = 0; i < n; i++)
    {
      const SBITMAP_ELT_TYPE tmp = *ap++ ^ *bp++;
      SBITMAP_ELT_TYPE wordchanged = *dstp ^ tmp;
      *dstp++ = tmp;
      changed |= wordchanged;
    }
  return changed != 0;
}

/* Set DST to be (A or B)).
   Return nonzero if any change is made.  */

bool
bitmap_ior (sbitmap dst, const_sbitmap a, const_sbitmap b)
{
  bitmap_check_sizes (a, b);
  bitmap_check_sizes (b, dst);

  unsigned int i, n = dst->size;
  sbitmap_ptr dstp = dst->elms;
  const_sbitmap_ptr ap = a->elms;
  const_sbitmap_ptr bp = b->elms;
  SBITMAP_ELT_TYPE changed = 0;

  for (i = 0; i < n; i++)
    {
      const SBITMAP_ELT_TYPE tmp = *ap++ | *bp++;
      SBITMAP_ELT_TYPE wordchanged = *dstp ^ tmp;
      *dstp++ = tmp;
      changed |= wordchanged;
    }
  return changed != 0;
}

/* Return nonzero if A is a subset of B.  */

bool
bitmap_subset_p (const_sbitmap a, const_sbitmap b)
{
  bitmap_check_sizes (a, b);

  unsigned int i, n = a->size;
  const_sbitmap_ptr ap, bp;

  for (ap = a->elms, bp = b->elms, i = 0; i < n; i++, ap++, bp++)
    if ((*ap | *bp) != *bp)
      return false;

  return true;
}

/* Set DST to be (A or (B and C)).
   Return nonzero if any change is made.  */

bool
bitmap_or_and (sbitmap dst, const_sbitmap a, const_sbitmap b, const_sbitmap c)
{
  bitmap_check_sizes (a, b);
  bitmap_check_sizes (b, c);
  bitmap_check_sizes (c, dst);

  unsigned int i, n = dst->size;
  sbitmap_ptr dstp = dst->elms;
  const_sbitmap_ptr ap = a->elms;
  const_sbitmap_ptr bp = b->elms;
  const_sbitmap_ptr cp = c->elms;
  SBITMAP_ELT_TYPE changed = 0;

  for (i = 0; i < n; i++)
    {
      const SBITMAP_ELT_TYPE tmp = *ap++ | (*bp++ & *cp++);
      changed |= *dstp ^ tmp;
      *dstp++ = tmp;
    }

  return changed != 0;
}

/* Set DST to be (A and (B or C)).
   Return nonzero if any change is made.  */

bool
bitmap_and_or (sbitmap dst, const_sbitmap a, const_sbitmap b, const_sbitmap c)
{
  bitmap_check_sizes (a, b);
  bitmap_check_sizes (b, c);
  bitmap_check_sizes (c, dst);

  unsigned int i, n = dst->size;
  sbitmap_ptr dstp = dst->elms;
  const_sbitmap_ptr ap = a->elms;
  const_sbitmap_ptr bp = b->elms;
  const_sbitmap_ptr cp = c->elms;
  SBITMAP_ELT_TYPE changed = 0;

  for (i = 0; i < n; i++)
    {
      const SBITMAP_ELT_TYPE tmp = *ap++ & (*bp++ | *cp++);
      changed |= *dstp ^ tmp;
      *dstp++ = tmp;
    }

  return changed != 0;
}

/* Return number of first bit set in the bitmap, -1 if none.  */

int
bitmap_first_set_bit (const_sbitmap bmap)
{
  unsigned int n = 0;
  sbitmap_iterator sbi;

  EXECUTE_IF_SET_IN_BITMAP (bmap, 0, n, sbi)
    return n;
  return -1;
}

/* Return number of last bit set in the bitmap, -1 if none.  */

int
bitmap_last_set_bit (const_sbitmap bmap)
{
  int i;
  const SBITMAP_ELT_TYPE *const ptr = bmap->elms;

  for (i = bmap->size - 1; i >= 0; i--)
    {
      const SBITMAP_ELT_TYPE word = ptr[i];

      if (word != 0)
	{
	  unsigned int index = (i + 1) * SBITMAP_ELT_BITS - 1;
	  SBITMAP_ELT_TYPE mask
	    = (SBITMAP_ELT_TYPE) 1 << (SBITMAP_ELT_BITS - 1);

	  while (1)
	    {
	      if ((word & mask) != 0)
		return index;

	      mask >>= 1;
	      index--;
	    }
	}
    }

  return -1;
}

void
dump_bitmap (FILE *file, const_sbitmap bmap)
{
  unsigned int i, n, j;
  unsigned int set_size = bmap->size;
  unsigned int total_bits = bmap->n_bits;

  fprintf (file, "  ");
  for (i = n = 0; i < set_size && n < total_bits; i++)
    for (j = 0; j < SBITMAP_ELT_BITS && n < total_bits; j++, n++)
      {
	if (n != 0 && n % 10 == 0)
	  fprintf (file, " ");

	fprintf (file, "%d",
		 (bmap->elms[i] & ((SBITMAP_ELT_TYPE) 1 << j)) != 0);
      }

  fprintf (file, "\n");
}

DEBUG_FUNCTION void
debug_raw (simple_bitmap_def &ref)
{
  dump_bitmap (stderr, &ref);
}

DEBUG_FUNCTION void
debug_raw (simple_bitmap_def *ptr)
{
  if (ptr)
    debug_raw (*ptr);
  else
    fprintf (stderr, "<nil>\n");
}

void
dump_bitmap_file (FILE *file, const_sbitmap bmap)
{
  unsigned int i, pos;

  fprintf (file, "n_bits = %d, set = {", bmap->n_bits);

  for (pos = 30, i = 0; i < bmap->n_bits; i++)
    if (bitmap_bit_p (bmap, i))
      {
	if (pos > 70)
	  {
	    fprintf (file, "\n  ");
	    pos = 0;
	  }

	fprintf (file, "%d ", i);
	pos += 2 + (i >= 10) + (i >= 100) + (i >= 1000);
      }

  fprintf (file, "}\n");
}

DEBUG_FUNCTION void
debug_bitmap (const_sbitmap bmap)
{
  dump_bitmap_file (stderr, bmap);
}

DEBUG_FUNCTION void
debug (simple_bitmap_def &ref)
{
  dump_bitmap_file (stderr, &ref);
}

DEBUG_FUNCTION void
debug (simple_bitmap_def *ptr)
{
  if (ptr)
    debug (*ptr);
  else
    fprintf (stderr, "<nil>\n");
}

void
dump_bitmap_vector (FILE *file, const char *title, const char *subtitle,
		     sbitmap *bmaps, int n_maps)
{
  int i;

  fprintf (file, "%s\n", title);
  for (i = 0; i < n_maps; i++)
    {
      fprintf (file, "%s %d\n", subtitle, i);
      dump_bitmap (file, bmaps[i]);
    }

  fprintf (file, "\n");
}

#if CHECKING_P

namespace selftest {

/* Selftests for sbitmaps.  */

/* Checking function that uses both bitmap_bit_in_range_p and
   loop of bitmap_bit_p and verifies consistent results.  */

static bool
bitmap_bit_in_range_p_checking (sbitmap s, unsigned int start,
				unsigned end)
{
  bool r1 = bitmap_bit_in_range_p (s, start, end);
  bool r2 = false;

  for (unsigned int i = start; i <= end; i++)
    if (bitmap_bit_p (s, i))
      {
	r2 = true;
	break;
      }

  ASSERT_EQ (r1, r2);
  return r1;
}

/* Verify bitmap_set_range functions for sbitmap.  */

static void
test_set_range ()
{
  sbitmap s = sbitmap_alloc (16);
  bitmap_clear (s);

  bitmap_set_range (s, 0, 1);
  ASSERT_TRUE (bitmap_bit_in_range_p_checking (s, 0, 0));
  ASSERT_FALSE (bitmap_bit_in_range_p_checking (s, 1, 15));
  bitmap_set_range (s, 15, 1);
  ASSERT_FALSE (bitmap_bit_in_range_p_checking (s, 1, 14));
  ASSERT_TRUE (bitmap_bit_in_range_p_checking (s, 15, 15));
  sbitmap_free (s);

  s = sbitmap_alloc (1024);
  bitmap_clear (s);
  bitmap_set_range (s, 512, 1);
  ASSERT_FALSE (bitmap_bit_in_range_p_checking (s, 0, 511));
  ASSERT_FALSE (bitmap_bit_in_range_p_checking (s, 513, 1023));
  ASSERT_TRUE (bitmap_bit_in_range_p_checking (s, 512, 512));
  ASSERT_TRUE (bitmap_bit_in_range_p_checking (s, 508, 512));
  ASSERT_TRUE (bitmap_bit_in_range_p_checking (s, 508, 513));
  ASSERT_FALSE (bitmap_bit_in_range_p_checking (s, 508, 511));

  bitmap_clear (s);
  bitmap_set_range (s, 512, 64);
  ASSERT_FALSE (bitmap_bit_in_range_p_checking (s, 0, 511));
  ASSERT_FALSE (bitmap_bit_in_range_p_checking (s, 512 + 64, 1023));
  ASSERT_TRUE (bitmap_bit_in_range_p_checking (s, 512, 512));
  ASSERT_TRUE (bitmap_bit_in_range_p_checking (s, 512 + 63, 512 + 63));
  sbitmap_free (s);
}

/* Verify bitmap_bit_in_range_p functions for sbitmap.  */

static void
test_bit_in_range ()
{
  sbitmap s = sbitmap_alloc (1024);
  bitmap_clear (s);

  ASSERT_FALSE (bitmap_bit_in_range_p (s, 512, 1023));
  bitmap_set_bit (s, 100);

  ASSERT_FALSE (bitmap_bit_in_range_p (s, 512, 1023));
  ASSERT_FALSE (bitmap_bit_in_range_p (s, 0, 99));
  ASSERT_FALSE (bitmap_bit_in_range_p (s, 101, 1023));
  ASSERT_TRUE (bitmap_bit_in_range_p (s, 1, 100));
  ASSERT_TRUE (bitmap_bit_in_range_p (s, 64, 100));
  ASSERT_TRUE (bitmap_bit_in_range_p (s, 100, 100));
  ASSERT_TRUE (bitmap_bit_p (s, 100));

  sbitmap_free (s);

  s = sbitmap_alloc (64);
  bitmap_clear (s);
  bitmap_set_bit (s, 63);
  ASSERT_TRUE (bitmap_bit_in_range_p (s, 0, 63));
  ASSERT_TRUE (bitmap_bit_in_range_p (s, 1, 63));
  ASSERT_TRUE (bitmap_bit_in_range_p (s, 63, 63));
  ASSERT_TRUE (bitmap_bit_p (s, 63));
  sbitmap_free (s);

  s = sbitmap_alloc (1024);
  bitmap_clear (s);
  bitmap_set_bit (s, 128);
  ASSERT_FALSE (bitmap_bit_in_range_p (s, 0, 127));
  ASSERT_FALSE (bitmap_bit_in_range_p (s, 129, 1023));

  ASSERT_TRUE (bitmap_bit_in_range_p (s, 0, 128));
  ASSERT_TRUE (bitmap_bit_in_range_p (s, 1, 128));
  ASSERT_TRUE (bitmap_bit_in_range_p (s, 128, 255));
  ASSERT_TRUE (bitmap_bit_in_range_p (s, 128, 254));
  ASSERT_TRUE (bitmap_bit_p (s, 128));

  bitmap_clear (s);
  bitmap_set_bit (s, 8);
  ASSERT_TRUE (bitmap_bit_in_range_p (s, 0, 8));
  ASSERT_TRUE (bitmap_bit_in_range_p (s, 0, 12));
  ASSERT_TRUE (bitmap_bit_in_range_p (s, 0, 63));
  ASSERT_TRUE (bitmap_bit_in_range_p (s, 0, 127));
  ASSERT_TRUE (bitmap_bit_in_range_p (s, 0, 512));
  ASSERT_TRUE (bitmap_bit_in_range_p (s, 8, 8));
  ASSERT_TRUE (bitmap_bit_p (s, 8));

  bitmap_clear (s);
  ASSERT_FALSE (bitmap_bit_in_range_p (s, 0, 0));
  ASSERT_FALSE (bitmap_bit_in_range_p (s, 0, 8));
  ASSERT_FALSE (bitmap_bit_in_range_p (s, 0, 63));
  ASSERT_FALSE (bitmap_bit_in_range_p (s, 1, 63));
  ASSERT_FALSE (bitmap_bit_in_range_p (s, 0, 256));

  bitmap_set_bit (s, 0);
  bitmap_set_bit (s, 16);
  bitmap_set_bit (s, 32);
  bitmap_set_bit (s, 48);
  bitmap_set_bit (s, 64);
  ASSERT_TRUE (bitmap_bit_in_range_p (s, 0, 0));
  ASSERT_TRUE (bitmap_bit_in_range_p (s, 1, 16));
  ASSERT_TRUE (bitmap_bit_in_range_p (s, 48, 63));
  ASSERT_TRUE (bitmap_bit_in_range_p (s, 64, 64));
  ASSERT_FALSE (bitmap_bit_in_range_p (s, 1, 15));
  ASSERT_FALSE (bitmap_bit_in_range_p (s, 17, 31));
  ASSERT_FALSE (bitmap_bit_in_range_p (s, 49, 63));
  ASSERT_FALSE (bitmap_bit_in_range_p (s, 65, 1023));
  sbitmap_free (s);
}

/* Run all of the selftests within this file.  */

void
sbitmap_cc_tests ()
{
  test_set_range ();
  test_bit_in_range ();
}

} // namespace selftest
#endif /* CHECKING_P */
