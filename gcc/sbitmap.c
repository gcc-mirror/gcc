/* Simple bitmaps.
   Copyright (C) 1999-2014 Free Software Foundation, Inc.

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

/* This suffices for roughly 99% of the hosts we run on, and the rest
   don't have 256 bit integers.  */
#if SBITMAP_ELT_BITS > 255
#error Need to increase size of datatype used for popcount
#endif

#if GCC_VERSION >= 3400
#  if SBITMAP_ELT_BITS == HOST_BITS_PER_LONG
#    define do_popcount(x) __builtin_popcountl (x)
#  elif SBITMAP_ELT_BITS == HOST_BITS_PER_LONGLONG
#    define do_popcount(x) __builtin_popcountll (x)
#  else
#    error "internal error: sbitmap.h and hwint.h are inconsistent"
#  endif
#else
static unsigned long sbitmap_elt_popcount (SBITMAP_ELT_TYPE);
#  define do_popcount(x) sbitmap_elt_popcount (x)
#endif

typedef SBITMAP_ELT_TYPE *sbitmap_ptr;
typedef const SBITMAP_ELT_TYPE *const_sbitmap_ptr;

/* Return the size in bytes of a bitmap MAP.  */

static inline unsigned int sbitmap_size_bytes (const_sbitmap map)
{
   return map->size * sizeof (SBITMAP_ELT_TYPE);
}

/* This macro controls debugging that is as expensive as the
   operations it verifies.  */

/* #define BITMAP_DEBUGGING  */
#ifdef BITMAP_DEBUGGING

/* Verify the population count of sbitmap A matches the cached value,
   if there is a cached value. */

static void
sbitmap_verify_popcount (const_sbitmap a)
{
  unsigned ix;
  unsigned int lastword;

  if (!a->popcount)
    return;

  lastword = a->size;
  for (ix = 0; ix < lastword; ix++)
    gcc_assert (a->popcount[ix] == do_popcount (a->elms[ix]));
}
#endif

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
  bmap->popcount = NULL;
  return bmap;
}

/* Allocate a simple bitmap of N_ELMS bits, and a popcount array.  */

sbitmap
sbitmap_alloc_with_popcount (unsigned int n_elms)
{
  sbitmap const bmap = sbitmap_alloc (n_elms);
  bmap->popcount = XNEWVEC (unsigned char, bmap->size);
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
      if (bmap->popcount)
	bmap->popcount = XRESIZEVEC (unsigned char, bmap->popcount, size);
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
	{
	  memset (bmap->elms + bmap->size, 0,
		  bytes - sbitmap_size_bytes (bmap));
	  if (bmap->popcount)
	    memset (bmap->popcount + bmap->size, 0,
		    (size * sizeof (unsigned char))
		    - (bmap->size * sizeof (unsigned char)));

	}
    }
  else if (n_elms < bmap->n_bits)
    {
      /* Clear the surplus bits in the last word.  */
      last_bit = n_elms % SBITMAP_ELT_BITS;
      if (last_bit)
	{
	  bmap->elms[size - 1]
	    &= (SBITMAP_ELT_TYPE)-1 >> (SBITMAP_ELT_BITS - last_bit);
	  if (bmap->popcount)
	    bmap->popcount[size - 1] = do_popcount (bmap->elms[size - 1]);
	}
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
  unsigned int i, bytes, offset, elm_bytes, size, amt, vector_bytes;
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
      b->popcount = NULL;
    }

  return bitmap_vector;
}

/* Copy sbitmap SRC to DST.  */

void
bitmap_copy (sbitmap dst, const_sbitmap src)
{
  memcpy (dst->elms, src->elms, sizeof (SBITMAP_ELT_TYPE) * dst->size);
  if (dst->popcount)
    memcpy (dst->popcount, src->popcount, sizeof (unsigned char) * dst->size);
}

/* Determine if a == b.  */
int
bitmap_equal_p (const_sbitmap a, const_sbitmap b)
{
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


/* Zero all elements in a bitmap.  */

void
bitmap_clear (sbitmap bmap)
{
  memset (bmap->elms, 0, sbitmap_size_bytes (bmap));
  if (bmap->popcount)
    memset (bmap->popcount, 0, bmap->size * sizeof (unsigned char));
}

/* Set all elements in a bitmap to ones.  */

void
bitmap_ones (sbitmap bmap)
{
  unsigned int last_bit;

  memset (bmap->elms, -1, sbitmap_size_bytes (bmap));
  if (bmap->popcount)
    memset (bmap->popcount, -1, bmap->size * sizeof (unsigned char));

  last_bit = bmap->n_bits % SBITMAP_ELT_BITS;
  if (last_bit)
    {
      bmap->elms[bmap->size - 1]
	= (SBITMAP_ELT_TYPE)-1 >> (SBITMAP_ELT_BITS - last_bit);
      if (bmap->popcount)
	bmap->popcount[bmap->size - 1]
	  = do_popcount (bmap->elms[bmap->size - 1]);
    }
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
  unsigned int i, n = dst->size;
  sbitmap_ptr dstp = dst->elms;
  const_sbitmap_ptr ap = a->elms;
  const_sbitmap_ptr bp = b->elms;
  const_sbitmap_ptr cp = c->elms;
  SBITMAP_ELT_TYPE changed = 0;

  gcc_assert (!dst->popcount);

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
  unsigned int i, n = dst->size;
  sbitmap_ptr dstp = dst->elms;
  const_sbitmap_ptr srcp = src->elms;
  unsigned int last_bit;

  gcc_assert (!dst->popcount);

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
  unsigned int i, dst_size = dst->size;
  unsigned int min_size = dst->size;
  sbitmap_ptr dstp = dst->elms;
  const_sbitmap_ptr ap = a->elms;
  const_sbitmap_ptr bp = b->elms;

  gcc_assert (!dst->popcount);

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
  unsigned int i, n = dst->size;
  sbitmap_ptr dstp = dst->elms;
  const_sbitmap_ptr ap = a->elms;
  const_sbitmap_ptr bp = b->elms;
  bool has_popcount = dst->popcount != NULL;
  unsigned char *popcountp = dst->popcount;
  SBITMAP_ELT_TYPE changed = 0;

  for (i = 0; i < n; i++)
    {
      const SBITMAP_ELT_TYPE tmp = *ap++ & *bp++;
      SBITMAP_ELT_TYPE wordchanged = *dstp ^ tmp;
      if (has_popcount)
	{
	  if (wordchanged)
	    *popcountp = do_popcount (tmp);
	  popcountp++;
	}
      *dstp++ = tmp;
      changed |= wordchanged;
    }
#ifdef BITMAP_DEBUGGING
  if (has_popcount)
    sbitmap_verify_popcount (dst);
#endif
  return changed != 0;
}

/* Set DST to be (A xor B)).
   Return nonzero if any change is made.  */

bool
bitmap_xor (sbitmap dst, const_sbitmap a, const_sbitmap b)
{
  unsigned int i, n = dst->size;
  sbitmap_ptr dstp = dst->elms;
  const_sbitmap_ptr ap = a->elms;
  const_sbitmap_ptr bp = b->elms;
  bool has_popcount = dst->popcount != NULL;
  unsigned char *popcountp = dst->popcount;
  SBITMAP_ELT_TYPE changed = 0;

  for (i = 0; i < n; i++)
    {
      const SBITMAP_ELT_TYPE tmp = *ap++ ^ *bp++;
      SBITMAP_ELT_TYPE wordchanged = *dstp ^ tmp;
      if (has_popcount)
	{
	  if (wordchanged)
	    *popcountp = do_popcount (tmp);
	  popcountp++;
	}
      *dstp++ = tmp;
      changed |= wordchanged;
    }
#ifdef BITMAP_DEBUGGING
  if (has_popcount)
    sbitmap_verify_popcount (dst);
#endif
  return changed != 0;
}

/* Set DST to be (A or B)).
   Return nonzero if any change is made.  */

bool
bitmap_ior (sbitmap dst, const_sbitmap a, const_sbitmap b)
{
  unsigned int i, n = dst->size;
  sbitmap_ptr dstp = dst->elms;
  const_sbitmap_ptr ap = a->elms;
  const_sbitmap_ptr bp = b->elms;
  bool has_popcount = dst->popcount != NULL;
  unsigned char *popcountp = dst->popcount;
  SBITMAP_ELT_TYPE changed = 0;

  for (i = 0; i < n; i++)
    {
      const SBITMAP_ELT_TYPE tmp = *ap++ | *bp++;
      SBITMAP_ELT_TYPE wordchanged = *dstp ^ tmp;
      if (has_popcount)
	{
	  if (wordchanged)
	    *popcountp = do_popcount (tmp);
	  popcountp++;
	}
      *dstp++ = tmp;
      changed |= wordchanged;
    }
#ifdef BITMAP_DEBUGGING
  if (has_popcount)
    sbitmap_verify_popcount (dst);
#endif
  return changed != 0;
}

/* Return nonzero if A is a subset of B.  */

bool
bitmap_subset_p (const_sbitmap a, const_sbitmap b)
{
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
  unsigned int i, n = dst->size;
  sbitmap_ptr dstp = dst->elms;
  const_sbitmap_ptr ap = a->elms;
  const_sbitmap_ptr bp = b->elms;
  const_sbitmap_ptr cp = c->elms;
  SBITMAP_ELT_TYPE changed = 0;

  gcc_assert (!dst->popcount);

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
  unsigned int i, n = dst->size;
  sbitmap_ptr dstp = dst->elms;
  const_sbitmap_ptr ap = a->elms;
  const_sbitmap_ptr bp = b->elms;
  const_sbitmap_ptr cp = c->elms;
  SBITMAP_ELT_TYPE changed = 0;

  gcc_assert (!dst->popcount);

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

/* Count the bits in an SBITMAP element A.  */

static unsigned long
sbitmap_elt_popcount (SBITMAP_ELT_TYPE a)
{
  unsigned long ret = 0;
  unsigned i;

  if (a == 0)
    return 0;

  /* Just do this the table way for now  */
  for (i = 0; i < SBITMAP_ELT_BITS; i += 8)
    ret += popcount_table[(a >> i) & 0xff];
  return ret;
}
#endif
