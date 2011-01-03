/* Simple bitmaps.
   Copyright (C) 1999, 2000, 2002, 2003, 2004, 2006, 2007, 2008, 2010
   Free Software Foundation, Inc.

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

#ifdef IN_GCC
/* FIXME: sbitmap is just a data structure, but we define dataflow functions
   here also.  This is conditional on IN_GCC (see second #ifdef IN_GCC
   further down).
   For now, also only conditionally include basic-block.h, but we should
   find a better place for the dataflow functions.  Perhaps cfganal.c?  */
#include "basic-block.h"
#endif

#if GCC_VERSION >= 3400
#  if HOST_BITS_PER_WIDEST_FAST_INT == HOST_BITS_PER_LONG
#    define do_popcount(x) __builtin_popcountl(x)
#  elif HOST_BITS_PER_WIDEST_FAST_INT == HOST_BITS_PER_LONGLONG
#    define do_popcount(x) __builtin_popcountll(x)
#  else
#    error "internal error: sbitmap.h and hwint.h are inconsistent"
#  endif
#else
static unsigned long sbitmap_elt_popcount (SBITMAP_ELT_TYPE);
#  define do_popcount(x) sbitmap_elt_popcount((x))
#endif

typedef SBITMAP_ELT_TYPE *sbitmap_ptr;
typedef const SBITMAP_ELT_TYPE *const_sbitmap_ptr;

/* This macro controls debugging that is as expensive as the
   operations it verifies.  */

/* #define BITMAP_DEBUGGING  */
#ifdef BITMAP_DEBUGGING

/* Verify the population count of sbitmap A matches the cached value,
   if there is a cached value. */

void
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
  if (bytes > SBITMAP_SIZE_BYTES (bmap))
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
		  bytes - SBITMAP_SIZE_BYTES (bmap));

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
		  bytes - SBITMAP_SIZE_BYTES (bmap));
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

  if (SBITMAP_SIZE_BYTES (src)  >= bytes)
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
sbitmap_copy (sbitmap dst, const_sbitmap src)
{
  memcpy (dst->elms, src->elms, sizeof (SBITMAP_ELT_TYPE) * dst->size);
  if (dst->popcount)
    memcpy (dst->popcount, src->popcount, sizeof (unsigned char) * dst->size);
}

/* Copy the first N elements of sbitmap SRC to DST.  */

void
sbitmap_copy_n (sbitmap dst, const_sbitmap src, unsigned int n)
{
  memcpy (dst->elms, src->elms, sizeof (SBITMAP_ELT_TYPE) * n);
  if (dst->popcount)
    memcpy (dst->popcount, src->popcount, sizeof (unsigned char) * n);
}

/* Determine if a == b.  */
int
sbitmap_equal (const_sbitmap a, const_sbitmap b)
{
  return !memcmp (a->elms, b->elms, sizeof (SBITMAP_ELT_TYPE) * a->size);
}

/* Return true if the bitmap is empty.  */

bool
sbitmap_empty_p (const_sbitmap bmap)
{
  unsigned int i;
  for (i=0; i<bmap->size; i++)
    if (bmap->elms[i])
      return false;

  return true;
}

/* Return false if any of the N bits are set in MAP starting at
   START.  */

bool
sbitmap_range_empty_p (const_sbitmap bmap, unsigned int start, unsigned int n)
{
  unsigned int i = start / SBITMAP_ELT_BITS;
  SBITMAP_ELT_TYPE elm;
  unsigned int shift = start % SBITMAP_ELT_BITS;

  gcc_assert (bmap->n_bits >= start + n);

  elm = bmap->elms[i];
  elm = elm >> shift;

  if (shift + n <= SBITMAP_ELT_BITS)
    {
      /* The bits are totally contained in a single element.  */
      if (shift + n < SBITMAP_ELT_BITS)
        elm &= ((1 << n) - 1);
      return (elm == 0);
    }

  if (elm)
    return false;

  n -= SBITMAP_ELT_BITS - shift;
  i++;

  /* Deal with full elts.  */
  while (n >= SBITMAP_ELT_BITS)
    {
      if (bmap->elms[i])
	return false;
      i++;
      n -= SBITMAP_ELT_BITS;
    }

  /* The leftover bits.  */
  if (n)
    {
      elm = bmap->elms[i];
      elm &= ((1 << n) - 1);
      return (elm == 0);
    }

  return true;
}



/* Zero all elements in a bitmap.  */

void
sbitmap_zero (sbitmap bmap)
{
  memset (bmap->elms, 0, SBITMAP_SIZE_BYTES (bmap));
  if (bmap->popcount)
    memset (bmap->popcount, 0, bmap->size * sizeof (unsigned char));
}

/* Set all elements in a bitmap to ones.  */

void
sbitmap_ones (sbitmap bmap)
{
  unsigned int last_bit;

  memset (bmap->elms, -1, SBITMAP_SIZE_BYTES (bmap));
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
sbitmap_vector_zero (sbitmap *bmap, unsigned int n_vecs)
{
  unsigned int i;

  for (i = 0; i < n_vecs; i++)
    sbitmap_zero (bmap[i]);
}

/* Set a vector of N_VECS bitmaps to ones.  */

void
sbitmap_vector_ones (sbitmap *bmap, unsigned int n_vecs)
{
  unsigned int i;

  for (i = 0; i < n_vecs; i++)
    sbitmap_ones (bmap[i]);
}

/* Set DST to be A union (B - C).
   DST = A | (B & ~C).
   Returns true if any change is made.  */

bool
sbitmap_union_of_diff_cg (sbitmap dst, const_sbitmap a, const_sbitmap b, const_sbitmap c)
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

void
sbitmap_union_of_diff (sbitmap dst, const_sbitmap a, const_sbitmap b, const_sbitmap c)
{
  unsigned int i, n = dst->size;
  sbitmap_ptr dstp = dst->elms;
  const_sbitmap_ptr ap = a->elms;
  const_sbitmap_ptr bp = b->elms;
  const_sbitmap_ptr cp = c->elms;

  gcc_assert (!dst->popcount && !a->popcount
	      && !b->popcount && !c->popcount);

  for (i = 0; i < n; i++)
    *dstp++ = *ap++ | (*bp++ & ~*cp++);
}

/* Set bitmap DST to the bitwise negation of the bitmap SRC.  */

void
sbitmap_not (sbitmap dst, const_sbitmap src)
{
  unsigned int i, n = dst->size;
  sbitmap_ptr dstp = dst->elms;
  const_sbitmap_ptr srcp = src->elms;
  unsigned int last_bit;

  gcc_assert (!dst->popcount);

  for (i = 0; i < n; i++)
    *dstp++ = ~*srcp++;

  /* Zero all bits past n_bits, by ANDing dst with sbitmap_ones.  */
  last_bit = src->n_bits % SBITMAP_ELT_BITS;
  if (last_bit)
    dst->elms[n-1] = dst->elms[n-1]
      & ((SBITMAP_ELT_TYPE)-1 >> (SBITMAP_ELT_BITS - last_bit));
}

/* Set the bits in DST to be the difference between the bits
   in A and the bits in B. i.e. dst = a & (~b).  */

void
sbitmap_difference (sbitmap dst, const_sbitmap a, const_sbitmap b)
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
sbitmap_any_common_bits (const_sbitmap a, const_sbitmap b)
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
sbitmap_a_and_b_cg (sbitmap dst, const_sbitmap a, const_sbitmap b)
{
  unsigned int i, n = dst->size;
  sbitmap_ptr dstp = dst->elms;
  const_sbitmap_ptr ap = a->elms;
  const_sbitmap_ptr bp = b->elms;
  SBITMAP_ELT_TYPE changed = 0;

  gcc_assert (!dst->popcount);

  for (i = 0; i < n; i++)
    {
      const SBITMAP_ELT_TYPE tmp = *ap++ & *bp++;
      changed |= *dstp ^ tmp;
      *dstp++ = tmp;
    }

  return changed != 0;
}

void
sbitmap_a_and_b (sbitmap dst, const_sbitmap a, const_sbitmap b)
{
  unsigned int i, n = dst->size;
  sbitmap_ptr dstp = dst->elms;
  const_sbitmap_ptr ap = a->elms;
  const_sbitmap_ptr bp = b->elms;
  bool has_popcount = dst->popcount != NULL;
  unsigned char *popcountp = dst->popcount;

  for (i = 0; i < n; i++)
    {
      const SBITMAP_ELT_TYPE tmp = *ap++ & *bp++;
      if (has_popcount)
	{
	  bool wordchanged = (*dstp ^ tmp) != 0;
	  if (wordchanged)
	    *popcountp = do_popcount (tmp);
	  popcountp++;
	}
      *dstp++ = tmp;
    }
#ifdef BITMAP_DEBUGGING
  if (has_popcount)
    sbitmap_verify_popcount (dst);
#endif
}

/* Set DST to be (A xor B)).
   Return nonzero if any change is made.  */

bool
sbitmap_a_xor_b_cg (sbitmap dst, const_sbitmap a, const_sbitmap b)
{
  unsigned int i, n = dst->size;
  sbitmap_ptr dstp = dst->elms;
  const_sbitmap_ptr ap = a->elms;
  const_sbitmap_ptr bp = b->elms;
  SBITMAP_ELT_TYPE changed = 0;

  gcc_assert (!dst->popcount);

  for (i = 0; i < n; i++)
    {
      const SBITMAP_ELT_TYPE tmp = *ap++ ^ *bp++;
      changed |= *dstp ^ tmp;
      *dstp++ = tmp;
    }

  return changed != 0;
}

void
sbitmap_a_xor_b (sbitmap dst, const_sbitmap a, const_sbitmap b)
{
  unsigned int i, n = dst->size;
  sbitmap_ptr dstp = dst->elms;
  const_sbitmap_ptr ap = a->elms;
  const_sbitmap_ptr bp = b->elms;
  bool has_popcount = dst->popcount != NULL;
  unsigned char *popcountp = dst->popcount;

  for (i = 0; i < n; i++)
    {
      const SBITMAP_ELT_TYPE tmp = *ap++ ^ *bp++;
      if (has_popcount)
	{
	  bool wordchanged = (*dstp ^ tmp) != 0;
	  if (wordchanged)
	    *popcountp = do_popcount (tmp);
	  popcountp++;
	}
      *dstp++ = tmp;
    }
#ifdef BITMAP_DEBUGGING
  if (has_popcount)
    sbitmap_verify_popcount (dst);
#endif
}

/* Set DST to be (A or B)).
   Return nonzero if any change is made.  */

bool
sbitmap_a_or_b_cg (sbitmap dst, const_sbitmap a, const_sbitmap b)
{
  unsigned int i, n = dst->size;
  sbitmap_ptr dstp = dst->elms;
  const_sbitmap_ptr ap = a->elms;
  const_sbitmap_ptr bp = b->elms;
  SBITMAP_ELT_TYPE changed = 0;

  gcc_assert (!dst->popcount);

  for (i = 0; i < n; i++)
    {
      const SBITMAP_ELT_TYPE tmp = *ap++ | *bp++;
      changed |= *dstp ^ tmp;
      *dstp++ = tmp;
    }

  return changed != 0;
}

void
sbitmap_a_or_b (sbitmap dst, const_sbitmap a, const_sbitmap b)
{
  unsigned int i, n = dst->size;
  sbitmap_ptr dstp = dst->elms;
  const_sbitmap_ptr ap = a->elms;
  const_sbitmap_ptr bp = b->elms;
  bool has_popcount = dst->popcount != NULL;
  unsigned char *popcountp = dst->popcount;

  for (i = 0; i < n; i++)
    {
      const SBITMAP_ELT_TYPE tmp = *ap++ | *bp++;
      if (has_popcount)
	{
	  bool wordchanged = (*dstp ^ tmp) != 0;
	  if (wordchanged)
	    *popcountp = do_popcount (tmp);
	  popcountp++;
	}
      *dstp++ = tmp;
    }
#ifdef BITMAP_DEBUGGING
  if (has_popcount)
    sbitmap_verify_popcount (dst);
#endif
}

/* Return nonzero if A is a subset of B.  */

bool
sbitmap_a_subset_b_p (const_sbitmap a, const_sbitmap b)
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
sbitmap_a_or_b_and_c_cg (sbitmap dst, const_sbitmap a, const_sbitmap b, const_sbitmap c)
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

void
sbitmap_a_or_b_and_c (sbitmap dst, const_sbitmap a, const_sbitmap b, const_sbitmap c)
{
  unsigned int i, n = dst->size;
  sbitmap_ptr dstp = dst->elms;
  const_sbitmap_ptr ap = a->elms;
  const_sbitmap_ptr bp = b->elms;
  const_sbitmap_ptr cp = c->elms;

  gcc_assert (!dst->popcount);

  for (i = 0; i < n; i++)
    *dstp++ = *ap++ | (*bp++ & *cp++);
}

/* Set DST to be (A and (B or C)).
   Return nonzero if any change is made.  */

bool
sbitmap_a_and_b_or_c_cg (sbitmap dst, const_sbitmap a, const_sbitmap b, const_sbitmap c)
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

void
sbitmap_a_and_b_or_c (sbitmap dst, const_sbitmap a, const_sbitmap b, const_sbitmap c)
{
  unsigned int i, n = dst->size;
  sbitmap_ptr dstp = dst->elms;
  const_sbitmap_ptr ap = a->elms;
  const_sbitmap_ptr bp = b->elms;
  const_sbitmap_ptr cp = c->elms;

  for (i = 0; i < n; i++)
    *dstp++ = *ap++ & (*bp++ | *cp++);
}

#ifdef IN_GCC
/* FIXME: depends on basic-block.h, see comment at start of this file.

   Ironically, the comments before the functions below suggest they do
   dataflow using the "new flow graph structures", but that's the *old*
   new data structures.  The functions receive basic block numbers and
   use BASIC_BLOCK(idx) to get the basic block.  They should receive
   the basic block directly,  *sigh*.  */

/* Set the bitmap DST to the intersection of SRC of successors of
   block number BB, using the new flow graph structures.  */

void
sbitmap_intersection_of_succs (sbitmap dst, sbitmap *src, int bb)
{
  basic_block b = BASIC_BLOCK (bb);
  unsigned int set_size = dst->size;
  edge e;
  unsigned ix;

  gcc_assert (!dst->popcount);

  for (e = NULL, ix = 0; ix < EDGE_COUNT (b->succs); ix++)
    {
      e = EDGE_SUCC (b, ix);
      if (e->dest == EXIT_BLOCK_PTR)
	continue;

      sbitmap_copy (dst, src[e->dest->index]);
      break;
    }

  if (e == 0)
    sbitmap_ones (dst);
  else
    for (++ix; ix < EDGE_COUNT (b->succs); ix++)
      {
	unsigned int i;
	sbitmap_ptr p, r;

	e = EDGE_SUCC (b, ix);
	if (e->dest == EXIT_BLOCK_PTR)
	  continue;

	p = src[e->dest->index]->elms;
	r = dst->elms;
	for (i = 0; i < set_size; i++)
	  *r++ &= *p++;
      }
}

/* Set the bitmap DST to the intersection of SRC of predecessors of
   block number BB, using the new flow graph structures.  */

void
sbitmap_intersection_of_preds (sbitmap dst, sbitmap *src, int bb)
{
  basic_block b = BASIC_BLOCK (bb);
  unsigned int set_size = dst->size;
  edge e;
  unsigned ix;

  gcc_assert (!dst->popcount);

  for (e = NULL, ix = 0; ix < EDGE_COUNT (b->preds); ix++)
    {
      e = EDGE_PRED (b, ix);
      if (e->src == ENTRY_BLOCK_PTR)
	continue;

      sbitmap_copy (dst, src[e->src->index]);
      break;
    }

  if (e == 0)
    sbitmap_ones (dst);
  else
    for (++ix; ix < EDGE_COUNT (b->preds); ix++)
      {
	unsigned int i;
	sbitmap_ptr p, r;

	e = EDGE_PRED (b, ix);
	if (e->src == ENTRY_BLOCK_PTR)
	  continue;

	p = src[e->src->index]->elms;
	r = dst->elms;
	for (i = 0; i < set_size; i++)
	  *r++ &= *p++;
      }
}

/* Set the bitmap DST to the union of SRC of successors of
   block number BB, using the new flow graph structures.  */

void
sbitmap_union_of_succs (sbitmap dst, sbitmap *src, int bb)
{
  basic_block b = BASIC_BLOCK (bb);
  unsigned int set_size = dst->size;
  edge e;
  unsigned ix;

  gcc_assert (!dst->popcount);

  for (ix = 0; ix < EDGE_COUNT (b->succs); ix++)
    {
      e = EDGE_SUCC (b, ix);
      if (e->dest == EXIT_BLOCK_PTR)
	continue;

      sbitmap_copy (dst, src[e->dest->index]);
      break;
    }

  if (ix == EDGE_COUNT (b->succs))
    sbitmap_zero (dst);
  else
    for (ix++; ix < EDGE_COUNT (b->succs); ix++)
      {
	unsigned int i;
	sbitmap_ptr p, r;

	e = EDGE_SUCC (b, ix);
	if (e->dest == EXIT_BLOCK_PTR)
	  continue;

	p = src[e->dest->index]->elms;
	r = dst->elms;
	for (i = 0; i < set_size; i++)
	  *r++ |= *p++;
      }
}

/* Set the bitmap DST to the union of SRC of predecessors of
   block number BB, using the new flow graph structures.  */

void
sbitmap_union_of_preds (sbitmap dst, sbitmap *src, int bb)
{
  basic_block b = BASIC_BLOCK (bb);
  unsigned int set_size = dst->size;
  edge e;
  unsigned ix;

  gcc_assert (!dst->popcount);

  for (ix = 0; ix < EDGE_COUNT (b->preds); ix++)
    {
      e = EDGE_PRED (b, ix);
      if (e->src== ENTRY_BLOCK_PTR)
	continue;

      sbitmap_copy (dst, src[e->src->index]);
      break;
    }

  if (ix == EDGE_COUNT (b->preds))
    sbitmap_zero (dst);
  else
    for (ix++; ix < EDGE_COUNT (b->preds); ix++)
      {
	unsigned int i;
	sbitmap_ptr p, r;

	e = EDGE_PRED (b, ix);
	if (e->src == ENTRY_BLOCK_PTR)
	  continue;

	p = src[e->src->index]->elms;
	r = dst->elms;
	for (i = 0; i < set_size; i++)
	  *r++ |= *p++;
      }
}
#endif

/* Return number of first bit set in the bitmap, -1 if none.  */

int
sbitmap_first_set_bit (const_sbitmap bmap)
{
  unsigned int n = 0;
  sbitmap_iterator sbi;

  EXECUTE_IF_SET_IN_SBITMAP (bmap, 0, n, sbi)
    return n;
  return -1;
}

/* Return number of last bit set in the bitmap, -1 if none.  */

int
sbitmap_last_set_bit (const_sbitmap bmap)
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
dump_sbitmap (FILE *file, const_sbitmap bmap)
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

void
dump_sbitmap_file (FILE *file, const_sbitmap bmap)
{
  unsigned int i, pos;

  fprintf (file, "n_bits = %d, set = {", bmap->n_bits);

  for (pos = 30, i = 0; i < bmap->n_bits; i++)
    if (TEST_BIT (bmap, i))
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
debug_sbitmap (const_sbitmap bmap)
{
  dump_sbitmap_file (stderr, bmap);
}

void
dump_sbitmap_vector (FILE *file, const char *title, const char *subtitle,
		     sbitmap *bmaps, int n_maps)
{
  int bb;

  fprintf (file, "%s\n", title);
  for (bb = 0; bb < n_maps; bb++)
    {
      fprintf (file, "%s %d\n", subtitle, bb);
      dump_sbitmap (file, bmaps[bb]);
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

/* Count the number of bits in SBITMAP a, up to bit MAXBIT.  */

unsigned long
sbitmap_popcount (const_sbitmap a, unsigned long maxbit)
{
  unsigned long count = 0;
  unsigned ix;
  unsigned int lastword;

  if (maxbit == 0)
    return 0;

  if (maxbit >= a->n_bits)
    maxbit = a->n_bits;

  /* Count the bits in the full word.  */
  lastword = MIN (a->size, SBITMAP_SET_SIZE (maxbit + 1) - 1);
  for (ix = 0; ix < lastword; ix++)
    {
      if (a->popcount)
	{
	  count += a->popcount[ix];
#ifdef BITMAP_DEBUGGING
	  gcc_assert (a->popcount[ix] == do_popcount (a->elms[ix]));
#endif
	}
      else
	count += do_popcount (a->elms[ix]);
    }

  /* Count the remaining bits.  */
  if (lastword < a->size)
    {
      unsigned int bitindex;
      SBITMAP_ELT_TYPE theword = a->elms[lastword];

      bitindex = maxbit % SBITMAP_ELT_BITS;
      if (bitindex != 0)
	{
	  theword &= (SBITMAP_ELT_TYPE)-1 >> (SBITMAP_ELT_BITS - bitindex);
	  count += do_popcount (theword);
	}
    }
  return count;
}

