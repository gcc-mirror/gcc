/* Simple bitmaps.
   Copyright (C) 1999 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "rtl.h"
#include "flags.h"
#include "basic-block.h"

/* Bitmap manipulation routines.  */

/* Allocate a simple bitmap of N_ELMS bits.  */

sbitmap
sbitmap_alloc (n_elms)
     int n_elms;
{
  int bytes, size, amt;
  sbitmap bmap;

  size = SBITMAP_SET_SIZE (n_elms);
  bytes = size * sizeof (SBITMAP_ELT_TYPE);
  amt = (sizeof (struct simple_bitmap_def)
	 + bytes - sizeof (SBITMAP_ELT_TYPE));
  bmap = (sbitmap) xmalloc (amt);
  bmap->n_bits = n_elms;
  bmap->size = size;
  bmap->bytes = bytes;
  return bmap;
}

/* Allocate a vector of N_VECS bitmaps of N_ELMS bits.  */

sbitmap *
sbitmap_vector_alloc (n_vecs, n_elms)
     int n_vecs, n_elms;
{
  int i, bytes, offset, elm_bytes, size, amt, vector_bytes;
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

  for (i = 0, offset = vector_bytes;
       i < n_vecs;
       i++, offset += elm_bytes)
    {
      sbitmap b = (sbitmap) ((char *) bitmap_vector + offset);
      bitmap_vector[i] = b;
      b->n_bits = n_elms;
      b->size = size;
      b->bytes = bytes;
    }

  return bitmap_vector;
}

/* Copy sbitmap SRC to DST.  */

void
sbitmap_copy (dst, src)
     sbitmap dst, src;
{
  bcopy ((PTR) src->elms, (PTR) dst->elms,
	 sizeof (SBITMAP_ELT_TYPE) * dst->size);
}

/* Zero all elements in a bitmap.  */

void
sbitmap_zero (bmap)
     sbitmap bmap;
{
  bzero ((char *) bmap->elms, bmap->bytes);
}

/* Set to ones all elements in a bitmap.  */

void
sbitmap_ones (bmap)
     sbitmap bmap;
{
  memset (bmap->elms, -1, bmap->bytes);
}

/* Zero a vector of N_VECS bitmaps.  */

void
sbitmap_vector_zero (bmap, n_vecs)
     sbitmap *bmap;
     int n_vecs;
{
  int i;

  for (i = 0; i < n_vecs; i++)
    sbitmap_zero (bmap[i]);
}

/* Set to ones a vector of N_VECS bitmaps.  */

void
sbitmap_vector_ones (bmap, n_vecs)
     sbitmap *bmap;
     int n_vecs;
{
  int i;

  for (i = 0; i < n_vecs; i++)
    sbitmap_ones (bmap[i]);
}

/* Set DST to be A union (B - C).
   DST = A | (B & ~C).
   Return non-zero if any change is made.  */

int
sbitmap_union_of_diff (dst, a, b, c)
     sbitmap dst, a, b, c;
{
  int i,changed;
  sbitmap_ptr dstp, ap, bp, cp;

  changed = 0;
  dstp = dst->elms;
  ap = a->elms;
  bp = b->elms;
  cp = c->elms;
  for (i = 0; i < dst->size; i++)
    {
      SBITMAP_ELT_TYPE tmp = *ap | (*bp & ~*cp);
      if (*dstp != tmp)
	changed = 1;
      *dstp = tmp;
      dstp++; ap++; bp++; cp++;
    }
  return changed;
}

/* Set bitmap DST to the bitwise negation of the bitmap SRC.  */

void
sbitmap_not (dst, src)
     sbitmap dst, src;
{
  int i;
  sbitmap_ptr dstp, ap;

  dstp = dst->elms;
  ap = src->elms;
  for (i = 0; i < dst->size; i++)
    {
      SBITMAP_ELT_TYPE tmp = ~(*ap);
      *dstp = tmp;
      dstp++; ap++;
    }
}

/* Set the bits in DST to be the difference between the bits
   in A and the bits in B. i.e. dst = a - b.
   The - operator is implemented as a & (~b).  */

void
sbitmap_difference (dst, a, b)
     sbitmap dst, a, b;
{
  int i;
  sbitmap_ptr dstp, ap, bp;

  dstp = dst->elms;
  ap = a->elms;
  bp = b->elms;
  for (i = 0; i < dst->size; i++)
    *dstp++ = *ap++ & (~*bp++);
}

/* Set DST to be (A and B)).
   Return non-zero if any change is made.  */

int
sbitmap_a_and_b (dst, a, b)
     sbitmap dst, a, b;
{
  int i,changed;
  sbitmap_ptr dstp, ap, bp;

  changed = 0;
  dstp = dst->elms;
  ap = a->elms;
  bp = b->elms;
  for (i = 0; i < dst->size; i++)
    {
      SBITMAP_ELT_TYPE tmp = *ap & *bp;
      if (*dstp != tmp)
	changed = 1;
      *dstp = tmp;
      dstp++; ap++; bp++;
    }
  return changed;
}
/* Set DST to be (A or B)).
   Return non-zero if any change is made.  */

int
sbitmap_a_or_b (dst, a, b)
     sbitmap dst, a, b;
{
  int i,changed;
  sbitmap_ptr dstp, ap, bp;

  changed = 0;
  dstp = dst->elms;
  ap = a->elms;
  bp = b->elms;
  for (i = 0; i < dst->size; i++)
    {
      SBITMAP_ELT_TYPE tmp = *ap | *bp;
      if (*dstp != tmp)
	changed = 1;
      *dstp = tmp;
      dstp++; ap++; bp++;
    }
  return changed;
}

/* Set DST to be (A or (B and C)).
   Return non-zero if any change is made.  */

int
sbitmap_a_or_b_and_c (dst, a, b, c)
     sbitmap dst, a, b, c;
{
  int i,changed;
  sbitmap_ptr dstp, ap, bp, cp;

  changed = 0;
  dstp = dst->elms;
  ap = a->elms;
  bp = b->elms;
  cp = c->elms;
  for (i = 0; i < dst->size; i++)
    {
      SBITMAP_ELT_TYPE tmp = *ap | (*bp & *cp);
      if (*dstp != tmp)
	changed = 1;
      *dstp = tmp;
      dstp++; ap++; bp++; cp++;
    }
  return changed;
}

/* Set DST to be (A ann (B or C)).
   Return non-zero if any change is made.  */

int
sbitmap_a_and_b_or_c (dst, a, b, c)
     sbitmap dst, a, b, c;
{
  int i,changed;
  sbitmap_ptr dstp, ap, bp, cp;

  changed = 0;
  dstp = dst->elms;
  ap = a->elms;
  bp = b->elms;
  cp = c->elms;
  for (i = 0; i < dst->size; i++)
    {
      SBITMAP_ELT_TYPE tmp = *ap & (*bp | *cp);
      if (*dstp != tmp)
	changed = 1;
      *dstp = tmp;
      dstp++; ap++; bp++; cp++;
    }
  return changed;
}

/* Set the bitmap DST to the intersection of SRC of all predecessors or
   successors of block number BB (PRED_SUCC says which).  */

void
sbitmap_intersect_of_predsucc (dst, src, bb, pred_succ)
     sbitmap dst;
     sbitmap *src;
     int bb;
     int_list_ptr *pred_succ;
{
  int_list_ptr ps;
  int ps_bb;
  int set_size = dst->size;

  ps = pred_succ[bb];

  /* It is possible that there are no predecessors(/successors).
     This can happen for example in unreachable code.  */

  if (ps == NULL)
    {
      /* In APL-speak this is the `and' reduction of the empty set and thus
	 the result is the identity for `and'.  */
      sbitmap_ones (dst);
      return;
    }

  /* Set result to first predecessor/successor.  */

  for ( ; ps != NULL; ps = ps->next)
    {
      ps_bb = INT_LIST_VAL (ps);
      if (ps_bb == ENTRY_BLOCK || ps_bb == EXIT_BLOCK)
	continue;
      sbitmap_copy (dst, src[ps_bb]);
      /* Break out since we're only doing first predecessor.  */
      break;
    }
  if (ps == NULL)
    return;

  /* Now do the remaining predecessors/successors.  */

  for (ps = ps->next; ps != NULL; ps = ps->next)
    {
      int i;
      sbitmap_ptr p,r;

      ps_bb = INT_LIST_VAL (ps);
      if (ps_bb == ENTRY_BLOCK || ps_bb == EXIT_BLOCK)
	continue;

      p = src[ps_bb]->elms;
      r = dst->elms;

      for (i = 0; i < set_size; i++)
	*r++ &= *p++;
    }
}

/* Set the bitmap DST to the union of SRC of all predecessors/successors of
   block number BB.  */

void
sbitmap_union_of_predsucc (dst, src, bb, pred_succ)
     sbitmap dst;
     sbitmap *src;
     int bb;
     int_list_ptr *pred_succ;
{
  int_list_ptr ps;
  int ps_bb;
  int set_size = dst->size;

  ps = pred_succ[bb];

  /* It is possible that there are no predecessors(/successors).
     This can happen for example in unreachable code.  */

  if (ps == NULL)
    {
      /* In APL-speak this is the `or' reduction of the empty set and thus
	 the result is the identity for `or'.  */
      sbitmap_zero (dst);
      return;
    }

  /* Set result to first predecessor/successor.  */

  for ( ; ps != NULL; ps = ps->next)
    {
      ps_bb = INT_LIST_VAL (ps);
      if (ps_bb == ENTRY_BLOCK || ps_bb == EXIT_BLOCK)
	continue;
      sbitmap_copy (dst, src[ps_bb]);
      /* Break out since we're only doing first predecessor.  */
      break;
    }
  if (ps == NULL)
    return;

  /* Now do the remaining predecessors/successors.  */

  for (ps = ps->next; ps != NULL; ps = ps->next)
    {
      int i;
      sbitmap_ptr p,r;

      ps_bb = INT_LIST_VAL (ps);
      if (ps_bb == ENTRY_BLOCK || ps_bb == EXIT_BLOCK)
	continue;

      p = src[ps_bb]->elms;
      r = dst->elms;

      for (i = 0; i < set_size; i++)
	*r++ |= *p++;
    }
}

void
dump_sbitmap (file, bmap)
     FILE *file;
     sbitmap bmap;
{
  int i,j,n;
  int set_size = bmap->size;
  int total_bits = bmap->n_bits;

  fprintf (file, "  ");
  for (i = n = 0; i < set_size && n < total_bits; i++)
    {
      for (j = 0; j < SBITMAP_ELT_BITS && n < total_bits; j++, n++)
	{
	  if (n != 0 && n % 10 == 0)
	    fprintf (file, " ");
	  fprintf (file, "%d", (bmap->elms[i] & (1L << j)) != 0);
	}
    }
  fprintf (file, "\n");
}

void
dump_sbitmap_vector (file, title, subtitle, bmaps, n_maps)
     FILE *file;
     const char *title, *subtitle;
     sbitmap *bmaps;
     int n_maps;
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
