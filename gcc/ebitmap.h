/* Sparse array based bitmaps.
   Copyright (C) 2007, 2008, 2009 Free Software Foundation, Inc.

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

#ifndef GCC_EBITMAP_H
#define GCC_EBITMAP_H

#include "sbitmap.h"

#define EBITMAP_ELT_BITS ((unsigned) HOST_BITS_PER_WIDEST_FAST_INT)
#define EBITMAP_ELT_TYPE unsigned HOST_WIDEST_FAST_INT

typedef struct ebitmap_def
{  
  unsigned int n_elts;		/* number of elements in the array.  */
  sbitmap wordmask;		/* wordmask saying which words are
				   nonzero.  */
  unsigned int numwords;	/* number of nonzero words.  */
  unsigned int cacheindex;	/* which word cache is.  */
  EBITMAP_ELT_TYPE *elts;	/* nonzero element array.  */
  EBITMAP_ELT_TYPE *cache;	/* last tested element, or NULL.  */
} *ebitmap;


#define ebitmap_empty_p(MAP) ((MAP)->numwords == 0)
#define ebitmap_free(MAP)  (free((MAP)->elts), \
			    sbitmap_free ((MAP)->wordmask),	\
			    free((MAP)))

extern void ebitmap_set_bit (ebitmap, unsigned int);
extern void ebitmap_clear_bit (ebitmap, unsigned int);
extern bool ebitmap_bit_p (ebitmap, unsigned int);
extern void dump_ebitmap (FILE *, ebitmap);
extern void dump_ebitmap_file (FILE *, ebitmap);
extern void dump_ebitmap_vector (FILE *, const char *, const char *, ebitmap *,
				 int);
extern ebitmap ebitmap_alloc (unsigned int);
extern ebitmap *ebitmap_vector_alloc (unsigned int, unsigned int);
extern void ebitmap_copy (ebitmap, ebitmap);
extern void ebitmap_and (ebitmap, ebitmap, ebitmap);
extern void ebitmap_and_into (ebitmap, ebitmap);
extern bool ebitmap_and_compl (ebitmap, ebitmap, ebitmap);
extern bool ebitmap_and_compl_into (ebitmap, ebitmap);
extern bool ebitmap_ior_into (ebitmap, ebitmap);
extern bool ebitmap_ior (ebitmap, ebitmap, ebitmap);
extern bool ebitmap_ior_and_compl (ebitmap, ebitmap, ebitmap, ebitmap);
extern bool ebitmap_ior_and_compl_into (ebitmap, ebitmap, ebitmap);
extern bool ebitmap_equal_p (ebitmap, ebitmap);
extern void ebitmap_clear (ebitmap);
extern int ebitmap_last_set_bit (ebitmap);
extern void debug_ebitmap (ebitmap);
extern unsigned long ebitmap_popcount(ebitmap, unsigned long);

/* The iterator for ebitmap.  */
typedef struct {
  /* The pointer to the first word of the bitmap.  */
  EBITMAP_ELT_TYPE *ptr;

  /* Element number inside ptr that we are at.  */
  unsigned int eltnum;

  /* The size of the bitmap.  */
  unsigned int size;

  /* Current bit index.  */
  unsigned int bit_num;

  /* The words currently visited.  */
  EBITMAP_ELT_TYPE word;

  /* The word mask iterator.  */
  sbitmap_iterator maskiter;
} ebitmap_iterator;
  
static inline void
ebitmap_iter_init (ebitmap_iterator *i, ebitmap bmp, unsigned int min)
{
  sbitmap_iter_init (&i->maskiter, bmp->wordmask, 
		     min / EBITMAP_ELT_BITS);
  i->size = bmp->numwords;
  if (i->size == 0)
    {
      i->ptr = NULL;
      i->eltnum = 0;
      i->bit_num = 0;
      i->word = 0;
      return;
    }
  i->ptr = bmp->elts;
  i->bit_num = min;
  i->eltnum = 0;

  if ((min / EBITMAP_ELT_BITS) >= bmp->wordmask->n_bits)
    {
      i->word = 0;
    }
  else
    {
      if (TEST_BIT (bmp->wordmask, min / EBITMAP_ELT_BITS) == 0)
	i->word = 0;
      else
	{
	  unsigned int wordindex = min / EBITMAP_ELT_BITS;
	  unsigned int count = sbitmap_popcount (bmp->wordmask, wordindex);
	  i->word = i->ptr[count] >> (i->bit_num % (unsigned int)EBITMAP_ELT_BITS);
	  i->eltnum = count + 1;
	}
    }
}

static inline bool
ebitmap_iter_cond (ebitmap_iterator *i, unsigned int *n)
{
  unsigned int ourn = 0;

  if (i->size == 0)
    return false;

  if (i->word == 0)
    {
      sbitmap_iter_next (&i->maskiter);
      if (!sbitmap_iter_cond (&i->maskiter, &ourn))
	return false;
      i->bit_num = ourn * EBITMAP_ELT_BITS;
      i->word = i->ptr[i->eltnum++];
    }

  /* Skip bits that are zero.  */

  for (; (i->word & 1) == 0; i->word >>= 1)
    i->bit_num++;

  *n = i->bit_num;
  return true;
}

static inline void
ebitmap_iter_next (ebitmap_iterator *i)
{
  i->word >>= 1;
  i->bit_num++;
}

/* Loop over all elements of EBITMAP, starting with MIN.  In each
   iteration, N is set to the index of the bit being visited.  ITER is
   an instance of ebitmap_iterator used to iterate the bitmap.  */

#define EXECUTE_IF_SET_IN_EBITMAP(EBITMAP, MIN, N, ITER)	\
  for (ebitmap_iter_init (&(ITER), (EBITMAP), (MIN));		\
       ebitmap_iter_cond (&(ITER), &(N));			\
       ebitmap_iter_next (&(ITER)))


#endif /* ! GCC_EBITMAP_H */
