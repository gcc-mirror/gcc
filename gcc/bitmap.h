/* Functions to support general ended bitmaps.
   Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002, 2003
   Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

#ifndef GCC_BITMAP_H
#define GCC_BITMAP_H

/* Fundamental storage type for bitmap.  */

/* typedef unsigned HOST_WIDE_INT BITMAP_WORD; */
/* #define nBITMAP_WORD_BITS HOST_BITS_PER_WIDE_INT */
typedef unsigned long BITMAP_WORD;
#define nBITMAP_WORD_BITS (CHAR_BIT * SIZEOF_LONG)
#define BITMAP_WORD_BITS (unsigned) nBITMAP_WORD_BITS

/* Number of words to use for each element in the linked list.  */

#ifndef BITMAP_ELEMENT_WORDS
#define BITMAP_ELEMENT_WORDS ((128 + nBITMAP_WORD_BITS - 1) / nBITMAP_WORD_BITS)
#endif

/* Number of bits in each actual element of a bitmap.  We get slightly better
   code for bit % BITMAP_ELEMENT_ALL_BITS and bit / BITMAP_ELEMENT_ALL_BITS if
   bits is unsigned, assuming it is a power of 2.  */

#define BITMAP_ELEMENT_ALL_BITS \
  ((unsigned) (BITMAP_ELEMENT_WORDS * BITMAP_WORD_BITS))

/* Bitmap set element.  We use a linked list to hold only the bits that
   are set.  This allows for use to grow the bitset dynamically without
   having to realloc and copy a giant bit array.  The `prev' field is
   undefined for an element on the free list.  */

typedef struct bitmap_element_def GTY(())
{
  struct bitmap_element_def *next;		/* Next element.  */
  struct bitmap_element_def *prev;		/* Previous element.  */
  unsigned int indx;			/* regno/BITMAP_ELEMENT_ALL_BITS.  */
  BITMAP_WORD bits[BITMAP_ELEMENT_WORDS]; /* Bits that are set.  */
} bitmap_element;

/* Head of bitmap linked list.  */
typedef struct bitmap_head_def GTY(()) {
  bitmap_element *first;	/* First element in linked list.  */
  bitmap_element *current;	/* Last element looked at.  */
  unsigned int indx;		/* Index of last element looked at.  */
  int using_obstack;		/* Are we using an obstack or ggc for
                                   allocation?  */
} bitmap_head;
typedef struct bitmap_head_def *bitmap;

/* Enumeration giving the various operations we support.  */
enum bitmap_bits {
  BITMAP_AND,			/* TO = FROM1 & FROM2 */
  BITMAP_AND_COMPL,		/* TO = FROM1 & ~ FROM2 */
  BITMAP_IOR,			/* TO = FROM1 | FROM2 */
  BITMAP_XOR,			/* TO = FROM1 ^ FROM2 */
  BITMAP_IOR_COMPL			/* TO = FROM1 | ~FROM2 */
};

/* Global data */
extern bitmap_element bitmap_zero_bits;	/* Zero bitmap element */

/* Clear a bitmap by freeing up the linked list.  */
extern void bitmap_clear (bitmap);

/* Copy a bitmap to another bitmap.  */
extern void bitmap_copy (bitmap, bitmap);

/* True if two bitmaps are identical.  */
extern int bitmap_equal_p (bitmap, bitmap);

/* Perform an operation on two bitmaps, yielding a third.  */
extern int bitmap_operation (bitmap, bitmap, bitmap, enum bitmap_bits);

/* `or' into one bitmap the `and' of a second bitmap witih the complement
   of a third.  */
extern void bitmap_ior_and_compl (bitmap, bitmap, bitmap);

/* Clear a single register in a register set.  */
extern void bitmap_clear_bit (bitmap, int);

/* Set a single register in a register set.  */
extern void bitmap_set_bit (bitmap, int);

/* Return true if a register is set in a register set.  */
extern int bitmap_bit_p (bitmap, int);

/* Debug functions to print a bitmap linked list.  */
extern void debug_bitmap (bitmap);
extern void debug_bitmap_file (FILE *, bitmap);

/* Print a bitmap.  */
extern void bitmap_print (FILE *, bitmap, const char *, const char *);

/* Initialize a bitmap header.  If HEAD is NULL, a new header will be
   allocated.  USING_OBSTACK indicates how elements should be allocated.  */
extern bitmap bitmap_initialize (bitmap head, int using_obstack);

/* Release all memory used by the bitmap obstack.  */
extern void bitmap_release_memory (void);

/* A few compatibility/functions macros for compatibility with sbitmaps */
#define dump_bitmap(file, bitmap) bitmap_print (file, bitmap, "", "\n")
#define bitmap_zero(a) bitmap_clear (a)
#define bitmap_a_or_b(a,b,c) bitmap_operation (a, b, c, BITMAP_IOR)
#define bitmap_a_and_b(a,b,c) bitmap_operation (a, b, c, BITMAP_AND)
extern int bitmap_union_of_diff (bitmap, bitmap, bitmap, bitmap);
extern int bitmap_first_set_bit (bitmap);
extern int bitmap_last_set_bit (bitmap);

/* Allocate a bitmap with oballoc.  */
#define BITMAP_OBSTACK_ALLOC(OBSTACK)				\
  bitmap_initialize (obstack_alloc (OBSTACK, sizeof (bitmap_head)), 1)

/* Allocate a bitmap with ggc_alloc.  */
#define BITMAP_GGC_ALLOC()			\
  bitmap_initialize (NULL, 0)

/* Allocate a bitmap with xmalloc.  */
#define BITMAP_XMALLOC()                                        \
  bitmap_initialize (xmalloc (sizeof (bitmap_head)), 1)

/* Do any cleanup needed on a bitmap when it is no longer used.  */
#define BITMAP_FREE(BITMAP)			\
do {						\
  if (BITMAP)					\
    {						\
      bitmap_clear (BITMAP);			\
      (BITMAP) = 0;				\
    }						\
} while (0)

/* Do any cleanup needed on an xmalloced bitmap when it is no longer used.  */
#define BITMAP_XFREE(BITMAP)			\
do {						\
  if (BITMAP)					\
    {						\
      bitmap_clear (BITMAP);			\
      free (BITMAP);				\
      (BITMAP) = 0;				\
    }						\
} while (0)

/* Do any one-time initializations needed for bitmaps.  */
#define BITMAP_INIT_ONCE()

/* Iterator for bitmaps.  */

typedef struct
{
  /* Actual elements in the bitmaps.  */
  bitmap_element *ptr1, *ptr2;

  /* Position of an actual word in the elements.  */
  unsigned word;

  /* Position of a bit corresponding to the start of word.  */
  unsigned word_bit;

  /* Position of the actual bit.  */
  unsigned bit;

  /* Contents of the actually processed word.  When finding next bit
     it is shifted right, so that the actual bit is always the least
     significant bit of ACTUAL.  */
  BITMAP_WORD actual;
} bitmap_iterator;

/* Moves the iterator BI to the first set bit on or after the current
   position in bitmap and returns the bit if available.  The bit is
   found in ACTUAL field only.  */

static inline unsigned
bmp_iter_common_next_1 (bitmap_iterator *bi)
{
  while (!(bi->actual & 1))
    {
      bi->actual >>= 1;
      bi->bit++;
    }

  return bi->bit;
}

/* Moves the iterator BI to the first set bit on or after the current
   position in bitmap and returns the bit if available.  */

static inline unsigned
bmp_iter_single_next_1 (bitmap_iterator *bi)
{
  if (bi->actual)
    return bmp_iter_common_next_1 (bi);

  bi->word++;
  bi->word_bit += BITMAP_WORD_BITS;

  while (1)
    {
      for (;
	   bi->word < BITMAP_ELEMENT_WORDS;
	   bi->word++, bi->word_bit += BITMAP_WORD_BITS)
	{
	  bi->actual = bi->ptr1->bits[bi->word];
	  if (bi->actual)
	    {
	      bi->bit = bi->word_bit;
	      return bmp_iter_common_next_1 (bi);
	    }
	}

      bi->ptr1 = bi->ptr1->next;
      if (!bi->ptr1)
	return 0;

      bi->word = 0;
      bi->word_bit = bi->ptr1->indx * BITMAP_ELEMENT_ALL_BITS;
    }
}

/* Initializes a bitmap iterator BI for looping over bits of bitmap
   BMP, starting with bit MIN.  Returns the first bit of BMP greater
   or equal to MIN if there is any.  */

static inline unsigned
bmp_iter_single_init (bitmap_iterator *bi, bitmap bmp, unsigned min)
{
  unsigned indx = min / BITMAP_ELEMENT_ALL_BITS;

  for (bi->ptr1 = bmp->first;
       bi->ptr1 && bi->ptr1->indx < indx;
       bi->ptr1 = bi->ptr1->next)
    continue;

  if (!bi->ptr1)
    {
      /* To avoid warnings.  */
      bi->word = 0;
      bi->bit = 0;
      bi->word_bit = 0;
      bi->actual = 0;
      bi->ptr2 = NULL;
      return 0;
    }

  if (bi->ptr1->indx == indx)
    {
      unsigned bit_in_elt = min - BITMAP_ELEMENT_ALL_BITS * indx;
      unsigned word_in_elt = bit_in_elt / BITMAP_WORD_BITS;
      unsigned bit_in_word = bit_in_elt % BITMAP_WORD_BITS;

      bi->word = word_in_elt;
      bi->word_bit = min - bit_in_word;
      bi->bit = min;
      bi->actual = bi->ptr1->bits[word_in_elt] >> bit_in_elt;
    }
  else
    {
      bi->word = 0;
      bi->bit = bi->ptr1->indx * BITMAP_ELEMENT_ALL_BITS;
      bi->word_bit = bi->bit;
      bi->actual = bi->ptr1->bits[0];
    }

  return bmp_iter_single_next_1 (bi);
}

/* Returns true if all elements of the bitmap referred to by iterator BI
   were processed.  */

static inline bool
bmp_iter_end_p (bitmap_iterator bi)
{
  return bi.ptr1 == NULL;
}

/* Moves the iterator BI to the next bit of bitmap and returns the bit
   if available.  */

static inline unsigned
bmp_iter_single_next (bitmap_iterator *bi)
{
  bi->bit++;
  bi->actual >>= 1;
  return bmp_iter_single_next_1 (bi);
}

/* Loop over all bits in BITMAP, starting with MIN and setting BITNUM to
   the bit number.  ITER is a bitmap iterator.  */

#define EXECUTE_IF_SET_IN_BITMAP(BITMAP, MIN, BITNUM, ITER)		\
  for ((BITNUM) = bmp_iter_single_init (&(ITER), (BITMAP), (MIN));	\
       !bmp_iter_end_p (ITER);					\
       (BITNUM) = bmp_iter_single_next (&(ITER)))

/* Moves the iterator BI to the first set bit on or after the current
   position in difference of bitmaps and returns the bit if available.  */

static inline unsigned
bmp_iter_and_not_next_1 (bitmap_iterator *bi)
{
  if (bi->actual)
    return bmp_iter_common_next_1 (bi);

  bi->word++;
  bi->word_bit += BITMAP_WORD_BITS;

  while (1)
    {
      bitmap_element *snd;

      if (bi->ptr2 && bi->ptr2->indx == bi->ptr1->indx)
	snd = bi->ptr2;
      else
	snd = &bitmap_zero_bits;

      for (;
	   bi->word < BITMAP_ELEMENT_WORDS;
	   bi->word++, bi->word_bit += BITMAP_WORD_BITS)
	{
	  bi->actual = (bi->ptr1->bits[bi->word]
			& ~snd->bits[bi->word]);
	  if (bi->actual)
	    {
	      bi->bit = bi->word_bit;
	      return bmp_iter_common_next_1 (bi);
	    }
	}

      bi->ptr1 = bi->ptr1->next;
      if (!bi->ptr1)
	return 0;

      while (bi->ptr2
	     && bi->ptr2->indx < bi->ptr1->indx)
	bi->ptr2 = bi->ptr2->next;

      bi->word = 0;
      bi->word_bit = bi->ptr1->indx * BITMAP_ELEMENT_ALL_BITS;
    }
}

/* Initializes a bitmap iterator BI for looping over bits of bitmap
   BMP1 &~ BMP2, starting with bit MIN.  Returns the first bit of
   BMP1 &~ BMP2 greater or equal to MIN if there is any.  */

static inline unsigned
bmp_iter_and_not_init (bitmap_iterator *bi, bitmap bmp1, bitmap bmp2,
		       unsigned min)
{
  unsigned indx = min / BITMAP_ELEMENT_ALL_BITS;

  for (bi->ptr1 = bmp1->first;
       bi->ptr1 && bi->ptr1->indx < indx;
       bi->ptr1 = bi->ptr1->next)
    continue;

  if (!bi->ptr1)
    {
      /* To avoid warnings.  */
      bi->word = 0;
      bi->bit = 0;
      bi->word_bit = 0;
      bi->actual = 0;
      bi->ptr2 = NULL;
      return 0;
    }

  for (bi->ptr2 = bmp2->first;
       bi->ptr2 && bi->ptr2->indx < bi->ptr1->indx;
       bi->ptr2 = bi->ptr2->next)
    continue;

  if (bi->ptr1->indx == indx)
    {
      unsigned bit_in_elt = min - BITMAP_ELEMENT_ALL_BITS * indx;
      unsigned word_in_elt = bit_in_elt / BITMAP_WORD_BITS;
      unsigned bit_in_word = bit_in_elt % BITMAP_WORD_BITS;

      bi->word = word_in_elt;
      bi->word_bit = min - bit_in_word;
      bi->bit = min;

      if (bi->ptr2 && bi->ptr2->indx == indx)
	bi->actual = (bi->ptr1->bits[word_in_elt]
		      & ~bi->ptr2->bits[word_in_elt]) >> bit_in_elt;
      else
	bi->actual = bi->ptr1->bits[word_in_elt] >> bit_in_elt;
    }
  else
    {
      bi->word = 0;
      bi->bit = bi->ptr1->indx * BITMAP_ELEMENT_ALL_BITS;
      bi->word_bit = bi->bit;

      if (bi->ptr2 && bi->ptr2->indx == bi->ptr1->indx)
	bi->actual = (bi->ptr1->bits[0] & ~bi->ptr2->bits[0]);
      else
	bi->actual = bi->ptr1->bits[0];
    }

  return bmp_iter_and_not_next_1 (bi);
}

/* Moves the iterator BI to the next bit of difference of bitmaps and returns
   the bit if available.  */

static inline unsigned
bmp_iter_and_not_next (bitmap_iterator *bi)
{
  bi->bit++;
  bi->actual >>= 1;
  return bmp_iter_and_not_next_1 (bi);
}

/* Loop over all bits in BMP1 and BMP2, starting with MIN, setting
   BITNUM to the bit number for all bits that are set in the first bitmap
   and not set in the second.  ITER is a bitmap iterator.  */

#define EXECUTE_IF_AND_COMPL_IN_BITMAP(BMP1, BMP2, MIN, BITNUM, ITER)	\
  for ((BITNUM) = bmp_iter_and_not_init (&(ITER), (BMP1), (BMP2), (MIN)); \
       !bmp_iter_end_p (ITER);						\
       (BITNUM) = bmp_iter_and_not_next (&(ITER)))

/* Moves the iterator BI to the first set bit on or after the current
   position in intersection of bitmaps and returns the bit if available.  */

static inline unsigned
bmp_iter_and_next_1 (bitmap_iterator *bi)
{
  if (bi->actual)
    return bmp_iter_common_next_1 (bi);

  bi->word++;
  bi->word_bit += BITMAP_WORD_BITS;

  while (1)
    {
      for (;
	   bi->word < BITMAP_ELEMENT_WORDS;
	   bi->word++, bi->word_bit += BITMAP_WORD_BITS)
	{
	  bi->actual = (bi->ptr1->bits[bi->word]
			& bi->ptr2->bits[bi->word]);
	  if (bi->actual)
	    {
	      bi->bit = bi->word_bit;
	      return bmp_iter_common_next_1 (bi);
	    }
	}

      do
	{
	  bi->ptr1 = bi->ptr1->next;
	  if (!bi->ptr1)
	    return 0;

	  while (bi->ptr2->indx < bi->ptr1->indx)
	    {
	      bi->ptr2 = bi->ptr2->next;
	      if (!bi->ptr2)
		{
		  bi->ptr1 = NULL;
		  return 0;
		}
	    }
	}
      while (bi->ptr1->indx != bi->ptr2->indx);

      bi->word = 0;
      bi->word_bit = bi->ptr1->indx * BITMAP_ELEMENT_ALL_BITS;
    }
}

/* Initializes a bitmap iterator BI for looping over bits of bitmap
   BMP1 & BMP2, starting with bit MIN.  Returns the first bit of
   BMP1 & BMP2 greater or equal to MIN if there is any.  */

static inline unsigned
bmp_iter_and_init (bitmap_iterator *bi, bitmap bmp1, bitmap bmp2,
		       unsigned min)
{
  unsigned indx = min / BITMAP_ELEMENT_ALL_BITS;

  for (bi->ptr1 = bmp1->first;
       bi->ptr1 && bi->ptr1->indx < indx;
       bi->ptr1 = bi->ptr1->next)
    continue;

  if (!bi->ptr1)
    goto empty;

  bi->ptr2 = bmp2->first;
  if (!bi->ptr2)
    goto empty;

  while (1)
    {
      while (bi->ptr2->indx < bi->ptr1->indx)
	{
	  bi->ptr2 = bi->ptr2->next;
	  if (!bi->ptr2)
	    goto empty;
	}

      if (bi->ptr1->indx == bi->ptr2->indx)
	break;

      bi->ptr1 = bi->ptr1->next;
      if (!bi->ptr1)
	goto empty;
    }

  if (bi->ptr1->indx == indx)
    {
      unsigned bit_in_elt = min - BITMAP_ELEMENT_ALL_BITS * indx;
      unsigned word_in_elt = bit_in_elt / BITMAP_WORD_BITS;
      unsigned bit_in_word = bit_in_elt % BITMAP_WORD_BITS;

      bi->word = word_in_elt;
      bi->word_bit = min - bit_in_word;
      bi->bit = min;

      bi->actual = (bi->ptr1->bits[word_in_elt]
		    & bi->ptr2->bits[word_in_elt]) >> bit_in_elt;
    }
  else
    {
      bi->word = 0;
      bi->bit = bi->ptr1->indx * BITMAP_ELEMENT_ALL_BITS;
      bi->word_bit = bi->bit;

      bi->actual = (bi->ptr1->bits[0] & bi->ptr2->bits[0]);
    }

  return bmp_iter_and_next_1 (bi);

empty:
  /* To avoid warnings.  */
  bi->word = 0;
  bi->bit = 0;
  bi->word_bit = 0;
  bi->actual = 0;
  bi->ptr1 = NULL;
  bi->ptr2 = NULL;
  return 0;
}

/* Moves the iterator BI to the next bit of intersection of bitmaps and returns
   the bit if available.  */

static inline unsigned
bmp_iter_and_next (bitmap_iterator *bi)
{
  bi->bit++;
  bi->actual >>= 1;
  return bmp_iter_and_next_1 (bi);
}

/* Loop over all bits in BMP1 and BMP2, starting with MIN, setting
   BITNUM to the bit number for all bits that are set in both bitmaps.
   ITER is a bitmap iterator.  */

#define EXECUTE_IF_AND_IN_BITMAP(BMP1, BMP2, MIN, BITNUM, ITER)		\
  for ((BITNUM) = bmp_iter_and_init (&(ITER), (BMP1), (BMP2), (MIN));	\
       !bmp_iter_end_p (ITER);						\
       (BITNUM) = bmp_iter_and_next (&(ITER)))

#endif /* GCC_BITMAP_H */
