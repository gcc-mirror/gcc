/* Functions to support general ended bitmaps.
   Copyright (C) 1997, 1998, 1999, 2000 Free Software Foundation, Inc.

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

#ifndef _BITMAP_H
#define _BITMAP_H 1

/* Number of words to use for each element in the linked list.  */

#ifndef BITMAP_ELEMENT_WORDS
#define BITMAP_ELEMENT_WORDS 2
#endif

/* Number of bits in each actual element of a bitmap.  We get slightly better
   code for bit % BITMAP_ELEMENT_ALL_BITS and bit / BITMAP_ELEMENT_ALL_BITS if
   bits is unsigned, assuming it is a power of 2.  */

#define BITMAP_ELEMENT_ALL_BITS \
  ((unsigned) (BITMAP_ELEMENT_WORDS * HOST_BITS_PER_WIDE_INT))

/* Bitmap set element.  We use a linked list to hold only the bits that
   are set.  This allows for use to grow the bitset dynamically without
   having to realloc and copy a giant bit array.  The `prev' field is
   undefined for an element on the free list.  */

typedef struct bitmap_element_def
{
  struct bitmap_element_def *next;		/* Next element. */
  struct bitmap_element_def *prev;		/* Previous element. */
  unsigned int indx;			/* regno/BITMAP_ELEMENT_ALL_BITS. */
  unsigned HOST_WIDE_INT bits[BITMAP_ELEMENT_WORDS]; /* Bits that are set. */
} bitmap_element;

/* Head of bitmap linked list.  */
typedef struct bitmap_head_def {
  bitmap_element *first;	/* First element in linked list. */
  bitmap_element *current;	/* Last element looked at. */
  unsigned int indx;		/* Index of last element looked at. */
} bitmap_head, *bitmap;

/* Enumeration giving the various operations we support.  */
enum bitmap_bits {
  BITMAP_AND,			/* TO = FROM1 & FROM2 */
  BITMAP_AND_COMPL,		/* TO = FROM1 & ~ FROM2 */
  BITMAP_IOR,			/* TO = FROM1 | FROM2 */
  BITMAP_XOR			/* TO = FROM1 ^ FROM2 */
};

/* Global data */
extern bitmap_element *bitmap_free;	/* Freelist of bitmap elements */
extern bitmap_element bitmap_zero;	/* Zero bitmap element */

/* Clear a bitmap by freeing up the linked list.  */
extern void bitmap_clear PARAMS ((bitmap));

/* Copy a bitmap to another bitmap. */
extern void bitmap_copy PARAMS ((bitmap, bitmap));

/* True if two bitmaps are identical.  */
extern int bitmap_equal_p PARAMS ((bitmap, bitmap));

/* Perform an operation on two bitmaps, yielding a third.  */
extern int bitmap_operation PARAMS ((bitmap, bitmap, bitmap, enum bitmap_bits));

/* `or' into one bitmap the `and' of a second bitmap witih the complement
   of a third.  */
extern void bitmap_ior_and_compl PARAMS ((bitmap, bitmap, bitmap));

/* Clear a single register in a register set.  */
extern void bitmap_clear_bit PARAMS ((bitmap, int));

/* Set a single register in a register set.  */
extern void bitmap_set_bit PARAMS ((bitmap, int));

/* Return true if a register is set in a register set.  */
extern int bitmap_bit_p PARAMS ((bitmap, int));

/* Debug functions to print a bitmap linked list.  */
extern void debug_bitmap PARAMS ((bitmap));
extern void debug_bitmap_file PARAMS ((FILE *, bitmap));

/* Print a bitmap */
extern void bitmap_print PARAMS ((FILE *, bitmap, const char *, const char *));

/* Initialize a bitmap header.  */
extern bitmap bitmap_initialize PARAMS ((bitmap));

/* Release all memory held by bitmaps.  */
extern void bitmap_release_memory PARAMS ((void));

extern void debug_bitmap PARAMS ((bitmap));

/* Allocate a bitmap with oballoc.  */
#define BITMAP_OBSTACK_ALLOC(OBSTACK)				\
  bitmap_initialize ((bitmap) obstack_alloc (OBSTACK, sizeof (bitmap_head)))

/* Allocate a bitmap with alloca.  */
#define BITMAP_ALLOCA()						\
  bitmap_initialize ((bitmap) alloca (sizeof (bitmap_head)))

/* Allocate a bitmap with xmalloc.  */
#define BITMAP_XMALLOC()                                        \
  bitmap_initialize ((bitmap) xmalloc (sizeof (bitmap_head)))

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

/* Loop over all bits in BITMAP, starting with MIN, setting BITNUM to the
   bit number and executing CODE for all bits that are set. */

#define EXECUTE_IF_SET_IN_BITMAP(BITMAP, MIN, BITNUM, CODE)		\
do {									\
  bitmap_element *ptr_ = (BITMAP)->first;				\
  unsigned int indx_ = (MIN) / BITMAP_ELEMENT_ALL_BITS;			\
  unsigned bit_num_ = (MIN) % ((unsigned) HOST_BITS_PER_WIDE_INT);	\
  unsigned word_num_ = (((MIN) / ((unsigned) HOST_BITS_PER_WIDE_INT))	\
			% BITMAP_ELEMENT_WORDS);			\
									\
									\
  /* Find the block the minimum bit is in.  */				\
  while (ptr_ != 0 && ptr_->indx < indx_)				\
    ptr_ = ptr_->next;							\
									\
  if (ptr_ != 0 && ptr_->indx != indx_)					\
    {									\
      bit_num_ = 0;							\
      word_num_ = 0;							\
    }									\
									\
  for (; ptr_ != 0; ptr_ = ptr_->next)					\
    {									\
      for (; word_num_ < BITMAP_ELEMENT_WORDS; word_num_++)		\
	{								\
	  unsigned HOST_WIDE_INT word_ = ptr_->bits[word_num_];		\
									\
	  if (word_ != 0)						\
	    {								\
	      for (; bit_num_ < HOST_BITS_PER_WIDE_INT; bit_num_++)	\
		{							\
		  unsigned HOST_WIDE_INT mask_				\
		    = ((unsigned HOST_WIDE_INT) 1) << bit_num_;		\
									\
		  if ((word_ & mask_) != 0)				\
		    {							\
		      word_ &= ~ mask_;					\
		      (BITNUM) = (ptr_->indx * BITMAP_ELEMENT_ALL_BITS  \
				  + word_num_ * HOST_BITS_PER_WIDE_INT  \
				  + bit_num_);				\
		      CODE;						\
									\
		      if (word_ == 0)					\
			break;						\
		    }							\
		}							\
	    }								\
									\
	  bit_num_ = 0;							\
	}								\
									\
      word_num_ = 0;							\
    }									\
} while (0)

/* Loop over all bits in BITMAP1 and BITMAP2, starting with MIN, setting
   BITNUM to the bit number and executing CODE for all bits that are set in
   the first bitmap and not set in the second. */

#define EXECUTE_IF_AND_COMPL_IN_BITMAP(BITMAP1, BITMAP2, MIN, BITNUM, CODE) \
do {									\
  bitmap_element *ptr1_ = (BITMAP1)->first;				\
  bitmap_element *ptr2_ = (BITMAP2)->first;				\
  unsigned int indx_ = (MIN) / BITMAP_ELEMENT_ALL_BITS;			\
  unsigned bit_num_ = (MIN) % ((unsigned) HOST_BITS_PER_WIDE_INT);	\
  unsigned word_num_ = (((MIN) / ((unsigned) HOST_BITS_PER_WIDE_INT))	\
			% BITMAP_ELEMENT_WORDS);			\
									\
  /* Find the block the minimum bit is in in the first bitmap.  */	\
  while (ptr1_ != 0 && ptr1_->indx < indx_)				\
    ptr1_ = ptr1_->next;						\
									\
  if (ptr1_ != 0 && ptr1_->indx != indx_)				\
    {									\
      bit_num_ = 0;							\
      word_num_ = 0;							\
    }									\
									\
  for (; ptr1_ != 0 ; ptr1_ = ptr1_->next)				\
    {									\
      /* Advance BITMAP2 to the equivalent link, using an all		\
	 zero element if an equivalent link doesn't exist.  */		\
      bitmap_element *tmp2_;						\
									\
      while (ptr2_ != 0 && ptr2_->indx < ptr1_->indx)			\
	ptr2_ = ptr2_->next;						\
									\
      tmp2_ = ((ptr2_ != 0 && ptr2_->indx == ptr1_->indx)		\
	       ? ptr2_ : &bitmap_zero); 				\
									\
      for (; word_num_ < BITMAP_ELEMENT_WORDS; word_num_++)		\
	{								\
	  unsigned HOST_WIDE_INT word_ = (ptr1_->bits[word_num_]	\
					  & ~ tmp2_->bits[word_num_]);	\
	  if (word_ != 0)						\
	    {								\
	      for (; bit_num_ < HOST_BITS_PER_WIDE_INT; bit_num_++)	\
		{							\
		  unsigned HOST_WIDE_INT mask_				\
		    = ((unsigned HOST_WIDE_INT)1) << bit_num_;		\
									\
		  if ((word_ & mask_) != 0)				\
		    {							\
		      word_ &= ~ mask_;					\
		      (BITNUM) = (ptr1_->indx * BITMAP_ELEMENT_ALL_BITS \
				  + word_num_ * HOST_BITS_PER_WIDE_INT  \
				  + bit_num_);				\
									\
		      CODE;						\
		      if (word_ == 0)					\
			break;						\
		    }							\
		}							\
	    }								\
									\
	  bit_num_ = 0;							\
	}								\
									\
      word_num_ = 0;							\
    }									\
} while (0)

/* Loop over all bits in BITMAP1 and BITMAP2, starting with MIN, setting
   BITNUM to the bit number and executing CODE for all bits that are set in
   the both bitmaps. */

#define EXECUTE_IF_AND_IN_BITMAP(BITMAP1, BITMAP2, MIN, BITNUM, CODE)	\
do {									\
  bitmap_element *ptr1_ = (BITMAP1)->first;				\
  bitmap_element *ptr2_ = (BITMAP2)->first;				\
  unsigned int indx_ = (MIN) / BITMAP_ELEMENT_ALL_BITS;			\
  unsigned bit_num_ = (MIN) % ((unsigned) HOST_BITS_PER_WIDE_INT);	\
  unsigned word_num_ = (((MIN) / ((unsigned) HOST_BITS_PER_WIDE_INT))	\
			% BITMAP_ELEMENT_WORDS);			\
									\
  /* Find the block the minimum bit is in in the first bitmap.  */	\
  while (ptr1_ != 0 && ptr1_->indx < indx_)				\
    ptr1_ = ptr1_->next;						\
									\
  if (ptr1_ != 0 && ptr1_->indx != indx_)				\
    {									\
      bit_num_ = 0;							\
      word_num_ = 0;							\
    }									\
									\
  for (; ptr1_ != 0 ; ptr1_ = ptr1_->next)				\
    {									\
      /* Advance BITMAP2 to the equivalent link */			\
      while (ptr2_ != 0 && ptr2_->indx < ptr1_->indx)			\
	ptr2_ = ptr2_->next;						\
									\
      if (ptr2_ == 0)							\
	{								\
	  /* If there are no more elements in BITMAP2, exit loop now.*/	\
	  ptr1_ = (bitmap_element *)0;					\
	  break;							\
	}								\
      else if (ptr2_->indx > ptr1_->indx)				\
	{								\
	  bit_num_ = word_num_ = 0;					\
	  continue;							\
	}								\
									\
      for (; word_num_ < BITMAP_ELEMENT_WORDS; word_num_++)		\
	{								\
	  unsigned HOST_WIDE_INT word_ = (ptr1_->bits[word_num_]	\
					  & ptr2_->bits[word_num_]);	\
	  if (word_ != 0)						\
	    {								\
	      for (; bit_num_ < HOST_BITS_PER_WIDE_INT; bit_num_++)	\
		{							\
		  unsigned HOST_WIDE_INT mask_				\
		    = ((unsigned HOST_WIDE_INT)1) << bit_num_;		\
									\
		  if ((word_ & mask_) != 0)				\
		    {							\
		      word_ &= ~ mask_;					\
		      (BITNUM) = (ptr1_->indx * BITMAP_ELEMENT_ALL_BITS \
				  + word_num_ * HOST_BITS_PER_WIDE_INT  \
				  + bit_num_);				\
									\
		      CODE;						\
		      if (word_ == 0)					\
			break;						\
		    }							\
		}							\
	    }								\
									\
	  bit_num_ = 0;							\
	}								\
									\
      word_num_ = 0;							\
    }									\
} while (0)

#endif /* _BITMAP_H */
