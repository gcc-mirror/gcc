/* Simple bitmaps.
   Copyright (C) 1999, 2000, 2002, 2003 Free Software Foundation, Inc.

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

#ifndef GCC_SBITMAP_H
#define GCC_SBITMAP_H 

/* It's not clear yet whether using bitmap.[ch] will be a win.
   It should be straightforward to convert so for now we keep things simple
   while more important issues are dealt with.  */

#define SBITMAP_ELT_BITS ((unsigned) HOST_BITS_PER_WIDE_INT)
#define SBITMAP_ELT_TYPE unsigned HOST_WIDE_INT

typedef struct simple_bitmap_def
{
  unsigned int n_bits;		/* Number of bits.  */
  unsigned int size;		/* Size in elements.  */
  unsigned int bytes;		/* Size in bytes.  */
  SBITMAP_ELT_TYPE elms[1];	/* The elements.  */
} *sbitmap;

typedef SBITMAP_ELT_TYPE *sbitmap_ptr;

/* Return the set size needed for N elements.  */
#define SBITMAP_SET_SIZE(N) (((N) + SBITMAP_ELT_BITS - 1) / SBITMAP_ELT_BITS)

/* Set bit number bitno in the bitmap.  */
#define SET_BIT(BITMAP, BITNO)					\
  ((BITMAP)->elms [(BITNO) / SBITMAP_ELT_BITS]			\
   |= (SBITMAP_ELT_TYPE) 1 << (BITNO) % SBITMAP_ELT_BITS)

/* Test if bit number bitno in the bitmap is set.  */
#define TEST_BIT(BITMAP, BITNO) \
((BITMAP)->elms [(BITNO) / SBITMAP_ELT_BITS] >> (BITNO) % SBITMAP_ELT_BITS & 1)

/* Reset bit number bitno in the bitmap.  */
#define RESET_BIT(BITMAP, BITNO)				\
  ((BITMAP)->elms [(BITNO) / SBITMAP_ELT_BITS]			\
   &= ~((SBITMAP_ELT_TYPE) 1 << (BITNO) % SBITMAP_ELT_BITS))

/* Loop over all elements of SBITSET, starting with MIN.  */
#define EXECUTE_IF_SET_IN_SBITMAP(SBITMAP, MIN, N, CODE)		\
do {									\
  unsigned int word_num_;						\
  unsigned int bit_num_ = (MIN) % (unsigned int) SBITMAP_ELT_BITS;	\
  unsigned int size_ = (SBITMAP)->size;					\
  SBITMAP_ELT_TYPE *ptr_ = (SBITMAP)->elms;				\
									\
  for (word_num_ = (MIN) / (unsigned int) SBITMAP_ELT_BITS;		\
       word_num_ < size_; word_num_++, bit_num_ = 0)			\
    {									\
      SBITMAP_ELT_TYPE word_ = ptr_[word_num_];				\
									\
      if (word_ != 0)							\
	for (; bit_num_ < SBITMAP_ELT_BITS; bit_num_++)			\
	  {								\
	    SBITMAP_ELT_TYPE _mask = (SBITMAP_ELT_TYPE) 1 << bit_num_;	\
									\
	    if ((word_ & _mask) != 0)					\
	      {								\
		word_ &= ~ _mask;					\
		(N) = word_num_ * SBITMAP_ELT_BITS + bit_num_;		\
		CODE;							\
		if (word_ == 0)						\
		  break;						\
	      }								\
	  }								\
    }									\
} while (0)

#define EXECUTE_IF_SET_IN_SBITMAP_REV(SBITMAP, N, CODE)			\
do {									\
  unsigned int word_num_;						\
  unsigned int bit_num_;						\
  unsigned int size_ = (SBITMAP)->size;					\
  SBITMAP_ELT_TYPE *ptr_ = (SBITMAP)->elms;				\
									\
  for (word_num_ = size_; word_num_ > 0; word_num_--)			\
    {									\
      SBITMAP_ELT_TYPE word_ = ptr_[word_num_ - 1];			\
									\
      if (word_ != 0)							\
	for (bit_num_ = SBITMAP_ELT_BITS; bit_num_ > 0; bit_num_--)	\
	  {								\
	    SBITMAP_ELT_TYPE _mask = (SBITMAP_ELT_TYPE)1 << (bit_num_ - 1);\
									\
	    if ((word_ & _mask) != 0)					\
	      {								\
		word_ &= ~ _mask;					\
		(N) = (word_num_ - 1) * SBITMAP_ELT_BITS + bit_num_ - 1;\
		CODE;							\
		if (word_ == 0)						\
		  break;						\
	      }								\
	  }								\
    }									\
} while (0)

#define sbitmap_free(MAP)		free(MAP)
#define sbitmap_vector_free(VEC)	free(VEC)

struct int_list;

extern void dump_sbitmap		PARAMS ((FILE *, sbitmap));
extern void dump_sbitmap_file		PARAMS ((FILE *, sbitmap));
extern void dump_sbitmap_vector 	PARAMS ((FILE *, const char *,
						 const char *, sbitmap *,
						 int));
extern sbitmap sbitmap_alloc		PARAMS ((unsigned int));
extern sbitmap *sbitmap_vector_alloc	PARAMS ((unsigned int, unsigned int));
extern sbitmap sbitmap_resize		PARAMS ((sbitmap, unsigned int, int));
extern void sbitmap_copy 		PARAMS ((sbitmap, sbitmap));
extern int sbitmap_equal                PARAMS ((sbitmap, sbitmap));
extern void sbitmap_zero		PARAMS ((sbitmap));
extern void sbitmap_ones		PARAMS ((sbitmap));
extern void sbitmap_vector_zero		PARAMS ((sbitmap *, unsigned int));
extern void sbitmap_vector_ones		PARAMS ((sbitmap *, unsigned int));

extern void sbitmap_union_of_diff	PARAMS ((sbitmap, sbitmap, sbitmap,
						 sbitmap));
extern bool sbitmap_union_of_diff_cg	PARAMS ((sbitmap, sbitmap, sbitmap,
						 sbitmap));
extern void sbitmap_difference		PARAMS ((sbitmap, sbitmap, sbitmap));
extern void sbitmap_not			PARAMS ((sbitmap, sbitmap));
extern void sbitmap_a_or_b_and_c	PARAMS ((sbitmap, sbitmap, sbitmap,
						 sbitmap));
extern bool sbitmap_a_or_b_and_c_cg	PARAMS ((sbitmap, sbitmap, sbitmap,
						 sbitmap));
extern void sbitmap_a_and_b_or_c	PARAMS ((sbitmap, sbitmap, sbitmap,
						 sbitmap));
extern bool sbitmap_a_and_b_or_c_cg	PARAMS ((sbitmap, sbitmap, sbitmap,
						 sbitmap));
extern void sbitmap_a_and_b		PARAMS ((sbitmap, sbitmap, sbitmap));
extern bool sbitmap_a_and_b_cg		PARAMS ((sbitmap, sbitmap, sbitmap));
extern void sbitmap_a_or_b		PARAMS ((sbitmap, sbitmap, sbitmap));
extern bool sbitmap_a_or_b_cg		PARAMS ((sbitmap, sbitmap, sbitmap));
extern void sbitmap_a_xor_b		PARAMS ((sbitmap, sbitmap, sbitmap));
extern bool sbitmap_a_xor_b_cg		PARAMS ((sbitmap, sbitmap, sbitmap));
extern bool sbitmap_a_subset_b_p	PARAMS ((sbitmap, sbitmap));

extern int sbitmap_first_set_bit	PARAMS ((sbitmap));
extern int sbitmap_last_set_bit		PARAMS ((sbitmap));

extern void sbitmap_intersect_of_predsucc PARAMS ((sbitmap, sbitmap *,
						  int, struct int_list **));
#define sbitmap_intersect_of_predecessors  sbitmap_intersect_of_predsucc
#define sbitmap_intersect_of_successors    sbitmap_intersect_of_predsucc

extern void sbitmap_union_of_predsucc	PARAMS ((sbitmap, sbitmap *, int,
						 struct int_list **));
#define sbitmap_union_of_predecessors  sbitmap_union_of_predsucc
#define sbitmap_union_of_successors    sbitmap_union_of_predsucc

/* Intersection and Union of preds/succs using the new flow graph 
   structure instead of the pred/succ arrays.  */

extern void sbitmap_intersection_of_succs  PARAMS ((sbitmap, sbitmap *, int));
extern void sbitmap_intersection_of_preds  PARAMS ((sbitmap, sbitmap *, int));
extern void sbitmap_union_of_succs	   PARAMS ((sbitmap, sbitmap *, int));
extern void sbitmap_union_of_preds	   PARAMS ((sbitmap, sbitmap *, int));

extern void debug_sbitmap		   PARAMS ((sbitmap));
#endif /* ! GCC_SBITMAP_H */
