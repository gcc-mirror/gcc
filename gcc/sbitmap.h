/* Simple bitmaps.
   Copyright (C) 1999, 2000 Free Software Foundation, Inc.

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

#ifndef _SBITMAP_H
#define _SBITMAP_H 1

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
  unsigned int _word_num;						\
  unsigned int _bit_num = (MIN) % (unsigned int) SBITMAP_ELT_BITS;	\
  unsigned int _size = (SBITMAP)->size;					\
  SBITMAP_ELT_TYPE *_ptr = (SBITMAP)->elms;				\
									\
  for (_word_num = (MIN) / (unsigned int) SBITMAP_ELT_BITS;		\
       _word_num < _size; _word_num++, _bit_num = 0)			\
    {									\
      SBITMAP_ELT_TYPE _word = _ptr[_word_num];				\
									\
      if (_word != 0)							\
	for (; _bit_num < SBITMAP_ELT_BITS; _bit_num++)			\
	  {								\
	    SBITMAP_ELT_TYPE _mask = (SBITMAP_ELT_TYPE)1 << _bit_num;	\
									\
	    if ((_word & _mask) != 0)					\
	      {								\
		_word &= ~ _mask;					\
		(N) = _word_num * SBITMAP_ELT_BITS + _bit_num;		\
		CODE;							\
		if (_word == 0)						\
		  break;						\
	      }								\
	  }								\
    }									\
} while (0)

#define sbitmap_free(MAP)		free(MAP)
#define sbitmap_vector_free(VEC)	free(VEC)

struct int_list;

extern void dump_sbitmap		PARAMS ((FILE *, sbitmap));
extern void dump_sbitmap_vector 	PARAMS ((FILE *, const char *,
						 const char *, sbitmap *,
						 int));
extern sbitmap sbitmap_alloc		PARAMS ((unsigned int));
extern sbitmap *sbitmap_vector_alloc	PARAMS ((unsigned int, unsigned int));
extern void sbitmap_copy 		PARAMS ((sbitmap, sbitmap));
extern void sbitmap_zero		PARAMS ((sbitmap));
extern void sbitmap_ones		PARAMS ((sbitmap));
extern void sbitmap_vector_zero		PARAMS ((sbitmap *, unsigned int));
extern void sbitmap_vector_ones		PARAMS ((sbitmap *, unsigned int));

extern int sbitmap_union_of_diff	PARAMS ((sbitmap, sbitmap, sbitmap,
						 sbitmap));
extern void sbitmap_difference		PARAMS ((sbitmap, sbitmap, sbitmap));
extern void sbitmap_not			PARAMS ((sbitmap, sbitmap));
extern int sbitmap_a_or_b_and_c		PARAMS ((sbitmap, sbitmap, sbitmap,
						 sbitmap));
extern int sbitmap_a_and_b_or_c		PARAMS ((sbitmap, sbitmap, sbitmap,
						 sbitmap));
extern int sbitmap_a_and_b		PARAMS ((sbitmap, sbitmap, sbitmap));
extern int sbitmap_a_or_b		PARAMS ((sbitmap, sbitmap, sbitmap));
extern int sbitmap_a_subset_b_p		PARAMS ((sbitmap, sbitmap));

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

#endif /* _SBITMAP_H */
