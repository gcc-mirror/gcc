/* Simple bitmaps.
   Copyright (C) 1999-2012  Free Software Foundation, Inc.

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

#ifndef GCC_SBITMAP_H
#define GCC_SBITMAP_H

/* Implementation of sets using simple bitmap vectors.

   This set representation is suitable for non-sparse sets with a known
   (a priori) universe.  The set is represented as a simple array of the
   host's fastest unsigned integer.  For a given member I in the set:
     - the element for I will be at sbitmap[I / (bits per element)]
     - the position for I within element is I % (bits per element)

   This representation is very space-efficient for large non-sparse sets
   with random access patterns.

   The following operations can be performed in O(1) time:

     * set_size			: SBITMAP_SIZE
     * member_p			: TEST_BIT
     * add_member		: SET_BIT
     * remove_member		: RESET_BIT

   Most other operations on this set representation are O(U) where U is
   the size of the set universe:

     * clear			: sbitmap_zero
     * cardinality		: sbitmap_popcount
     * choose_one		: sbitmap_first_set_bit /
				  sbitmap_last_set_bit
     * forall			: EXECUTE_IF_SET_IN_SBITMAP
     * set_copy			: sbitmap_copy / sbitmap_copy_n
     * set_intersection		: sbitmap_a_and_b
     * set_union		: sbitmap_a_or_b
     * set_difference		: sbitmap_difference
     * set_disjuction		: (not implemented)
     * set_compare		: sbitmap_equal

   Some operations on 3 sets that occur frequently in in data flow problems
   are also implemented:

      * A | (B & C)		: sbitmap_a_or_b_and_c
      * A | (B & ~C)		: sbitmap_union_of_diff
      * A & (B | C)		: sbitmap_a_and_b_or_c

   Most of the set functions have two variants: One that returns non-zero
   if members were added or removed from the target set, and one that just
   performs the operation without feedback.  The former operations are a
   bit more expensive but the result can often be used to avoid iterations
   on other sets.

   Allocating a bitmap is done with sbitmap_alloc, and resizing is
   performed with sbitmap_resize.

   The storage requirements for simple bitmap sets is O(U) where U is the
   size of the set universe (colloquially the number of bits in the bitmap).

   This set representation works well for relatively small data flow problems
   (there are special routines for that, see sbitmap_vector_*).  The set
   operations can be vectorized and there is almost no computating overhead,
   so that even sparse simple bitmap sets outperform dedicated sparse set
   representations like linked-list bitmaps.  For larger problems, the size
   overhead of simple bitmap sets gets too high and other set representations
   have to be used.  */

#define SBITMAP_ELT_BITS (HOST_BITS_PER_WIDEST_FAST_INT * 1u)
#define SBITMAP_ELT_TYPE unsigned HOST_WIDEST_FAST_INT

struct simple_bitmap_def
{
  unsigned char *popcount;      /* Population count.  */
  unsigned int n_bits;		/* Number of bits.  */
  unsigned int size;		/* Size in elements.  */
  SBITMAP_ELT_TYPE elms[1];	/* The elements.  */
};

/* Return the set size needed for N elements.  */
#define SBITMAP_SET_SIZE(N) (((N) + SBITMAP_ELT_BITS - 1) / SBITMAP_ELT_BITS)
#define SBITMAP_SIZE_BYTES(BITMAP) ((BITMAP)->size * sizeof (SBITMAP_ELT_TYPE))

/* Return the number of bits in BITMAP.  */
#define SBITMAP_SIZE(BITMAP) ((BITMAP)->n_bits)

/* Test if bit number bitno in the bitmap is set.  */
static inline SBITMAP_ELT_TYPE
TEST_BIT (const_sbitmap map, unsigned int bitno)
{
  size_t i = bitno / SBITMAP_ELT_BITS;
  unsigned int s = bitno % SBITMAP_ELT_BITS;
  return (map->elms[i] >> s) & (SBITMAP_ELT_TYPE) 1;
}

/* Set bit number BITNO in the sbitmap MAP.  */

static inline void
SET_BIT (sbitmap map, unsigned int bitno)
{
  gcc_checking_assert (! map->popcount);
  map->elms[bitno / SBITMAP_ELT_BITS]
    |= (SBITMAP_ELT_TYPE) 1 << (bitno) % SBITMAP_ELT_BITS;
}

/* Like SET_BIT, but updates population count.  */

static inline void
SET_BIT_WITH_POPCOUNT (sbitmap map, unsigned int bitno)
{
  bool oldbit;
  gcc_checking_assert (map->popcount);
  oldbit = TEST_BIT (map, bitno);
  if (!oldbit)
    map->popcount[bitno / SBITMAP_ELT_BITS]++;
  map->elms[bitno / SBITMAP_ELT_BITS]
    |= (SBITMAP_ELT_TYPE) 1 << (bitno) % SBITMAP_ELT_BITS;
}

/* Reset bit number BITNO in the sbitmap MAP.  */

static inline void
RESET_BIT (sbitmap map,  unsigned int bitno)
{
  gcc_checking_assert (! map->popcount);
  map->elms[bitno / SBITMAP_ELT_BITS]
    &= ~((SBITMAP_ELT_TYPE) 1 << (bitno) % SBITMAP_ELT_BITS);
}

/* Like RESET_BIT, but updates population count.  */

static inline void
RESET_BIT_WITH_POPCOUNT (sbitmap map,  unsigned int bitno)
{
  bool oldbit;
  gcc_checking_assert (map->popcount);
  oldbit = TEST_BIT (map, bitno);
  if (oldbit)
    map->popcount[bitno / SBITMAP_ELT_BITS]--;
  map->elms[bitno / SBITMAP_ELT_BITS]
    &= ~((SBITMAP_ELT_TYPE) 1 << (bitno) % SBITMAP_ELT_BITS);
}

/* The iterator for sbitmap.  */
typedef struct {
  /* The pointer to the first word of the bitmap.  */
  const SBITMAP_ELT_TYPE *ptr;

  /* The size of the bitmap.  */
  unsigned int size;

  /* The current word index.  */
  unsigned int word_num;

  /* The current bit index (not modulo SBITMAP_ELT_BITS).  */
  unsigned int bit_num;

  /* The words currently visited.  */
  SBITMAP_ELT_TYPE word;
} sbitmap_iterator;

/* Initialize the iterator I with sbitmap BMP and the initial index
   MIN.  */

static inline void
sbitmap_iter_init (sbitmap_iterator *i, const_sbitmap bmp, unsigned int min)
{
  i->word_num = min / (unsigned int) SBITMAP_ELT_BITS;
  i->bit_num = min;
  i->size = bmp->size;
  i->ptr = bmp->elms;

  if (i->word_num >= i->size)
    i->word = 0;
  else
    i->word = (i->ptr[i->word_num]
	       >> (i->bit_num % (unsigned int) SBITMAP_ELT_BITS));
}

/* Return true if we have more bits to visit, in which case *N is set
   to the index of the bit to be visited.  Otherwise, return
   false.  */

static inline bool
sbitmap_iter_cond (sbitmap_iterator *i, unsigned int *n)
{
  /* Skip words that are zeros.  */
  for (; i->word == 0; i->word = i->ptr[i->word_num])
    {
      i->word_num++;

      /* If we have reached the end, break.  */
      if (i->word_num >= i->size)
	return false;

      i->bit_num = i->word_num * SBITMAP_ELT_BITS;
    }

  /* Skip bits that are zero.  */
  for (; (i->word & 1) == 0; i->word >>= 1)
    i->bit_num++;

  *n = i->bit_num;

  return true;
}

/* Advance to the next bit.  */

static inline void
sbitmap_iter_next (sbitmap_iterator *i)
{
  i->word >>= 1;
  i->bit_num++;
}

/* Loop over all elements of SBITMAP, starting with MIN.  In each
   iteration, N is set to the index of the bit being visited.  ITER is
   an instance of sbitmap_iterator used to iterate the bitmap.  */

#define EXECUTE_IF_SET_IN_SBITMAP(SBITMAP, MIN, N, ITER)	\
  for (sbitmap_iter_init (&(ITER), (SBITMAP), (MIN));		\
       sbitmap_iter_cond (&(ITER), &(N));			\
       sbitmap_iter_next (&(ITER)))

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

#define sbitmap_free(MAP)		(free((MAP)->popcount), free((MAP)))
#define sbitmap_vector_free(VEC)	free(VEC)

extern void dump_sbitmap (FILE *, const_sbitmap);
extern void dump_sbitmap_file (FILE *, const_sbitmap);
extern void dump_sbitmap_vector (FILE *, const char *, const char *, sbitmap *,
				 int);
extern sbitmap sbitmap_alloc (unsigned int);
extern sbitmap sbitmap_alloc_with_popcount (unsigned int);
extern sbitmap *sbitmap_vector_alloc (unsigned int, unsigned int);
extern sbitmap sbitmap_resize (sbitmap, unsigned int, int);
extern void sbitmap_copy (sbitmap, const_sbitmap);
extern void sbitmap_copy_n (sbitmap, const_sbitmap, unsigned int);
extern int sbitmap_equal (const_sbitmap, const_sbitmap);
extern bool sbitmap_empty_p (const_sbitmap);
extern bool sbitmap_range_empty_p (const_sbitmap, unsigned int, unsigned int);
extern void sbitmap_zero (sbitmap);
extern void sbitmap_ones (sbitmap);
extern void sbitmap_vector_zero (sbitmap *, unsigned int);
extern void sbitmap_vector_ones (sbitmap *, unsigned int);

extern void sbitmap_union_of_diff (sbitmap, const_sbitmap,
				   const_sbitmap, const_sbitmap);
extern bool sbitmap_union_of_diff_cg (sbitmap, const_sbitmap,
				      const_sbitmap, const_sbitmap);
extern void sbitmap_difference (sbitmap, const_sbitmap, const_sbitmap);
extern void sbitmap_not (sbitmap, const_sbitmap);
extern void sbitmap_a_or_b_and_c (sbitmap, const_sbitmap,
				  const_sbitmap, const_sbitmap);
extern bool sbitmap_a_or_b_and_c_cg (sbitmap, const_sbitmap,
				     const_sbitmap, const_sbitmap);
extern void sbitmap_a_and_b_or_c (sbitmap, const_sbitmap,
				  const_sbitmap, const_sbitmap);
extern bool sbitmap_a_and_b_or_c_cg (sbitmap, const_sbitmap,
				     const_sbitmap, const_sbitmap);
extern bool sbitmap_any_common_bits (const_sbitmap, const_sbitmap);
extern void sbitmap_a_and_b (sbitmap, const_sbitmap, const_sbitmap);
extern bool sbitmap_a_and_b_cg (sbitmap, const_sbitmap, const_sbitmap);
extern void sbitmap_a_or_b (sbitmap, const_sbitmap, const_sbitmap);
extern bool sbitmap_a_or_b_cg (sbitmap, const_sbitmap, const_sbitmap);
extern void sbitmap_a_xor_b (sbitmap, const_sbitmap, const_sbitmap);
extern bool sbitmap_a_xor_b_cg (sbitmap, const_sbitmap, const_sbitmap);
extern bool sbitmap_a_subset_b_p (const_sbitmap, const_sbitmap);

extern int sbitmap_first_set_bit (const_sbitmap);
extern int sbitmap_last_set_bit (const_sbitmap);

extern void debug_sbitmap (const_sbitmap);
extern sbitmap sbitmap_realloc (sbitmap, unsigned int);
extern unsigned long sbitmap_popcount (const_sbitmap, unsigned long);
extern void sbitmap_verify_popcount (const_sbitmap);
#endif /* ! GCC_SBITMAP_H */
