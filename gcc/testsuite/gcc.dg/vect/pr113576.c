/* { dg-additional-options "-O3" } */
/* { dg-additional-options "-march=skylake-avx512" { target avx512f } } */

#include "tree-vect.h"

#include<stdbool.h>
#include<stdlib.h>
#include<stddef.h>
#include<string.h>

#define SBITMAP_ELT_BITS ((unsigned) 64)
#define SBITMAP_ELT_TYPE unsigned long long
#define SBITMAP_SIZE_BYTES(BITMAP) ((BITMAP)->size * sizeof (SBITMAP_ELT_TYPE))
#define do_popcount(x) __builtin_popcountll(x)

typedef struct simple_bitmap_def
{
  unsigned char *popcount;      /* Population count.  */
  unsigned int n_bits;		/* Number of bits.  */
  unsigned int size;		/* Size in elements.  */
  SBITMAP_ELT_TYPE elms[1];	/* The elements.  */
} *sbitmap;
typedef const struct simple_bitmap_def *const_sbitmap;

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

#define SBITMAP_SET_SIZE(N) (((N) + SBITMAP_ELT_BITS - 1) / SBITMAP_ELT_BITS)
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
  bmap = (sbitmap) malloc (amt);
  bmap->n_bits = n_elms;
  bmap->size = size;
  bmap->popcount = NULL;
  return bmap;
}

#define sbitmap_free(MAP)		(free((MAP)->popcount), free((MAP)))
/* Loop over all elements of SBITMAP, starting with MIN.  In each
   iteration, N is set to the index of the bit being visited.  ITER is
   an instance of sbitmap_iterator used to iterate the bitmap.  */

#define EXECUTE_IF_SET_IN_SBITMAP(SBITMAP, MIN, N, ITER)	\
  for (sbitmap_iter_init (&(ITER), (SBITMAP), (MIN));		\
       sbitmap_iter_cond (&(ITER), &(N));			\
       sbitmap_iter_next (&(ITER)))

int
__attribute__((noinline))
sbitmap_first_set_bit (const_sbitmap bmap)
{
  unsigned int n = 0;
  sbitmap_iterator sbi;

  EXECUTE_IF_SET_IN_SBITMAP (bmap, 0, n, sbi)
    return n;
  return -1;
}

void
sbitmap_zero (sbitmap bmap)
{
  memset (bmap->elms, 0, SBITMAP_SIZE_BYTES (bmap));
  if (bmap->popcount)
    memset (bmap->popcount, 0, bmap->size * sizeof (unsigned char));
}

int main ()
{
  check_vect ();

  sbitmap tmp = sbitmap_alloc(1856);
  sbitmap_zero (tmp);
  int res = sbitmap_first_set_bit (tmp);
  if (res != -1)
    abort ();
  sbitmap_free (tmp);
  return 0;
}
