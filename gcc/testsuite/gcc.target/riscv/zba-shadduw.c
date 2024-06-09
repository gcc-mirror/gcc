/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64gc_zba -mabi=lp64" } */

typedef struct simple_bitmap_def
{
  unsigned char *popcount;
  unsigned int n_bits;
  unsigned int size;
  unsigned long elms[1];
} *sbitmap;
typedef const struct simple_bitmap_def *const_sbitmap;

typedef unsigned long *sbitmap_ptr;
typedef const unsigned long *const_sbitmap_ptr;
static unsigned long sbitmap_elt_popcount (unsigned long);

void
sbitmap_a_or_b (sbitmap dst, const_sbitmap a, const_sbitmap b)
{
  unsigned int i, n = dst->size;
  sbitmap_ptr dstp = dst->elms;
  const_sbitmap_ptr ap = a->elms;
  const_sbitmap_ptr bp = b->elms;
  unsigned char has_popcount = dst->popcount != ((void *) 0);

  for (i = 0; i < n; i++)
    {
      const unsigned long tmp = *ap++ | *bp++;
      *dstp++ = tmp;
    }
}


/* { dg-final { scan-assembler "sh3add.uw" } } */
/* { dg-final { scan-assembler-not {\mslli.uw} } } */
