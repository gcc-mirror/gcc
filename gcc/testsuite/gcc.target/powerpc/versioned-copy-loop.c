/* { dg-do compile } */
/* { dg-options "-O3 -mvsx -fdump-tree-vect-details" } */
/* { dg-require-effective-target powerpc_vsx } */

/* Verify that a pure copy loop with a vectorization factor of two
   that requires alignment will not be vectorized.  See the cost
   model hooks in rs6000.c.  */

typedef long unsigned int size_t;
typedef unsigned char uint8_t;

extern void *memcpy (void *__restrict __dest, const void *__restrict __src,
       size_t __n) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));

void foo (void *dstPtr, const void *srcPtr, void *dstEnd)
{
    uint8_t *d = (uint8_t*)dstPtr;
    const uint8_t *s = (const uint8_t*)srcPtr;
    uint8_t* const e = (uint8_t*)dstEnd;

    do
      {
	memcpy (d, s, 8);
	d += 8;
	s += 8;
      }
    while (d < e);
}

/* { dg-final { scan-tree-dump-times "vectorized 0 loops" 1 "vect" } } */
