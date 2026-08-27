/* { dg-do compile } */
/* { dg-options "-O2" } */

/* Check that the shuffle is NOT optimized to rev64+ext inside a loop.  */
/* { dg-final { scan-assembler-not "rev64" } } */

#define vector __attribute__ ((vector_size (16)))

void
f (vector char *dst, vector char *src, int n)
{
  for (int i = 0; i < n; i++)
    dst[i]
      = __builtin_shuffle (src[i], (vector char) {15, 14, 13, 12, 11, 10, 9, 8,
						  7, 6, 5, 4, 3, 2, 1, 0});
}
