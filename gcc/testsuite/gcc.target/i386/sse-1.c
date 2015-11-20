/* PR 12902 */
/* { dg-do compile } */
/* { dg-options "-O1 -msse" } */

#include <xmmintrin.h>

typedef union
{
  int i[4];
  float f[4];
  __m128 v;
} vector4_t;

void
swizzle (const void *a, vector4_t * b, vector4_t * c)
{
  __m64 *t = __builtin_assume_aligned (a, 64);

  b->v = _mm_loadl_pi (b->v, t);
  c->v = _mm_loadl_pi (c->v, t + 1);
}

/* While one legal rendering of each statement would be movaps;movlps;movaps,
   we can implmenent this with just movlps;movlps.  Since we do now, anything
   less would be a regression.  */
/* { dg-final { scan-assembler-not "movaps" } } */
/* { dg-final { scan-assembler "movlps" } } */
