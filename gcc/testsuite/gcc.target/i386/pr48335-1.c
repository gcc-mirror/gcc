/* PR middle-end/48335 */
/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-sra -msse2" } */

#include <emmintrin.h>

typedef __float128 T __attribute__((may_alias));

struct S
{
  _Complex double d __attribute__((aligned (16)));
};

void bar (struct S);

void
f1 (T x)
{
  struct S s;
  *(T *) &s.d = x;
  __real__ s.d *= 7.0;
  bar (s);
}

void
f2 (__m128d x)
{
  struct S s;
  _mm_store_pd ((double *) &s.d, x);
  __real__ s.d *= 7.0;
  bar (s);
}
