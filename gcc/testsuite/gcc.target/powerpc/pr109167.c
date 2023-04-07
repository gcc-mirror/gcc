/* { dg-do run } */
/* { dg-require-effective-target vsx_hw } */
/* { dg-options "-O2 -mvsx" } */

/* Verify there is no warning message.  */

#define NO_WARN_X86_INTRINSICS 1

#include <emmintrin.h>

#define N 5

__attribute__ ((noipa)) __m128i
test1 (__m128i v)
{
  return _mm_bslli_si128 (v, N);
}

__attribute__ ((noipa)) __m128i
test2 (__m128i v)
{
  return _mm_slli_si128 (v, N);
}

typedef union
{
  __m128i x;
  unsigned char a[16];
} union128i_ub;

int main()
{
  union128i_ub v;
  v.x
    = _mm_set_epi8 (1, 2, 3, 4, 10, 20, 30, 90, 80, 40, 100, 15, 98, 25, 98, 7);

  union128i_ub r1, r2;
  r1.x = test1 (v.x);
  r2.x = test2 (v.x);

  for (int i = 0; i < 16; i++)
    if (r1.a[i] != r2.a[i])
      __builtin_abort();

  return 0;
}

