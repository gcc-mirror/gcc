/* { dg-do compile } */
/* { dg-options "-O2 -mavx" } */
/* { dg-final { scan-assembler "\tvmovdqa\t" } } */
/* { dg-final { scan-assembler "\tvmovdqu\t" } } */
/* { dg-final { scan-assembler "\tvmovapd\t" } } */
/* { dg-final { scan-assembler "\tvmovupd\t" } } */
/* { dg-final { scan-assembler-not "\tvmovaps\t" } } */
/* { dg-final { scan-assembler-not "\tvmovups\t" } } */

#include <immintrin.h>

void
foo1 (__m128i *p, __m128i x)
{
  *p = x;
}

void
foo2 (__m128d *p, __m128d x)
{
  *p = x;
}

void
foo3 (__float128 *p, __float128 x)
{
  *p = x;
}

void
foo4 (__m128i_u *p, __m128i x)
{
  *p = x;
}

void
foo5 (__m128d_u *p, __m128d x)
{
  *p = x;
}

typedef __float128 __float128_u __attribute__ ((__aligned__ (1)));

void
foo6 (__float128_u *p, __float128 x)
{
  *p = x;
}

#ifdef __x86_64__
typedef __int128 __int128_u __attribute__ ((__aligned__ (1)));

extern __int128 int128;

void
foo7 (__int128 *p)
{
  *p = int128;
}

void
foo8 (__int128_u *p)
{
  *p = int128;
}
#endif
