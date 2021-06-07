/* { dg-do run } */
/* { dg-options "-O2 -mavx" } */
/* { dg-require-effective-target avx } */

#include "avx-check.h"

void
__attribute__ ((noipa))
mtest(char *dest)
{
  __m256i ymm1 = _mm256_set1_epi8((char)0x1);
  _mm256_storeu_si256((__m256i *)(dest + 32), ymm1);
  _mm256_zeroupper();
  __m256i ymm2 = _mm256_set1_epi8((char)0x1);
  _mm256_storeu_si256((__m256i *)dest, ymm2);
}

void
avx_test ()
{
  char buf[64];
  for (int i = 0; i != 64; i++)
    buf[i] = 2;
  mtest (buf);

  for (int i = 0; i < 32; ++i)
    if (buf[i] != 1)
      __builtin_abort ();
}
