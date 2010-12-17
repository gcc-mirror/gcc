/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx -mvzeroupper" } */

#include "avx-check.h"

int s[8] = {1, 2, 3, 4, 5, 6, 7, 8};
int d[8] = {11, 22, 33, 44, 55, 66, 77, 88};

void
__attribute__((noinline))
foo ()
{
  int i;
  for (i = 0; i < ARRAY_SIZE (d); i++)
    d[i] = s[i] + 0x1000;
}

static void
__attribute__((noinline))
bar (__m256i src)
{
  foo ();
  _mm256_storeu_si256 ((__m256i*) d, src);
  if (__builtin_memcmp (d, s, sizeof (d)))
    abort ();
}

static void
avx_test (void)
{
  __m256i src = _mm256_loadu_si256 ((__m256i*) s);
  bar (src);
}
