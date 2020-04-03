/* PR target/94460 */
/* { dg-do run { target { avx2 && int128 } } } */
/* { dg-options "-O2 -mavx2" } */

#include <x86intrin.h>
#include "avx2-check.h"

typedef __int128 v2ti __attribute__ ((__vector_size__ (32)));

static inline v2ti
foo (__v16hi b)
{
  return (v2ti) _mm256_hsub_epi16 ((__m256i) b, (__m256i) b);
}

static inline v2ti
bar (__v8si b)
{
  return (v2ti) _mm256_hsub_epi32 ((__m256i) b, (__m256i) b);
}

static void
avx2_test (void)
{
  v2ti x = foo ((__v16hi) { 1 });
  if (x[0] != ((__int128)1 << 64 | 1) || x[1] != 0)
    abort ();
  x = bar ((__v8si) { 1 });
  if (x[0] != ((__int128)1 << 64 | 1) || x[1] != 0)
    abort ();
}
