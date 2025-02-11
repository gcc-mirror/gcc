/* { dg-do run } */
/* { dg-options "-O2 -march=x86-64-v3 -mavx10.2-256" } */
/* { dg-require-effective-target avx10_2_256 } */

#define AVX10_2
#define AVX10_SCALAR
#include "avx10-helper.h"
#include <limits.h>

void
TEST (void)
{
  UNION_TYPE (128, ) s;
  int res1;
  long long res2;
  int res1_ref = 0;
  long long res2_ref = 0;
  int i, sign = 1;

  s.a[0] = 2.46;

  res1 = _mm_cvtts_roundss_epi32 (s.x, 8);

  if (s.a[0] > INT_MAX)
    res1_ref = INT_MAX;
  else if (s.a[0] < INT_MIN)
    res1_ref = INT_MIN;
  else
    res1_ref = s.a[0];

  if (res1 != res1_ref)
    abort();

#ifdef __x86_64__
  res2 = _mm_cvtts_roundss_epi64 (s.x, 8);

  if (s.a[0] > LLONG_MAX)
    res2_ref = LLONG_MAX;
  else if (s.a[0] < LLONG_MIN)
    res2_ref = LLONG_MIN;
  else
    res2_ref = s.a[0];

  if (res2 != res2_ref)
    abort();
#endif
}
