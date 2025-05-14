/* { dg-do run } */
/* { dg-options "-O2 -march=x86-64-v3 -mavx10.2" } */
/* { dg-require-effective-target avx10_2 } */

#define AVX10_2
#define AVX10_SCALAR
#include "avx10-helper.h"
#include <limits.h>

void
TEST (void)
{
  UNION_TYPE (128, ) s;
  unsigned int res1;
  unsigned long long res2;
  unsigned int res1_ref = 0;
  unsigned long long res2_ref = 0;

  s.a[0] = 2.46;

  res1 = _mm_cvtts_roundss_epu32 (s.x, 8);

  if (s.a[0] > UINT_MAX)
    res1_ref = UINT_MAX;
  else if (s.a[0] < 0)
    res1_ref = 0;
  else
    res1_ref = s.a[0];

  if (res1 != res1_ref)
    abort();

  res1 = _mm_cvtts_ss_epu32 (s.x);

  if (s.a[0] > UINT_MAX)
    res1_ref = UINT_MAX;
  else if (s.a[0] < 0)
    res1_ref = 0;
  else
    res1_ref = s.a[0];

  if (res1 != res1_ref)
    abort();

#ifdef __x86_64__
  res2 = _mm_cvtts_roundss_epu64 (s.x, 8);

  if (s.a[0] > ULONG_MAX)
    res2_ref = ULONG_MAX;
  else if (s.a[0] < 0)
    res2_ref = 0;
  else
    res2_ref = s.a[0];

  if (res2 != res2_ref)
    abort();

  res2 = _mm_cvtts_ss_epu64 (s.x);

  if (s.a[0] > ULONG_MAX)
    res2_ref = ULONG_MAX;
  else if (s.a[0] < 0)
    res2_ref = 0;
  else
    res2_ref = s.a[0];

  if (res2 != res2_ref)
    abort();
#endif
}
