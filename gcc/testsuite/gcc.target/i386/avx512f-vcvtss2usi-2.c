/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#include "avx512f-check.h"

void static
avx512f_test (void)
{
  union128 s1;
  unsigned int d;
  unsigned int e;

  s1.x = _mm_set_ps (24.43, 68.346, 35.7765, 34508.51);
  d =  _mm_cvtss_u32 (s1.x);
  e = (unsigned int)(s1.a[0] + 0.5);

  if (e != d)
    abort ();
}
