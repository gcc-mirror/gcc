/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

#ifndef CTRL
#define CTRL 11
#endif

void static
avx_test ()
{
  union128 u, s;
  float e[4];

  s.x = _mm_set_ps (1, 2, 3, 4);
  u.x = _mm_permute_ps(s.x, CTRL);

  e[0] = s.a[ (CTRL & 0x03)];
  e[1] = s.a[((CTRL & 0x0c) >> 2)];
  e[2] = s.a[((CTRL & 0x30) >> 4)];
  e[3] = s.a[((CTRL & 0xc0) >> 6)];

  if (check_union128 (u, e))
    abort ();
}
