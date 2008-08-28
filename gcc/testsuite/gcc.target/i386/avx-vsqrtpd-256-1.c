/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

void static
avx_test (void)
{
  union256d u, s1;
  double e [4] = {0x1.d3881b2c32ed7p+7, 0x1.54abaed51711cp+4, 0x1.19195c08a8d23p+5, 0x1.719741d6c0b0bp+5};

  s1.x = _mm256_set_pd (2134.3343,1234.635654,453.345635,54646.464356);
  u.x = _mm256_sqrt_pd (s1.x);

  if (check_union256d (u, e))
    abort ();
}
