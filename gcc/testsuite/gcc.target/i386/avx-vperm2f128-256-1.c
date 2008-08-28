/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

#define CONTROL 0xCC

void static
avx_test (void)
{
  union256 u, s1, s2;
  float e [8];

  s1.x = _mm256_set_ps (1, 2, 3, 4, 5, 6, 7, 8);
  s2.x = _mm256_set_ps (9, 10, 11, 12, 13, 14, 15, 16);
  u.x = _mm256_permute2f128_ps (s1.x, s2.x, CONTROL);

  switch (CONTROL & 0x3)
    {
      case 0:
        __builtin_memcpy (e, s1.a, 16);
        break;
      case 1: 
        __builtin_memcpy (e, s1.a+4, 16);
        break;
      case 2:
        __builtin_memcpy (e, s2.a, 16);
        break;
      case 3:
        __builtin_memcpy (e, s2.a+4, 16);
        break;
      default:
        abort ();
    }

 switch ((CONTROL & 0xc)>>2)
    {
      case 0:
        __builtin_memcpy (e+4, s1.a, 16);
        break;
      case 1: 
        __builtin_memcpy (e+4, s1.a+4, 16);
        break;
      case 2:
        __builtin_memcpy (e+4, s2.a, 16);
        break;
      case 3:
        __builtin_memcpy (e+4, s2.a+4, 16);
        break;
      default:
        abort ();
    }

  if (CONTROL & (1<<3))
    __builtin_memset (e, 0, 16);

  if (CONTROL & (1<<7))
    __builtin_memset (e+4, 0, 16);

  if (check_union256 (u, e))
    abort ();
}
