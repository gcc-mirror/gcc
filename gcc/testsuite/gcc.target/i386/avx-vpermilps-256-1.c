/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

#ifndef CTRL
#define CTRL 129
#endif

void static
avx_test ()
{
    union256 src, u;
    float e[8] = {0.0};

    src.x = _mm256_set_ps (1, 2, 3, 4, 5, 6, 7, 8);
    u.x = _mm256_permute_ps(src.x, CTRL);

    e[0] = src.a[0 +  (CTRL & 0x03)];
    e[1] = src.a[0 + ((CTRL & 0x0c) >> 2)];
    e[2] = src.a[0 + ((CTRL & 0x30) >> 4)];
    e[3] = src.a[0 + ((CTRL & 0xc0) >> 6)];
    e[4] = src.a[4 +  (CTRL & 0x03)];
    e[5] = src.a[4 + ((CTRL & 0x0c) >> 2)];
    e[6] = src.a[4 + ((CTRL & 0x30) >> 4)];
    e[7] = src.a[4 + ((CTRL & 0xc0) >> 6)];

    if (check_union256 (u, e))
      abort ();
}
