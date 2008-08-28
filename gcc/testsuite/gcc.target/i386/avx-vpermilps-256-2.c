/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

#ifndef CTRL
#define CTRL 233
#endif

#define mask_v(pos) ((CTRL & (0x3 << (pos))) >> (pos))

void static
avx_test ()
{
    union256 u, s;
    union256i_q ctl;
    int m[8] = {mask_v(0), mask_v(1), mask_v(2), mask_v(3), mask_v(4), mask_v(5), mask_v(6), mask_v(7)};
    float e[8];

    s.x = _mm256_set_ps (1, 2, 3, 4, 5, 6, 7, 8);
    ctl.x = _mm256_loadu_si256((__m256i*) m);
    u.x = _mm256_permutevar_ps(s.x, ctl.x);

    e[0] = s.a[0 + (m[0] & 0x03)];
    e[1] = s.a[0 + (m[1] & 0x03)];
    e[2] = s.a[0 + (m[2] & 0x03)];
    e[3] = s.a[0 + (m[3] & 0x03)];
    e[4] = s.a[4 + (m[4] & 0x03)];
    e[5] = s.a[4 + (m[5] & 0x03)];
    e[6] = s.a[4 + (m[6] & 0x03)];
    e[7] = s.a[4 + (m[7] & 0x03)];

    if (check_union256 (u, e))
      abort ();
}

