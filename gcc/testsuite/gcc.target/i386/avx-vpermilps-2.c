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
    union128 u, s;
    union128i_q ctl;
    int m[4] = {mask_v(0), mask_v(1), mask_v(2), mask_v(3)};
    float e[4];

    s.x = _mm_set_ps (1, 2, 3, 4);
    ctl.x = _mm_loadu_si128((__m128i*) m);
    u.x = _mm_permutevar_ps(s.x, ctl.x);

    e[0] = s.a[0 + (m[0] & 0x03)];
    e[1] = s.a[0 + (m[1] & 0x03)];
    e[2] = s.a[0 + (m[2] & 0x03)];
    e[3] = s.a[0 + (m[3] & 0x03)];

    if (check_union128 (u, e))
      abort ();
}

