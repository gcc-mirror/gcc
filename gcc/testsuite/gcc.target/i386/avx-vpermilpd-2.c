/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

#ifndef CTRL
#define CTRL 2
#endif

#define mask_v(pos) (((CTRL & (1ULL << (pos))) >> (pos)) << 1)

void static
avx_test ()
{
    union128d u, src;
    union128i_q ctl;
 
    double  s[2] = {9674.67456, 13543.9788};
    long long m[2] = {mask_v(0), mask_v(1)};
    double  e[2];

    src.x = _mm_loadu_pd(s);
    ctl.x = _mm_loadu_si128((__m128i*) m);
    u.x = _mm_permutevar_pd(src.x, ctl.x);

    e[0] = s[((m[0] & 0x02) >> 1)];
    e[1] = s[((m[1] & 0x02) >> 1)];

    if (check_union128d (u, e))
      abort ();
}

