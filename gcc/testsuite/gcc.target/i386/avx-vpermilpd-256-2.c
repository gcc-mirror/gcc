/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

#ifndef CTRL
#define CTRL 6
#endif

#define mask_v(pos) (((CTRL & (1ULL << (pos))) >> (pos)) << 1)

void static
avx_test ()
{
    union256d u, src; 
    union256i_q ctl;

    double  s[4] = {39578.467285, 7856.342941, 9674.67456, 13543.9788};
    long long m[4] = {mask_v(0), mask_v(1), mask_v(2), mask_v(3)};
    double  e[4] = {0.0};

    src.x = _mm256_loadu_pd(s);
    ctl.x  = _mm256_loadu_si256((__m256i*) m);
    u.x = _mm256_permutevar_pd(src.x, ctl.x);

    e[0] = s[0 + ((m[0] & 0x02) >> 1)];
    e[1] = s[0 + ((m[1] & 0x02) >> 1)];
    e[2] = s[2 + ((m[2] & 0x02) >> 1)];
    e[3] = s[2 + ((m[3] & 0x02) >> 1)];

    if (check_union256d (u, e))
      abort ();
}


