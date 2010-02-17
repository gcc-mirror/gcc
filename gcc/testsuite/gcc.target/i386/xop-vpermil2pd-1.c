/* { dg-do run } */
/* { dg-require-effective-target xop } */
/* { dg-options "-O2 -mxop" } */

#include "xop-check.h"

#include <x86intrin.h>

#ifndef ZERO_MATCH
#define ZERO_MATCH  2
#endif

static double
select2dp(double *src1, double *src2, long long  sel)
{
    double tmp = 0.0;

    if ((sel & 0x3) == 0) tmp = src1[0];
    if ((sel & 0x3) == 1) tmp = src1[1];
    if ((sel & 0x3) == 2) tmp = src2[0];
    if ((sel & 0x3) == 3) tmp = src2[1];

    return tmp;
}

static double
sel_and_condzerodp(double *src1, double *src2, long long  sel, int imm8)
{
    double tmp;

    tmp = select2dp(src1, src2, sel & 0x3);

    if (((imm8 & 0x3) == 2) && ((sel & 0x4) == 0x4)) tmp = 0;
    if (((imm8 & 0x3) == 3) && ((sel & 0x4) == 0x0)) tmp = 0;

    return tmp;
}

void static
xop_test ()
{
    union128d s1, s2, u;
    union128i_q s3;
    double e[2];

    s1.x = _mm_set_pd (1, 2);
    s2.x = _mm_set_pd (3, 4);
    s3.x = _mm_set_epi64x (1, 2);
    u.x = _mm_permute2_pd(s1.x, s2.x, s3.x, ZERO_MATCH);

    e[0] = sel_and_condzerodp (s1.a, s2.a, (s3.a[0] & 0xe)>>1, ZERO_MATCH);
    e[1] = sel_and_condzerodp (s1.a, s2.a, (s3.a[1] & 0xe)>>1, ZERO_MATCH);

    if (check_union128d (u, e))
      abort ();
}

