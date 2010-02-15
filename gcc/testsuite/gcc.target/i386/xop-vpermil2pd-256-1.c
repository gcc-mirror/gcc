/* { dg-do run } */
/* { dg-require-effective-target xop } */
/* { dg-options "-O2 -mxop" } */

#include "xop-check.h"

#ifndef ZERO_MATCH
#define ZERO_MATCH 1
#endif

static double
select2dp(double *src1, double *src2, long long  sel)
{
    double tmp = 3.414;

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

    tmp = select2dp(src1, src2, sel);

    if (((imm8 & 0x3) == 2) && ((sel & 0x4) == 0x4)) tmp = 0;
    if (((imm8 & 0x3) == 3) && ((sel & 0x4) == 0x0)) tmp = 0;

    return tmp;
}

void static
xop_test ()
{
    union256d u, s1, s2;
    double   e[4] = {0.0};
    union256i_q s3;

    s1.x = _mm256_set_pd (1, 2, 3, 4);
    s2.x = _mm256_set_pd (5, 6, 7, 8);
    s3.x = _mm256_set_epi64x (0, 1, 2, 3);
    u.x = _mm256_permute2_pd(s1.x, s2.x, s3.x, ZERO_MATCH);

    e[0] = sel_and_condzerodp (s1.a, s2.a, (s3.a[0] & 0xe)>>1, ZERO_MATCH);
    e[1] = sel_and_condzerodp (s1.a, s2.a, (s3.a[1] & 0xe)>>1, ZERO_MATCH);
    e[2] = sel_and_condzerodp (s1.a + 2, s2.a + 2, (s3.a[2] & 0xe)>>1, ZERO_MATCH);
    e[3] = sel_and_condzerodp (s1.a + 2, s2.a + 2, (s3.a[3] & 0xe)>>1, ZERO_MATCH);

    if (check_union256d (u, e))
      abort ();
}
