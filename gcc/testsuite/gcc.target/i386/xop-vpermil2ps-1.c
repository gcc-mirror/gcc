/* { dg-do run } */
/* { dg-require-effective-target xop } */
/* { dg-options "-O2 -mxop" } */

#include "xop-check.h"

#include <x86intrin.h>

#ifndef ZERO_MATCH
#define ZERO_MATCH  1
#endif

static float
select2sp(float *src1, float *src2, int sel)
{
    float tmp;

    if ((sel & 0x7) == 0) tmp = src1[0];
    if ((sel & 0x7) == 1) tmp = src1[1];
    if ((sel & 0x7) == 2) tmp = src1[2];
    if ((sel & 0x7) == 3) tmp = src1[3];
    if ((sel & 0x7) == 4) tmp = src2[0];
    if ((sel & 0x7) == 5) tmp = src2[1];
    if ((sel & 0x7) == 6) tmp = src2[2];
    if ((sel & 0x7) == 7) tmp = src2[3];

    return tmp;
}
static float
sel_and_condzerosp(float *src1, float *src2, int sel, int imm8)
{
    float tmp;

    tmp = select2sp(src1, src2, sel & 0x7);

    if (((imm8 & 0x3) == 2) && ((sel & 0x8) == 0x8)) tmp = 0;
    if (((imm8 & 0x3) == 3) && ((sel & 0x8) == 0x0)) tmp = 0;

    return tmp;
}

void static
xop_test ()
{
    int i;
    union128  source1, source2, u;
    union128i_d source3;
    float s1[4] = {1, 2, 3, 4};
    float s2[4] = {5, 6, 7, 8};
    int   s3[4] = {0, 1, 0, 1};
    float e[4];

    source1.x = _mm_loadu_ps(s1);
    source2.x = _mm_loadu_ps(s2);
    source3.x = _mm_loadu_si128((__m128i*) s3);
    u.x = _mm_permute2_ps(source1.x, source2.x, source3.x, ZERO_MATCH);

    for (i = 0; i < 4; ++i) {
        e[i] = sel_and_condzerosp(&s1[i & 0x4], &s2[i & 0x4], s3[i] & 0xf, ZERO_MATCH & 0x3);
    }

    if (check_union128 (u, e))
      abort ();
}
