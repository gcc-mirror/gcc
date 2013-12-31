/* { dg-do run } */
/* { dg-require-effective-target avx512f } */
/* { dg-require-effective-target c99_runtime } */
/* { dg-options "-O2 -mavx512f -std=c99" } */

#include "avx512f-check.h"
#include <math.h>

double s1[2] = {2134.3343, 6678.346};
double s2[2] = {1485.1288, 6678.346};

__mmask8 dst_ref;

#define CMP(imm, rel)					\
    dst_ref = 0;					\
    dst_ref = ((int) rel) | dst_ref;			\
    source1 = _mm_loadu_pd(s1);				\
    source2 = _mm_loadu_pd(s2);				\
    dst = _mm_cmp_sd_mask(source1, source2, imm);	\
    dst2 = _mm_mask_cmp_sd_mask(mask, source1, source2, imm);\
    if (dst_ref != dst) abort();			\
    if ((dst_ref & mask) != dst2) abort();

static void
avx512f_test ()
{
    __m128d source1, source2;
    __mmask8 dst, dst2, mask;
    mask = 1;
    int i;

    CMP(_CMP_EQ_OQ, !isunordered(s1[0], s2[0]) && s1[0] == s2[0]);
    CMP(_CMP_LT_OS, !isunordered(s1[0], s2[0]) && s1[0] < s2[0]);
    CMP(_CMP_LE_OS, !isunordered(s1[0], s2[0]) && s1[0] <= s2[0]);
    CMP(_CMP_UNORD_Q, isunordered(s1[0], s2[0]));
    CMP(_CMP_NEQ_UQ, isunordered(s1[0], s2[0]) || s1[0] != s2[0]);
    CMP(_CMP_NLT_US, isunordered(s1[0], s2[0]) || s1[0] >= s2[0]);
    CMP(_CMP_NLE_US, isunordered(s1[0], s2[0]) || s1[0] > s2[0]);
    CMP(_CMP_ORD_Q, !isunordered(s1[0], s2[0]));

    CMP(_CMP_EQ_UQ, isunordered(s1[0], s2[0]) || s1[0] == s2[0]);
    CMP(_CMP_NGE_US, isunordered(s1[0], s2[0]) || s1[0] < s2[0]);
    CMP(_CMP_NGT_US, isunordered(s1[0], s2[0]) || s1[0] <= s2[0]);

    CMP(_CMP_FALSE_OQ, 0);
    CMP(_CMP_NEQ_OQ, !isunordered(s1[0], s2[0]) && s1[0] != s2[0]);
    CMP(_CMP_GE_OS, !isunordered(s1[0], s2[0]) && s1[0] >= s2[0]);
    CMP(_CMP_GT_OS, !isunordered(s1[0], s2[0]) && s1[0] > s2[0]);
    CMP(_CMP_TRUE_UQ, 1);

    CMP(_CMP_EQ_OS, !isunordered(s1[0], s2[0]) && s1[0] == s2[0]);
    CMP(_CMP_LT_OQ, !isunordered(s1[0], s2[0]) && s1[0] < s2[0]);
    CMP(_CMP_LE_OQ, !isunordered(s1[0], s2[0]) && s1[0] <= s2[0]);
    CMP(_CMP_UNORD_S, isunordered(s1[0], s2[0]));
    CMP(_CMP_NEQ_US, isunordered(s1[0], s2[0]) || s1[0] != s2[0]);
    CMP(_CMP_NLT_UQ, isunordered(s1[0], s2[0]) || s1[0] >= s2[0]);
    CMP(_CMP_NLE_UQ, isunordered(s1[0], s2[0]) || s1[0] > s2[0]);
    CMP(_CMP_ORD_S, !isunordered(s1[0], s2[0]));
    CMP(_CMP_EQ_US, isunordered(s1[0], s2[0]) || s1[0] == s2[0]);
    CMP(_CMP_NGE_UQ, isunordered(s1[0], s2[0]) || s1[0] < s2[0]);
    CMP(_CMP_NGT_UQ, isunordered(s1[0], s2[0]) || s1[0] <= s2[0]);
    CMP(_CMP_FALSE_OS, 0);
    CMP(_CMP_NEQ_OS, !isunordered(s1[0], s2[0]) && s1[0] != s2[0]);
    CMP(_CMP_GE_OQ, !isunordered(s1[0], s2[0]) && s1[0] >= s2[0]);
    CMP(_CMP_GT_OQ, !isunordered(s1[0], s2[0]) && s1[0] > s2[0]);
    CMP(_CMP_TRUE_US, 1)
}
