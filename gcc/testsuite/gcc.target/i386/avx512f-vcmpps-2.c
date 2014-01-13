/* { dg-do run } */
/* { dg-options "-O2 -mavx512f -std=c99" } */
/* { dg-require-effective-target avx512f } */
/* { dg-require-effective-target c99_runtime } */

#define AVX512F

#include "avx512f-helper.h"

#include <math.h>
#define SIZE (AVX512F_LEN / 32)
#include "avx512f-mask-type.h"

#if AVX512F_LEN == 512
#define CMP(imm, rel)					\
    dst_ref = 0;					\
    for (i = 0; i < 16; i++)				\
    {							\
      dst_ref = (((int) rel) << i) | dst_ref;		\
    }							\
    source1.x = _mm512_loadu_ps(s1);			\
    source2.x = _mm512_loadu_ps(s2);			\
    dst1 = _mm512_cmp_ps_mask(source1.x, source2.x, imm);\
    dst2 = _mm512_mask_cmp_ps_mask(mask, source1.x, source2.x, imm);\
    if (dst_ref != dst1) abort();			\
    if ((dst_ref & mask) != dst2) abort();
#endif

static void
TEST ()
{
    UNION_TYPE (AVX512F_LEN,) source1, source2;
    MASK_TYPE dst1, dst2, dst_ref;
    MASK_TYPE mask = MASK_VALUE;
    int i;
    float s1[16] = {2134.3343, 6678.346, 453.345635, 54646.464,
		    231.23311, 5674.455, 111.111111, 23241.152,
		    123.14811, 1245.124, 244.151353, 53454.141,
		    926.16717, 3733.261, 643.161644, 23514.633};
    float s2[16] = {41124.234, 6678.346, 8653.65635, 856.43576,
		    231.23311, 4646.123, 111.111111, 124.12455,
		    123.14811, 1245.124, 244.151353, 53454.141,
		    2134.3343, 6678.346, 453.345635, 54646.464};

    CMP(_CMP_EQ_OQ, !isunordered(s1[i], s2[i]) && s1[i] == s2[i]);
    CMP(_CMP_LT_OS, !isunordered(s1[i], s2[i]) && s1[i] < s2[i]);
    CMP(_CMP_LE_OS, !isunordered(s1[i], s2[i]) && s1[i] <= s2[i]);
    CMP(_CMP_UNORD_Q, isunordered(s1[i], s2[i]));
    CMP(_CMP_NEQ_UQ, isunordered(s1[i], s2[i]) || s1[i] != s2[i]);
    CMP(_CMP_NLT_US, isunordered(s1[i], s2[i]) || s1[i] >= s2[i]);
    CMP(_CMP_NLE_US, isunordered(s1[i], s2[i]) || s1[i] > s2[i]);
    CMP(_CMP_ORD_Q, !isunordered(s1[i], s2[i]));

    CMP(_CMP_EQ_UQ, isunordered(s1[i], s2[i]) || s1[i] == s2[i]);
    CMP(_CMP_NGE_US, isunordered(s1[i], s2[i]) || s1[i] < s2[i]);
    CMP(_CMP_NGT_US, isunordered(s1[i], s2[i]) || s1[i] <= s2[i]);

    CMP(_CMP_FALSE_OQ, 0);
    CMP(_CMP_NEQ_OQ, !isunordered(s1[i], s2[i]) && s1[i] != s2[i]);
    CMP(_CMP_GE_OS, !isunordered(s1[i], s2[i]) && s1[i] >= s2[i]);
    CMP(_CMP_GT_OS, !isunordered(s1[i], s2[i]) && s1[i] > s2[i]);
    CMP(_CMP_TRUE_UQ, 1);

    CMP(_CMP_EQ_OS, !isunordered(s1[i], s2[i]) && s1[i] == s2[i]);
    CMP(_CMP_LT_OQ, !isunordered(s1[i], s2[i]) && s1[i] < s2[i]);
    CMP(_CMP_LE_OQ, !isunordered(s1[i], s2[i]) && s1[i] <= s2[i]);
    CMP(_CMP_UNORD_S, isunordered(s1[i], s2[i]));
    CMP(_CMP_NEQ_US, isunordered(s1[i], s2[i]) || s1[i] != s2[i]);
    CMP(_CMP_NLT_UQ, isunordered(s1[i], s2[i]) || s1[i] >= s2[i]);
    CMP(_CMP_NLE_UQ, isunordered(s1[i], s2[i]) || s1[i] > s2[i]);
    CMP(_CMP_ORD_S, !isunordered(s1[i], s2[i]));
    CMP(_CMP_EQ_US, isunordered(s1[i], s2[i]) || s1[i] == s2[i]);
    CMP(_CMP_NGE_UQ, isunordered(s1[i], s2[i]) || s1[i] < s2[i]);
    CMP(_CMP_NGT_UQ, isunordered(s1[i], s2[i]) || s1[i] <= s2[i]);
    CMP(_CMP_FALSE_OS, 0);
    CMP(_CMP_NEQ_OS, !isunordered(s1[i], s2[i]) && s1[i] != s2[i]);
    CMP(_CMP_GE_OQ, !isunordered(s1[i], s2[i]) && s1[i] >= s2[i]);
    CMP(_CMP_GT_OQ, !isunordered(s1[i], s2[i]) && s1[i] > s2[i]);
    CMP(_CMP_TRUE_US, 1)
}
