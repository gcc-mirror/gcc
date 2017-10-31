/* { dg-do run } */
/* { dg-options "-O2 -mavx512f -std=c99" } */
/* { dg-require-effective-target avx512f } */
/* { dg-require-effective-target c99_runtime } */

#define AVX512F

#include "avx512f-helper.h"

#include <math.h>
#define SIZE (AVX512F_LEN / 64)
#include "avx512f-mask-type.h"

#undef SUF
#undef SSIZE
#undef GEN_CMP
#undef CHECK_CMP

#if AVX512F_LEN == 512
#define SUF(fun) _mm512##fun
#define SSIZE 8

#define GEN_CMP(type)				\
    {						\
    dst3 = _mm512_cmp##type##_pd_mask(source1.x, source2.x);\
    dst4 = _mm512_mask_cmp##type##_pd_mask(mask, source1.x, source2.x);\
    if (dst3 != dst1) abort();			\
    if (dst4 != dst2) abort();			\
    }

#define CHECK_CMP(imm)				\
    if (imm == _CMP_EQ_OQ) GEN_CMP(eq)		\
    if (imm == _CMP_LT_OS) GEN_CMP(lt)		\
    if (imm == _CMP_LE_OS) GEN_CMP(le)		\
    if (imm == _CMP_UNORD_Q) GEN_CMP(unord)	\
    if (imm == _CMP_NEQ_UQ) GEN_CMP(neq)	\
    if (imm == _CMP_NLT_US) GEN_CMP(nlt)	\
    if (imm == _CMP_NLE_US) GEN_CMP(nle)	\
    if (imm == _CMP_ORD_Q) GEN_CMP(ord)	

#endif

#if AVX512F_LEN == 256
#define SUF(fun) _mm256##fun
#define SSIZE 4
#define GEN_CMP(type)
#define CHECK_CMP(imm)
#endif

#if AVX512F_LEN == 128
#define SUF(fun) _mm##fun
#define SSIZE 2
#define GEN_CMP(type)
#define CHECK_CMP(imm)
#endif

#undef CMP
#define CMP(imm, rel)					\
    dst_ref = 0;					\
    for (i = 0; i < SSIZE; i++)				\
    {							\
      dst_ref = (((int) rel) << i) | dst_ref;		\
    }							\
    source1.x = SUF(_loadu_pd)(s1);			\
    source2.x = SUF(_loadu_pd)(s2);			\
    dst1 = SUF(_cmp_pd_mask)(source1.x, source2.x, imm);\
    dst2 = SUF(_mask_cmp_pd_mask)(mask, source1.x, source2.x, imm);\
    if (dst_ref != dst1) abort();			\
    if ((dst_ref & mask) != dst2) abort();		\
    CHECK_CMP(imm)

void
TEST ()
{
    UNION_TYPE (AVX512F_LEN, d) source1, source2;
    MASK_TYPE dst1, dst2, dst3, dst4, dst_ref;
    MASK_TYPE mask = MASK_VALUE;
    int i;
    double s1[8]={2134.3343, 6678.346, 453.345635, 54646.464,
		  231.23311, 5674.455, 111.111111, 23241.152};
    double s2[8]={41124.234, 6678.346, 8653.65635, 856.43576,
		  231.23311, 4646.123, 111.111111, 124.12455};

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
