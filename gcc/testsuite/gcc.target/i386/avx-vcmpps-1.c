/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx -std=c99" } */

#include "avx-check.h"
#include <math.h>

float s1[4]={2134.3343,6678.346,453.345635,54646.464356};
float s2[4]={41124.234,6678.346,8653.65635,856.43576};
int e[4];

union
{
  float f[4];
  int   i[4];
}d;

void check(unsigned imm, char *id)
{
    if(checkVi(d.i, e, 4)){
	printf("mm_cmp_ps(0x%x, %s) FAILED\n", imm, id);
    }
}

static void
avx_test ()
{
    __m128 source1, source2, dest;
    int i;

#define CMP(imm, rel)					\
    for (i = 0; i < 4; i++) e[i] = rel ? -1 : 0;	\
    source1 = _mm_loadu_ps(s1);				\
    source2 = _mm_loadu_ps(s2);				\
    dest = _mm_cmp_ps(source1, source2, imm);		\
    _mm_storeu_ps(d.f, dest);			\
    check(imm, "" #imm "");

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
    CMP(_CMP_TRUE_US, 1);
}
