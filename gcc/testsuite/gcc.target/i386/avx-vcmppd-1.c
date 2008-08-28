/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx -std=c99" } */

#include "avx-check.h"
#include <math.h>

double s1[2]={2134.3343,6678.346};
double s2[2]={41124.234,6678.346};
long long  e[2];

union
{
  double d[2];
  long long ll[2];
}d;

void check(unsigned imm, char *id)
{
    if(checkVl(d.ll, e, 2)){
	printf("mm_cmp_pd(%s: 0x%x) FAILED\n", id, imm);
    }
}

#define CMP(imm, rel)					\
    for (i = 0; i < 2; i++) e[i] = rel ? -1 : 0;	\
    source1 = _mm_loadu_pd(s1);				\
    source2 = _mm_loadu_pd(s2);				\
    dest = _mm_cmp_pd(source1, source2, imm);		\
    _mm_storeu_pd(d.d, dest);			\
    check(imm, "" #imm "");

static void
avx_test ()
{
    __m128d source1, source2, dest;
    int i;

    d.ll[0] = e[0] = 222;
    d.ll[1] = e[1] = -33;
    
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
