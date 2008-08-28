/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx -std=c99" } */

#include "avx-check.h"
#include <math.h>

double s1[4]={2134.3343,6678.346,453.345635,54646.464356};
double s2[4]={41124.234,6678.346,8653.65635,856.43576};
long long e[4];

union
{
  double d[4];
  long long ll[4];
}d;

void check(unsigned imm, char *id)
{
    if(checkVl(d.ll, e, 4)){
	printf("mm256_cmp_pd(%s: 0x%x) FAILED\n", id, imm);
    }
}

#define CMP(imm, rel)					\
    for (i = 0; i < 4; i++) e[i] = rel ? -1 : 0;	\
    source1 = _mm256_loadu_pd(s1);			\
    source2 = _mm256_loadu_pd(s2);			\
    dest = _mm256_cmp_pd(source1, source2, imm);	\
    _mm256_storeu_pd(d.d, dest);		\
    check(imm, "" #imm "");

static void
avx_test ()
{
    __m256d source1, source2, dest;
    int i;
    
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
