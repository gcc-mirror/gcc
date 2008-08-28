/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx -std=c99" } */

#include "avx-check.h"
#include <math.h>

double s1[] = {2134.3343, 6678.346};
double s2[] = {41124.234, 6678.346};
long long dd[] =  {1, 2}, d[2];
union{long long l[2]; double d[2];} e;

void check(unsigned imm, char *id)
{
    if(checkVl(d, e.l, 2)){
	printf("mm_cmp_sd(%s: 0x%x) FAILED\n", id, imm);
    }
}

#define CMP(imm, rel)					\
    e.l[0] = rel ? -1 : 0;	                        \
    dest = _mm_loadu_pd((double*)dd);	      		\
    source1 = _mm_loadu_pd(s1);				\
    source2 = _mm_loadu_pd(s2);				\
    dest = _mm_cmp_sd(source1, source2, imm);		\
    _mm_storeu_pd((double*) d, dest);			\
    check(imm, "" #imm "");

static void
avx_test ()
{
    __m128d source1, source2, dest;

    e.d[1] = s1[1];
    
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
    CMP(_CMP_TRUE_US, 1);
}
