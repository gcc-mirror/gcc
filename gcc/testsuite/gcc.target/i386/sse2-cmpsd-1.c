/* { dg-do run } */
/* { dg-options "-O2 -msse2 -std=c99" } */
/* { dg-require-effective-target sse2 } */
/* { dg-require-effective-target c99_runtime } */

#ifndef CHECK_H
#define CHECK_H "sse2-check.h"
#endif

#ifndef TEST
#define TEST sse2_test
#endif

#include CHECK_H

#include <math.h>

double s1[] = {2134.3343, 6678.346};
double s2[] = {41124.234, 6678.346};
long long dd[] =  {1, 2}, d[2];
union{long long l[2]; double d[2];} e;

void check(char *id)
{
    if(checkVl(d, e.l, 2)){
	printf("mm_cmp%s_sd FAILED\n", id);
    }
}

#define CMP(cmp, rel)					\
    e.l[0] = rel ? -1 : 0;	                        \
    dest = _mm_loadu_pd((double*)dd);	      		\
    source1 = _mm_loadu_pd(s1);				\
    source2 = _mm_loadu_pd(s2);				\
    dest = _mm_cmp##cmp##_sd(source1, source2);		\
    _mm_storeu_pd((double*) d, dest);			\
    check("" #cmp "");

static void
TEST ()
{
    __m128d source1, source2, dest;

    e.d[1] = s1[1];
    
    CMP(eq, !isunordered(s1[0], s2[0]) && s1[0] == s2[0]);
    CMP(lt, !isunordered(s1[0], s2[0]) && s1[0] < s2[0]);
    CMP(le, !isunordered(s1[0], s2[0]) && s1[0] <= s2[0]);
    CMP(unord, isunordered(s1[0], s2[0]));
    CMP(neq, isunordered(s1[0], s2[0]) || s1[0] != s2[0]);
    CMP(nlt, isunordered(s1[0], s2[0]) || s1[0] >= s2[0]);
    CMP(nle, isunordered(s1[0], s2[0]) || s1[0] > s2[0]);
    CMP(ord, !isunordered(s1[0], s2[0]));

    CMP(ge, isunordered(s1[0], s2[0]) || s1[0] >= s2[0]);
    CMP(gt, isunordered(s1[0], s2[0]) || s1[0] > s2[0]);
    CMP(nge, !isunordered(s1[0], s2[0]) && s1[0] < s2[0]);
    CMP(ngt, !isunordered(s1[0], s2[0]) && s1[0] <= s2[0]);
}
