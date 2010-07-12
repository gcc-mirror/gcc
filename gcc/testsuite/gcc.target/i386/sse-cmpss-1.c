/* { dg-do run } */
/* { dg-options "-O2 -msse -std=c99" } */
/* { dg-require-effective-target sse } */
/* { dg-require-effective-target c99_runtime } */

#ifndef CHECK_H
#define CHECK_H "sse-check.h"
#endif

#ifndef TEST
#define TEST sse_test
#endif

#include CHECK_H

#include <math.h>

float s1[]={2134.3343, 6678.346, 453.345635, 54646.464356};
float s2[]={41124.234, 6678.346, 8653.65635, 856.43576};
int dd[] = {1, 2, 3, 4};
float d[4];
union{int i[4]; float f[4];} e;

void check(char *id)
{
    if(checkVi((int*)d, e.i, 4)){
	printf("mm_cmp%s_ss FAILED\n", id);
    }
}

static void
TEST ()
{
    __m128 source1, source2, dest;
    int i;

#define CMP(cmp, rel)					\
    e.i[0] = rel ? -1 : 0;	                        \
    dest = _mm_loadu_ps((float*)dd);			\
    source1 = _mm_loadu_ps(s1);				\
    source2 = _mm_loadu_ps(s2);				\
    dest = _mm_cmp##cmp##_ss(source1, source2);		\
    _mm_storeu_ps(d, dest);			        \
    check("" #cmp "");

    for(i = 1; i < 4; i++) e.f[i] = s1[i];
    
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
