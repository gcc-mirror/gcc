/* { dg-do run } */
/* { dg-options "-O3 -mvsx" } */
/* { dg-additional-options "-mdejagnu-cpu=power8" { target { ! has_arch_pwr8 } } } */
/* { dg-require-effective-target p8vector_hw } */

#define NO_WARN_X86_INTRINSICS 1

#ifndef CHECK_H
#define CHECK_H "sse-check.h"
#endif

#include CHECK_H

#ifndef TEST
#define TEST sse_test_cmpss_1
#endif

#include <math.h>

static float s1[]={2134.3343, 6678.346, 453.345635, 54646.464356};
static float s2[]={41124.234, 6678.346, 8653.65635, 856.43576};
static int dd[] = {1, 2, 3, 4};
static float d[4];
static union{int i[4]; float f[4];} e;

void sse_cmp_check(char *id)
{
  int *pd = (int*)&d;
    if(checkVi((int*)d, e.i, 4)){
	printf("mm_cmp%s_ss FAILED\n", id);
	printf("\td %f %s %f]\n",
	       s1[0] , id, s2[1]);
	printf("\td [%x,%x,%x,%x]\n",
	       pd[0], pd[1], pd[2], pd[3]);
	printf("\te [%x,%x,%x,%x]\n",
	       e.i[0], e.i[1], e.i[2], e.i[3]);
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
    sse_cmp_check("" #cmp "");

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
