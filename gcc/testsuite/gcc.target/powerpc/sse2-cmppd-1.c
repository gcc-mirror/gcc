/* { dg-do run } */
/* { dg-options "-O3 -mpower8-vector -Wno-psabi" } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target p8vector_hw } */

#ifndef CHECK_H
#define CHECK_H "sse2-check.h"
#endif

#include CHECK_H

#ifndef TEST
#define TEST sse2_test_cmp_pd_1
#endif

#include <emmintrin.h>
#include <math.h>

double ps1[] = {2134.3343, 6678.346};
double ps2[] = {41124.234, 6678.346};
long long pdd[] =  {1, 2}, pd[2];
union{long long l[2]; double d[2];} pe;

void pd_check(char *id, __m128d dst)
{
    __v2di dest = (__v2di)dst;

    if(checkVl(pd, pe.l, 2))
    {
        printf("mm_cmp%s_pd FAILED\n", id);
	printf("dst [%lld, %lld], e.l[%lld, %lld]\n", 
		dest[0], dest[1], pe.l[0], pe.l[1]);
    }
}

#define CMP(cmp, rel0, rel1)					\
    pe.l[0] = rel0 ? -1 : 0;	                        \
    pe.l[1] = rel1 ? -1 : 0;	                        \
    dest = _mm_loadu_pd((double*)pdd);	      		\
    source1 = _mm_loadu_pd(ps1);				\
    source2 = _mm_loadu_pd(ps2);				\
    dest = _mm_cmp##cmp##_pd(source1, source2);		\
    _mm_storeu_pd((double*) pd, dest);			\
    pd_check("" #cmp "", dest);

static void
TEST ()
{
    __m128d source1, source2, dest;

    CMP(eq, !isunordered(ps1[0], ps2[0]) && ps1[0] == ps2[0],
    		!isunordered(ps1[1], ps2[1]) && ps1[1] == ps2[1]);
    CMP(lt, !isunordered(ps1[0], ps2[0]) && ps1[0] < ps2[0],
    		!isunordered(ps1[1], ps2[1]) && ps1[1] < ps2[1]);
    CMP(le, !isunordered(ps1[0], ps2[0]) && ps1[0] <= ps2[0],
    		!isunordered(ps1[1], ps2[1]) && ps1[1] <= ps2[1]);
    CMP(unord, isunordered(ps1[0], ps2[0]),
    		isunordered(ps1[1], ps2[1]));
    CMP(neq, isunordered(ps1[0], ps2[0]) || ps1[0] != ps2[0],
    		isunordered(ps1[1], ps2[1]) || ps1[1] != ps2[01]);
    CMP(nlt, isunordered(ps1[0], ps2[0]) || ps1[0] >= ps2[0],
    		isunordered(ps1[1], ps2[1]) || ps1[1] >= ps2[1]);
    CMP(nle, isunordered(ps1[0], ps2[0]) || ps1[0] > ps2[0],
    		isunordered(ps1[1], ps2[1]) || ps1[1] > ps2[1]);
    CMP(ord, !isunordered(ps1[0], ps2[0]),
    		!isunordered(ps1[1], ps2[1]));

    CMP(ge, isunordered(ps1[0], ps2[0]) || ps1[0] >= ps2[0],
    		isunordered(ps1[1], ps2[1]) || ps1[1] >= ps2[1]);
    CMP(gt, isunordered(ps1[0], ps2[0]) || ps1[0] > ps2[0],
    		isunordered(ps1[1], ps2[1]) || ps1[1] > ps2[1]);
    CMP(nge, !isunordered(ps1[0], ps2[0]) && ps1[0] < ps2[0],
    		!isunordered(ps1[1], ps2[1]) && ps1[1] < ps2[1]);
    CMP(ngt, !isunordered(ps1[0], ps2[0]) && ps1[0] <= ps2[0],
    		!isunordered(ps1[1], ps2[1]) && ps1[1] <= ps2[1]);
}
