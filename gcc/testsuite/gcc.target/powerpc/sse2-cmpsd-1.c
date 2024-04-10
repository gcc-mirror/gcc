/* { dg-do run } */
/* { dg-options "-O3 -mvsx -Wno-psabi" } */
/* { dg-additional-options "-mdejagnu-cpu=power8" { target { ! has_arch_pwr8 } } } */
/* { dg-require-effective-target p8vector_hw } */

#ifndef CHECK_H
#define CHECK_H "sse2-check.h"
#endif

#include CHECK_H

#ifndef TEST
#define TEST sse2_test_cmp_sd_1
#endif

#include <emmintrin.h>
#include <math.h>

double s1[] = {2134.3343, 6678.346};
double s2[] = {41124.234, 6678.346};
long long dd[] =  {1, 2}, d[2];
union{long long l[2]; double d[2];} e;

void check(char *id, __m128d dst)
{
   __v2di dest = (__v2di)dst;

   if(checkVl(d, e.l, 2))
    {
      printf("mm_cmp%s_sd FAILED\n", id);
      printf("dst [%lld, %lld], e.l[%lld]\n",
             dest[0], dest[1], e.l[0]);
    }
}

#define CMP(cmp, rel)					\
    e.l[0] = rel ? -1 : 0;	                        \
    dest = _mm_loadu_pd((double*)dd);	      		\
    source1 = _mm_loadu_pd(s1);				\
    source2 = _mm_loadu_pd(s2);				\
    dest = _mm_cmp##cmp##_sd(source1, source2);	\
    _mm_storeu_pd((double*) d, dest);			\
    check("" #cmp "", dest);

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
