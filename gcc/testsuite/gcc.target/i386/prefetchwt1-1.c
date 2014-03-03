/* { dg-do compile } */
/* { dg-options "-mprefetchwt1 -O2" } */
/* { dg-final { scan-assembler "\[ \\t\]+prefetchwt1\[ \\t\]+" } } */

#include <x86intrin.h>

void *p;

void extern
prefetchw__test (void)
{
    _mm_prefetch (p, _MM_HINT_ET1);
}

