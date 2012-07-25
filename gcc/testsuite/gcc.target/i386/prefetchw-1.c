/* { dg-do compile } */
/* { dg-options "-mprfchw -O2" } */
/* { dg-final { scan-assembler "\[ \\t\]+prefetchw\[ \\t\]+" } } */

#include <x86intrin.h>

void *p;

void extern
prefetchw__test (void)
{
    _m_prefetchw (p);
}
