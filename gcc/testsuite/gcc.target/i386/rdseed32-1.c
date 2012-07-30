/* { dg-do compile } */
/* { dg-options "-mrdseed -O2" } */
/* { dg-final { scan-assembler "rdseed\[ \\t\]+" } } */

#include <x86intrin.h>

void extern
rdseed_test (unsigned int *p)
{
    volatile int r;
    r = _rdseed32_step (p);
}

