/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-mrdseed -O2" } */
/* { dg-final { scan-assembler "rdseed\[ \\t\]+" } } */

#include <x86intrin.h>

void extern
rdseed_test (unsigned long long *p)
{
    volatile int r;
    r = _rdseed64_step (p);
}

