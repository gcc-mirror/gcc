/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-mno-adx -O2" } */
/* { dg-final { scan-assembler "adcq" } } */

#include <x86intrin.h>

volatile unsigned char c;
volatile unsigned long long x, y;
unsigned long long *sum;

void extern
adx_test (void)
{
    c = _addcarryx_u64 (c, x, y, sum);
}
