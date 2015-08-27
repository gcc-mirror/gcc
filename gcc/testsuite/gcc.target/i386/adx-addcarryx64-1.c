/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-madx -O2" } */
/* { dg-final { scan-assembler-times "adc\[xq\]" 2 } } */
/* { dg-final { scan-assembler-times "sbbq" 1 } } */

#include <x86intrin.h>

volatile unsigned char c;
volatile unsigned long long x, y;
unsigned long long *sum;

void extern
adx_test (void)
{
    c = _addcarryx_u64 (c, x, y, sum);
    c = _addcarry_u64 (c, x, y, sum);
    c = _subborrow_u64 (c, x, y, sum);
}
