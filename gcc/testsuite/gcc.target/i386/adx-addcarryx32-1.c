/* { dg-do compile } */
/* { dg-options "-madx -O2" } */
/* { dg-final { scan-assembler-times "adc\[xl\]" 2 } } */
/* { dg-final { scan-assembler-times "sbbl" 1 } } */

#include <x86intrin.h>

volatile unsigned char c;
volatile unsigned int x, y;
unsigned int *sum;

void extern
adx_test (void)
{
    c = _addcarryx_u32 (c, x, y, sum);
    c = _addcarry_u32 (c, x, y, sum);
    c = _subborrow_u32 (c, x, y, sum);
}
