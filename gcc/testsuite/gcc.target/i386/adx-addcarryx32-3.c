/* { dg-do compile } */
/* { dg-options "-mno-adx -O2" } */
/* { dg-final { scan-assembler "adcl" } } */

#include <x86intrin.h>

volatile unsigned char c;
volatile unsigned int x, y;
unsigned int *sum;

void extern
adx_test (void)
{
    c = _addcarryx_u32 (c, x, y, sum);
}
