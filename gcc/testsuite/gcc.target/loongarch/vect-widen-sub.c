/* { dg-do compile } */
/* { dg-options "-O3 -mlasx" } */
/* { dg-final { scan-assembler "xvsubwev.w.h"  } } */
/* { dg-final { scan-assembler "xvsubwod.w.h"  } } */
/* { dg-final { scan-assembler "xvsubwev.w.hu"  } } */
/* { dg-final { scan-assembler "xvsubwod.w.hu"  } } */

#include <stdint.h>

#define SIZE 1024

void
wide_usub (uint32_t *foo, uint16_t *a, uint16_t *b)
{
  for ( int i = 0; i < SIZE; i++)
    foo[i]   = a[i] - b[i];
}

void
wide_ssub (int32_t *foo, int16_t *a, int16_t *b)
{
  for ( int i = 0; i < SIZE; i++)
    foo[i]   = a[i] - b[i];
}
