/* { dg-do compile } */
/* { dg-options "-O3 -mlasx" } */
/* { dg-final { scan-assembler "xvmulwev.w.h"  } } */
/* { dg-final { scan-assembler "xvmulwod.w.h"  } } */
/* { dg-final { scan-assembler "xvmulwev.w.hu"  } } */
/* { dg-final { scan-assembler "xvmulwod.w.hu"  } } */

#include <stdint.h>

#define SIZE 1024

void
wide_umul (uint32_t *foo, uint16_t *a, uint16_t *b)
{
  for ( int i = 0; i < SIZE; i++)
    foo[i] = a[i] * b[i];
}

void
wide_smul (int32_t *foo, int16_t *a, int16_t *b)
{
  for ( int i = 0; i < SIZE; i++)
    foo[i]   = a[i] * b[i];
}
