/* { dg-do compile } */
/* { dg-options "-O3 -mlasx" } */
/* { dg-final { scan-assembler "xvaddwev.w.h"  } } */
/* { dg-final { scan-assembler "xvaddwod.w.h"  } } */
/* { dg-final { scan-assembler "xvaddwev.w.hu"  } } */
/* { dg-final { scan-assembler "xvaddwod.w.hu"  } } */

#include <stdint.h>

#define SIZE 1024

void
wide_uadd (uint32_t *foo, uint16_t *a, uint16_t *b)
{
  for ( int i = 0; i < SIZE; i++)
    foo[i]   = a[i] + b[i];
}

void
wide_sadd (int32_t *foo, int16_t *a, int16_t *b)
{
  for ( int i = 0; i < SIZE; i++)
    foo[i]   = a[i] + b[i];
}
