/* { dg-do compile } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-O3 -mcpu=power7 -mabi=altivec -ffast-math -fno-unroll-loops" } */
/* { dg-final { scan-assembler-times "xvadddp" 1 } } */
/* { dg-final { scan-assembler-times "xvmindp" 1 } } */
/* { dg-final { scan-assembler-times "xvmaxdp" 1 } } */
/* { dg-final { scan-assembler-times "xsadddp" 1 } } */
/* { dg-final { scan-assembler-times "xsmindp" 1 } } */
/* { dg-final { scan-assembler-times "xsmaxdp" 1 } } */
/* { dg-final { scan-assembler-not "xxsldwi" } } */
/* { dg-final { scan-assembler-not "stvx" } } */
/* { dg-final { scan-assembler-not "stxvd2x" } } */
/* { dg-final { scan-assembler-not "stxvw4x" } } */

#include <stddef.h>

#ifndef SIZE
#define SIZE 1024
#endif

double values[SIZE] __attribute__((__aligned__(32)));

double
vector_sum (void)
{
  size_t i;
  double sum = 0.0;

  for (i = 0; i < SIZE; i++)
    sum += values[i];

  return sum;
}

double
vector_min (void)
{
  size_t i;
  double min = values[0];

  for (i = 0; i < SIZE; i++)
    min = __builtin_fmin (min, values[i]);

  return min;
}

double
vector_max (void)
{
  size_t i;
  double max = values[0];

  for (i = 0; i < SIZE; i++)
    max = __builtin_fmax (max, values[i]);

  return max;
}
