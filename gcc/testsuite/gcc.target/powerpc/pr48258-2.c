/* { dg-do compile } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-O3 -mdejagnu-cpu=power7 -mabi=altivec -ffast-math" } */
/* { dg-final { scan-assembler "xvadddp" } } */
/* { dg-final { scan-assembler "xvmindp" } } */
/* { dg-final { scan-assembler "xvmaxdp" } } */

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
