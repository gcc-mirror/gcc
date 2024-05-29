/* { dg-do compile } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-skip-if "" { powerpc*le-*-* } } */
/* { dg-options "-O3 -mdejagnu-cpu=power7 -mabi=altivec -ffast-math -fno-unroll-loops" } */
/* { dg-require-effective-target powerpc_vsx } */
/* { dg-final { scan-assembler-times "xvaddsp" 3 } } */
/* { dg-final { scan-assembler-times "xvminsp" 3 } } */
/* { dg-final { scan-assembler-times "xvmaxsp" 3 } } */
/* { dg-final { scan-assembler-times "xxsldwi" 6 } } */
/* { dg-final { scan-assembler-times "xscvspdp" 3 } } */
/* { dg-final { scan-assembler-not "stvewx" } } */
/* { dg-final { scan-assembler-not "stvx" } } */
/* { dg-final { scan-assembler-not "stxvd2x" } } */
/* { dg-final { scan-assembler-not "stxvw4x" } } */

#include <stddef.h>

#ifndef SIZE
#define SIZE 1024
#endif

float values[SIZE] __attribute__((__aligned__(32)));

float
vector_sum (void)
{
  size_t i;
  float sum = 0.0f;

  for (i = 0; i < SIZE; i++)
    sum += values[i];

  return sum;
}

float
vector_min (void)
{
  size_t i;
  float min = values[0];

  for (i = 0; i < SIZE; i++)
    min = __builtin_fminf (min, values[i]);

  return min;
}

float
vector_max (void)
{
  size_t i;
  float max = values[0];

  for (i = 0; i < SIZE; i++)
    max = __builtin_fmaxf (max, values[i]);

  return max;
}
