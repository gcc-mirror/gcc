/* { dg-do compile } */
/* { dg-options "-O1" } */

#include <stdint.h>

double
f1 (uint16_t x)
{
  return (double)(float)x;
}

float
f2 (uint16_t x)
{
  return (float)(double)x;
}

/* { dg-final { scan-assembler-not "fcvt" } } */
