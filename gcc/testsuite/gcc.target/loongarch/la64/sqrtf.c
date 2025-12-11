/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math -mrecip -mfrecipe -fno-unsafe-math-optimizations" } */
/* { dg-final { scan-assembler-times "fsqrt.s" 3 } } */
/* { dg-final { scan-assembler-not "frsqrte.s" } } */

extern float sqrtf (float);

float
foo1 (float a, float b)
{
  return a/sqrtf(b);
}

float
foo2 (float a, float b)
{
  return sqrtf(a/b);
}

float
foo3 (float a)
{
  return sqrtf(a);
}
