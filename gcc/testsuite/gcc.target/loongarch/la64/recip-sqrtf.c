/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math -mrecip -mfrecipe" } */
/* { dg-final { scan-assembler-times "frsqrte.s" 3 } } */

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
