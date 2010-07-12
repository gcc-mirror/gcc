/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math -msse -mfpmath=sse -mrecip" } */
/* { dg-require-effective-target sse } */

extern float sqrtf (float);

float t1(float a, float b)
{
  return a/sqrtf(b);
}

float t2(float a, float b)
{
  return sqrtf(a/b);
}

float t3(float a)
{
  return sqrtf(a);
}

/* { dg-final { scan-assembler-times "rsqrtss" 3 } } */
