/* { dg-do compile } */
/* { dg-options "-O2 -mlasx -ffast-math" } */
/* { dg-final { scan-assembler "xvfrsqrt.s" } } */
/* { dg-final { scan-assembler "xvfrsqrt.d" } } */

extern float sqrtf (float);

float a[8], b[8];

void
foo1(void)
{
  for (int i = 0; i < 8; i++)
    a[i] = 1 / sqrtf (b[i]);
}

extern double sqrt (double);

double da[4], db[4];

void
foo2(void)
{
  for (int i = 0; i < 4; i++)
    da[i] = 1 / sqrt (db[i]);
}
