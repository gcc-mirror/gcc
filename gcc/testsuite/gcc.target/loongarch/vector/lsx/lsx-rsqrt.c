/* { dg-do compile } */
/* { dg-options "-O2 -mlsx -ffast-math" } */
/* { dg-final { scan-assembler "vfrsqrt.s" } } */
/* { dg-final { scan-assembler "vfrsqrt.d" } } */

extern float sqrtf (float);

float a[4], b[4];

void
foo1(void)
{
  for (int i = 0; i < 4; i++)
    a[i] = 1 / sqrtf (b[i]);
}

extern double sqrt (double);

double da[2], db[2];

void
foo2(void)
{
  for (int i = 0; i < 2; i++)
    da[i] = 1 / sqrt (db[i]);
}
