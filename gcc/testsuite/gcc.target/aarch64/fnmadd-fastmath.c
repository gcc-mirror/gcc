/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math" } */

extern double fma (double, double, double);
extern float fmaf (float, float, float);

double test_fma1 (double x, double y, double z)
{
  return - fma (x, y, z);
}

float test_fma2 (float x, float y, float z)
{
  return - fmaf (x, y, z);
}

/* { dg-final { scan-assembler-times "fnmadd\td\[0-9\]" 1 } } */
/* { dg-final { scan-assembler-times "fnmadd\ts\[0-9\]" 1 } } */

