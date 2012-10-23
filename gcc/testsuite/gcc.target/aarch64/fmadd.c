/* { dg-do compile } */
/* { dg-options "-O2" } */

extern double fma (double, double, double);
extern float fmaf (float, float, float);

double test_fma1 (double x, double y, double z)
{
  return fma (x, y, z);
}

float test_fma2 (float x, float y, float z)
{
  return fmaf (x, y, z);
}

double test_fnma1 (double x, double y, double z)
{
  return fma (-x, y, z);
}

float test_fnma2 (float x, float y, float z)
{
  return fmaf (-x, y, z);
}

double test_fms1 (double x, double y, double z)
{
  return fma (x, y, -z);
}

float test_fms2 (float x, float y, float z)
{
  return fmaf (x, y, -z);
}

double test_fnms1 (double x, double y, double z)
{
  return fma (-x, y, -z);
}

float test_fnms2 (float x, float y, float z)
{
  return fmaf (-x, y, -z);
}

/* { dg-final { scan-assembler-times "fmadd\td\[0-9\]" 1 } } */
/* { dg-final { scan-assembler-times "fmadd\ts\[0-9\]" 1 } } */
/* { dg-final { scan-assembler-times "fmsub\td\[0-9\]" 1 } } */
/* { dg-final { scan-assembler-times "fmsub\ts\[0-9\]" 1 } } */
/* { dg-final { scan-assembler-times "fnmsub\td\[0-9\]" 1 } } */
/* { dg-final { scan-assembler-times "fnmsub\ts\[0-9\]" 1 } } */
/* { dg-final { scan-assembler-times "fnmadd\td\[0-9\]" 1 } } */
/* { dg-final { scan-assembler-times "fnmadd\ts\[0-9\]" 1 } } */

