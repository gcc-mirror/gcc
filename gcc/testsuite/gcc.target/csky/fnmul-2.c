/* { dg-do compile } */
/* { dg-options "-mcpu=ck810f -mhard-float -O2 -frounding-math" } */

double
fnmuld (double a, double b)
{
  /* { dg-final { scan-assembler "fnegd" } } */
  /* { dg-final { scan-assembler "fmuld" } } */
  return -a * b;
}

float
fnmuls (float a, float b)
{
  /* { dg-final { scan-assembler "fnegs" } } */
  /* { dg-final { scan-assembler "fmuls" } } */
  return -a * b;
}

