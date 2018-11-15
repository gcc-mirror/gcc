/* { dg-do compile } */
/* { dg-options "-mcpu=ck810f -mhard-float -O2" } */

double
fnmuld (double a, double b)
{
  /* { dg-final { scan-assembler "fnmuld" } } */
  return -(a * b);
}

float
fnmuls (float a, float b)
{
  /* { dg-final { scan-assembler "fnmuls" } } */
  return -(a * b);
}

