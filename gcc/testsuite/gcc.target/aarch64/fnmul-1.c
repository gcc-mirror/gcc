/* { dg-do compile } */
/* { dg-options "-O2" } */

double
foo_d (double a, double b)
{
  /* { dg-final { scan-assembler "fnmul\\td\[0-9\]+, d\[0-9\]+, d\[0-9\]+" } } */
  return -a * b;
}

float
foo_s (float a, float b)
{
  /* { dg-final { scan-assembler "fnmul\\ts\[0-9\]+, s\[0-9\]+, s\[0-9\]+" } } */
  return -a * b;
}
