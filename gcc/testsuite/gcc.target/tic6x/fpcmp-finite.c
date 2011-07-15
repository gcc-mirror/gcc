/* { dg-do compile } */
/* { dg-require-effective-target ti_c67x } */
/* { dg-options "-O2 -ffinite-math-only" } */
/* { dg-final { scan-assembler-not "cmpeq" } } */

double gedf (double x, double y)
{
  return x >= y;
}

double ledf (double x, double y)
{
  return x <= y;
}

float gesf (float x, float y)
{
  return x >= y;
}

float lesf (float x, float y)
{
  return x <= y;
}
