/* PR target/24055 */
/* Testcase reduced by Serge Belyshev */
/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math" } */

extern double rint(double);

void foo_1 (int *p, double x)
{
  *p = rint (x);
}

void foo_2 (long long *p, double x)
{
  *p = rint (x);
}

int foo_3 (double x)
{
  return rint (x);
}

long long foo_4 (double x)
{
  return rint (x);
}
