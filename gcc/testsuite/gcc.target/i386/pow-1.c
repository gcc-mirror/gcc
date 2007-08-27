/* { dg-do compile } */
/* { dg-options "-O -ffast-math" } */

double test1 (double x)
{
  return __builtin_pow (x, 1./2.);
}

double test2 (double x)
{
  return __builtin_pow (x, 3./2.);
}

double test3 (double x)
{
  return __builtin_pow (x, 5./2.);
}

double test4 (double x)
{
  return __builtin_pow (x, -5./2.);
}

/* { dg-final { scan-assembler-not "call\[ \t\]*pow" } } */
