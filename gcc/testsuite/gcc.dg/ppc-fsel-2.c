/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-options "-O -mpowerpc-gfxopt -g0 -ffinite-math-only" } */
/* { dg-final { scan-assembler-not "^L" } } */

/* Every single one of these should be compiled into straight-line
   code using fsel (or, in a few cases, hardwired to 'true' or
   'false'), no branches anywhere.  */

double
test_isunordered(double x, double y, double a, double b)
{
  return __builtin_isunordered(x, y) ? a : b;
}

double
test_not_isunordered(double x, double y, double a, double b)
{
  return !__builtin_isunordered(x, y) ? a : b;
}

double
test_isless(double x, double y, double a, double b)
{
  return __builtin_isless(x, y) ? a : b;
}

double
test_not_isless(double x, double y, double a, double b)
{
  return !__builtin_isless(x, y) ? a : b;
}

double
test_islessequal(double x, double y, double a, double b)
{
  return __builtin_islessequal(x, y) ? a : b;
}

double
test_not_islessequal(double x, double y, double a, double b)
{
  return !__builtin_islessequal(x, y) ? a : b;
}

double
test_isgreater(double x, double y, double a, double b)
{
  return __builtin_isgreater(x, y) ? a : b;
}

double
test_not_isgreater(double x, double y, double a, double b)
{
  return !__builtin_isgreater(x, y) ? a : b;
}

double
test_isgreaterequal(double x, double y, double a, double b)
{
  return __builtin_isgreaterequal(x, y) ? a : b;
}

double
test_not_isgreaterequal(double x, double y, double a, double b)
{
  return !__builtin_isgreaterequal(x, y) ? a : b;
}

double
test_islessgreater(double x, double y, double a, double b)
{
  return __builtin_islessgreater(x, y) ? a : b;
}

double
test_not_islessgreater(double x, double y, double a, double b)
{
  return !__builtin_islessgreater(x, y) ? a : b;
}

