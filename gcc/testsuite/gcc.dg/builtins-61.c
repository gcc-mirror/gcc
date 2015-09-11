/* { dg-do compile } */
/* { dg-options "-O -ffast-math -fdump-tree-optimized" } */
/* { dg-require-effective-target c99_runtime } */
/* { dg-require-effective-target libc_has_complex_functions } */

double test1 (double x)
{
  return __real __builtin_cexp(x * (__extension__ 1.0iF));
}

double test2(double x)
{
  return __imag __builtin_cexp((__extension__ 1.0iF) * x);
}

double test3(double x)
{
  _Complex c = __builtin_cexp(x * (__extension__ 1.0iF));
  return __imag c + __real c;
}

double test4(double x, double y)
{
  _Complex c = __builtin_cexp(x);
  x = __builtin_exp (x);
  return x - __real c;
}

/* { dg-final { scan-tree-dump "cexpi" "optimized" } } */
/* { dg-final { scan-tree-dump "sin" "optimized" } } */
/* { dg-final { scan-tree-dump "cos" "optimized" } } */
/* { dg-final { scan-tree-dump "return 0.0" "optimized" } } */
