/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-cplxlower1" } */

double f(double a, double c)
{
  _Complex double b = a;
  b+= c;
  return __builtin_cabs(b);
}

double f1(double a, double c)
{
  _Complex double b = __builtin_complex(0.0, a);
  b+= __builtin_complex(0.0, c);
  return __builtin_cabs(b);
}

/* Check that cabs is expanded into ABS for both f and f1 during complex lowering. */
/* { dg-final { scan-tree-dump-not "__builtin_cabs " "cplxlower1" } } */
/* { dg-final { scan-tree-dump-not "__builtin_sqrt " "cplxlower1" } } */
/* { dg-final { scan-tree-dump-times "ABS_EXPR <" 2 "cplxlower1" } } */

