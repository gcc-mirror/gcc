/* { dg-do compile } */
/* { dg-options "-Ofast -fdump-tree-cplxlower1" } */

double f(double a, double c)
{
  _Complex double d = __builtin_complex (a, 0.0);
  d+=__builtin_complex(0.0, a);
  return __builtin_cabs(d);
}

/* Check that cabs is expanded during complex lowering and there is no sqrt (since it is a constant). */
/* { dg-final { scan-tree-dump-not "__builtin_cabs " "cplxlower1" } } */
/* { dg-final { scan-tree-dump-not "__builtin_sqrt " "cplxlower1" } } */
/* { dg-final { scan-tree-dump-times "ABS_EXPR <" 1 "cplxlower1" } } */


