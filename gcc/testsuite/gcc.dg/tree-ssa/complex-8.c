/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-cplxlower1-raw" } */

_Complex double f(double a, double c)
{
  _Complex double d = __builtin_complex (a, a);
  d+=__builtin_complex(c, c);
  return d;
}

/* There should only be one plus as (a+c) is still (a+c) */
/* { dg-final { scan-tree-dump-times "plus_expr, " 1 "cplxlower1" } } */
