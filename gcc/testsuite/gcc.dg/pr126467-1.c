/* PR tree-optimization/126467 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-add-options ieee } */

/* 0.0 - x is not -x.
   For x == +0.0, +0.0 - +0.0 is +0.0, but -x is -0.0.
   Likewise fabs/negate preserve a NaN's payload but
   subtraction doesn't.  */

double foo (double x)
{
  return 0.0 - x;
}

double bar (double y)
{
  return 0.0 - __builtin_fabs (y);
}

/* { dg-final { scan-tree-dump-times " \\- " 2 "optimized" } } */
