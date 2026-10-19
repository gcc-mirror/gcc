/* { dg-do compile } */
/* { dg-options "-O2 -fsignaling-nans -fdump-tree-optimized" } */
/* { dg-add-options ieee } */

double foo(double x)
{
  return (x * 1.0) * 1.0;
}

/* { dg-final { scan-tree-dump-times " \\* " 1 "optimized" } } */

