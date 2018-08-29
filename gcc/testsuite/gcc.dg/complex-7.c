/* PR tree-optimization/70291.  */

/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-cplxlower" } */

__complex double
foo (__complex double a, __complex double b)
{
  return a * b;
}

/* { dg-final { scan-tree-dump-times "unord" 1 "cplxlower1" } } */
/* { dg-final { scan-tree-dump-times "__muldc3" 1 "cplxlower1" } } */
