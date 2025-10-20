/* PR tree-optimization/122325.  */

/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-cplxlower" } */
/* { dg-require-effective-target double64 } */

__complex double
foo (__complex double a, __complex double b)
{
  return a / b;
}

/* { dg-final { scan-tree-dump-times "__(?:gnu_)?divdc3" 1 "cplxlower1" } } */
