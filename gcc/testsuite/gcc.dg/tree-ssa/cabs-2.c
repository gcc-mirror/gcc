/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-cplxlower1" } */


double f(_Complex double a)
{
  a+= 1.0f;
  return __builtin_cabs(a);
}

/* Check that cabs is not expanded during complex lowering. */
/* { dg-final { scan-tree-dump "__builtin_cabs " "cplxlower1" } } */
/* { dg-final { scan-tree-dump-not "__builtin_sqrt " "cplxlower1" } } */
