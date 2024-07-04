/* { dg-do compile { target sqrt_insn } } */
/* { dg-options "-Ofast -fdump-tree-cplxlower1" } */
/* { dg-add-options sqrt_insn } */


double f(_Complex double a)
{
  a+= 1.0f;
  return __builtin_cabs(a);
}

/* Check that cabs is expanded during complex lowering. */
/* { dg-final { scan-tree-dump-not "__builtin_cabs " "cplxlower1" } } */
/* { dg-final { scan-tree-dump "__builtin_sqrt " "cplxlower1" } } */
