/* Verify that gimple-level NRV is occurring even for SSA_NAMEs.  *./
/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-optimized" } */
/* { dg-require-effective-target ia32 } */

_Complex double foo (_Complex double x)
{
  return __builtin_cexp (x);
}

/* { dg-final { scan-tree-dump-times "return slot optimization" 1 "optimized" } } */
