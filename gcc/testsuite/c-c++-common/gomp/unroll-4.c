/* { dg-additional-options "-O2 -fdump-tree-original -fdump-tree-gimple" } */

extern void dummy (int);

void
test1 (void)
{
  int i;
#pragma omp unroll
  for (int i = 0; i < 96; i++)
    dummy (i);
}

/* GCC unrolls the loop 8 times, but only during RTL optimizations.  */
/* { dg-final { scan-tree-dump "#pragma omp unroll" "original" } } */
/* { dg-final { scan-tree-dump-not "#pragma omp" "gimple" } } */
/* { dg-final { scan-tree-dump-times "dummy" 1 "gimple" } } */
/* { dg-final { scan-assembler-times "dummy" 8 { xfail hppa*-*-hpux* } } } */
