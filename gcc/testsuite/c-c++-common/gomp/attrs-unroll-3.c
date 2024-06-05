/* { dg-do compile { target { c || c++11 } } } */
/* { dg-additional-options "-std=c23" { target c } } */
/* { dg-additional-options "-O2 -fdump-tree-original -fdump-tree-gimple -fdump-tree-cunroll" } */

extern void dummy (int);

void
test1 (void)
{
  [[omp::directive (unroll full)]]
  for (int i = 0; i < 10; i++)
    dummy (i);
}

/* Loop should be removed with 10 copies of the body remaining */
/* { dg-final { scan-tree-dump-times "dummy" 10 "cunroll" } } */
/* { dg-final { scan-tree-dump "#pragma omp unroll" "original" } } */
/* { dg-final { scan-tree-dump-not "#pragma omp" "gimple" } } */
