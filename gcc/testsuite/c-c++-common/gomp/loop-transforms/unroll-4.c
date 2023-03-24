/* { dg-additional-options "-fdump-tree-omp_transform_loops" }
 * { dg-additional-options "-fdump-tree-original" } */

extern void dummy (int);

void
test1 ()
{
  int i;
#pragma omp unroll
  for (int i = 0; i < 100; i++)
    dummy (i);
}

/* Loop should not be unrolled, but the internal representation should be lowered
 * { dg-final { scan-tree-dump "#pragma omp loop_transform" "original" } }
 * { dg-final { scan-tree-dump-not "#pragma omp" "omp_transform_loops" } }
 * { dg-final { scan-tree-dump-times "dummy" 1 "omp_transform_loops" } }
 * { dg-final { scan-tree-dump-times {if \(i < .+?.+goto.+else goto.*?$} 1 "omp_transform_loops" } } */
