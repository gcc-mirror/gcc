/* { dg-additional-options "-fdump-tree-omp_transform_loops -fopt-info-omp-optimized-missed" }
 * { dg-additional-options "-fdump-tree-original" } */

extern void dummy (int);

void
test1 ()
{
  int i;
#pragma omp unroll partial  /* { dg-optimized {'partial' clause without unrolling factor turned into 'partial\(5\)' clause} } */
  for (int i = 0; i < 100; i++)
    dummy (i);
}

/* Loop should be unrolled 5 times and the internal representation should be lowered
 * { dg-final { scan-tree-dump "#pragma omp loop_transform" "original" } }
 * { dg-final { scan-tree-dump-not "#pragma omp" "omp_transform_loops" } }
 * { dg-final { scan-tree-dump-times "dummy" 5 "omp_transform_loops" } }
 * { dg-final { scan-tree-dump-times {if \(i < .+?.+goto.+else goto.*?$} 1 "omp_transform_loops" } } */
