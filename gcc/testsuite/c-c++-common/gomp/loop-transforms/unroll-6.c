/* { dg-additional-options "--param=omp-unroll-default-factor=100" }
 * { dg-additional-options "-fdump-tree-omp_transform_loops -fopt-info-omp-optimized-missed" }
 * { dg-additional-options "-fdump-tree-original" } */

extern void dummy (int);

void
test1 ()
{
  int i;
#pragma omp unroll /* { dg-optimized {added 'partial\(100\)' clause to 'omp unroll' directive} } */
  for (int i = 0; i < 100; i++)
    dummy (i);
}

/* Loop should be unrolled 5 times and the internal representation should be lowered
 * { dg-final { scan-tree-dump "#pragma omp loop_transform unroll_none" "original" } }
 * { dg-final { scan-tree-dump-not "#pragma omp" "omp_transform_loops" } }
 * { dg-final { scan-tree-dump-times "dummy" 100 "omp_transform_loops" } }
 * { dg-final { scan-tree-dump-times {if \(i < .+?.+goto.+else goto.*?$} 1 "omp_transform_loops" } } */
