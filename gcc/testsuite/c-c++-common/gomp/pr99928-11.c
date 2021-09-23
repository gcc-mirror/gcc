/* PR middle-end/99928 */
/* { dg-do compile } */
/* { dg-options "-fopenmp -fdump-tree-gimple" } */

int r00, r01, r02;

void
bar (void)
{
  /* { dg-final { scan-tree-dump-not "omp master\[^\n\r]*in_reduction\\(\\+:r00\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*in_reduction\\(\\+:r00\\)" "gimple" } } */
  #pragma omp master taskloop in_reduction(+:r00)
  for (int i = 0; i < 64; i++)
    r00++;
  /* { dg-final { scan-tree-dump-not "omp master\[^\n\r]*in_reduction\\(\\+:r01\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*in_reduction\\(\\+:r01\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp simd\[^\n\r]*in_reduction\\(\\+:r01\\)" "gimple" } } */
  #pragma omp master taskloop simd in_reduction(+:r01)
  for (int i = 0; i < 64; i++)
    r01++;
  /* { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*in_reduction\\(\\+:r02\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp simd\[^\n\r]*in_reduction\\(\\+:r02\\)" "gimple" } } */
  #pragma omp taskloop simd in_reduction(+:r02)
  for (int i = 0; i < 64; i++)
    r02++;
  /* FIXME: We don't support in_reduction clause on target yet, once we do, should
     add testcase coverage for all combined/composite constructs with target as leaf construct.  */
}
