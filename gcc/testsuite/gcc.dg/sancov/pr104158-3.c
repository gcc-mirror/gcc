/* PR sanitizer/104158 */
/* { dg-do compile } */
/* { dg-options "-fsanitize-coverage=trace-cmp,trace-pc -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump "__sanitizer_cov_trace_cmp" "optimized" } } */
/* { dg-final { scan-tree-dump "__sanitizer_cov_trace_pc" "optimized" } } */

int
foo (int a, int b)
{
  return a == b;
}
