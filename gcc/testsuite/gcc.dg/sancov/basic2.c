/* Basic test on number of inserted callbacks.  */
/* { dg-do compile } */
/* { dg-options "-fsanitize-coverage=trace-pc -fdump-tree-optimized" } */

void foo(int *a, int *b, int *c, int *d)
{
  *a = 1;
  if (*b)
    *c = 2;
  else
    *d = 3;
}

/* { dg-final { scan-tree-dump-times "__builtin___sanitizer_cov_trace_pc \\(\\)" 4 "optimized" } } */
