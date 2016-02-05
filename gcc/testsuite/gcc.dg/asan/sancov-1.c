/* Test coverage/asan interaction:
     - coverage instruments __asan_init ctor (thus 4 covarage callbacks)
     - coverage does not instrument asan-emitted basic blocks
     - asan considers coverage callback as "nonfreeing" (thus 1 asan store
       callback.  */
/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-flto" } { "" } } */
/* { dg-options "-fsanitize-coverage=trace-pc -fsanitize=address -fdump-tree-optimized" } */

void foo(volatile int *a, int *b)
{
  *a = 1;
  if (*b)
    *a = 2;
}

/* { dg-final { scan-tree-dump-times "__builtin___sanitizer_cov_trace_pc \\(\\)" 4 "optimized" } } */
/* { dg-final { scan-tree-dump-times "__builtin___asan_report_load4 \\(" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "__builtin___asan_report_store4 \\(" 1 "optimized" } } */
