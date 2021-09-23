/* { dg-do compile } */
/* { dg-options "-fsanitize-coverage=trace-pc -fdump-tree-optimized" } */

void foo(void)
{
}

void
__attribute__((no_sanitize_coverage))
bar(void)
{
}

static void inline
__attribute__((always_inline))
inline_fn(void)
{
}

void
__attribute__((no_sanitize_coverage))
baz(void)
{
  inline_fn();
}

/* { dg-final { scan-tree-dump-times "__builtin___sanitizer_cov_trace_pc \\(\\)" 1 "optimized" } } */
