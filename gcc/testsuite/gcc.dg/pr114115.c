/* { dg-do compile } */
/* { dg-options "-O0 -fprofile-generate -fdump-tree-optimized" } */
/* { dg-require-profiling "-fprofile-generate" } */
/* { dg-require-ifunc "" } */

void *foo_ifunc2() __attribute__((ifunc("foo_resolver")));

void bar(void)
{
}

static int f3()
{
  bar ();
  return 5;
}

void (*foo_resolver(void))(void)
{
  f3();
  return bar;
}

/* { dg-final { scan-tree-dump-not "__gcov_indirect_call_profiler_v" "optimized" } } */
