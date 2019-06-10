/* { dg-require-effective-target global_constructor } */
/* { dg-options "-O2 -fprofile-generate -fprofile-update=single -fdump-tree-optimized" } */

__attribute__ ((no_profile_instrument_function))
int foo()
{
  return 0;
}

__attribute__ ((no_profile_instrument_function))
int bar()
{
  return 1;
}

int main ()
{
  return foo ();
}

/* { dg-final { scan-tree-dump-times "__gcov0\\.main.* = PROF_edge_counter" 1 "optimized"} } */
/* { dg-final { scan-tree-dump-times "__gcov_indirect_call_profiler_v" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "__gcov_time_profiler_counter = " 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "__gcov_init" 1 "optimized" } } */
