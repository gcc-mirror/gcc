/* { dg-do run } */
/* { dg-require-effective-target power10_hw } */
/* { dg-options "-mdejagnu-cpu=power10 -O2" } */

/* This test will only run when the power10_hw_available test passes.
   If that test passes, then we expect to see that the cpu is Power10.
   If this is not the case, then the test environment has problems.
   If in the future there are cpus that pass the power10_hw test but
   are not power10, they will need to be added to this check. */

#include <stdio.h>
#include <stdlib.h>

int
main (int argc, char *argv[])
{
  int ret = 0;
#ifdef __BUILTIN_CPU_SUPPORTS__
  if ( !__builtin_cpu_is ("power10"))
    {
      printf ("Error: __builtin_cpu_is says this is not power10, but power10_hw test passed.\n");
      ret++;
    }
#endif
  return ret;
}
