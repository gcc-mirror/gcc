/* { dg-do run } */
/* { dg-require-effective-target power10_hw } */
/* { dg-options "-mdejagnu-cpu=power10 -O2" } */

/* This test will only run when the power10_hw_available test passes.
   If that test passes, then we expect to see that ISA 3.1 is
   supported.  If this is not the case, then the test environment has
   problems.  */

#include <stdio.h>
#include <stdlib.h>

int
main (int argc, char *argv[])
{
  int ret = 0;
#ifdef __BUILTIN_CPU_SUPPORTS__
  if ( !__builtin_cpu_supports ("arch_3_1"))
    {
      printf ("Error: __builtin_cpu_supports says arch_3_1 not supported, but power10_hw test passed.\n");
      ret++;
    }
#endif
  return ret;
}
