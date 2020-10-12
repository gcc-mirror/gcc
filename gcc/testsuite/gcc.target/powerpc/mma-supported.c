/* { dg-do run } */
/* { dg-require-effective-target ppc_mma_hw } */
/* { dg-options "-mdejagnu-cpu=power10 -O2" } */

/* This test will only run when the ppc_mma_hw test passes.  If that
   test passes, then we expect to see that mma feature is supported.
   If this is not the case, then the test environment has problems. */

#include <stdio.h>
#include <stdlib.h>

int
main (int argc, char *argv[])
{
#ifdef __BUILTIN_CPU_SUPPORTS__
  if ( !__builtin_cpu_supports ("mma"))
    {
#ifdef DEBUG      
      printf ("Error: __builtin_cpu_supports says mma not supported, but ppc_mma_hw test passed.\n");
#endif
      abort();
    }
#endif
  exit (0);
}
