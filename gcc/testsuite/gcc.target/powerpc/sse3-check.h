#include <stdio.h>
#include <stdlib.h>

#include "m128-check.h"

/* define DEBUG replace abort with printf on error.  */
//#define DEBUG 1

#define TEST sse3_test

static void sse3_test (void);

static void
__attribute__ ((noinline))
do_test (void)
{
  sse3_test ();
}

int
main ()
{
#ifdef __BUILTIN_CPU_SUPPORTS__
  /* Most SSE intrinsic operations can be implemented via VMX
     instructions, but some operations may be faster / simpler
     using the POWER8 VSX instructions.  This is especially true
     when we are transferring / converting to / from __m64 types.
     The direct register transfer instructions from POWER8 are
     especially important.  So we test for arch_2_07.  */
  if (__builtin_cpu_supports ("arch_2_07"))
    {
      do_test ();
#ifdef DEBUG
      printf ("PASSED\n");
#endif
    }
#ifdef DEBUG
  else
    printf ("SKIPPED\n");
#endif
#endif /* __BUILTIN_CPU_SUPPORTS__ */
  return 0;
}
