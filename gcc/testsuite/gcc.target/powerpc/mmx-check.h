#include <stdio.h>
#include <stdlib.h>

static void mmx_test (void);

static void
__attribute__ ((noinline))
do_test (void)
{
  mmx_test ();
}

int
main ()
  {
#ifdef __BUILTIN_CPU_SUPPORTS__
    /* Many MMX intrinsics are simpler / faster to implement by
       transferring the __m64 (long int) to vector registers for SIMD
       operations.  To be efficient we also need the direct register
       transfer instructions from POWER8.  So we can test for
       arch_2_07.  */
    if ( __builtin_cpu_supports ("arch_2_07") )
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
