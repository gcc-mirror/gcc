#include <stdlib.h>

/* Define this to enable the combination of VSX vector double and
   SSE2 data types.  */
#define __VSX_SSE2__ 1

#include "m128-check.h"

/* define DEBUG replace abort with printf on error.  */
//#define DEBUG 1

#if 1

#define TEST sse2_test

static void sse2_test (void);

static void
__attribute__ ((noinline))
do_test (void)
{
  sse2_test ();
}

int
main ()
  {
#ifdef __BUILTIN_CPU_SUPPORTS__
    /* Most SSE2 (vector double) intrinsic operations require VSX
       instructions, but some operations may need only VMX
       instructions.  This also true for SSE2 scalar doubles as they
       imply that "other half" of the vector remains unchanged or set
       to zeros.  The VSX scalar operations leave ther "other half"
       undefined, and require additional merge operations.
       Some conversions (to/from integer) need the  direct register
       transfer instructions from POWER8 for best performance.
       So we test for arch_2_07.  */
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
#endif
