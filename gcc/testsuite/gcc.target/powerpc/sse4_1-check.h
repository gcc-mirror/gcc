#include <stdio.h>
#include <stdlib.h>

/* Define this to enable the combination of VSX vector double and
   SSE2 data types.  */
#define __VSX_SSE2__ 1

#include "m128-check.h"

//#define DEBUG 1

#define TEST sse4_1_test

static void sse4_1_test (void);

static void
__attribute__ ((noinline))
do_test (void)
{
  sse4_1_test ();
}

int
main ()
{
  do_test ();
#ifdef DEBUG
  printf ("PASSED\n");
#endif
  return 0;
}
