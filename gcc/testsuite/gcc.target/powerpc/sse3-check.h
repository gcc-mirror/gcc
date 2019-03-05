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
  do_test ();
#ifdef DEBUG
  printf ("PASSED\n");
#endif
  return 0;
}
