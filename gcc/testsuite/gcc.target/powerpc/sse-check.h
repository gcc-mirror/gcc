#include <stdlib.h>
#include "m128-check.h"

// #define DEBUG 1

#define TEST sse_test

static void sse_test (void);

static void
__attribute__ ((noinline))
do_test (void)
{
  sse_test ();
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

