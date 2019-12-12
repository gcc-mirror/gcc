#include <stdio.h>
#include <stdlib.h>

#include "m128-check.h"

//#define DEBUG 1

#define TEST ssse3_test

static void ssse3_test (void);

static void
__attribute__ ((noinline))
do_test (void)
{
  ssse3_test ();
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
