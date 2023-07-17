#include <stdlib.h>
#include "m128-check.h"

static void sm3_test (void);

static unsigned
rol32 (unsigned w, int n)
{
  int count = n % 32;
  return ((w << n) | (w >> (32 - n)));
}

static void
__attribute__ ((noinline))
do_test (void)
{
  sm3_test ();
}

int
main ()
{
  /* Run SM3 test only if host has SM3 support.  */
  if (__builtin_cpu_supports ("sm3"))
    {
      do_test ();
#ifdef DEBUG
      printf ("PASSED\n");
#endif
      return 0;
    }

#ifdef DEBUG
  printf ("SKIPPED\n");
#endif
  return 0;
}
