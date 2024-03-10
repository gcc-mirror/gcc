#include <stdlib.h>
#include "m256-check.h"

static void sha512_test (void);

static unsigned long long
ror64 (unsigned long long w, int n)
{
  int count = n % 64;
  return ((w >> n) | (w << (64 - n)));
}

static unsigned long long
shr64 (unsigned long long w, int n)
{
  return (w >> n);
}

static void
__attribute__ ((noinline))
do_test(void)
{
  sha512_test ();
}

int
main ()
{
  /* Check CPU support for SHA512.  */
  if (__builtin_cpu_supports ("sha512"))
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
