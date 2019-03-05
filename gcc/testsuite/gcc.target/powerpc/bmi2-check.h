#include <stdio.h>
#include <stdlib.h>

static void bmi2_test (void);

static void
__attribute__ ((noinline))
do_test (void)
{
  bmi2_test ();
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

