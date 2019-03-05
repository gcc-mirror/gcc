#include <stdio.h>
#include <stdlib.h>

static void bmi_test (void);

static void
__attribute__ ((noinline))
do_test (void)
{
  bmi_test ();
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
