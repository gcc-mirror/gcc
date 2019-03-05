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
    do_test ();
#ifdef DEBUG
    printf ("PASSED\n");
#endif
    return 0;
  }
