#include <stdlib.h>
#include "../bf16-helper.h"

static void do_test (void);

int
main ()
{

  if (__builtin_cpu_supports ("avx512f"))
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
