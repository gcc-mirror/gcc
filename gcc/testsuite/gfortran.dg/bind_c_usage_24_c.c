/* Compiled and linked by bind_c.f90.  */

#include <stdlib.h>
#include <stdbool.h>

void subtest (bool, int *);

void
c_proc (bool present, int *val)
{
  int val2;
  if (!present && val)
    abort ();
  else if (present)
    {
      if (!val) abort ();
      if (*val != 4) abort ();
      *val = 7;
    }

  val2 = 43;
  subtest (1, &val2);
  subtest (0, NULL);
  if (val2 != -45) abort ();
}
