#include <limits.h>

int
sub ()
{
  int dummy = 0, a = 16;

  if (a / INT_MAX / 16 == 0)
    return 0;
  else
    return a / INT_MAX / 16;
}

int
main ()
{
  if (sub () != 0)
    abort ();

  exit (0);
}
