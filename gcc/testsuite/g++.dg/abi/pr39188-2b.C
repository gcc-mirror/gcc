#include "pr39188-2.h"

extern "C" void abort ();

extern int x (int);

int
main (void)
{
  if (x (1) != 0)
    abort ();
  if (f<int> (1) != 1)
    abort ();
  return 0;
}
