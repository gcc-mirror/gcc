#include "pr39188-1.h"

extern "C" void abort ();

extern int x (int);

int
main (void)
{
  if (x (1) != 0)
    abort ();
  if (f (1) != 1)
    abort ();
  return 0;
}
