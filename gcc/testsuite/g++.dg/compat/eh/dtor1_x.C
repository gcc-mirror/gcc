extern "C" void exit (int);
extern "C" void abort (void);

#include "dtor1.h"

int r;

void dtor1_x ()
{
  { B b; }
  if (r != 0)
    abort ();
  exit (0);
}
