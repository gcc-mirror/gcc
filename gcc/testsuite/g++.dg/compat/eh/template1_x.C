extern "C" void exit (int);
extern "C" void abort (void);

#include "template1.h"

void template1_x ()
{
  int caught = 0;
  try
    {
      C<int> x;
      x.f();
    }
  catch (A)
    {
      ++caught;
    }
  if (caught != 1)
    abort ();
  exit (0);
}
