// { dg-options "-w" }

extern "C" void abort (void);

#include "bitfield7.h"

void bitfield7_y (U* u)
{
  if (u[0].i != 7)
    abort ();
  if (u[1].i != 8)
    abort ();
}
