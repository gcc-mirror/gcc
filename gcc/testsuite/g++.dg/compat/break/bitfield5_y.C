extern "C" void abort (void);

#include "bitfield5.h"

void A::f () {}

void bitfield5_y (B& b)
{
  if (b.f3 != 7)
    abort ();
  if (b.f4 != 3)
    abort ();
}
