extern "C" void abort (void);

#include "vbase10.h"

void A::f () {}
B::B() {}

void vbase10_y (C& c)
{
  if (c.c1 != 1)
    abort ();
  if (c.c2 != 2)
    abort ();
}
