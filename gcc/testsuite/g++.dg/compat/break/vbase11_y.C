extern "C" void abort (void);

#include "vbase11.h"

void vbase11_y (derived& d)
{
  if (d.foo() != 2)
    abort ();
  if (d.bar() != 3)
    abort ();
}
