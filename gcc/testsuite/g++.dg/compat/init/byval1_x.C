#include "byval1.h"

extern "C" void abort (void);
extern void Foo (C c);
extern int r;

void
byval1_x ()
{       
  C c;

  Foo (c);
  if (r != 0)
    abort ();
}
