#include "elide1.h"

extern "C" void abort (void);
extern void f (A);
extern int d;

void
elide1_x (void)
{
  int r;
  f (A ()), r = d;

  if (r >= d || !d)
    abort ();
}
