// { dg-options "-w -ansi -pedantic-errors -funsigned-bitfields" }

extern "C" void abort (void);

#include "bitfield1.h"

void bitfield1_y (A& a)
{
  if (a.bitS != -1)
    abort ();
  if (a.bitU != 1)
    abort ();
  if (a.bit != 1)
    abort ();
}
