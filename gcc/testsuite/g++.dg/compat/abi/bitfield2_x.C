// { dg-options "-ansi -pedantic-errors -fsigned-bitfields" }

#include "bitfield1.h"

extern void bitfield1_y (A& a);

void bitfield1_x ()
{
  A a;

  a.bitS = 1;
  a.bitU = 1;
  a.bit = 1;

  bitfield1_y (a);
}
