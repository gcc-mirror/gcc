#include "bitfield5.h"

extern void bitfield5_y (B&);

void bitfield5_x ()
{
  B b;

  b.f3 = 7;
  b.f4 = 3;

  bitfield5_y (b);
}
