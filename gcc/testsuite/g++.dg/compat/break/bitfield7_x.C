// { dg-options "-w" }

#include "bitfield7.h"

extern void bitfield7_y (U*);

void bitfield7_x ()
{
  U u[2];

  u[0].i = 7;
  u[1].i = 8;

  bitfield7_y (u);
}
