/* { dg-require-effective-target int32plus } */

#include "tree-vect.h"

unsigned a;
short b, d, e;
int main()
{
  check_vect ();
  short h = d;
  short *z = &h;
  for (_Bool i = 0; i < 1; i = 1)
    for (unsigned j = 0; j < (z[i] ?: 10); j += 3)
      {
	a -= 9;
	b -= ~e;
      }
  if (a != 4294967260)
    __builtin_abort ();
}
