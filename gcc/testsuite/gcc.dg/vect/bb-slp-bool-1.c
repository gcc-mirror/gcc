#include "tree-vect.h"

void __attribute__ ((noipa))
f1 (_Bool *x, unsigned short *y)
{
  x[0] = (y[0] == 1);
  x[1] = (y[1] == 1);
}

void __attribute__ ((noipa))
f2 (_Bool *x, unsigned short *y)
{
  x[0] = (y[0] == 1);
  x[1] = (y[1] == 1);
  x[2] = (y[2] == 1);
  x[3] = (y[3] == 1);
  x[4] = (y[4] == 1);
  x[5] = (y[5] == 1);
  x[6] = (y[6] == 1);
  x[7] = (y[7] == 1);
}

_Bool x[8];
unsigned short y[8] = { 11, 1, 9, 5, 1, 44, 1, 1 };

int
main (void)
{
  check_vect ();

  f1 (x, y);

  if (x[0] || !x[1])
    __builtin_abort ();

  x[1] = 0;

  f2 (x, y);

  if (x[0] || !x[1] || x[2] | x[3] || !x[4] || x[5] || !x[6] || !x[7])
    __builtin_abort ();

  return 0;
}
