#include "tree-vect.h"

extern void abort (void);

int a, b[8] = { 2, 0, 0, 0, 0, 0, 0, 0 }, c[8];

int
main ()
{
  int d;
  check_vect ();
  for (; a < 8; a++)
    {
      d = b[a] >> 1;
      c[a] = d != 0;
    }
  if (c[0] != 1)
    abort ();
  return 0;
}

