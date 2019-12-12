/* { dg-require-effective-target vect_int } */

#include "tree-vect.h"

int a[8][10] = { [2][5] = 4 }, c;

int
main ()
{
  short b;
  int i, d;
  check_vect ();
  for (b = 4; b >= 0; b--)
    for (c = 0; c <= 6; c++)
      a[c + 1][b + 2] = a[c][b + 1];
  for (i = 0; i < 8; i++)
    for (d = 0; d < 10; d++)
      if (a[i][d] != (i == 3 && d == 6) * 4)
	__builtin_abort ();
  return 0;
}
