/* { dg-do run } */

#include "tree-vect.h"

int a = 0;
static int b = 0;
long c = 0;

int
main()
{
  check_vect ();
  for (int d = 0; d < 8; d++)
    {
      a ^= c;
      b = a;
      a ^= 1;
    }
  if (a != 0 || b != 1)
    __builtin_abort();
  return 0;
}
