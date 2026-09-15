/* { dg-additional-options "-O3" } */

#include "tree-vect.h"

int __attribute__((noipa))
fn1 (void)
{
  int b = 0;
  for (int f = 0; f < 16; f++)
    switch (f % 3) {
    case 2:
      b += f;
      break;
    case 0:
      b |= f;
    case 1:
      b |= f;
    }
  return b;
}
int main()
{
  check_vect ();
  if (fn1 () != 63)
    __builtin_abort ();
  return 0;
}

