/* { dg-additional-options "-O3" } */

#include "tree-vect.h"

long b = 1;
signed char c;
int d[25];

int main()
{
  check_vect ();
  for (signed char g = 0; g < 8; g += 1)
    for (short h = 0; h < 25; h += 2) {
      b *= -1;
      c ^= d[h];
    }
  if (b != 1)
    abort ();
  return 0;
}
