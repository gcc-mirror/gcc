/* { dg-additional-options "-O3" } */

#include "tree-vect.h"

signed char a, b, c, f;
int d, e, g, h;

int main()
{
  int j, k;
  signed char m;
  check_vect ();
  while (f < 4) {
    k = b = 3;
    for (; b >= 0; b--) {
      j = a < 0 ? a : a >> h;
      g = k;
      e = j;
      k = 0;
      while (1) {
        if (j)
          break;
        k = f == 0;
        break;
      }
    }
    m = g * 87;
    if (m < 70)
      __builtin_abort();
    return 0;
  }
}
