#include "tree-vect.h"

int b, c = 8, d;
int e[23];
int main()
{
  check_vect ();

  int *h = e;
  for (int i = 1; i < b + 21; i += 2)
    {
      c *= -1;
      d = h[i] ? i : 0;
    }
  if (c != 8)
    abort ();
  return 0;
}
