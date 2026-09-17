#include "tree-vect.h"

__attribute__ ((noipa)) void
f (int *px, int n)
{
  int i;
  for (i = 0; i < n; i++, px += 2)
    {
      px[0] = i;
      px[-2] = i + 100;
      px[1] = 7;
    }
}

int a[256];

int
main (void)
{
  check_vect ();

  f (a + 2, 64);

  for (int k = 1; k < 64; k++)
    if (a[2 * k] != k + 100)
      __builtin_abort ();

  return 0;
}
