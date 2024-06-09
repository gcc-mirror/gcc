#include "tree-vect.h"

int data[4 * 16 * 16] __attribute__((aligned(16)));

__attribute__((noipa)) void
foo (__SIZE_TYPE__ n)
{
  for (__SIZE_TYPE__ i = 1; i < n; ++i)
    {
      data[i * n * 4] = data[(i - 1) * n * 4] + 1;
      data[i * n * 4 + 1] = data[(i - 1) * n * 4 + 1] + 2;
    }
}

int
main ()
{
  check_vect ();

  data[0] = 10;
  data[1] = 20;

  foo (3);

  if (data[24] != 12 || data[25] != 24)
    __builtin_abort ();
  return 0;
}
