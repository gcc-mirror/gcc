#include "tree-vect.h"

int __attribute__((noipa))
foo (int *f, int n)
{
  int res = 0;
  for (int i = 0; i < n; ++i)
    {
      if (f[2*i])
        res = 2;
      if (f[2*i+1])
        res = -2;
    }
  return res;
}

int f[] = { 1, 1, 1, 1, 1, 1, 1, 1,
            1, 1, 1, 1, 1, 1, 1, 0 };

int
main ()
{
  if (foo (f, 8) != 2)
    __builtin_abort ();
  return 0;
}
