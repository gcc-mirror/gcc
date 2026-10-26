#include "tree-vect.h"

int __attribute__((noipa))
f(int j)
{
  int s = 0, k = 1;
  for (int i = 0; i < 16; i++)
    {
      s += j + k;
      j += 1;
      k *= 2;
    }
  return s;
}

int main(void)
{
  check_vect ();

  if (f (0) != 65655)
    __builtin_abort ();
  return 0;
}
