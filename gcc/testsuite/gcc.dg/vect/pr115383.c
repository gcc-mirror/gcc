#include "tree-vect.h"

int __attribute__((noipa))
s331 (int i, int n)
{
  int j = 0;
  for (; i < n; i++)
    if ((float)i < 0.)
      j = i;
  return j;
}

int main()
{
  check_vect ();
  int j = s331(-13, 17);
  if (j != -1)
    abort ();
  return 0;
}
