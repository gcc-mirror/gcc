#include "tree-vect.h"

unsigned b[1024];

int __attribute__((noipa))
foo (int n)
{
  int res = 0;
  for (int i = 0; i < n; ++i)
    res = res > b[i] ? res : b[i];
  return res;
}

int main ()
{
  check_vect ();
  b[15] = (unsigned)__INT_MAX__ + 1;
  if (foo (16) != -__INT_MAX__ - 1)
    __builtin_abort ();
  return 0;
}
