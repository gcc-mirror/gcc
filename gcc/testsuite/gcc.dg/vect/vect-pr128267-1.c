#include "tree-vect.h"

__attribute__((noipa)) void
foo(signed char *a)
{
  signed char b = 64;
  for (unsigned i = 0; i < 257; ++i)
    {
      a[i] = b;
      b >>= 1;
    }
}

int main(void)
{
  signed char a[257];
  check_vect ();
  foo(a);
  if (a[256] != 0)
    __builtin_abort ();
  return 0;
}
