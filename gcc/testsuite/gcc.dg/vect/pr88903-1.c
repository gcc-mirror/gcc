#include "tree-vect.h"

int x[1024];

void __attribute__((noinline))
foo()
{
  for (int i = 0; i < 512; ++i)
    {
      x[2*i] = x[2*i] << ((i+1) & 31);
      x[2*i+1] = x[2*i+1] << ((i+1) & 31);
    }
}

int
main()
{
  check_vect ();
  for (int i = 0; i < 1024; ++i)
    x[i] = i;
  foo ();
#pragma GCC novector
  for (int i = 0; i < 1024; ++i)
    if (x[i] != i << ((i/2+1) & 31))
      __builtin_abort ();
  return 0;
}
