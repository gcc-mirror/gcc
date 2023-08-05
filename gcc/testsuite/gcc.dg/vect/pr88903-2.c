#include "tree-vect.h"

int x[1024];
int y[1024];
int z[1024];

void __attribute__((noinline)) foo()
{
  for (int i = 0; i < 512; ++i)
    {
      x[2*i] = x[2*i] << y[2*i];
      x[2*i+1] = x[2*i+1] << y[2*i];
      z[2*i] = y[2*i];
      z[2*i+1] = y[2*i+1];
    }
}

int main()
{
  check_vect ();
  for (int i = 0; i < 1024; ++i)
    x[i] = i, y[i] = i % 8;
  foo ();
#pragma GCC novector
  for (int i = 0; i < 1024; ++i)
    if (x[i] != i << ((i & ~1) % 8))
      __builtin_abort ();
  return 0;
}
