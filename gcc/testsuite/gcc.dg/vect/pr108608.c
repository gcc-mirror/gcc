#include "tree-vect.h"

double __attribute__((noipa))
foo (double m, float *ptr)
{
  for (int i = 0; i < 256; i++)
    m = __builtin_fmax (m, ptr[i]);
  return m;
}

int
main (void)
{
  check_vect ();
  float ptr[256];
#pragma GCC novector
  for (int j = 0; j < 16; ++j)
    {
      for (int i = 0; i < 256; ++i)
	ptr[i] = i == 128 + j ? 2 + j : i == 161 ? 1 : 0;
      if (foo (0, ptr) != 2 + j)
	__builtin_abort ();
    }
  return 0;
}
