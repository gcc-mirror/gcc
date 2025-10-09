#include "tree-vect.h"

short p[128];

bool __attribute__((noipa))
fxort (int n)
{
  bool r = true;
  for (int i = 0; i < n; ++i)
    r ^= (p[i] != 0);
  return r;
}

bool __attribute__((noipa))
fxorf (int n)
{
  bool r = false;
  for (int i = 0; i < n; ++i)
    r ^= (p[i] != 0);
  return r;
}

int main()
{
  check_vect ();

  __builtin_memset (p, 1, sizeof(p));

  for (int n = 0; n < 77; ++n)
    if (fxort (n) != !(n & 1))
      abort ();

  for (int n = 0; n < 77; ++n)
    if (fxorf (n) != (n & 1))
      abort ();

  __builtin_memset (p, 0, sizeof(p));

  for (int n = 0; n < 77; ++n)
    if (!fxort (n))
      abort ();

  for (int n = 0; n < 77; ++n)
    if (fxorf (n))
      abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "optimized: loop vectorized" 2 "vect" { target { vect_int && vect_condition } } } } */
