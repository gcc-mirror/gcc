#include "tree-vect.h"

int p[128];

bool __attribute__((noipa))
fand (int n)
{
  bool r = true;
  for (int i = 0; i < n; ++i)
    r &= (p[i] != 0);
  return r;
}

bool __attribute__((noipa))
fior (int n)
{
  bool r = false;
  for (int i = 0; i < n; ++i)
    r |= (p[i] != 0);
  return r;
}

int main()
{
  check_vect ();

  __builtin_memset (p, 1, sizeof(p));

  for (int n = 0; n < 77; ++n)
    if (!fand (n))
      abort ();

  p[0] = 0;
  for (int n = 1; n < 77; ++n)
    if (fand (n))
      abort ();

  __builtin_memset (p, 0, sizeof(p));

  for (int n = 0; n < 77; ++n)
    if (fior (n))
      abort ();

  p[0] = 1;
  for (int n = 1; n < 77; ++n)
    if (!fior (n))
      abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "optimized: loop vectorized" 2 "vect" { target { vect_int && vect_condition } } } } */
