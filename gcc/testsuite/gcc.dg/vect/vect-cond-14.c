#include "tree-vect.h"

short a[256];
short b[256];
short c[256];
_Bool pb[256];

void __attribute__((noipa))
predicate_by_bool()
{
  for (int i = 0; i < 256; i++)
    c[i] = pb[i] ? a[i] : b[i];
}

int
main ()
{
  check_vect ();

#pragma GCC novector
  for (int i = 0; i < 256; i++)
    {
      a[i] = i;
      b[i] = -i;
      pb[i] = (i % 3) == 0;
    }

  predicate_by_bool();

#pragma GCC novector
  for (int i = 0; i < 256; i++)
    if (c[i] != (pb[i] ? a[i] : b[i]))
      abort ();

  return 0;
}

/* { dg-final { scan-tree-dump "optimized: loop vectorized" "vect" { target { vect_unpack && vect_condition } } } } */
