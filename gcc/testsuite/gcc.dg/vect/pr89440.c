/* { dg-additional-options "-ffast-math" } */

#include "tree-vect.h"

float __attribute__((noinline,noclone))
f (float x)
{
  int i;
  float j;
  float a = 0;
  for (i = 0; i < 4; ++i)
    {
      for (j = 0; j < 4; ++j)
	{
	  a += 1;
	  x += a;
	}
    }
  return x;
}

int
main()
{
  check_vect ();
  if (f (1.0f) != 137.0f)
    abort ();
  return 0;
}

/* { dg-final { scan-tree-dump "OUTER LOOP VECTORIZED" "vect" { target vect_float } } } */
