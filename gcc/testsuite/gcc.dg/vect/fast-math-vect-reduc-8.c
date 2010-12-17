/* { dg-do compile } */
/* { dg-require-effective-target vect_float } */

#include "tree-vect.h"

extern float x[128] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
extern float y[128] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
extern float z[128] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));

float f (unsigned n)
{
  float ret = 0.0;
  unsigned i;
  for (i = 0; i < n; i++)
    {
      float diff = x[i] - y[i];
      ret -= diff * diff * z[i];
    }
  return ret;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
