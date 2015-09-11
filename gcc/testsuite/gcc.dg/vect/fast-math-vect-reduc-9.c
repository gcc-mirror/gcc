/* { dg-require-effective-target vect_float } */

#include "tree-vect.h"

float x[1024];
float
test (void)
{
  int i;
  float gosa = 0.0;
  for (i = 0; i < 1024; ++i)
    {
      float tem = x[i];
      gosa += tem * tem;
    }
  return gosa;
}

int main (void)
{
  check_vect ();

  if (test () != 0.0)
    abort ();

  return 0;
}

/* { dg-final { scan-tree-dump "vectorized 1 loops" "vect" } } */
