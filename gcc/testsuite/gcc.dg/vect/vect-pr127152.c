/* { dg-additional-options "--param vect-partial-vector-usage=0" } */
/* { dg-additional-options "-march=armv8-a+sve" { target aarch64-*-* } } */

#include "tree-vect.h"

__attribute__((noipa)) float
f (float *a, int n)
{
  float s = 0.0f;
  for (int i = 0; i < n; i++)
    if (a[i] > 0.0f)
      s += a[i];
  return s;
}

int
main (void)
{
  float a[16];

  check_vect ();

#pragma GCC novector
  for (int i = 0; i < 16; i++)
    a[i] = (i & 1) ? 1.0f : -1.0f;
  if (f (a, 16) != 8.0f)
    __builtin_abort ();
  return 0;
}
