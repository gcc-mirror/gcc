/* { dg-do compile } */
/* { dg-additional-options "-march=rv32gcv -mabi=ilp32d -mrvv-vector-bits=scalable -fdump-tree-optimized-details" } */

#include <stdint-gcc.h>

void __attribute__ ((noipa))
f (float *__restrict f, double *__restrict d, int n)
{
  for (int i = 0; i < n; ++i)
    {
      f[i * 2 + 0] = 1;
      f[i * 2 + 1] = 2;
      d[i] = 3;
    }
}
