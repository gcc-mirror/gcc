/* { dg-options "-O3 -msve-vector-bits=128" } */

void
vset (int *restrict dst, int *restrict src, int count)
{
  for (int i = 0; i < count; ++i)
#pragma GCC unroll 4
    for (int j = 0; j < 4; ++j)
      *dst++ = 1;
}

/* { dg-final { scan-assembler-times {\tstr\tq} 1 } } */
