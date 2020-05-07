/* { dg-options "-O3 -msve-vector-bits=scalable" } */

void
vset (int *restrict dst, int *restrict src, int count)
{
  for (int i = 0; i < count; ++i)
#pragma GCC unroll 4
    for (int j = 0; j < 4; ++j)
      *dst++ = 1;
}

/* { dg-final { scan-assembler-times {\tst1w\tz} 1 } } */
