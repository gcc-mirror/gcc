/* { dg-options "-O3 -msve-vector-bits=128" } */

void
vset (int *restrict dst, int *restrict src, int count)
{
  for (int i = 0; i < count; ++i)
#pragma GCC unroll 8
    for (int j = 0; j < 8; ++j)
      *dst++ = 1;
}

/* { dg-final { scan-assembler-not {\tst1w\tz} } } */
/* { dg-final { scan-assembler-times {\tstp\tq} 1 } } */
