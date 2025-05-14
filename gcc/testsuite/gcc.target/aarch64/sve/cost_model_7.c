/* { dg-options "-O3 -msve-vector-bits=512" } */

void
vset (int *restrict dst, int *restrict src, int count)
{
  for (int i = 0; i < count; ++i)
#pragma GCC unroll 32
    for (int j = 0; j < 32; ++j)
      *dst++ = 1;
}

/* { dg-final { scan-assembler-times {\tst1w\tz} 2 {target aarch64_big_endian} } } */
/* { dg-final { scan-assembler-times {\tstr\tz} 2 {target aarch64_little_endian} } } */
