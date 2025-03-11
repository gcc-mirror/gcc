/* { dg-options "-O3 -msve-vector-bits=256" } */

void
vset (int *restrict dst, int *restrict src, int count)
{
  for (int i = 0; i < count; ++i)
#pragma GCC unroll 16
    for (int j = 0; j < 16; ++j)
      *dst++ = 1;
}

/* { dg-final { scan-assembler-times {\tst1w\tz} 2 {target aarch64_big_endian} } } */
/* { dg-final { scan-assembler-times {\tstr\tz} 2 {target aarch64_little_endian} } } */
/* { dg-final { scan-assembler-not {\tstp\tq} } } */
