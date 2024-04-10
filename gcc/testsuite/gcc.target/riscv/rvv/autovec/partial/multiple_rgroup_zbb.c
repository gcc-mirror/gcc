/* { dg-do compile } *.
/* { dg-options "-march=rv64gcv_zbb -mabi=lp64d -O2 -mrvv-vector-bits=zvl -fno-schedule-insns -fno-schedule-insns2" } */

#include <stdint-gcc.h>

void __attribute__ ((noipa))
test (uint16_t *__restrict f, uint32_t *__restrict d, uint64_t *__restrict e,
      uint16_t x, uint16_t x2, uint16_t x3, uint16_t x4, uint32_t y,
      uint32_t y2, uint64_t z, int n)
{
  for (int i = 0; i < n; ++i)
    {
      f[i * 4 + 0] = x;
      f[i * 4 + 1] = x2;
      f[i * 4 + 2] = x3;
      f[i * 4 + 3] = x4;
      d[i * 2 + 0] = y;
      d[i * 2 + 1] = y2;
      e[i] = z;
    }
}

/* { dg-final { scan-assembler-times "vsetvli\tzero,\s*\[a-z0-9\]+,\s*e16,\s*m1,\s*ta,\s*ma" 4 } } */
